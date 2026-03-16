# did_imputation and event_plot for R
# Approximate R equivalents of the Stata commands from Borusyak, Jaravel, and Spiess (2023).

.did_imputation_assert_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install it with install.packages('%s').", pkg, pkg), call. = FALSE)
  }
}

.did_imputation_as_name <- function(x) {
  if (length(x) != 1) stop("Expected a single variable name.", call. = FALSE)
  as.character(x)
}

.did_imputation_to_fixest_fe <- function(fe_terms) {
  if (length(fe_terms) == 0) return("0")
  gsub("#", "^", fe_terms, fixed = TRUE)
}

.did_imputation_make_weights <- function(dt, D_col, touse_col, wtr, wei_col) {
  if (is.null(wtr)) {
    dt[, .__w_base := data.table::fifelse(get(D_col) == 1 & get(touse_col), 1, 0)]
    return(list(cols = ".__w_base", names = "tau", wtr_count_init = 0L))
  }

  if (is.character(wtr)) {
    missing_vars <- setdiff(wtr, names(dt))
    if (length(missing_vars) > 0) {
      stop(sprintf("wtr variables not found: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
    }
    cols <- paste0(".__w_", seq_along(wtr))
    for (j in seq_along(wtr)) {
      dt[, (cols[j]) := get(wtr[j])]
    }
    nm <- if (length(wtr) == 1) "tau" else paste0("tau_", wtr)
    return(list(cols = cols, names = nm, wtr_count_init = length(wtr)))
  }

  if (is.numeric(wtr) && length(wtr) == nrow(dt)) {
    dt[, .__w_base := as.numeric(wtr)]
    return(list(cols = ".__w_base", names = "tau", wtr_count_init = 1L))
  }

  stop("wtr must be NULL, a character vector of column names, or a numeric vector of length nrow(data).", call. = FALSE)
}

.did_imputation_expand_horizons <- function(dt, K_col, w_cols, w_names, horizons, all_horizons, hbalance, i_col, D_col, touse_col, wei_col) {
  if (length(w_cols) > 1 && (!is.null(horizons) || isTRUE(all_horizons))) {
    stop("horizons/all_horizons cannot be combined with multiple wtr variables.", call. = FALSE)
  }

  if (isTRUE(all_horizons)) {
    if (!is.null(horizons)) stop("Use either horizons or all_horizons, not both.", call. = FALSE)
    horizons <- sort(unique(dt[get(touse_col) & get(D_col) == 1 & !is.na(get(K_col)), get(K_col)]))
    horizons <- horizons[horizons >= 0]
  }

  if (is.null(horizons)) {
    return(list(cols = w_cols, names = w_names, horizons = NULL))
  }

  if (any(horizons < 0) || any(abs(horizons - round(horizons)) > 1e-9)) {
    stop("horizons must contain non-negative integers.", call. = FALSE)
  }
  horizons <- as.integer(sort(unique(horizons)))

  base <- w_cols[1]

  if (isTRUE(hbalance)) {
    dt[, .__in_h := get(K_col) %in% horizons]
    n_h <- length(horizons)
    dt[, .__num_h_i := sum(.__in_h), by = i_col]
    dt[get(touse_col), (base) := data.table::fifelse(.__in_h & .__num_h_i >= n_h, get(base), 0)]

    tmp <- dt[get(touse_col) & get(D_col) == 1 & .__in_h & .__num_h_i >= n_h, .(
      min_w = min(get(base) * get(wei_col), na.rm = TRUE),
      max_w = max(get(base) * get(wei_col), na.rm = TRUE)
    ), by = i_col]
    if (nrow(tmp) > 0 && any(tmp$max_w > 1.000001 * tmp$min_w, na.rm = TRUE)) {
      stop("With hbalance, weights must be constant across selected horizons within unit.", call. = FALSE)
    }
    dt[, c(".__in_h", ".__num_h_i") := NULL]
  }

  out_cols <- character(length(horizons))
  out_names <- character(length(horizons))
  for (j in seq_along(horizons)) {
    h <- horizons[j]
    col <- paste0(".__w_h", h)
    dt[, (col) := get(base) * as.numeric(get(K_col) == h)]
    out_cols[j] <- col
    out_names[j] <- paste0("tau", h)
  }

  return(list(cols = out_cols, names = out_names, horizons = horizons))
}

.did_imputation_split_het <- function(dt, w_cols, w_names, hetby, D_col, touse_col) {
  if (is.null(hetby)) return(list(cols = w_cols, names = w_names))
  if (!hetby %in% names(dt)) stop(sprintf("hetby variable '%s' not found.", hetby), call. = FALSE)

  vals <- unique(dt[get(touse_col) & get(D_col) == 1, get(hetby)])
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) stop("hetby is always missing among treated observations.", call. = FALSE)
  if (length(vals) > 30) stop("hetby takes too many values (>30).", call. = FALSE)

  new_cols <- character(0)
  new_names <- character(0)
  for (j in seq_along(w_cols)) {
    for (g in vals) {
      safe_g <- gsub("[^A-Za-z0-9_]+", "_", as.character(g))
      col <- paste0(w_cols[j], "_g_", safe_g)
      dt[, (col) := data.table::fifelse(get(hetby) == g, get(w_cols[j]), 0)]
      new_cols <- c(new_cols, col)
      new_names <- c(new_names, paste0(w_names[j], "_", as.character(g)))
    }
  }

  return(list(cols = new_cols, names = new_names))
}

.did_imputation_project_weights <- function(dt, w_cols, w_names, project, D_col, touse_col, wei_col) {
  if (is.null(project)) return(list(cols = w_cols, names = w_names))

  proj_cols <- character(0)
  proj_names <- character(0)

  for (j in seq_along(w_cols)) {
    use_idx <- which(dt[[touse_col]] & dt[[D_col]] == 1 & !is.na(dt[[w_cols[j]]]) & dt[[w_cols[j]]] > 0)
    if (length(use_idx) == 0) next

    X <- stats::model.matrix(stats::as.formula(paste("~", paste(project, collapse = " + "))), data = dt[use_idx])
    wei <- dt[[wei_col]][use_idx]

    for (k in seq_len(ncol(X))) {
      target <- X[, k]
      others <- X[, -k, drop = FALSE]
      if (ncol(others) == 0) {
        resid <- target
      } else {
        fit <- stats::lm.wfit(x = others, y = target, w = wei)
        resid <- fit$residuals
      }

      denom <- sum(wei * resid^2, na.rm = TRUE)
      if (!is.finite(denom) || denom < 1e-8) next

      col <- paste0(".__w_proj_", j, "_", k)
      dt[, (col) := 0]
      dt[use_idx, (col) := resid / denom]

      nm <- colnames(X)[k]
      if (nm == "(Intercept)") nm <- "cons"
      proj_cols <- c(proj_cols, col)
      proj_names <- c(proj_names, paste0(w_names[j], "_", nm))
    }
  }

  if (length(proj_cols) == 0) {
    stop("Projection failed (likely collinearity).", call. = FALSE)
  }

  list(cols = proj_cols, names = proj_names)
}

.did_imputation_effective_n <- function(w, wei) {
  keep <- is.finite(w) & is.finite(wei) & wei > 0
  if (!any(keep)) return(0)
  abs_w <- abs(w[keep] * wei[keep])
  s <- sum(abs_w)
  if (s <= 0) return(0)
  p <- abs_w / s
  1 / sum(p^2)
}

.did_imputation_cluster_vcov_tau <- function(dt, coef_names, w_cols, coef_vals, effect_col, D_col, touse_col, cluster_col, wei_col) {
  use <- dt[[touse_col]] & dt[[D_col]] == 1
  if (!any(use)) {
    v <- matrix(NA_real_, nrow = length(w_cols), ncol = length(w_cols))
    dimnames(v) <- list(coef_names, coef_names)
    return(v)
  }

  dt_use <- dt[use, c(cluster_col, effect_col, wei_col, w_cols), with = FALSE]
  clus <- unique(dt_use[[cluster_col]])
  G <- length(clus)
  if (G < 2) {
    v <- matrix(NA_real_, nrow = length(w_cols), ncol = length(w_cols))
    dimnames(v) <- list(coef_names, coef_names)
    return(v)
  }

  phi <- matrix(0, nrow = G, ncol = length(w_cols))
  colnames(phi) <- coef_names
  rownames(phi) <- as.character(clus)

  for (j in seq_along(w_cols)) {
    tmp <- dt_use[[w_cols[j]]] * dt_use[[wei_col]] * (dt_use[[effect_col]] - coef_vals[j])
    agg <- tapply(tmp, dt_use[[cluster_col]], sum, na.rm = TRUE)
    phi[names(agg), j] <- agg
  }

  v <- stats::cov(phi)
  v <- v * G / (G - 1)
  dimnames(v) <- list(coef_names, coef_names)
  v
}

.did_imputation_vcov_fixest <- function(fit, cluster) {
  if (is.null(fit)) return(NULL)
  if (is.null(cluster)) {
    return(as.matrix(stats::vcov(fit)))
  }
  as.matrix(fixest::vcov(fit, cluster = stats::as.formula(paste0("~", cluster))))
}

#' did_imputation
#'
#' R equivalent of Stata's did_imputation command.
#' Implements imputation-based staggered DiD estimation with horizon-specific effects,
#' pre-trend testing, and plotting-compatible output.
#'
#' @return An object of class did_imputation with elements:
#'   coefficients, vcov, Nt, Nc, pre_F, pre_p, pre_df, effect, cannot_impute, call.
did_imputation <- function(data,
                           y,
                           i,
                           t,
                           ei,
                           wtr = NULL,
                           sum = FALSE,
                           horizons = NULL,
                           all_horizons = FALSE,
                           hbalance = FALSE,
                           hetby = NULL,
                           project = NULL,
                           minn = 30,
                           shift = 0,
                           autosample = FALSE,
                           fe = NULL,
                           controls = NULL,
                           unit_controls = NULL,
                           time_controls = NULL,
                           cluster = NULL,
                           weights = NULL,
                           pretrends = 0,
                           alpha = 0.05,
                           se = TRUE,
                           delta = 1,
                           verbose = FALSE) {
  .did_imputation_assert_installed("data.table")
  .did_imputation_assert_installed("fixest")

  dt <- data.table::as.data.table(data)

  y <- .did_imputation_as_name(y)
  i <- .did_imputation_as_name(i)
  t <- .did_imputation_as_name(t)
  ei <- .did_imputation_as_name(ei)

  req <- c(y, i, t, ei)
  if (!is.null(controls)) req <- c(req, controls)
  if (!is.null(unit_controls)) req <- c(req, unit_controls)
  if (!is.null(time_controls)) req <- c(req, time_controls)
  if (!is.null(cluster)) req <- c(req, cluster)

  missing_vars <- setdiff(unique(req), names(dt))
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing variables in data: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  if (is.null(cluster)) cluster <- i
  if (!cluster %in% names(dt)) stop(sprintf("cluster variable '%s' not found.", cluster), call. = FALSE)

  if (is.null(fe)) fe <- c(i, t)
  if (length(fe) == 1 && fe == ".") fe <- character(0)

  dt[, .__wei := 1]
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      if (!weights %in% names(dt)) stop(sprintf("weights column '%s' not found.", weights), call. = FALSE)
      dt[, .__wei := as.numeric(get(weights))]
    } else if (is.numeric(weights) && length(weights) == nrow(dt)) {
      dt[, .__wei := as.numeric(weights)]
    } else {
      stop("weights must be NULL, a single weight column name, or a numeric vector of length nrow(data).", call. = FALSE)
    }
  }

  dt[, .__touse := TRUE]
  dt[is.na(get(y)) | is.na(get(i)) | is.na(get(t)) | is.na(.__wei), .__touse := FALSE]
  if (!is.null(controls)) {
    for (v in controls) dt[is.na(get(v)), .__touse := FALSE]
  }
  if (!is.null(unit_controls)) {
    for (v in unit_controls) dt[is.na(get(v)), .__touse := FALSE]
  }
  if (!is.null(time_controls)) {
    for (v in time_controls) dt[is.na(get(v)), .__touse := FALSE]
  }
  dt[.__wei == 0, .__wei := NA_real_]
  dt[is.na(.__wei), .__touse := FALSE]

  dt[, .__K := (get(t) - get(ei) + shift) / delta]
  ok_k <- dt$.__touse & !is.na(dt$.__K)
  if (any(abs(dt$.__K[ok_k] - round(dt$.__K[ok_k])) > 1e-9)) {
    stop("Non-integer values detected in relative time K=(t-ei+shift)/delta.", call. = FALSE)
  }
  dt[, .__K := as.integer(round(.__K))]
  dt[, .__D := as.integer(!is.na(.__K) & .__K >= 0)]

  untreated <- dt$.__touse & dt$.__D == 0
  if (!any(untreated)) {
    stop("No untreated observations available (not-yet-treated or never-treated).", call. = FALSE)
  }

  fe_terms <- .did_imputation_to_fixest_fe(fe)
  if (!is.null(unit_controls) && length(unit_controls) > 0) {
    fe_terms <- c(fe_terms, paste0(i, "[", unit_controls, "]"))
  }
  if (!is.null(time_controls) && length(time_controls) > 0) {
    fe_terms <- c(fe_terms, paste0(t, "[", time_controls, "]"))
  }

  rhs <- if (is.null(controls) || length(controls) == 0) "1" else paste(controls, collapse = " + ")
  fml <- stats::as.formula(paste0(y, " ~ ", rhs, " | ", if (length(fe_terms) == 0) "0" else paste(fe_terms, collapse = " + ")))

  if (isTRUE(verbose)) message("Step 1: fitting untreated-outcome model")
  fit0 <- fixest::feols(fml, data = dt[untreated], weights = ~.__wei, warn = FALSE)

  dt[, .__Y0 := as.numeric(stats::predict(fit0, newdata = dt))]
  dt[, .__effect := data.table::fifelse(.__D == 1, get(y) - .__Y0, NA_real_)]

  w <- .did_imputation_make_weights(dt, D_col = ".__D", touse_col = ".__touse", wtr = wtr, wei_col = ".__wei")
  w_cols <- w$cols
  w_names <- w$names

  hz <- .did_imputation_expand_horizons(
    dt,
    K_col = ".__K",
    w_cols = w_cols,
    w_names = w_names,
    horizons = horizons,
    all_horizons = all_horizons,
    hbalance = hbalance,
    i_col = i,
    D_col = ".__D",
    touse_col = ".__touse",
    wei_col = ".__wei"
  )
  w_cols <- hz$cols
  w_names <- hz$names

  if (!is.null(hetby)) {
    het <- .did_imputation_split_het(dt, w_cols = w_cols, w_names = w_names, hetby = hetby, D_col = ".__D", touse_col = ".__touse")
    w_cols <- het$cols
    w_names <- het$names
  }

  if (!is.null(project)) {
    if (w$wtr_count_init > 0) {
      stop("project can be combined with horizons/all_horizons but not with explicit wtr.", call. = FALSE)
    }
    proj <- .did_imputation_project_weights(dt, w_cols = w_cols, w_names = w_names, project = project, D_col = ".__D", touse_col = ".__touse", wei_col = ".__wei")
    w_cols <- proj$cols
    w_names <- proj$names
  }

  if (!isTRUE(sum) && is.null(project)) {
    for (j in seq_along(w_cols)) {
      cur <- dt$.__touse & dt$.__D == 1
      if (any(dt[[w_cols[j]]][cur] < 0, na.rm = TRUE)) {
        stop("Negative wtr weights are only allowed when sum=TRUE.", call. = FALSE)
      }
      s <- sum(dt[[w_cols[j]]][cur] * dt$.__wei[cur], na.rm = TRUE)
      if (is.finite(s) && s != 0) dt[, (w_cols[j]) := get(w_cols[j]) / s]
    }
  }

  dt[, .__need_imp := FALSE]
  for (j in seq_along(w_cols)) {
    dt[.__touse & .__D == 1 & !is.na(get(w_cols[j])) & get(w_cols[j]) != 0, .__need_imp := TRUE]
  }

  dt[, .__sample := .__touse & (.__D == 0 | .__need_imp)]
  dt[, cannot_impute := .__need_imp & is.na(.__effect)]

  autosample_drop <- character(0)
  autosample_trim <- character(0)

  if (any(dt$cannot_impute & dt$.__sample)) {
    if (!isTRUE(autosample)) {
      stop("Could not impute effects for some treated observations. Set autosample=TRUE to drop them automatically.", call. = FALSE)
    }

    for (j in seq_along(w_cols)) {
      keep <- dt$.__sample & dt$.__D == 1 & !dt$cannot_impute
      cur_use <- dt$.__sample & dt$.__D == 1 & !is.na(dt[[w_cols[j]]]) & dt[[w_cols[j]]] != 0
      n_drop <- sum(cur_use & dt$cannot_impute)
      if (n_drop == 0) next

      s <- sum(dt[[w_cols[j]]][keep] * dt$.__wei[keep], na.rm = TRUE)
      if (!is.finite(s) || s == 0) {
        dt[, (w_cols[j]) := 0]
        autosample_drop <- c(autosample_drop, w_names[j])
      } else {
        dt[keep, (w_cols[j]) := get(w_cols[j]) / s]
        dt[dt$cannot_impute, (w_cols[j]) := 0]
        autosample_trim <- c(autosample_trim, w_names[j])
      }
    }

    dt[, .__sample := .__sample & !cannot_impute]
  }

  droplist <- character(0)
  for (j in seq_along(w_cols)) {
    use <- dt$.__sample & dt$.__D == 1
    n_eff <- .did_imputation_effective_n(dt[[w_cols[j]]][use], dt$.__wei[use])
    if (n_eff < minn) {
      dt[, (w_cols[j]) := 0]
      droplist <- c(droplist, w_names[j])
    }
  }

  coef_tau <- numeric(length(w_cols))
  Nt <- numeric(length(w_cols))
  for (j in seq_along(w_cols)) {
    use <- dt$.__sample & dt$.__D == 1
    coef_tau[j] <- sum(dt$.__effect[use] * dt[[w_cols[j]]][use] * dt$.__wei[use], na.rm = TRUE)
    Nt[j] <- sum(use & !is.na(dt[[w_cols[j]]]) & dt[[w_cols[j]]] != 0)
  }
  names(coef_tau) <- w_names
  names(Nt) <- w_names

  pre_b <- numeric(0)
  pre_V <- matrix(numeric(0), 0, 0)
  pre_F <- NA_real_
  pre_p <- NA_real_
  pre_df <- NA_real_
  fit_pre <- NULL

  if (pretrends > 0) {
    pre_vars <- paste0(".__pre", seq_len(pretrends))
    for (h in seq_len(pretrends)) {
      dt[, (pre_vars[h]) := as.numeric(.__K == -h)]
    }

    rhs_pre <- c(controls, pre_vars)
    rhs_pre_txt <- if (length(rhs_pre) == 0) "1" else paste(rhs_pre, collapse = " + ")
    fml_pre <- stats::as.formula(paste0(y, " ~ ", rhs_pre_txt, " | ", if (length(fe_terms) == 0) "0" else paste(fe_terms, collapse = " + ")))

    fit_pre <- fixest::feols(fml_pre, data = dt[.__touse & .__D == 0], weights = ~.__wei, warn = FALSE)
    c_pre <- stats::coef(fit_pre)

    pre_b <- setNames(rep(NA_real_, pretrends), paste0("pre", seq_len(pretrends)))
    for (h in seq_len(pretrends)) {
      nm <- pre_vars[h]
      if (nm %in% names(c_pre)) pre_b[h] <- unname(c_pre[nm])
    }

    if (isTRUE(se)) {
      v_all <- .did_imputation_vcov_fixest(fit_pre, cluster = cluster)
      keep <- intersect(pre_vars, rownames(v_all))
      if (length(keep) > 0) {
        pre_V <- matrix(0, nrow = pretrends, ncol = pretrends, dimnames = list(names(pre_b), names(pre_b)))
        row_idx <- match(keep, pre_vars)
        pre_V[row_idx, row_idx] <- v_all[keep, keep, drop = FALSE]

        b_use <- pre_b[row_idx]
        V_use <- pre_V[row_idx, row_idx, drop = FALSE]
        if (length(b_use) > 0 && all(is.finite(b_use)) && all(is.finite(V_use))) {
          stat <- tryCatch(as.numeric(t(b_use) %*% solve(V_use, b_use)), error = function(e) NA_real_)
          if (is.finite(stat)) {
            pre_df <- length(b_use)
            pre_F <- stat / pre_df
            pre_p <- stats::pf(pre_F, df1 = pre_df, df2 = 1e9, lower.tail = FALSE)
          }
        }
      }
    }
  }

  ctrl_b <- numeric(0)
  ctrl_V <- matrix(numeric(0), 0, 0)
  if (!is.null(controls) && length(controls) > 0) {
    c0 <- stats::coef(fit0)
    ctrl_b <- setNames(rep(NA_real_, length(controls)), controls)
    for (j in seq_along(controls)) {
      if (controls[j] %in% names(c0)) ctrl_b[j] <- unname(c0[controls[j]])
    }

    if (isTRUE(se)) {
      v0 <- .did_imputation_vcov_fixest(fit0, cluster = cluster)
      keep <- intersect(controls, rownames(v0))
      ctrl_V <- matrix(0, nrow = length(controls), ncol = length(controls), dimnames = list(controls, controls))
      if (length(keep) > 0) {
        idx <- match(keep, controls)
        ctrl_V[idx, idx] <- v0[keep, keep, drop = FALSE]
      }
    }
  }

  b <- c(coef_tau, pre_b, ctrl_b)

  V <- matrix(NA_real_, nrow = length(b), ncol = length(b), dimnames = list(names(b), names(b)))
  if (isTRUE(se)) {
    V_tau <- .did_imputation_cluster_vcov_tau(
      dt,
      coef_names = names(coef_tau),
      w_cols = w_cols,
      coef_vals = coef_tau,
      effect_col = ".__effect",
      D_col = ".__D",
      touse_col = ".__sample",
      cluster_col = cluster,
      wei_col = ".__wei"
    )
    V[names(coef_tau), names(coef_tau)] <- V_tau

    if (length(pre_b) > 0) V[names(pre_b), names(pre_b)] <- pre_V
    if (length(ctrl_b) > 0) V[names(ctrl_b), names(ctrl_b)] <- ctrl_V
  }

  se_vec <- sqrt(diag(V))
  z <- stats::qnorm(1 - alpha / 2)
  out_table <- data.table::data.table(
    term = names(b),
    estimate = as.numeric(b),
    std.error = as.numeric(se_vec),
    conf.low = as.numeric(b - z * se_vec),
    conf.high = as.numeric(b + z * se_vec)
  )

  res <- list(
    call = match.call(),
    cmd = "did_imputation",
    coefficients = b,
    vcov = V,
    table = out_table,
    Nt = Nt,
    Nc = sum(dt$.__sample & dt$.__D == 0),
    Nall = sum(dt$.__sample),
    pre_F = pre_F,
    pre_p = pre_p,
    pre_df = pre_df,
    droplist = droplist,
    autosample_drop = autosample_drop,
    autosample_trim = autosample_trim,
    effect = dt$.__effect,
    cannot_impute = dt$cannot_impute,
    sample = dt$.__sample,
    alpha = alpha
  )

  class(res) <- "did_imputation"
  res
}

print.did_imputation <- function(x, ...) {
  cat("did_imputation\n")
  cat(sprintf("Observations in sample: %s\n", x$Nall))
  cat(sprintf("Control observations (Nc): %s\n", x$Nc))
  print(x$table)
  if (!is.na(x$pre_p)) {
    cat(sprintf("Pre-trends test: F = %.4f, p = %.4g, df = %s\n", x$pre_F, x$pre_p, x$pre_df))
  }
  invisible(x)
}

.did_extract_bv <- function(model) {
  if (inherits(model, "did_imputation")) {
    return(list(
      b = model$coefficients,
      V = model$vcov,
      cmd = "did_imputation"
    ))
  }

  if (inherits(model, "fixest") || inherits(model, "lm")) {
    b <- stats::coef(model)
    V <- tryCatch(stats::vcov(model), error = function(e) NULL)
    return(list(b = b, V = V, cmd = class(model)[1]))
  }

  if (is.list(model) && !is.null(model$b)) {
    V <- model$V
    if (is.null(V) && !is.null(model$v)) V <- model$v
    return(list(b = model$b, V = V, cmd = "list"))
  }

  stop("Unsupported model type. Use did_imputation/fixest/lm object or list with b and optional V.", call. = FALSE)
}

.did_parse_stub <- function(stub) {
  if (is.null(stub) || is.na(stub) || stub == "") return(NULL)
  pos <- regexpr("#", stub, fixed = TRUE)
  if (pos < 1) stop("stub must contain '#'.", call. = FALSE)
  prefix <- substr(stub, 1, pos - 1)
  postfix <- substr(stub, pos + 1, nchar(stub))
  list(prefix = prefix, postfix = postfix)
}

.did_extract_event_terms <- function(b, V, lag_stub, lead_stub, trimlag, trimlead, shift, model_name, alpha) {
  nm <- names(b)
  if (is.null(nm)) stop("Coefficient vector must be named.", call. = FALSE)

  extract_side <- function(stub, side = c("lag", "lead")) {
    side <- match.arg(side)
    if (is.null(stub)) return(data.table::data.table())

    pref <- gsub("([][{}()+*^$|\\?.])", "\\\\\\1", stub$prefix)
    post <- gsub("([][{}()+*^$|\\?.])", "\\\\\\1", stub$postfix)
    rgx <- paste0("^", pref, "([0-9]+)", post, "$")
    hit <- regexec(rgx, nm)
    m <- regmatches(nm, hit)
    ok <- lengths(m) > 0
    if (!any(ok)) return(data.table::data.table())

    k <- as.integer(vapply(m[ok], function(x) x[2], character(1)))
    term <- nm[ok]
    H <- if (side == "lag") k else -k
    out <- data.table::data.table(term = term, H = H, side = side)

    if (side == "lag" && !is.null(trimlag)) out <- out[H <= trimlag]
    if (side == "lead" && !is.null(trimlead)) out <- out[H >= -trimlead]

    out[, estimate := b[term]]
    if (!is.null(V) && all(term %in% rownames(V))) {
      se <- sqrt(diag(V[term, term, drop = FALSE]))
      z <- stats::qnorm(1 - alpha / 2)
      out[, std.error := se]
      out[, conf.low := estimate - z * se]
      out[, conf.high := estimate + z * se]
    } else {
      out[, `:=`(std.error = NA_real_, conf.low = NA_real_, conf.high = NA_real_)]
    }

    out[, x := H - shift]
    out
  }

  lag_dt <- extract_side(lag_stub, "lag")
  lead_dt <- extract_side(lead_stub, "lead")
  out <- data.table::rbindlist(list(lead_dt, lag_dt), fill = TRUE)

  if (nrow(out) == 0) {
    stop(sprintf("No event-study coefficients found for model '%s'.", model_name), call. = FALSE)
  }

  out[order(H)]
}

#' event_plot
#'
#' R equivalent of Stata's event_plot command.
#' Accepts one or more models and plots leads/lags with confidence intervals.
#'
#' @return A list with plot data and ggplot object (unless noplot=TRUE).
event_plot <- function(...,
                       models = NULL,
                       stub_lag = NULL,
                       stub_lead = NULL,
                       trimlag = NULL,
                       trimlead = NULL,
                       plottype = c("connected", "line", "scatter"),
                       ciplottype = c("auto", "rarea", "rcap", "none"),
                       together = FALSE,
                       shift = 0,
                       perturb = NULL,
                       default_look = TRUE,
                       alpha = 0.05,
                       savecoef = FALSE,
                       reportcommand = FALSE,
                       noplot = FALSE,
                       graph_opt = list(),
                       verbose = FALSE) {
  .did_imputation_assert_installed("data.table")
  .did_imputation_assert_installed("ggplot2")

  dots <- list(...)
  if (!is.null(models)) dots <- c(dots, models)
  if (length(dots) == 0) stop("Provide at least one model.", call. = FALSE)
  if (length(dots) > 8) stop("At most 8 models are supported.", call. = FALSE)

  plottype <- match.arg(plottype)
  ciplottype <- match.arg(ciplottype)
  if (ciplottype == "auto") {
    ciplottype <- if (plottype %in% c("connected", "line")) "rarea" else "rcap"
  }

  if (is.null(perturb)) {
    if (length(dots) == 1) {
      perturb <- 0
    } else {
      perturb <- seq(0, 0.2, length.out = length(dots))
    }
  }
  if (length(perturb) == 1) perturb <- rep(perturb, length(dots))

  if (length(shift) == 1) shift <- rep(shift, length(dots))

  get_opt <- function(x, j) {
    if (is.null(x)) return(NULL)
    if (length(x) == 1) return(x)
    x[[j]]
  }

  all_df <- vector("list", length(dots))
  for (j in seq_along(dots)) {
    ex <- .did_extract_bv(dots[[j]])

    lag_stub_j <- get_opt(stub_lag, j)
    lead_stub_j <- get_opt(stub_lead, j)

    if (is.null(lag_stub_j) && identical(ex$cmd, "did_imputation")) lag_stub_j <- "tau#"
    if (is.null(lead_stub_j) && identical(ex$cmd, "did_imputation")) lead_stub_j <- "pre#"

    lag_parsed <- .did_parse_stub(lag_stub_j)
    lead_parsed <- .did_parse_stub(lead_stub_j)

    if (is.null(lag_parsed) && is.null(lead_parsed)) {
      stop(sprintf("For model %s, specify at least one of stub_lag or stub_lead.", j), call. = FALSE)
    }

    df_j <- .did_extract_event_terms(
      b = ex$b,
      V = ex$V,
      lag_stub = lag_parsed,
      lead_stub = lead_parsed,
      trimlag = trimlag,
      trimlead = trimlead,
      shift = shift[j],
      model_name = paste0("model", j),
      alpha = alpha
    )

    df_j[, model := paste0("model", j)]
    df_j[, x := x + perturb[j]]
    all_df[[j]] <- df_j
  }

  plot_df <- data.table::rbindlist(all_df, fill = TRUE)

  if (isTRUE(savecoef)) {
    for (j in seq_along(dots)) {
      cur <- plot_df[model == paste0("model", j)]
      assign(paste0("__event_H", j), cur$H, envir = parent.frame())
      assign(paste0("__event_pos", j), cur$x, envir = parent.frame())
      assign(paste0("__event_coef", j), cur$estimate, envir = parent.frame())
      assign(paste0("__event_lo", j), cur$conf.low, envir = parent.frame())
      assign(paste0("__event_hi", j), cur$conf.high, envir = parent.frame())
    }
  }

  if (isTRUE(reportcommand)) {
    message("event_plot call: ", deparse(match.call()))
  }

  if (isTRUE(noplot)) {
    return(invisible(list(data = plot_df, plot = NULL, call = match.call())))
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = estimate, color = model, group = interaction(model, if (isTRUE(together)) "all" else side)))

  if (!isTRUE(together) && plottype != "scatter") {
    p <- p + ggplot2::geom_line(ggplot2::aes(linetype = side))
  } else if (plottype != "scatter") {
    p <- p + ggplot2::geom_line()
  }

  if (plottype %in% c("connected", "scatter")) {
    p <- p + ggplot2::geom_point(size = 1.8)
  }

  if (ciplottype != "none" && any(is.finite(plot_df$conf.low))) {
    if (ciplottype == "rarea") {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = model),
        alpha = 0.18,
        color = NA,
        inherit.aes = FALSE,
        data = plot_df
      )
    }
    if (ciplottype == "rcap") {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.08, alpha = 0.75)
    }
  }

  p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = 0, color = "gray50") +
    ggplot2::labs(x = "Periods since treatment", y = "Estimate")

  if (isTRUE(default_look)) {
    p <- p + ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.position = if (isTRUE(together)) "none" else "right"
      )
  }

  if (length(graph_opt) > 0) {
    p <- p + do.call(ggplot2::theme, graph_opt)
  }

  invisible(list(data = plot_df, plot = p, call = match.call()))
}
