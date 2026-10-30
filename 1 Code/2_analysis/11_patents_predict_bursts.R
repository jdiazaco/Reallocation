# =============================================================================
# Module 11: Do Patents Predict Product Bursts? Heterogeneity by Size and Age
# =============================================================================
# Objective: Test whether patenting at t predicts product creation (and bursts,
#   in the sense of Berlingieri, De Ridder, Lashkari & Rigo) at t+1..t+3, and
#   whether this predictive power declines with firm size (Argente, Baslandze,
#   Hanley & Moreira) and/or age.
# Design:
#   - Outcomes are continuous or relative to the firm's portfolio (no absolute
#     burst thresholds): n_c (new vs t-1), n_never (never produced before),
#     creation rate n_c / N_{t-1}, and a burst flag relative to lagN x year.
#   - Local projections h = -3..+3 on t in 2013-2019 (h < 0 = placebo).
#   - Firm FE + NACE4 x year FE; controls for patents at t-1..t-3, so the
#     coefficient captures new patenting rather than habitual patenters.
#   - Size (employment quintile within NACE2 x year, at t-1) and age enter
#     jointly and additively, never as saturated size x age cells.
# Data: product panel (2a), BR firm panel (1_firm_yr_lvl_br_dta), firm-level
#   patents (4b).
# Notes:
#   - Patents are counted by filing year (as built in 4_make_firm_lvl_patent_dta.R).
#   - No EAP1 list in our pipeline: n_c is only defined if the firm is observed
#     at t-1; "no_gaps" and "emp20" samples are robustness proxies for EAP1.
# =============================================================================


# --- Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# --- Parameters --------------------------------------------------------------
PROD_LEVEL  <- "prodcom"   # product definition: "prodcom" (8d, main) or "cpa" (6d, robustness)
H_PRE       <- 3           # placebo horizons
H_POST      <- 3           # main post horizons
H_EXT       <- 5           # extended (unbalanced) post horizons, appendix only
T_MIN       <- 2013        # first treatment year: t - 3 >= 2010 (first year n_c exists)
T_MAX       <- 2019        # last treatment year: t + 3 <= 2022, avoids patent truncation
T_MAX_COVID <- 2016        # robustness: t + 3 <= 2019, no COVID years in any horizon
BURST_Q     <- 0.95        # relative burst: top 5% of n_c within lagN bin x year...
BURST_MIN   <- 2           # ...and at least this many new products
MIN_OBS     <- 5           # disclosure: censor cells with fewer obs / treated obs

# --- Export / Render Mode ----------------------------------------------------
# EXPORT_MODE = TRUE : run analysis on server, save results as CSVs (no plots)
# EXPORT_MODE = FALSE: load CSVs locally, generate plots (no heavy computation)
EXPORT_MODE     <- TRUE
results_csv_dir <- file.path(output_dir, "csv_results", "11_patents_bursts")
plot_dir        <- file.path(output_dir, "11_patents_bursts")
dir.create(results_csv_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir,        showWarnings = FALSE, recursive = TRUE)

h_name <- function(h) ifelse(h < 0, paste0("m", abs(h)), paste0("p", h))
pad_id <- function(x) str_pad(as.character(x), 9, side = "left", pad = "0")


if (EXPORT_MODE) {

  # ===========================================================================
  # 1. Firm-year product panel (built directly from product-level data)
  # ===========================================================================
  prod <- read_parquet(
    paste0("2_product_data/", PROD_LEVEL, "/2a_product_yr_lvl_dta.parquet"),
    col_select = all_of(c("firmid", "year", PROD_LEVEL, "active"))
  ) %>% setDT()
  setnames(prod, PROD_LEVEL, "product")
  prod <- unique(prod[as.logical(active), .(firmid = pad_id(firmid), year = as.integer(year), product)])

  # New vs t-1 (paper's main definition) and never produced before (their Fig. C10)
  prod[, first_active := min(year), by = .(firmid, product)]
  prev <- prod[, .(firmid, product, year = year + 1L, was_active = TRUE)]
  prod <- merge(prod, prev, by = c("firmid", "product", "year"), all.x = TRUE)
  prod[is.na(was_active), was_active := FALSE]

  firm_prod <- prod[, .(
    N       = .N,
    n_c     = sum(!was_active),
    n_never = sum(year == first_active)
  ), by = .(firmid, year)]
  rm(prod, prev); gc()

  # Complete firm x year grid so that shifts are exact leads/lags
  yr_min <- min(firm_prod$year); yr_max <- max(firm_prod$year)
  panel  <- CJ(firmid = unique(firm_prod$firmid), year = yr_min:yr_max)
  panel  <- merge(panel, firm_prod, by = c("firmid", "year"), all.x = TRUE)
  setorder(panel, firmid, year)

  panel[, obs   := !is.na(N)]
  panel[, obs_l := shift(obs), by = firmid]
  panel[, N_l   := shift(N),   by = firmid]
  panel[, N_b   := shift(N, 4), by = firmid]   # baseline scale at t-4, predetermined for all h >= -3

  # Flows only defined if the firm is observed at t and t-1 (excludes entry year and survey re-entry)
  panel[!(obs & obs_l %in% TRUE), c("n_c", "n_never") := NA_integer_]

  # Creation rate (winsorized at p99) and burst relative to portfolio size
  panel[, rate := n_c / N_l]
  rate_cap <- panel[!is.na(rate), quantile(rate, 0.99)]
  panel[rate > rate_cap, rate := rate_cap]

  panel[, lagN_bin := cut(N_l, c(0, 1, 2, 4, 9, Inf), labels = c("1", "2", "3-4", "5-9", "10+"))]
  panel[!is.na(n_c), burst_q := quantile(n_c, BURST_Q), by = .(lagN_bin, year)]
  panel[!is.na(n_c), burst_rel := as.integer(n_c >= pmax(burst_q, BURST_MIN))]

  # Firms continuously observed between first and last appearance (EAP1 proxy)
  panel[, `:=`(first_obs = min(year[obs]), last_obs = max(year[obs])), by = firmid]
  panel[, no_gaps := all(obs[year >= first_obs & year <= last_obs]), by = firmid]

  # ===========================================================================
  # 2. Firm covariates and patents
  # ===========================================================================
  # Straight from the BR panel: the temp file of analysis script 2 drops entry/exit
  # years, years with missing growth and non-prodcom sectors (~25% of EAP firm-years)
  firm_cov <- read_parquet("1_firm_yr_lvl_br_dta.parquet",
                           col_select = c("firmid", "year", "NACE_BR", "NACE_2d_BR", "empl", "firm_age",
                                          "consolidated_birth_year", "economic_death_year")) %>% setDT()
  firm_cov[, `:=`(firmid = pad_id(firmid), year = as.integer(year))]
  firm_cov <- firm_cov[firmid %in% panel$firmid]
  firm_cov[, firm_age := fcoalesce(as.numeric(firm_age), as.numeric(year - consolidated_birth_year))]
  firm_cov <- unique(firm_cov[, .(firmid, year, NACE_BR, NACE_2d_BR, empl, firm_age, economic_death_year)],
                     by = c("firmid", "year"))
  panel <- merge(panel, firm_cov, by = c("firmid", "year"), all.x = TRUE)
  setorder(panel, firmid, year)
  panel[, empl_l := shift(empl), by = firmid]
  panel[, death_year := suppressWarnings(min(economic_death_year, na.rm = TRUE)), by = firmid]
  panel[is.infinite(death_year), death_year := NA]

  # 4b is a complete firm x year panel (1980-2022) for patenting firms
  pat <- read_rds("4b_patenting_products_firm_level.rds") %>% setDT()
  pat <- pat[, .(firmid = pad_id(firmid), year = as.integer(year), npf = num_pat_families)]
  setorder(pat, firmid, year)
  pat[, paste0("npf_l", 1:3) := shift(npf, 1:3, fill = 0), by = firmid]
  pat[, ever_patent := as.integer(any(npf > 0)), by = firmid]
  panel <- merge(panel, pat, by = c("firmid", "year"), all.x = TRUE)
  for (v in c("npf", paste0("npf_l", 1:3), "ever_patent")) panel[is.na(get(v)), (v) := 0]

  # ===========================================================================
  # 3. Heterogeneity variables (measured at t-1 or t, predetermined w.r.t. Pat_t)
  # ===========================================================================
  panel[obs & !is.na(empl_l) & empl_l > 0,
        size_q := ceiling(5 * frank(empl_l, ties.method = "average") / .N), by = .(NACE_2d_BR, year)]
  panel[, size_q := factor(paste0("Q", size_q), levels = paste0("Q", 1:5))]
  panel[is.na(empl_l) | empl_l <= 0 | !obs, size_q := NA]
  panel[, age4 := cut(firm_age, c(-Inf, 5, 10, 20, Inf), labels = c("0-5", "6-10", "11-20", "21+"))]

  panel[, logN_b := log1p(fifelse(is.na(N_b), 0, N_b))]
  panel[, N_b_missing := as.integer(is.na(N_b))]

  # ===========================================================================
  # 4. Outcomes at each horizon
  # ===========================================================================
  for (h in -H_PRE:H_EXT) {
    if (h == 0) next
    hn   <- h_name(h)
    type <- if (h < 0) "lag" else "lead"
    panel[, paste0(c("n_c", "n_never", "rate", "burst_rel"), "_", hn) :=
            shift(.SD, abs(h), type = type), by = firmid, .SDcols = c("n_c", "n_never", "rate", "burst_rel")]
    if (h > 0) {
      panel[, paste0("absent_", hn) := as.integer(!shift(obs, h, type = "lead")), by = firmid]
      panel[year + h > yr_max, paste0("absent_", hn) := NA]
      panel[, paste0("exit_", hn) := as.integer(!is.na(death_year) & death_year <= year + h)]
      panel[year + h > yr_max, paste0("exit_", hn) := NA]
    }
  }
  for (v in c("n_c", "n_never", "rate", "burst_rel")) panel[, paste0(v, "_p0") := get(v)]

  # Cumulative windows: post = t+1..t+3, pre = t-3..t-1 (joint placebo in a single regression)
  panel[, n_c_cum_post := n_c_p1 + n_c_p2 + n_c_p3]
  panel[, n_c_cum_pre  := n_c_m1 + n_c_m2 + n_c_m3]

  # ===========================================================================
  # 5. Samples
  # ===========================================================================
  panel[, main := obs & obs_l %in% TRUE & year >= T_MIN & year <= T_MAX &
                  !is.na(size_q) & !is.na(age4) & !is.na(NACE_BR)]
  bal_cols <- paste0("n_c_", h_name(c(-H_PRE:-1, 0:H_POST)))
  panel[, balanced := main & complete.cases(panel[, bal_cols, with = FALSE])]

  est_data <- panel[main == TRUE]
  est_data[, z_lemp := as.numeric(scale(log(empl_l)))]
  est_data[, z_lage := as.numeric(scale(log1p(firm_age)))]

  samples <- list(
    main     = quote(main == TRUE),
    balanced = quote(balanced == TRUE),
    no_covid = quote(year <= T_MAX_COVID),
    emp20    = quote(empl_l >= 20),
    no_gaps  = quote(no_gaps == TRUE),
    prodcom_sec = quote(NACE_2d_BR %in% prodcom_sectors)   # sectors with high EAP coverage (Main.R)
  )

  # ===========================================================================
  # 6. Step 0: validate the burst facts on our sample
  # ===========================================================================
  val <- panel[obs & !is.na(n_c)]

  top_shares <- function(x) {
    o <- sort(x, decreasing = TRUE); tot <- sum(o); n <- length(o)
    list(n_obs = n, n_creators = sum(x > 0), total_new = tot,
         share_creating = mean(x > 0),
         top1  = sum(o[seq_len(ceiling(0.01 * n))]) / tot,
         top5  = sum(o[seq_len(ceiling(0.05 * n))]) / tot,
         top10 = sum(o[seq_len(ceiling(0.10 * n))]) / tot)
  }
  conc <- rbind(
    val[, top_shares(n_c), by = year][, group := "all"],
    val[!is.na(lagN_bin), top_shares(n_c), by = .(year, group = paste0("lagN_", lagN_bin))],
    use.names = TRUE
  )
  conc[n_creators < MIN_OBS, c("top1", "top5", "top10") := NA]
  fwrite(conc, file.path(results_csv_dir, "step0_concentration.csv"))

  # CCDF of n_c among creators (Pareto tail check)
  ccdf_k <- c(1, 2, 3, 5, 7, 10, 15, 20, 30, 50)
  ccdf <- rbindlist(lapply(ccdf_k, function(k) val[n_c >= 1, .(k = k, n_ge_k = sum(n_c >= k), n_creators = .N)]))
  ccdf[, share_ge_k := n_ge_k / n_creators]
  ccdf[n_ge_k < MIN_OBS, share_ge_k := NA]
  fwrite(ccdf, file.path(results_csv_dir, "step0_ccdf.csv"))

  # Who creates the products? Patent status x size / age
  val[, patent_status := fcase(
    ever_patent == 0,                          "never",
    npf + npf_l1 + npf_l2 + npf_l3 > 0,        "recent (t-3..t)",
    default =                                  "past only"
  )]
  decomp <- rbind(
    val[, .(n_obs = .N, new_products = sum(n_c), burst_products = sum(n_c * burst_rel, na.rm = TRUE)),
        by = .(patent_status)][, `:=`(bin_var = "all", bin = "all")],
    val[!is.na(size_q), .(n_obs = .N, new_products = sum(n_c), burst_products = sum(n_c * burst_rel, na.rm = TRUE)),
        by = .(patent_status, bin = as.character(size_q))][, bin_var := "size_q"],
    val[!is.na(age4), .(n_obs = .N, new_products = sum(n_c), burst_products = sum(n_c * burst_rel, na.rm = TRUE)),
        by = .(patent_status, bin = as.character(age4))][, bin_var := "age4"],
    use.names = TRUE
  )
  decomp[n_obs < MIN_OBS, c("new_products", "burst_products") := NA]
  fwrite(decomp, file.path(results_csv_dir, "step0_decomposition.csv"))

  # ===========================================================================
  # 7. Local projections
  # ===========================================================================
  CONTROLS <- "pat_l1 + pat_l2 + pat_l3 + logN_b + N_b_missing + i(size_q) + i(age4)"
  FE       <- "firmid + NACE_BR^year"

  spec_rhs <- c(
    pooled     = "pat",
    size       = "i(size_q, pat)",
    lagN       = "i(lagN_bin, pat) + i(lagN_bin)",
    age        = "i(age4, pat)",
    joint      = "i(size_q, pat) + i(age4, pat, ref = '21+')",
    continuous = "pat + pat:z_lemp + pat:z_lage + z_lemp + z_lage"
  )

  set_treatment <- function(dt, treat) {
    f <- if (treat == "asinh") asinh else function(x) as.integer(x > 0)
    dt[, `:=`(pat = f(npf), pat_l1 = f(npf_l1), pat_l2 = f(npf_l2), pat_l3 = f(npf_l3))]
  }

  # Linear contrast across ordered bins: tests a monotone gradient in the slopes
  trend_contrast <- function(m, terms) {
    w <- seq_along(terms) - mean(seq_along(terms))
    V <- vcov(m)[terms, terms, drop = FALSE]
    list(beta = sum(w * coef(m)[terms]), se = sqrt(as.numeric(t(w) %*% V %*% w)))
  }

  setFixest_notes(FALSE)
  run_lp <- function(dt, y, spec, family) {
    fml <- as.formula(paste0(y, " ~ ", spec_rhs[[spec]], " + ", CONTROLS, " | ", FE))
    m <- tryCatch(
      if (family == "pois") fixest::fepois(fml, data = dt, cluster = ~firmid)
      else fixest::feols(fml, data = dt, cluster = ~firmid),
      error = function(e) { message("  failed: ", y, " / ", spec, ": ", conditionMessage(e)); NULL }
    )
    if (is.null(m)) return(NULL)

    ct <- as.data.table(coeftable(m), keep.rownames = "term")[, 1:3]
    setnames(ct, c("term", "beta", "se"))
    ct <- ct[grepl("(^|:)pat$|^pat:", term)]
    ct[, bin_var := fcase(term == "pat", "all", grepl("::", term), sub("::.*", "", term), default = "continuous")]
    ct[, bin     := fcase(term == "pat", "all", grepl("::", term), sub("^\\w+::(.+):pat$", "\\1", term), default = term)]

    # Cell counts for disclosure control and power
    used <- dt[fixest::obs(m)]
    for (bv in setdiff(unique(ct$bin_var), c("all", "continuous"))) {
      cnt <- used[, .(n_obs_bin = .N, n_treated_bin = sum(npf > 0)), by = .(bin = as.character(get(bv)))]
      ct[bin_var == bv, c("n_obs_bin", "n_treated_bin") := cnt[.SD, on = "bin", .(n_obs_bin, n_treated_bin)]]
    }
    ct[bin_var %in% c("all", "continuous"), `:=`(n_obs_bin = nobs(m), n_treated_bin = used[, sum(npf > 0)])]

    # Monotone gradient tests for ordered bins, over bins that pass disclosure:
    # a slope identified off 1-4 treated firms (e.g. smallest size quintile) explodes the contrast
    for (bv in intersect(unique(ct$bin_var), c("size_q", "lagN_bin", "age4"))) {
      ok <- ct[bin_var == bv & n_obs_bin >= MIN_OBS & n_treated_bin >= MIN_OBS, term]
      if (length(ok) < 2) next
      tr <- trend_contrast(m, ok)
      ct <- rbind(ct, data.table(term = paste0(bv, "_linear_trend"), beta = tr$beta, se = tr$se,
                                 bin_var = bv, bin = "linear_trend",
                                 n_obs_bin = nobs(m), n_treated_bin = used[, sum(npf > 0)]))
    }
    ct[, `:=`(nobs = nobs(m), n_firms = uniqueN(used$firmid), outcome_mean = mean(used[[y]]))]
    ct
  }

  run_grid <- function(grid) {
    rbindlist(lapply(seq_len(nrow(grid)), function(i) {
      g <- grid[i]
      message(sprintf("[%d/%d] %s | %s | %s | %s | %s", i, nrow(grid), g$outcome, g$h, g$spec, g$sample, g$treatment))
      dt <- est_data[eval(samples[[g$sample]])]
      set_treatment(dt, g$treatment)
      y  <- if (is.na(g$h)) g$outcome else paste0(g$outcome, "_", h_name(g$h))
      res <- run_lp(dt, y, g$spec, g$family)
      if (!is.null(res)) res[, names(g) := g]
      res
    }), fill = TRUE)
  }

  # Main: all outcomes, all specs, h = -3..+3
  lp_outcomes <- data.table(outcome = c("n_c", "n_never", "rate", "burst_rel"),
                            family  = c("pois", "pois", "ols", "ols"))
  grid_main <- CJ(outcome = lp_outcomes$outcome, h = -H_PRE:H_POST, spec = names(spec_rhs))
  grid_main <- merge(grid_main, lp_outcomes, by = "outcome")
  grid_attr <- CJ(outcome = c("absent", "exit"), h = 1:H_POST, spec = c("pooled", "size", "age", "joint"))[, family := "ols"]
  grid_cum  <- CJ(outcome = c("n_c_cum_pre", "n_c_cum_post"), h = NA_integer_, spec = names(spec_rhs))[, family := "pois"]
  grid_main <- rbind(grid_main, grid_attr, grid_cum, use.names = TRUE)[, `:=`(sample = "main", treatment = "asinh")]

  # Robustness: n_c only, core specs, alternative samples and extensive-margin treatment
  grid_rob <- rbind(
    CJ(outcome = "n_c", h = -H_PRE:H_POST, spec = c("pooled", "size", "age"),
       sample = setdiff(names(samples), "main"), treatment = "asinh"),
    CJ(outcome = "n_c", h = -H_PRE:H_POST, spec = c("pooled", "size", "age"),
       sample = "main", treatment = "dummy")
  )[, family := "pois"]

  # Extended horizons (unbalanced by construction): appendix only
  grid_ext <- CJ(outcome = "n_c", h = (H_POST + 1):H_EXT, spec = c("pooled", "size", "age"),
                 sample = "main", treatment = "asinh")[, family := "pois"]

  lp_results <- run_grid(rbind(grid_main, grid_rob, grid_ext, use.names = TRUE))

  lp_results[, `:=`(ci_low = beta - 1.96 * se, ci_high = beta + 1.96 * se, mde80 = 2.8 * se)]
  lp_results[n_obs_bin < MIN_OBS | n_treated_bin < MIN_OBS, c("beta", "se", "ci_low", "ci_high", "mde80") := NA]
  fwrite(lp_results, file.path(results_csv_dir, "lp_results.csv"))

} else {

  # ===========================================================================
  # Render: plots from exported CSVs
  # ===========================================================================
  lp_results <- fread(file.path(results_csv_dir, "lp_results.csv"))
  conc       <- fread(file.path(results_csv_dir, "step0_concentration.csv"))
  ccdf       <- fread(file.path(results_csv_dir, "step0_ccdf.csv"))
  decomp     <- fread(file.path(results_csv_dir, "step0_decomposition.csv"))

  bin_levels <- c("all", paste0("Q", 1:5), "1", "2", "3-4", "5-9", "10+", "0-5", "6-10", "11-20", "21+")
  y_labels <- c(n_c = "New products (vs t-1)", n_never = "Never-produced products",
                rate = "Product creation rate", burst_rel = "Relative burst (LPM)",
                absent = "Absent from EAP", exit = "Firm exit")

  lp_plot <- function(dt, title) {
    dt <- copy(dt)[, bin := factor(bin, levels = intersect(bin_levels, unique(bin)))]
    ggplot(dt, aes(x = h, y = beta)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = -0.5, linetype = "dashed", alpha = 0.5) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15) +
      geom_line() + geom_point() +
      facet_wrap(~bin, nrow = 1) +
      scale_x_continuous(breaks = sort(unique(dt$h))) +
      theme_minimal() +
      labs(title = title, x = "Horizon (years from patent filing)", y = "Coefficient on patents at t")
  }

  # Main LP profiles: pooled and by size / lagN / age
  for (y in c("n_c", "n_never", "rate", "burst_rel", "absent", "exit")) {
    for (s in c("pooled", "size", "lagN", "age")) {
      d <- lp_results[outcome == y & spec == s & sample == "main" & treatment == "asinh" &
                        bin != "linear_trend" & !is.na(h) & h <= H_POST]
      if (!nrow(d)) next
      p <- lp_plot(d, paste0(y_labels[y], " | ", s))
      ggsave(file.path(plot_dir, paste0("lp_", y, "_", s, ".png")), p, width = 12, height = 4)
    }
  }

  # Joint size + age model at h = 0..3: size slopes (at age 21+) and age differentials
  d <- lp_results[outcome == "n_c" & spec == "joint" & sample == "main" & h >= 0 & bin != "linear_trend"]
  d[, bin := factor(bin, levels = intersect(bin_levels, unique(bin)))]
  p <- ggplot(d, aes(x = bin, y = beta, color = factor(h))) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.5)) +
    facet_wrap(~bin_var, scales = "free_x") +
    theme_minimal() +
    labs(title = "New products: joint size + age model", x = NULL, y = "Coefficient", color = "h")
  ggsave(file.path(plot_dir, "joint_size_age_n_c.png"), p, width = 10, height = 5)

  # Robustness overlay: pooled n_c across samples / treatments
  d <- lp_results[outcome == "n_c" & spec == "pooled" & h <= H_POST]
  d[, variant := paste(sample, treatment, sep = " / ")]
  p <- ggplot(d, aes(x = h, y = beta, color = variant)) +
    geom_hline(yintercept = 0) +
    geom_line(position = position_dodge(width = 0.3)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.3)) +
    theme_minimal() + theme(legend.position = "bottom") +
    labs(title = "New products: robustness", x = "Horizon", y = "Coefficient", color = NULL)
  ggsave(file.path(plot_dir, "robustness_n_c_pooled.png"), p, width = 10, height = 6)

  # Extended horizons (appendix)
  d <- lp_results[outcome == "n_c" & spec %in% c("pooled", "size", "age") & sample == "main" &
                    treatment == "asinh" & bin != "linear_trend" & !is.na(h)]
  for (s in unique(d$spec)) {
    p <- lp_plot(d[spec == s], paste0("New products, extended horizons | ", s))
    ggsave(file.path(plot_dir, paste0("lp_n_c_", s, "_extended.png")), p, width = 12, height = 4)
  }

  # Step 0: concentration of product creation (paper: top 5% of firms ~ 76%)
  p <- ggplot(conc[group == "all"], aes(x = year)) +
    geom_line(aes(y = top5, color = "Top 5%")) + geom_line(aes(y = top10, color = "Top 10%")) +
    geom_line(aes(y = top1, color = "Top 1%")) +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_minimal() + labs(y = "Share of new products", x = NULL, color = NULL)
  ggsave(file.path(plot_dir, "step0_concentration.png"), p, width = 8, height = 5)

  p <- ggplot(ccdf, aes(x = k, y = share_ge_k)) +
    geom_point() + geom_line() +
    scale_x_log10() + scale_y_log10() +
    theme_minimal() + labs(x = "New products (k)", y = "Share of creators with n_c >= k")
  ggsave(file.path(plot_dir, "step0_ccdf.png"), p, width = 7, height = 5)

  decomp[, share_new := new_products / sum(new_products, na.rm = TRUE), by = .(bin_var, bin)]
  decomp[, share_burst := burst_products / sum(burst_products, na.rm = TRUE), by = .(bin_var, bin)]
  fwrite(decomp, file.path(plot_dir, "step0_decomposition_shares.csv"))
}
