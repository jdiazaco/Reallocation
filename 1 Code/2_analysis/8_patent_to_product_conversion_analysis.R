# =============================================================================
# Module 7: From Patents to Products – Has French Innovation Become Less Disruptive?
# =============================================================================
# Objective: Analyze the conversion of patents to products, its evolution, and its real effects.
# Data: Uses patenting_products and variables constructed in previous modules.
# =============================================================================


# --- Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# --- Data Loading ------------------------------------------------------------
# Always load the main dataset explicitly (standalone script)
patenting_products <- read_rds("temp/patenting_products_firm_level.RDS")
tm_data <- read_rds("4c_tm_firm_level.rds")

if(length(setdiff(names(tm_data), names(patenting_products))) > 0){
  patenting_products <- merge(patenting_products, tm_data %>% select(firmid, year, setdiff(names(tm_data), names(patenting_products))), by=c("firmid", "year"), all.x=T)
}

patenting_products <- as.data.table(patenting_products)
setorder(patenting_products, firmid, year)

# Load patent to product links
nace_pat_prod <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2e_firm_lvl_patent_product_nace2d_dta.parquet")) %>% setDT()
# remove any variable that has "h1, h3, h4, h5" in its name
nace_pat_prod <- nace_pat_prod[, !grepl("h[1345]", names(nace_pat_prod)), with = FALSE]

# --- Parameters --------------------------------------------------------------
conversion_windows <- c(0, 2, 3, 5) # years: baseline=2, robustness=3,5
burst_threshold <- 5
burst_thresholds <- c(2, 5, 10)

# --- Variable Construction ---------------------------------------------------
# Product burst indicator
patenting_products[, product_burst := as.integer(prod_added >= burst_threshold)]
patenting_products[, product_nonburst := as.integer(prod_added >0 & prod_added < burst_threshold)]

for (i in burst_thresholds) {
  patenting_products[, (paste0("burst_", i)) := as.integer(prod_added >= i)]
}

# Patent burst indicator (90th percentile within firm)
burst_thresholds <- read_rds("4b_patenting_products_firm_level.rds") %>%
  setDT() %>%
  .[num_pat_families > 0, .(
    p90_patents = quantile(num_pat_families, 0.90, na.rm = TRUE),
    p75_patents = quantile(num_pat_families, 0.75, na.rm = TRUE)
  ), by = year]

get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
burst_90_threshold <- get_mode(burst_thresholds$p90_patents)
burst_75_threshold <- get_mode(burst_thresholds$p75_patents)
patenting_products[, patent_burst := as.integer(num_pat_families >= burst_90_threshold)]
patenting_products[, patent_nonburst := as.integer(num_pat_families >0 & num_pat_families < burst_90_threshold)]
patenting_products[, patent_burst_p75 := as.integer(num_pat_families > burst_75_threshold)]

# --- 1. Limit variables, create conversion variables and produce descriptives ---------------------------------------------------------------------------
# Leave patenting_products only with necessary columns
patenting_products <- patenting_products[
  ,
  .(firmid, year, NACE_BR,
  number_of_products, prod_added, age_bin,
  pat_families_dummy, num_pat_families, total_pat_filings_growth,
  tm_dummy, num_tm, total_tm_growth,
  product_burst, product_nonburst, patent_burst, patent_nonburst,
  nq_growth, empl_growth, capital_growth,
  nq_bar, empl_bar, capital_bar,
  size_bin,
    size_quartile, size_decile, size_percentile, size_1000tile, top_4_leaders, top_10_leaders)
]
patenting_products <- merge(patenting_products,
                            nace_pat_prod ,
                            by = c("firmid", "year"), all.x = TRUE, sort = TRUE)

# For each firm-year, count patents filed in year t, and products added in t:(t+window)

for (w in conversion_windows) {
  # For each firm-year, sum products added in t:(t+w)
  patenting_products[, paste0("future_products_", w) :=
    shift(frollsum(prod_added, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]

  # For each firm-year, sum burst products added in t:(t+w)
  patenting_products[, paste0("future_prod_burst", w) :=
    shift(frollsum(product_burst, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]

  #  For each firm-year, sum non-burst products added in t:(t+w)
  patenting_products[, paste0("future_prod_nonburst", w) :=
    shift(frollsum(product_nonburst, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]

  # Number of converted patents: min(patents, future products)
  patenting_products[, paste0("converted_patents_", w) :=
    fifelse(num_pat_families > 0, pmin(1, num_pat_families, get(paste0("future_products_", w))), NA_real_)]

  # For burst/non-burst breakdowns, assign converted patents to burst vs non-burst products (if both exist, assign to burst first)
  patenting_products[, paste0("non_burst_patent_to_non_burst_products_", w) :=
                       fifelse(patent_nonburst>0, pmin(1, get(paste0("future_prod_nonburst", w))), NA_real_)]
  patenting_products[, paste0("burst_patent_to_non_burst_products_", w) :=
                       fifelse(patent_burst>0, pmin(1, get(paste0("future_prod_nonburst", w))), NA_real_)]
  patenting_products[, paste0("non_burst_patent_to_burst_products_", w) :=
                       fifelse(patent_nonburst>0, pmin(1, get(paste0("future_prod_burst", w))), NA_real_)]
  patenting_products[, paste0("burst_patent_to_burst_products_", w) :=
                       fifelse(patent_burst>0, pmin(1, get(paste0("future_prod_burst", w))), NA_real_)]

  # Non-converted patents: remainder
  patenting_products[, paste0("nonconverted_patents_", w) :=
    1 - get(paste0("converted_patents_", w))
  ]
}

View(patenting_products[, .(firmid, year, num_pat_families, prod_added, future_products_2, converted_patents_2, nonconverted_patents_2, share_new_patent_in_new_products_h2,
                             future_prod_burst2, future_prod_nonburst2, burst_patent_to_burst_products_2, burst_patent_to_non_burst_products_2,
                             non_burst_patent_to_burst_products_2, non_burst_patent_to_non_burst_products_2)])

baseline_window <- 2
conversion_rates_vars <- list(
  "Patenting" = "pat_families_dummy",
  # "Log N. Patents" = "asinh(num_pat_families)",
  "Patent Stock Growth" = "total_pat_filings_growth",
  "Trademark" = "tm_dummy",
  # "Log N. Trademarks" = "asinh(num_tm)",
  "Trademark Stock Growth" = "total_tm_growth",
  "Products Added" = "prod_added",
  "Converted patents (General)" = paste0("converted_patents_", baseline_window),
  "Converted patents (Specific)" = paste0("share_new_patent_in_new_products_h", baseline_window),
  "Non-burst patents to non-burst products" = paste0("non_burst_patent_to_non_burst_products_", baseline_window),
  "Burst patents to non-burst products" = paste0("burst_patent_to_non_burst_products_", baseline_window),
  "Non-burst patents to burst products" = paste0("non_burst_patent_to_burst_products_", baseline_window),
  "Burst patents to burst products" = paste0("burst_patent_to_burst_products_", baseline_window)
)

# Descriptive statistics
for (i in seq_along(conversion_rates_vars)) {
  var <- conversion_rates_vars[[i]]

  conv_by_size <- patenting_products[, .(mean_conv = mean(get(var), na.rm = TRUE)),
    by = c("size_bin", "age_bin")
  ]
  setorder(conv_by_size, size_bin, age_bin)

  conv_by_size <- conv_by_size %>%
    pivot_wider(names_from = all_of("size_bin"), values_from = mean_conv) %>%
    # select(-`NA`) %>%
    filter(!is.na(.data[["age_bin"]]))

  create_latex_table(
    data = conv_by_size,
    var_name = names(conversion_rates_vars)[i],
    caption = paste0("Variables by Size Bin and Age Bin - ", names(conversion_rates_vars)[i]),
    output_dir = output_dir,
    include_preamble = FALSE,
    digits = 4,
    filename = paste0("test_", names(conversion_rates_vars)[i])
  )
}

# --- 2. Effects of conversion (and other variables) on growth by age and size --------------------------------

y_vars <- c("nq_growth", "empl_growth", "capital_growth")
y_labels <- c(
  nq_growth = "Output Growth",
  empl_growth = "Employment Growth",
  capital_growth = "Capital Growth"
)

y_weights <- c(
  nq_growth = "nq_bar",
  empl_growth = "empl_bar",
  capital_growth = "capital_bar"
)

x_vars <- conversion_rates_vars
event_window <- 4  # years before/after

patenting_products_og <- patenting_products

# --- Helper functions --------------------------------------------------------
get_slopes <- function(model, by = NULL, pattern = NULL) {
  
  dt_test <- emmeans::emmeans(model, specs = by)
  
  # Convert via as.data.frame() first to avoid node stack overflow
  df <- as.data.frame(dt_test)
  setDT(df)
  
  # Rename emmeans output columns to standard names
  setnames(df, old = c("emmean", "SE"), new = c("estimate", "std.error"))
  
  # Keep only grouping vars + estimate + std.error, with grouping vars first
  keep_cols <- c(as.character(by), "estimate", "std.error")
  df <- df[, ..keep_cols]
  
  df
}
# Vcov-corrected SE: sqrt(w' Σ w), grouped by group_var
vcov_weighted_se <- function(slopes_idx, weights, weight_col, join_col, group_var, vcov_mat) {
  slopes_idx %>%
    left_join(weights, by = join_col) %>%
    group_by(across(all_of(group_var))) %>%
    group_modify(~ {
      sub <- .x[!is.na(.x$estimate) & !is.na(.x[[weight_col]]), ]
      w <- sub[[weight_col]]; idx <- sub$.row_idx
      data.frame(se_vcov = sqrt(as.numeric(w %*% vcov_mat[idx, idx, drop = FALSE] %*% w)))
    }) %>%
    ungroup()
}

# Aggregate cross-dimension slopes: weighted average + vcov-corrected SE
compute_effects <- function(slopes_cross, slopes_direct, weights, weight_col, join_col,
                             group_var, vcov_se_df, beta_ctrl_name, direct_col) {
  se_ctrl_name <- sub("^beta_", "se_", beta_ctrl_name)
  slopes_cross %>%
    left_join(weights, by = join_col) %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      !!beta_ctrl_name := sum(estimate * .data[[weight_col]], na.rm = TRUE),
      !!se_ctrl_name   := sqrt(sum(.data[[weight_col]]^2 * std.error^2, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    left_join(vcov_se_df, by = group_var) %>%
    left_join(
      slopes_direct %>% transmute(
        across(all_of(group_var)),
        !!paste0("beta_", direct_col) := estimate,
        !!paste0("se_",   direct_col) := std.error
      ),
      by = group_var
    )
}

# Reshape effects wide→long for plotting (excludes _vcov columns)
make_plot_data <- function(effects_df, bin_var, type_levels, type_labels) {
  plot_cols <- grep("^(beta|se)_", names(effects_df), value = TRUE)
  plot_cols <- plot_cols[!grepl("_vcov$", plot_cols)]
  effects_df %>%
    select(all_of(c(bin_var, plot_cols))) %>%
    pivot_longer(
      cols = -all_of(bin_var),
      names_to = c(".value", "type"),
      names_pattern = "^(beta|se)_(.*)"
    ) %>%
    mutate(
      ci_low  = beta - 1.96 * se,
      ci_high = beta + 1.96 * se,
      type    = factor(type, levels = type_levels, labels = type_labels)
    )
}

# Cross-section plot by bin at a given event time
bin_plot <- function(data, bin_var, x_name, y_label,  smooth_method = "lm", time=NULL) {
  ggplot(data, aes(x = .data[[bin_var]], y = beta, color = type, fill = type, group = type)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed", linewidth = 0.6) +
    geom_hline(yintercept = 0) +
    geom_point(size = 2) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title    = paste0(x_name, " \u2192 ", y_label),
      subtitle = if (!is.null(time)) paste0("Time: ", time, " years from event") else NULL,
      x = bin_var, y = "Estimated effect", color = "Effect", fill = "Effect"
    )
}

# Faceted plot of effects over event time, one panel per bin
over_time_plot <- function(data, bin_var, x_name, y_label) {
  ggplot(data, aes(x = time, y = beta, color = type, fill = type, group = type)) +
    geom_line(linewidth = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.6) +
    geom_hline(yintercept = 0) +
    geom_point(aes(size = ifelse(ci_low > 0 | ci_high < 0, 3, 1.5))) +
    scale_size_identity() +
    facet_wrap(as.formula(paste0("~", bin_var))) +
    theme_minimal() +
    labs(
      title = paste0(x_name, " \u2192 ", y_label),
      x = "Time from event (years)", y = "Estimated effect",
      color = "Effect", fill = "Effect"
    )
}

save_plot <- function(p, path, width = 8, height = 6) {
  print(p)
  ggsave(path, plot = p, width = width, height = height)
}

output_dir_og <- output_dir
fixed_effects <- c("NACE_BR + year", "NACE_BR^year", "firmid + year", "")

# --- Main analysis loop ------------------------------------------------------
for (x_name in names(x_vars)) {
  message(x_name)
  x_var <- x_vars[[x_name]]

  dat <- copy(patenting_products_og)

  # Assign sunab treatment time: first year with positive x_var (never-treated → 10000)
  first_treat <- dat[get(x_var) > 0, .(event_time = min(year)), by = firmid]
  dat <- merge(dat, first_treat, by = "firmid", all.x = TRUE, sort = FALSE)
  dat[is.na(event_time), event_time := 10000]
  dat <- dat[!is.na(get(x_var))]
  
names(dat)
dat$num_pat_families

  w_name     <- "nq_bar"
  weight_vec <- dat[[w_name]]

      # Interaction models: y ~ x * (size | age | size*age)
      fml_rhs <- function(rhs) as.formula(paste0(x_var, " ~ ", rhs))
      model_size     <- lm(fml_rhs("size_bin"),           data = dat, weights = weight_vec)
      model_age      <- lm(fml_rhs("age_bin"),            data = dat, weights = weight_vec)
      model_size_age <- lm(fml_rhs("size_bin * age_bin"), data = dat, weights = weight_vec)

      slopes_size <- get_slopes(model_size, "size_bin", "^size_bin")
      slopes_age  <- get_slopes(model_age,  "age_bin",  "^age_bin")

      slopes_size_age <- get_slopes(model_size_age, by = c("size_bin", "age_bin"), pattern = "size_bin[^:]*:age_bin[^:]*")
      vcov_size_age   <- vcov(model_size_age)
      slopes_idx      <- slopes_size_age %>% mutate(.row_idx = row_number())

      # Marginal weights
      age_weights  <- dat[, .(w_age  = sum(get(w_name), na.rm = TRUE)), by = age_bin]  %>% mutate(w_age  = w_age  / sum(w_age))
      size_weights <- dat[, .(w_size = sum(get(w_name), na.rm = TRUE)), by = size_bin] %>% mutate(w_size = w_size / sum(w_size))

      # Vcov-corrected SEs: sqrt(w' Σ w)
      vcov_se_size <- vcov_weighted_se(slopes_idx, age_weights,  "w_age",  "age_bin",  "size_bin", vcov_size_age) %>% rename(se_size_age_control_vcov = se_vcov)
      vcov_se_age  <- vcov_weighted_se(slopes_idx, size_weights, "w_size", "size_bin", "age_bin",  vcov_size_age) %>% rename(se_age_size_control_vcov  = se_vcov)

      # Weighted-average effects: size controlled for age distribution, and vice versa
      size_effects <- compute_effects(slopes_size_age, slopes_size, age_weights,  "w_age",  "age_bin",  "size_bin", vcov_se_size, "beta_size_age_control", "size")
      age_effects  <- compute_effects(slopes_size_age, slopes_age,  size_weights, "w_size", "size_bin", "age_bin",  vcov_se_age,  "beta_age_size_control", "age")

      plot_data_size <- make_plot_data(size_effects, "size_bin",
        c("size", "size_age_control"), c("Direct (size-only)", "Age-weighted")) %>% setDT()
      plot_data_size[, size_bin := factor(size_bin, levels = c("1-4", "5-9", "10-19", "20-49", "50-99", "100-249", "250-499", "500-999", "1000+"))]
      setorder(plot_data_size, size_bin)

      plot_data_age  <- make_plot_data(age_effects,  "age_bin",
        c("age",  "age_size_control"),  c("Direct (age-only)",  "Size-weighted")) %>% setDT()
      plot_data_age[, age_bin := factor(age_bin, levels = c("0", "1-2", "3-5", "6-10", "11-20", "21+"))]
      setorder(plot_data_age, age_bin)

    output_dir <- file.path(output_dir_og, paste0(x_name))
    dir.create(output_dir, showWarnings = FALSE)

      save_plot(
          bin_plot(plot_data_size, "size_bin", x_name, "By Size", "glm"),
          file.path(output_dir, paste0(x_name, "_size_bin.png"))
        )
      save_plot(
          bin_plot(plot_data_age, "age_bin", x_name, "By Age", "lm"),
          file.path(output_dir, paste0(x_name, "_age_bin.png"))
        )

  for (y_var in y_vars) {


    output_dir <- file.path(output_dir_og, paste0(x_name, "/", y_var))
    dir.create(output_dir, showWarnings = FALSE)
    
    # Sun & Abraham event study
    # es_model <- feols(
    #   as.formula(paste0(y_var, " ~ sunab(event_time, year, ref.p = -1) | NACE_BR + year")),
    #   data = dat
    # )
    # png(file.path(output_dir, paste0("event_study_", x_name, "_", y_var, ".png")), 800, 600)
    # iplot(es_model, type = "dynamic",
    #       xlab = "Years from Event", ylab = y_labels[y_var],
    #       main = paste0("Event Study: ", y_labels[y_var], " by ", x_name))
    # dev.off()

    w_name     <- y_weights[y_var]
    weight_vec <- dat[[w_name]]
    dat        <- lead_lag_creator(dat, y_var, n_lags = event_window, to_dummy = FALSE)

    results_size <- list()
    results_age  <- list()

    for (i in -event_window:event_window) {

      y_adj <- if (i == 0) y_var else if (i < 0) paste0(y_var, "_lag", abs(i)) else paste0(y_var, "_lead", i)

      # Interaction models: y ~ x * (size | age | size*age)
      fml_rhs <- function(rhs) as.formula(paste0(y_adj, " ~ ", x_var, " * ", rhs, " | NACE_BR + year"))
      model_size     <- feols(fml_rhs("size_bin"),           data = dat, weights = weight_vec)
      model_age      <- feols(fml_rhs("age_bin"),            data = dat, weights = weight_vec)
      model_size_age <- feols(fml_rhs("size_bin * age_bin"), data = dat, weights = weight_vec)

      # Marginal slopes by cell
      slopes_size     <- marginaleffects::slopes(model_size,     by = "size_bin",              var = x_var)
      slopes_age      <- marginaleffects::slopes(model_age,      by = "age_bin",               var = x_var)
      slopes_size_age <- marginaleffects::slopes(model_size_age, by = c("age_bin", "size_bin"), var = x_var)
      vcov_size_age   <- vcov(model_size_age)
      slopes_idx      <- slopes_size_age %>% mutate(.row_idx = row_number())

      # Marginal weights
      age_weights  <- dat[, .(w_age  = sum(get(w_name), na.rm = TRUE)), by = age_bin]  %>% mutate(w_age  = w_age  / sum(w_age))
      size_weights <- dat[, .(w_size = sum(get(w_name), na.rm = TRUE)), by = size_bin] %>% mutate(w_size = w_size / sum(w_size))

      # Vcov-corrected SEs: sqrt(w' Σ w)
      vcov_se_size <- vcov_weighted_se(slopes_idx, age_weights,  "w_age",  "age_bin",  "size_bin", vcov_size_age) %>% rename(se_size_age_control_vcov = se_vcov)
      vcov_se_age  <- vcov_weighted_se(slopes_idx, size_weights, "w_size", "size_bin", "age_bin",  vcov_size_age) %>% rename(se_age_size_control_vcov  = se_vcov)

      # Weighted-average effects: size controlled for age distribution, and vice versa
      size_effects <- compute_effects(slopes_size_age, slopes_size, age_weights,  "w_age",  "age_bin",  "size_bin", vcov_se_size, "beta_size_age_control", "size")
      age_effects  <- compute_effects(slopes_size_age, slopes_age,  size_weights, "w_size", "size_bin", "age_bin",  vcov_se_age,  "beta_age_size_control", "age")

      plot_data_size <- make_plot_data(size_effects, "size_bin",
        c("size", "size_age_control"), c("Direct (size-only)", "Age-weighted"))
      plot_data_age  <- make_plot_data(age_effects,  "age_bin",
        c("age",  "age_size_control"),  c("Direct (age-only)",  "Size-weighted"))

      results_size[[as.character(i)]] <- setDT(plot_data_size)[, time := i]
      results_age[[as.character(i)]]  <- setDT(plot_data_age)[,  time := i]

      if (i %in% 0:2) {
        save_plot(
          bin_plot(plot_data_size, "size_bin", x_name, y_labels[y_var], "glm", i),
          file.path(output_dir, paste0("effect_", x_name, "_", y_var, "_size_bin_", i, ".png"))
        )
        save_plot(
          bin_plot(plot_data_age, "age_bin", x_name, y_labels[y_var], "lm", i),
          file.path(output_dir, paste0("effect_", x_name, "_", y_var, "_age_bin_", i, ".png"))
        )
      }
    }

    stop("Check plots for time points 0, 1, 2 before generating over-time panels")

    # Over-time panels
    results_age <- rbindlist(results_age)
    results_age[, age_bin := factor(age_bin, levels = c("0", "1-2", "3-5", "6-10", "11-20", "21+"))]
    setorder(results_age, age_bin)
    save_plot(
      over_time_plot(results_age, "age_bin", x_name, y_labels[y_var]),
      file.path(output_dir, paste0("effect_over_time_", x_name, "_", y_var, "_age_bin.png"))
    )

    results_size <- rbindlist(results_size)
    results_size[, size_bin := factor(size_bin, levels = c("1-4", "5-9", "10-19", "20-49", "50-99", "100-249", "250-499", "500-999", "1000+"))]
    save_plot(
      over_time_plot(results_size, "size_bin", x_name, y_labels[y_var]),
      file.path(output_dir, paste0("effect_over_time_", x_name, "_", y_var, "_size_bin.png"))
    )
  }
}
# --- 3. Industry dynamics --------------------------------------

product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet"))

patenting_products[, burst:=burst_5]

subset_firms <- patenting_products[burst == 1 & ever_patent == 1] %>%
  select(firmid, year, new_0, new_1, new_2, new_4, new_6, new_8, burst, NACE_cum, new_NACE, num_pat_families, number_of_products, prod_added) %>%
  merge(product_data, by = c("firmid", "year"), all.x = T) 
length(unique(subset_firms$prodcom))

leader_vars <- list(
  "Top Leader" = "leader",
  "Top 4 Leaders" = "top_4_leaders",
  "Top 10 Leaders" = "top_10_leaders"
)

burst_vars <- list(
  "Product Burst (Leader)" = "product_burst_leader",
  "Product Burst (Rest)" = "product_burst_rest",
  "Patent Burst (Leader)" = "patent_burst_leader",
  "Patent Burst (Rest)" = "patent_burst_rest"
)

y_vars <- list(
  "Employment Reallocation" = "empl_reallocation",
  "Product Creation Rate" = "product_creation_rate", 
  "Patent Intensity" = "patent_intensity", 
  "Patent per Firm" = "patent_per_firm", 
  "Patent Adoption Rate" = "patent_adoption_rate", 
  "TM Intensity" = "tm_intensity", 
  "TM per Firm" = "tm_per_firm",
  "TM Adoption Rate" = "tm_adoption_rate"
)

output_dir_og <- output_dir
dir.create(file.path(output_dir_og, paste0("industry_dynamics")), showWarnings = FALSE)

for(i in seq_len(length(leader_vars))){

  leader_var <- leader_vars[[i]]
  leader_name <- names(leader_vars)[i]

  dir.create(file.path(output_dir_og, paste0("industry_dynamics/", leader_name)), showWarnings = FALSE)


  burst_events_industry_granular <- patenting_products[,
  .(product_burst_leader = sum(burst * get(leader_var), na.rm = TRUE),
  product_burst_rest = sum(burst, na.rm = TRUE),
  patent_burst_leader = sum(patent_burst * get(leader_var), na.rm = TRUE),
  patent_burst_rest = sum(patent_burst, na.rm = TRUE),
  nq=sum(nq, na.rm = TRUE),
  empl_reallocation = sum(empl_reallocation_weighted, na.rm = TRUE) / sum(empl_share, na.rm = TRUE),
  n_firms = uniqueN(firmid),
  n_patenting_firms = sum(pat_families_dummy == 1, na.rm = TRUE),
  n_tm_firms = sum(tm_dummy == 1, na.rm = TRUE),
  number_of_products = sum(number_of_products, na.rm = TRUE),
  patent_industry = sum(num_pat_families, na.rm = TRUE),

  # Relevant variables for leader
    num_pat_families_leader = sum(num_pat_families * get(leader_var), na.rm = TRUE),
    num_tm_leader = sum(num_tm * get(leader_var), na.rm = TRUE),
    number_of_products_leader = sum(number_of_products * get(leader_var), na.rm = TRUE),
    prod_added_leader = sum(prod_added * get(leader_var), na.rm = TRUE),
    prod_destr_leader = sum(prod_destr * get(leader_var), na.rm = TRUE),
  # Relevant variables for rest of rest
    num_pat_families_rest = sum(num_pat_families * (1 - get(leader_var)), na.rm = TRUE),
    num_tm_rest = sum(num_tm * (1 - get(leader_var)), na.rm = TRUE),
    number_of_products_rest = sum(number_of_products * (1 - get(leader_var)), na.rm = TRUE),
    prod_added_rest = sum(prod_added * (1 - get(leader_var)), na.rm = TRUE),
    prod_destr_rest = sum(prod_destr * (1 - get(leader_var)), na.rm = TRUE)
    # 
  )
  , by=.(NACE_BR, year)]
  # Calculate leader and rest shares for product creation

  burst_events_industry_granular[, `:=`(
    product_creation_rate = (prod_added_leader + prod_added_rest) / number_of_products,
    product_creation_rate_leader = prod_added_leader / number_of_products,
    product_creation_rate_rest = prod_added_rest / number_of_products,
    patent_intensity = patent_industry / number_of_products,
    patent_per_firm = patent_industry / n_firms,
    patent_adoption_rate = n_patenting_firms / n_firms,
    tm_intensity = (num_tm_leader + num_tm_rest) / number_of_products,
    tm_per_firm = (num_tm_leader + num_tm_rest) / n_firms,
    tm_adoption_rate = n_tm_firms / n_firms
  )]

  burst_events_industry_granular[, `:=`(
    first_product_burst_year = ifelse(any(product_burst_leader > 0), min(year[product_burst_leader > 0], na.rm = TRUE), NA_real_),
    first_patent_burst_year = ifelse(any(patent_burst_leader > 0), min(year[patent_burst_leader > 0], na.rm = TRUE), NA_real_)
  ), by = NACE_BR]

  burst_events_industry_granular[, `:=`(
    treatment_year_product_burst = year-first_product_burst_year,
    treatment_year_patent_burst = year-first_patent_burst_year
  )]
  
  burst_events_industry_granular[, NACE_BR_num := as.numeric(factor(NACE_BR))]

  for (j in seq_len(length(burst_vars))) {
    burst_var <- burst_vars[[j]]
    burst_name <- names(burst_vars)[j]
    dir.create(file.path(output_dir_og, paste0("industry_dynamics/", leader_name, "/", burst_name)), showWarnings = FALSE)

    for (k in seq_len(length(y_vars))) {
      y_var <- y_vars[[k]]
      y_name <- names(y_vars)[k]

      print(paste0("Running event study for ", y_name, " by ", burst_name, " timing (", leader_name, ")"))

      did2s <- event_study(
        data = burst_events_industry_granular,
        yname = y_name,
        tname = "year",
        idname = "NACE_BR_num",
        gname = burst_var,
        estimator = "all"
      )

      plot_event_study(did2s) +
        labs(
          title = paste0("Event Study: ", y_name, " by ", burst_name, " Timing (", leader_var, ")"),
          x = "Years from Burst", y = paste0("Estimated Effect on ", y_name)
        ) +
        theme_minimal() +
        theme(legend.position = "none")

      ggsave(file.path(output_dir, paste0(y_name, "_by_", burst_name, "_", leader_name, ".png")), plot = p, width = 8, height = 6)
    }
  }

}






# --- 5. Deterrence Test: Competitor Product Entry ----------------------------
# For each firm-year, define competitor set (same 4-digit industry)
# Regress competitor product entry on leader's converted/non-converted patents




# For each firm-year, sum product entry of competitors (excluding focal firm)
competitor_entry <- patenting_products[, .(
  competitor_prod_entry = sum(prod_added[firmid != .BY$firmid], na.rm = T),
  converted_patents_2 = unique(converted_patents_2),
  nonconverted_patents_2 = unique(nonconverted_patents_2)
), by = .(NACE_BR, year, firmid)]

competitor_entry <- patenting_products[, `:=`(
  competitor_prod_entry = sum(prod_added, na.rm = T) - prod_added
  # converted_patents_2 = unique(converted_patents_2),
  # nonconverted_patents_2 = unique(nonconverted_patents_2)
), by = .(NACE_BR, year)] %>% select(firmid, NACE_BR, year, prod_added, competitor_prod_entry,
                                     empl_growth, nq_growth, capital_growth,
                                     converted_patents_2, nonconverted_patents_2,
                                     share_new_patent_in_new_products_h2,
                                     matches("burst"),
                                     starts_with("size_"), starts_with("top_"))

# Merge back leader's patent variables
competitor_entry <- merge(competitor_entry,
                         patenting_products[, .(firmid, year, converted_patents_2, nonconverted_patents_2)],
                         by = c("firmid", "year"), suffixes = c("_comp", "_leader"))
vars_to_lag <- c("converted_patents_2", "share_new_patent_in_new_products_h2", "patent_burst", "product_burst_5")
for(var in vars_to_lag){
  competitor_entry <- lead_lag_creator(competitor_entry, var, 2)

}

# Regression: competitor product entry on leader's converted/non-converted patents
library(fixest)
deterrence_model <- feols(empl_growth ~  converted_patents_2  | NACE_BR^year, data = competitor_entry)
summary(deterrence_model)

# --- 6. Aggregate Dynamism Link ----------------------------------------------
# Construct industry-year conversion rate and relate to reallocation, entry/exit, etc.

industry_year <- patenting_products[, .(
  total_patents = sum(num_pat_families, na.rm=TRUE),
  total_converted = sum(converted_patents_2, na.rm=TRUE),
  total_products = sum(prod_added, na.rm=TRUE),
  n_firms = .N
), by = .(NACE_BR, year)]
industry_year[, conversion_rate := total_converted / total_patents]

# Example: plot conversion rate vs product reallocation rate (proxy: std dev of prod_added)
industry_year[, prod_reallocation := sd(total_products, na.rm=TRUE), by = NACE_BR]
ggplot(industry_year, aes(x=conversion_rate, y=prod_reallocation)) +
  geom_point() +
  labs(title="Industry-Year: Conversion Rate vs Product Reallocation", x="Conversion Rate", y="Product Reallocation (SD)")

# --- 7. Robustness: Alternative Windows --------------------------------------
# Repeat above for 3-year and 5-year windows
for (w in c(3,5)) {
  patenting_products[, paste0("conversion_rate_", w) := get(paste0("converted_patents_", w)) / num_pat_families]
  conv_by_year <- patenting_products[, .(mean_conv = mean(get(paste0("conversion_rate_", w)), na.rm=TRUE)), by=year]
  print(conv_by_year)
}


# --- 8. Burst Linkage --------------------------------------------------------
# Are product bursts and patent bursts related at the firm level?
# Cross-tabulate by firm size/age
patenting_products[, burst_link := fifelse(product_burst == 1 & patent_burst == 1, "Both",
                                    fifelse(product_burst == 1, "Product Only",
                                    fifelse(patent_burst == 1, "Patent Only", "Neither")))]
if ("size_quartile" %in% names(patenting_products)) {
  burst_table <- patenting_products[, .N, by = .(burst_link, size_quartile)]
  print(burst_table)
}
if ("age_bin" %in% names(patenting_products)) {
  burst_age_table <- patenting_products[, .N, by = .(burst_link, age_bin)]
  print(burst_age_table)
}

# Document each step and adjust code to match actual variable names and data structure as needed.
