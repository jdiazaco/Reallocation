# ==============================================================================
# Data Test 1: Core Product Drop Validity
# ==============================================================================
# Tests whether observed "core product drops" in the data are real economic
# events or measurement noise. A "core product drop" occurs when a firm exits
# the product category (at the highest resolution digit level for the chosen
# cpa_or_pf) that generated its highest revenue in the prior year — flagged by
# get(exit_digit) == TRUE in the product-level data.
#
# Five tests:
#   1. Growth paths before/after the drop — did firms actually shrink?
#   2. Revenue share at t-1 — was the product truly "core" before the drop?
#   3. Product reappearance — do "dropped" products come back (noise signal)?
#   4. Formal event study with pre-trend test (firmid + year FE via feols)
#   5. Descriptive distribution of drops by year, industry, and size
#
# Data inputs (all from the new infrastructure):
#   - 7_final_firm_lvl_dta.rds
#       firm-year panel (nq_growth, empl_growth, capital_growth, size, etc.)
#   - 2_product_data/{cpa_or_pf}/2a_product_yr_lvl_dta.parquet
#       product-firm-year: exit_{digit}, discontinued, first_introduction, rev
#   - 2_product_data/2b_product_core_dta.parquet
#       firm-year: share_core_pf_{digit} (core product revenue share)
#   - 2_product_data/{cpa_or_pf}/2c_firm_lvl_product_dta.parquet
#       firm-year: rev_growth, rev_bar (already computed by pipeline)
#
# All global parameters (cpa_or_pf, exit_digit, ext, digits, prodcom_sectors,
# output_dir, br_start, br_end) are set by Main.R.
# ==============================================================================

source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

output_dir <- paste0(output_dir, "3_data_tests/core_product_drop/")
output_dir_creator(output_dir)
for (d in c("growth_paths", "revenue_share", "reappearance", "event_study", "descriptive")) {
  output_dir_creator(paste0(output_dir, d, "/"))
}

# --- Parameters ---------------------------------------------------------------
event_window <- 5     # years around drop to display / model
prod_start   <- 2010  # first clean year of product data
growth_vars  <- c("nq", "empl", "capital", "rev")

# Digit level for the core product share column (derived from exit_digit)
# e.g. exit_digit = "exit_8"  →  core_digit = 8  →  share_col = "share_core_pf_8"
core_digit <- as.integer(gsub("exit_", "", exit_digit))
share_core_col <- paste0("share_core_pf_", core_digit)

# Helper: weighted mean ignoring NAs in both x and weights
safe_wm <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (sum(keep) == 0) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}

# ==============================================================================
# 1. Load Data
# ==============================================================================
firm_data <- as.data.table(read_rds("7_final_firm_lvl_dta.rds"))
firm_data  <- firm_data[NACE_2d_BR %in% prodcom_sectors & year >= prod_start]

# Product-firm-year level: exit flags, discontinued, product codes, rev
product_data <- as.data.table(
  read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet"))
)
product_data <- product_data[year >= prod_start]

# Firm-year core share metrics
product_core <- as.data.table(
  read_parquet("2_product_data/2b_product_core_dta.parquet")
)

# Firm-year revenue growth (already computed by cleaning pipeline)
firm_rev <- as.data.table(
  read_parquet(paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet"))
)
firm_rev <- firm_rev[year >= prod_start, .(firmid, year, rev_growth, rev_bar)]

# ==============================================================================
# 2. Identify Core Product Drops and Build Event Time
# ==============================================================================

# First core product drop per firm (exit_digit == TRUE in 2a)
drop_events <- product_data[
  get(exit_digit) == TRUE,
  .(yr_core_drop = min(year, na.rm = TRUE)),
  by = firmid
]

# Merge event time onto firm panel
firm_data <- merge(firm_data, drop_events, by = "firmid", all.x = TRUE)
firm_data <- merge(firm_data, firm_rev,    by = c("firmid", "year"), all.x = TRUE)
firm_data[, event_time   := year - yr_core_drop]
firm_data[, ever_dropped := !is.na(yr_core_drop)]

# ==============================================================================
# 3. Test 1 — Growth Paths Before and After Core Product Drop
# ==============================================================================
# Mirrors archive section 6.1 but uses proper infrastructure conventions.
# Stratifies by size and age_bin; plots unweighted and revenue-weighted means.

plot_data <- firm_data[
  ever_dropped == TRUE &
    !is.na(size) & size != "micro" &
    event_time %in% (-event_window):(event_window)
]

for (strat_var in c("size", "age_bin")) {

  temp <- plot_data[!is.na(get(strat_var)), .(
    nq_growth_uw      = mean(nq_growth,      na.rm = TRUE),
    nq_growth_w       = safe_wm(nq_growth,      nq_bar),
    empl_growth_uw    = mean(empl_growth,    na.rm = TRUE),
    empl_growth_w     = safe_wm(empl_growth,    empl_bar),
    capital_growth_uw = mean(capital_growth, na.rm = TRUE),
    capital_growth_w  = safe_wm(capital_growth, capital_bar),
    rev_growth_uw     = mean(rev_growth,     na.rm = TRUE),
    rev_growth_w      = safe_wm(rev_growth,     rev_bar),
    n = .N
  ), by = c("event_time", strat_var)]

  for (gv in growth_vars) {
    long <- melt(
      temp,
      id.vars      = c("event_time", strat_var),
      measure.vars = paste0(gv, c("_growth_uw", "_growth_w")),
      variable.name = "type",
      value.name    = "growth"
    )
    long[, type  := fifelse(grepl("_uw$", type), "Unweighted", "Weighted")]
    long[, group := paste0(get(strat_var), " (", type, ")")]

    p <- ggplot(long, aes(x = event_time, y = growth,
                          color = group, linetype = type, group = group)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      geom_hline(yintercept = 0, color = "darkred",
                 linetype = "solid", linewidth = 0.8) +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = seq(-event_window, event_window, by = 1)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      labs(
        title    = "Firm Growth Before and After Dropping Core Product",
        subtitle = paste0("Growth variable: ", gv,
                          " | Stratified by: ", strat_var),
        x        = "Years Relative to Core Product Drop",
        y        = "Growth Rate"
      )

    ggsave(
      paste0(output_dir, "growth_paths/", strat_var, "_", gv, "_growth_paths.png"),
      plot = p, width = 9, height = 5, dpi = 300
    )
  }
}

# ==============================================================================
# 4. Test 2 — Revenue Share of Core Product at t-1
# ==============================================================================
# If the core product's share was tiny at t-1, the "core" label was marginal
# and the drop may be noise rather than a true strategic exit.

# share_core_col is a firm-year variable in 2b_product_core_dta.parquet
firm_year_share <- product_core[, .SD, .SDcols = c("firmid", "year", share_core_col)]

share_before_drop <- merge(
  drop_events, firm_year_share, by = "firmid", all.x = TRUE
)[year == yr_core_drop - 1]
setnames(share_before_drop, share_core_col, "share_core")

noise_lo <- 0.10
noise_hi <- 0.25
valid    <- share_before_drop[!is.na(share_core)]
noise_rate_lo <- mean(valid$share_core < noise_lo)
noise_rate_hi <- mean(valid$share_core < noise_hi)

p_share <- ggplot(valid, aes(x = share_core)) +
  geom_histogram(bins = 40, fill = "#1B9E77", alpha = 0.85, color = "white") +
  geom_vline(xintercept = noise_lo, color = "darkred",    linetype = "dashed",
             linewidth = 0.8) +
  geom_vline(xintercept = noise_hi, color = "darkorange", linetype = "dashed",
             linewidth = 0.8) +
  annotate("text", x = noise_lo + 0.02, y = Inf, vjust = 2.2, hjust = 0,
           label = paste0(round(noise_rate_lo * 100, 1), "% below 10%"),
           color = "darkred", size = 3.5) +
  annotate("text", x = noise_hi + 0.02, y = Inf, vjust = 4.0, hjust = 0,
           label = paste0(round(noise_rate_hi * 100, 1), "% below 25%"),
           color = "darkorange", size = 3.5) +
  theme_minimal() +
  labs(
    title    = paste0("Revenue Share of Core Product in Year Before Drop (t-1)"),
    subtitle = paste0("Column: ", share_core_col,
                      "  |  N = ", nrow(valid)),
    x        = paste0("Core Product Revenue Share at t-1 (", share_core_col, ")"),
    y        = "Number of Drop Events"
  )

ggsave(
  paste0(output_dir, "revenue_share/share_at_drop_density.png"),
  plot = p_share, width = 8, height = 5, dpi = 300
)

share_summary <- data.table(
  Statistic = c(
    "N drop events with share data",
    "Mean core share (t-1)",
    "Median core share (t-1)",
    "% drops with share < 10% at t-1",
    "% drops with share < 25% at t-1"
  ),
  Value = c(
    nrow(valid),
    round(mean(valid$share_core),   3),
    round(median(valid$share_core), 3),
    round(noise_rate_lo * 100, 1),
    round(noise_rate_hi * 100, 1)
  )
)
fwrite(share_summary, paste0(output_dir, "revenue_share/share_summary.csv"))

# ==============================================================================
# 5. Test 3 — Product Reappearance After Drop
# ==============================================================================
# A truly dropped product should not reappear in the firm's portfolio.
# High reappearance rates indicate measurement noise or reclassification.
# The product code column in 2a is named by the value of cpa_or_pf
# (e.g. column "prodcom" when cpa_or_pf == "prodcom").

# Identify the specific product code that was dropped at the drop event
dropped_codes <- product_data[
  get(exit_digit) == TRUE,
  .(firmid, year, dropped_code = get(cpa_or_pf))
][
  drop_events, on = .(firmid, year = yr_core_drop), nomatch = 0
][, .(firmid, yr_core_drop = year, dropped_code)]

# All product presence records
presence <- unique(product_data[, .(firmid, year, code = get(cpa_or_pf))])

# Check reappearance of dropped code within 1–5 years post-drop
reapp <- merge(
  dropped_codes,
  presence,
  by.x = c("firmid", "dropped_code"),
  by.y = c("firmid", "code")
)
reapp <- reapp[year > yr_core_drop & year - yr_core_drop <= 5]
reapp[, horizon := year - yr_core_drop]

n_dropped <- uniqueN(dropped_codes$firmid)
reapp_rate <- reapp[, .(n_reapp = uniqueN(firmid)), by = horizon]
reapp_rate[, total := n_dropped]
reapp_rate[, core_rate := n_reapp / total]

# Baseline: non-core product exits
non_core_exits <- unique(
  product_data[discontinued == TRUE & get(exit_digit) == FALSE,
               .(firmid, yr_exit = year, code = get(cpa_or_pf))]
)
non_core_reapp <- merge(non_core_exits, presence, by = c("firmid", "code"))
non_core_reapp <- non_core_reapp[year > yr_exit & year - yr_exit <= 5]
non_core_reapp[, horizon := year - yr_exit]

n_non_core     <- uniqueN(non_core_exits$firmid)
baseline_rate  <- non_core_reapp[, .(n_reapp = uniqueN(firmid)), by = horizon]
baseline_rate[, total        := n_non_core]
baseline_rate[, baseline_rate := n_reapp / total]

# Combine
reapp_plot <- merge(
  reapp_rate[,    .(horizon, core_rate)],
  baseline_rate[, .(horizon, baseline_rate)],
  by = "horizon", all = TRUE
)
reapp_long <- melt(reapp_plot, id.vars = "horizon",
                   variable.name = "group", value.name = "rate")
reapp_long[, group := fifelse(group == "core_rate",
                               "Core Product Exits",
                               "Non-Core Product Exits")]

p_reapp <- ggplot(reapp_long,
                   aes(x = factor(horizon), y = rate, fill = group)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Core Product Exits"     = "#D95F02",
    "Non-Core Product Exits" = "#1B9E77"
  )) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(
    title    = "Cumulative Product Reappearance Rate After Exit",
    subtitle = "Core product drops vs. non-core product exits",
    x        = "Years Since Exit",
    y        = "Fraction of Products That Reappeared"
  )

ggsave(
  paste0(output_dir, "reappearance/reappearance_rates.png"),
  plot = p_reapp, width = 8, height = 5, dpi = 300
)
fwrite(reapp_plot, paste0(output_dir, "reappearance/reappearance_table.csv"))

# ==============================================================================
# 6. Test 4 — Formal Event Study with Pre-Trend Test
# ==============================================================================
# y_{it} = α_i + λ_t + Σ_{k≠-1} β_k × 1{event_time = k} + ε_{it}
# - Treated:  firms with ever_dropped == TRUE (event_time defined relative to drop)
# - Controls: never-dropping firms (assigned placeholder 9999, absorbed as ref)
# - Reference period: k = -1
# - Endpoints binned at ±event_window to avoid sparse-cell noise
# - Clustered SEs at NACE_2d_BR level

event_data <- copy(firm_data[!is.na(nq_bar) & !is.na(nq_growth)])

event_data[ever_dropped == TRUE,
           event_time_fe := pmax(-event_window, pmin(event_window, event_time))]
event_data[ever_dropped == FALSE | is.na(event_time), event_time_fe := 9999L]

run_event_study <- function(outcome, weight_col, fe_spec) {
  fml <- as.formula(
    paste0(outcome, " ~ i(event_time_fe, ref = c(-1L, 9999L)) | ", fe_spec)
  )
  feols(fml,
        data    = event_data,
        weights = event_data[[weight_col]],
        cluster = ~NACE_2d_BR)
}

# Two specifications: within-firm FE (preferred) and industry×year FE (robustness)
outcomes <- c("nq_growth", "empl_growth", "capital_growth")
models_firm <- setNames(lapply(outcomes, run_event_study,
                                weight_col = "nq_bar", fe_spec = "firmid + year"),
                         outcomes)
models_ind  <- setNames(lapply(outcomes, run_event_study,
                                weight_col = "nq_bar", fe_spec = "NACE_2d_BR^year"),
                         outcomes)

# Extract coefficients for plotting
extract_coefs <- function(model) {
  est <- coef(model)
  ci  <- confint(model, level = 0.95)
  coefs <- data.table(term = names(est), estimate = est,
                       conf_lo = ci[, 1], conf_hi = ci[, 2])
  coefs <- coefs[grepl("event_time_fe::", term)]
  coefs[, k := as.integer(gsub(".*::([-0-9]+)", "\\1", term))]
  rbind(
    coefs[, .(k, estimate, conf_lo, conf_hi)],
    data.table(k = -1L, estimate = 0, conf_lo = 0, conf_hi = 0)
  ) |> setorder(k)
}

plot_event_study <- function(model, outcome_label, fe_label) {
  coefs <- extract_coefs(model)
  ggplot(coefs, aes(x = k, y = estimate)) +
    geom_ribbon(aes(ymin = conf_lo, ymax = conf_hi),
                alpha = 0.2, fill = "#1B9E77") +
    geom_line(color  = "#1B9E77", linewidth = 0.9) +
    geom_point(color = "#1B9E77", size = 2.2) +
    geom_hline(yintercept = 0, color = "darkred", linetype = "solid") +
    geom_vline(xintercept = -0.5, color = "black", linetype = "dashed") +
    scale_x_continuous(breaks = -event_window:event_window) +
    theme_minimal() +
    labs(
      title    = paste0("Event Study: ", outcome_label),
      subtitle = paste0("FE: ", fe_label,
                        " | Cluster: NACE_2d | Ref: t = -1"),
      x        = "Years Relative to Core Product Drop",
      y        = "Estimated coefficient"
    )
}

outcome_labels <- c(nq_growth = "Output Growth",
                     empl_growth = "Employment Growth",
                     capital_growth = "Capital Growth")

for (v in outcomes) {
  ggsave(
    paste0(output_dir, "event_study/", v, "_firmfe.png"),
    plot  = plot_event_study(models_firm[[v]], outcome_labels[v], "firmid + year"),
    width = 8, height = 5, dpi = 300
  )
  ggsave(
    paste0(output_dir, "event_study/", v, "_indfe.png"),
    plot  = plot_event_study(models_ind[[v]], outcome_labels[v], "NACE_2d × year"),
    width = 8, height = 5, dpi = 300
  )
}

# Joint pre-trend F-test: H0 = all β_k for k < -1 are zero
pretrend_results <- rbindlist(lapply(outcomes, function(v) {
  tryCatch({
    w <- wald(models_firm[[v]], keep = "event_time_fe::-[2-9]|event_time_fe::-[0-9]{2}")
    data.table(outcome = v, F_stat  = round(w$stat, 3),
               p_value = round(w$p, 4), df = w$df)
  }, error = function(e) {
    data.table(outcome = v, F_stat = NA_real_, p_value = NA_real_, df = NA_integer_)
  })
}))
fwrite(pretrend_results, paste0(output_dir, "event_study/pretrend_ftest.csv"))

# Regression table (firm FE only to keep output concise)
modelsummary(
  setNames(models_firm, paste0(outcome_labels[outcomes], " (firm FE)")),
  output   = paste0(output_dir, "event_study/event_study_models.tex"),
  stars    = TRUE,
  title    = "Event Study: Firm Growth Around Core Product Drop",
  gof_omit = "AIC|BIC|Log.Lik.|Std.Errors",
  notes    = paste0(
    "Treated firms dropped their core product (", exit_digit,
    " == TRUE) at t = 0. ",
    "Control: firms that never drop. Reference period: t = -1. ",
    "Endpoints binned at \\(\\pm", event_window, "\\). ",
    "Weights: nq\\_bar. Cluster: NACE 2d."
  )
)

# ==============================================================================
# 7. Test 5 — Distribution of Drops by Year, Industry, and Size
# ==============================================================================
# Systematic spikes in specific years or industries could indicate data artifacts.

drops_per_firm <- firm_data[
  ever_dropped == TRUE & event_time == 0,
  .(firmid, yr_core_drop, NACE_2d_BR, size_bin)
]

# 5a: by year
drops_by_year <- drops_per_firm[, .N, by = yr_core_drop]
p_year <- ggplot(drops_by_year, aes(x = yr_core_drop, y = N)) +
  geom_col(fill = "#1B9E77", alpha = 0.85) +
  scale_x_continuous(
    breaks = seq(prod_start, max(drops_by_year$yr_core_drop), by = 1)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Core Product Drops by Year",
       x = "Year of First Core Drop", y = "Number of Firms")
ggsave(paste0(output_dir, "descriptive/drops_by_year.png"),
       plot = p_year, width = 8, height = 4, dpi = 300)

# 5b: by 2-digit NACE (sorted by frequency)
drops_by_ind <- drops_per_firm[, .N, by = NACE_2d_BR]
setorder(drops_by_ind, -N)
drops_by_ind[, NACE_2d_BR := factor(NACE_2d_BR, levels = NACE_2d_BR)]

p_ind <- ggplot(drops_by_ind, aes(x = NACE_2d_BR, y = N)) +
  geom_col(fill = "#D95F02", alpha = 0.85) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Core Product Drops by 2-digit NACE Industry",
       x = "NACE 2d", y = "Number of Firms")
ggsave(paste0(output_dir, "descriptive/drops_by_industry.png"),
       plot = p_ind, width = 9, height = 4, dpi = 300)

# 5c: drop rate by size bin
total_by_size  <- firm_data[!is.na(size_bin), .(total = uniqueN(firmid)), by = size_bin]
drops_by_size  <- drops_per_firm[!is.na(size_bin), .N, by = size_bin]
size_drop_rate <- merge(total_by_size, drops_by_size, by = "size_bin", all.x = TRUE)
size_drop_rate[is.na(N), N := 0L]
size_drop_rate[, drop_rate := N / total]

p_size <- ggplot(size_drop_rate, aes(x = size_bin, y = drop_rate)) +
  geom_col(fill = "#7570B3", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title    = "Core Product Drop Rate by Size Bin",
       subtitle = "Share of firms in each size bin that ever drop their core product",
       x = "Size Bin", y = "Drop Rate")
ggsave(paste0(output_dir, "descriptive/drops_by_size.png"),
       plot = p_size, width = 8, height = 4, dpi = 300)

message("=== Core product drop validity tests complete. Outputs: ", output_dir, " ===")
