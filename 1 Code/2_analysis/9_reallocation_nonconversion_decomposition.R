# =============================================================================
# Module 9: Reallocation Decline and Non-Converting Patenting
# =============================================================================
# Objective: Decompose the decline in reallocation into the component explained
#            by rising non-converting incumbent patenting vs. other factors.
#
# Strategy:
#   1. Build firm-year patent conversion variables (parallel to module 8)
#   2. Aggregate to industry-year: within-industry reallocation rate +
#      incumbent non-conversion rate
#   3. Panel regression: reallocation ~ nonconversion + NACE_BR FE + year FE
#      → partial elasticity of reallocation w.r.t. non-conversion
#   4. Aggregate time-series regression → counterfactual decomposition:
#      how much of the reallocation decline is attributable to rising
#      non-conversion (hold non-conversion at 2010 level)?
#   5. Heterogeneity: large vs. small incumbents
#   6. Robustness: alternative conversion windows
#
# Data inputs:
#   - 7_final_firm_lvl_dta.rds           (full merged firm-year panel)
#   - 2_product_data/.../2e_...parquet   (patent-product NACE concordance)
#
# Key variables used:
#   - num_pat_families, prod_added       (from cleaning modules 4, 2)
#   - nq, nq_bar, nq_reallocation        (from growth_creator in module 1)
#   - within_industry_rev_share          (from module 1 cleaning)
#   - consolidated_birth_year            (entry cohort)
#   - size_bin, leader, HHI_industry     (industry structure)
# =============================================================================

source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

output_dir <- paste0(output_dir, "9_reallocation_nonconversion/")
output_dir_creator(output_dir)

# --- Parameters ---------------------------------------------------------------
baseline_window  <- 2
conversion_windows <- c(0, 2, 3, 5)
prod_start       <- 2010   # First clean year of product data
min_patents_ind  <- 3      # Min patenting firms per industry-year for nc rate

# --- 1. Data Loading ----------------------------------------------------------
firm_data <- as.data.table(read_rds("7_final_firm_lvl_dta.rds"))
firm_data <- firm_data[NACE_2d_BR %in% prodcom_sectors & year >= prod_start]

nace_pat_prod <- as.data.table(read_parquet(
  paste0("2_product_data/", cpa_or_pf, "/2e_firm_lvl_patent_product_nace2d_dta.parquet")
))
# Keep only h0 and h2 measures
nace_pat_prod <- nace_pat_prod[, !grepl("h[1345]", names(nace_pat_prod)), with = FALSE]

# --- 2. Conversion Variables at Firm-Year Level -------------------------------
# Replicates the logic in module 8 so this script is self-contained.
setorder(firm_data, firmid, year)

for (w in conversion_windows) {
  # Products created in [t, t+w]
  firm_data[, paste0("future_products_", w) :=
    shift(frollsum(prod_added, n = w + 1, align = "left"), type = "lead", n = 0),
    by = firmid]

  # Converted: patenting firm that launched at least one product in window
  firm_data[, paste0("converted_patents_", w) :=
    fifelse(num_pat_families > 0,
            pmin(1, num_pat_families, get(paste0("future_products_", w))),
            NA_real_)]

  # Non-converted: patenting firm with zero product launch in window
  firm_data[, paste0("nonconverted_patents_", w) :=
    fifelse(num_pat_families > 0,
            1 - get(paste0("converted_patents_", w)),
            NA_real_)]
}

# Merge NACE-concordance conversion measure (patent-product NACE linkage)
firm_data <- merge(firm_data, nace_pat_prod, by = c("firmid", "year"),
                   all.x = TRUE, sort = FALSE)

# Firm-level flags
firm_data[, incumbent  := as.integer(consolidated_birth_year < year)]
firm_data[, large_firm := as.integer(size_bin %in% c("100-249", "250-499", "500-999", "1000+"))]
firm_data[, patenting  := as.integer(num_pat_families > 0)]

# --- 3. Industry-Year Aggregation: Reallocation + Non-Conversion --------------
# Within-industry reallocation: firm's share of industry revenue × |revenue growth|
# nq_bar and nq_reallocation are created by growth_creator in the cleaning pipeline.
# within_industry_rev_share is the firm's share of revenue within its NACE_BR industry.

industry_year <- firm_data[, {
  # Within-industry revenue shares (for weighting reallocation)
  ind_nq_bar_sum <- sum(nq_bar, na.rm = TRUE)
  ind_share      <- nq_bar / ind_nq_bar_sum

  # Within-industry reallocation rate = sum(ind_share × |revenue growth|)
  rev_realloc_ind <- sum(ind_share * nq_reallocation, na.rm = TRUE)
  # Employment reallocation (if empl_reallocation exists)
  empl_realloc_ind <- if ("empl_reallocation" %in% names(.SD))
    sum((empl_bar / sum(empl_bar, na.rm = TRUE)) * empl_reallocation, na.rm = TRUE)
    else NA_real_

  # Non-conversion rates among patenting incumbents
  patenting_inc      <- patenting == 1 & incumbent == 1
  nc_all             <- mean(nonconverted_patents_2[patenting_inc],  na.rm = TRUE)
  nc_large           <- mean(nonconverted_patents_2[patenting_inc & large_firm == 1], na.rm = TRUE)
  nc_small           <- mean(nonconverted_patents_2[patenting_inc & large_firm == 0], na.rm = TRUE)
  nc_nace            <- mean(1 - share_new_patent_in_new_products_h2[patenting_inc], na.rm = TRUE)

  list(
    rev_realloc        = rev_realloc_ind,
    empl_realloc       = empl_realloc_ind,
    nonconversion_rate       = nc_all,
    nonconversion_rate_large = nc_large,
    nonconversion_rate_small = nc_small,
    nonconversion_rate_nace  = nc_nace,
    n_patenting_inc    = sum(patenting_inc, na.rm = TRUE),
    n_firms            = .N,
    HHI_industry       = mean(HHI_industry, na.rm = TRUE),
    entry_rate         = mean(born, na.rm = TRUE),
    exit_rate          = mean(died, na.rm = TRUE),
    avg_log_markup     = mean(log(mu_sepcal_TL_sBFSR_t1), na.rm = TRUE)
  )
}, by = .(NACE_BR, NACE_2d_BR, year)]

# Require at least min_patents_ind patenting incumbents for a reliable nc rate
industry_year <- industry_year[n_patenting_inc >= min_patents_ind]
setorder(industry_year, NACE_BR, year)

# Lagged non-conversion (predetermined)
for (v in c("nonconversion_rate", "nonconversion_rate_large", "nonconversion_rate_small")) {
  industry_year[, paste0(v, "_l") := shift(get(v), 1, type = "lag"), by = NACE_BR]
}

# --- 4. Aggregate Time Series: Co-movement ------------------------------------
agg_ts <- firm_data[patenting == 1 & incumbent == 1, .(
  rev_realloc_agg      = sum((nq_bar / sum(nq_bar, na.rm = TRUE)) * nq_reallocation,
                             na.rm = TRUE),
  nonconversion_agg    = mean(nonconverted_patents_2, na.rm = TRUE),
  nonconversion_large  = mean(nonconverted_patents_2[large_firm == 1], na.rm = TRUE)
), by = year]

# 3-year moving average for display
ma3 <- function(x) zoo::rollmean(x, k = 3, fill = NA, align = "right")
agg_ts[, `:=`(
  rev_realloc_ma3   = ma3(rev_realloc_agg),
  nonconv_ma3       = ma3(nonconversion_agg),
  nonconv_large_ma3 = ma3(nonconversion_large)
)]

# Index to base year = 1 for dual-axis plot
base_year  <- min(agg_ts[!is.na(rev_realloc_ma3)]$year)
base_vals  <- agg_ts[year == base_year, .(r = rev_realloc_ma3, nc = nonconv_ma3)]
agg_ts[!is.na(rev_realloc_ma3), `:=`(
  rev_idx   = rev_realloc_ma3  / base_vals$r,
  nonconv_idx = nonconv_ma3    / base_vals$nc
)]

trend_long <- melt(agg_ts[!is.na(rev_idx), .(year, rev_idx, nonconv_idx)],
                   id.vars = "year", variable.name = "series", value.name = "index")
trend_long[, series := factor(series,
  levels = c("rev_idx", "nonconv_idx"),
  labels = c("Revenue Reallocation Rate", "Non-Conversion Rate (incumbents)"))]

p_trends <- ggplot(trend_long, aes(x = year, y = index, colour = series, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey50") +
  scale_colour_manual(values = c("#1B9E77", "#D95F02")) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Revenue Reallocation and Incumbent Non-Converting Patenting",
       subtitle = "Index (base year = 1), 3-year moving average",
       y = "Index (base year = 1)")
ggsave(paste0(output_dir, "nonconv_vs_reallocation_trend.png"), p_trends, width = 9, height = 6)

# --- 5. Panel Regressions: Reallocation ~ Non-Conversion ---------------------
# All specs: industry (NACE 4-digit) and year FE. SEs clustered at NACE 2-digit.

# 5.1 Baseline
m1 <- feols(rev_realloc ~ nonconversion_rate | NACE_BR + year,
            data = industry_year, cluster = ~NACE_2d_BR)

# 5.2 Decompose by firm size: large vs. small incumbent
m2 <- feols(rev_realloc ~ nonconversion_rate_large + nonconversion_rate_small | NACE_BR + year,
            data = industry_year, cluster = ~NACE_2d_BR)

# 5.3 Lagged non-conversion (predetermined, avoids any reverse-causality concern)
m3 <- feols(rev_realloc ~ nonconversion_rate_l | NACE_BR + year,
            data = industry_year, cluster = ~NACE_2d_BR)

# 5.4 Control for HHI (exclude concentration-driven reallocation effects)
m4 <- feols(rev_realloc ~ nonconversion_rate + HHI_industry | NACE_BR + year,
            data = industry_year, cluster = ~NACE_2d_BR)

# 5.5 NACE concordance measure of non-conversion
m5 <- feols(rev_realloc ~ nonconversion_rate_nace | NACE_BR + year,
            data = industry_year[!is.na(nonconversion_rate_nace)], cluster = ~NACE_2d_BR)

# 5.6 Employment reallocation
m6 <- feols(empl_realloc ~ nonconversion_rate | NACE_BR + year,
            data = industry_year[!is.na(empl_realloc)], cluster = ~NACE_2d_BR)

# 5.7 Employment reallocation, large vs. small
m7 <- feols(empl_realloc ~ nonconversion_rate_large + nonconversion_rate_small | NACE_BR + year,
            data = industry_year[!is.na(empl_realloc)], cluster = ~NACE_2d_BR)

models_panel <- list(
  "Rev (1)"        = m1,
  "Rev (Size)"     = m2,
  "Rev (Lagged)"   = m3,
  "Rev (HHI ctrl)" = m4,
  "Rev (NACE nc)"  = m5,
  "Empl (1)"       = m6,
  "Empl (Size)"    = m7
)

modelsummary(
  models_panel,
  output    = paste0(output_dir, "reallocation_nonconversion_panel.tex"),
  stars     = TRUE,
  title     = "Reallocation Rate and Incumbent Non-Conversion (Industry Panel)",
  coef_rename = c(
    "nonconversion_rate"       = "Non-Conv. Rate (incumbents)",
    "nonconversion_rate_l"     = "Non-Conv. Rate (t-1)",
    "nonconversion_rate_large" = "Non-Conv. Rate (large incumbents)",
    "nonconversion_rate_small" = "Non-Conv. Rate (small incumbents)",
    "nonconversion_rate_nace"  = "Non-Conv. Rate (NACE concordance)",
    "HHI_industry"             = "HHI"
  ),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = paste0(
    "NACE 4-digit industry and year FEs. Clustered SEs at NACE 2-digit. ",
    "Sample: prodcom sectors, ", prod_start, "-2022. ",
    "Industry-years with >=", min_patents_ind, " patenting incumbents."
  )
)

# --- 6. Time-Varying Panel: Has the Relationship Strengthened Over Time? ------
industry_year[, year_trend := year - min(year, na.rm = TRUE)]
m_trend <- feols(rev_realloc ~ nonconversion_rate * year_trend | NACE_BR,
                 data = industry_year, cluster = ~NACE_2d_BR)

modelsummary(
  list("Rev. ~ NC × Trend" = m_trend),
  output    = paste0(output_dir, "reallocation_nonconversion_time_trend.tex"),
  stars     = TRUE,
  title     = "Time-Varying Effect: Non-Conversion on Reallocation",
  coef_rename = c(
    "nonconversion_rate"            = "Non-Conv. Rate",
    "year_trend"                    = "Year Trend",
    "nonconversion_rate:year_trend" = "Non-Conv. Rate × Year Trend"
  ),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = "NACE 4-digit FE only (year trend enters as linear regressor). Clustered SEs at NACE 2-digit."
)

# --- 7. Counterfactual Decomposition ------------------------------------------
# Strategy (aggregate time series):
#   (a) Regress aggregate reallocation on aggregate non-conversion rate + linear
#       time trend to absorb common macro variation.
#   (b) Estimate β̂: effect of non-conversion on reallocation.
#   (c) Counterfactual: hold non-conversion at its base-year level.
#       counterfactual_t = actual_t - β̂ × (nonconversion_t - nonconversion_base)
#   (d) "Explained" portion = gap between actual and counterfactual.
#   (e) Express as share of total reallocation change over the sample period.

agg_reg_data <- agg_ts[!is.na(rev_realloc_agg) & !is.na(nonconversion_agg)]
agg_reg <- lm(rev_realloc_agg ~ nonconversion_agg + year, data = agg_reg_data)
beta_nc <- coef(agg_reg)["nonconversion_agg"]

cat("\n--- Aggregate time-series coefficient ---\n")
cat(sprintf("β (non-conversion → reallocation): %.4f\n", beta_nc))
cat(sprintf("Implied: a 10pp rise in non-conversion → %.1f pp change in reallocation\n",
            beta_nc * 0.10 * 100))

# Base-year non-conversion level
nc_base <- agg_reg_data[year == min(year)]$nonconversion_agg

agg_reg_data[, `:=`(
  nc_contribution    = beta_nc * (nonconversion_agg - nc_base),
  counterfactual     = rev_realloc_agg - beta_nc * (nonconversion_agg - nc_base)
)]

# Express as share of total reallocation change
realloc_start  <- agg_reg_data[year == min(year)]$rev_realloc_agg
realloc_end    <- agg_reg_data[year == max(year)]$rev_realloc_agg
total_change   <- realloc_end - realloc_start

nc_change_total <- beta_nc * (agg_reg_data[year == max(year)]$nonconversion_agg - nc_base)
share_explained <- nc_change_total / total_change

cat(sprintf("\nTotal reallocation change (start → end): %.4f\n", total_change))
cat(sprintf("Non-conversion contribution:               %.4f\n", nc_change_total))
cat(sprintf("Share of change explained by non-conversion: %.1f%%\n", share_explained * 100))

# Counterfactual plot
decomp_long <- melt(
  agg_reg_data[, .(year, rev_realloc_agg, counterfactual)],
  id.vars = "year", variable.name = "component", value.name = "value"
)
decomp_long[, component := factor(component,
  levels = c("rev_realloc_agg", "counterfactual"),
  labels = c("Actual", "Counterfactual\n(non-conversion fixed at base year)"))]

p_decomp <- ggplot(decomp_long, aes(x = year, y = value, colour = component, linetype = component)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    data = agg_reg_data,
    aes(x = year,
        ymin = pmin(rev_realloc_agg, counterfactual),
        ymax = pmax(rev_realloc_agg, counterfactual),
        colour = NULL),
    fill  = "#7570B3", alpha = 0.25, inherit.aes = FALSE
  ) +
  annotate("text", x = median(agg_reg_data$year), y = mean(agg_reg_data$rev_realloc_agg),
           label = sprintf("Non-conv. explains\n~%.0f%% of decline", share_explained * 100),
           colour = "#7570B3", size = 3.5, hjust = 0.5) +
  scale_colour_manual(values = c("Actual" = "#1B9E77",
                                 "Counterfactual\n(non-conversion fixed at base year)" = "#D95F02")) +
  scale_linetype_manual(values = c("Actual" = "solid",
                                   "Counterfactual\n(non-conversion fixed at base year)" = "dashed")) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(
    title    = "Counterfactual Decomposition of Revenue Reallocation Decline",
    subtitle = paste0("Shaded area = non-conversion contribution (~",
                      round(share_explained * 100, 0), "% of total change)"),
    y = "Aggregate Revenue Reallocation Rate"
  )
ggsave(paste0(output_dir, "reallocation_counterfactual_decomposition.png"),
       p_decomp, width = 9, height = 6)

# Decade-level decomposition summary table
decomp_table <- agg_reg_data[, .(
  avg_actual_realloc     = mean(rev_realloc_agg,   na.rm = TRUE),
  avg_nc_contribution    = mean(nc_contribution,    na.rm = TRUE),
  avg_counterfactual     = mean(counterfactual,     na.rm = TRUE)
), by = .(period = cut(year, breaks = c(prod_start - 1, 2014, 2019, 2023),
                       labels = c("2010-2014", "2015-2019", "2020-2022"),
                       right = FALSE))]
decomp_table <- decomp_table[!is.na(period)]
decomp_table[, share_explained_by_nc := avg_nc_contribution / (avg_actual_realloc - agg_reg_data[year == min(year)]$rev_realloc_agg)]

create_latex_table(
  data        = decomp_table,
  caption     = "Decomposition of Revenue Reallocation: Non-Conversion Contribution by Period",
  output_dir  = output_dir,
  include_preamble = FALSE,
  digits      = 4,
  filename    = "reallocation_decomp_period_table"
)

# --- 8. Robustness: Alternative Conversion Windows ---------------------------
models_windows <- setNames(
  lapply(conversion_windows, function(w) {
    nc_var <- paste0("nonconverted_patents_", w)
    iy_w   <- firm_data[patenting == 1 & incumbent == 1 & !is.na(get(nc_var)),
                        .(
                          rev_realloc      = sum((nq_bar / sum(nq_bar, na.rm = TRUE)) * nq_reallocation, na.rm = TRUE),
                          nonconversion_rate = mean(get(nc_var), na.rm = TRUE),
                          n_pat = .N
                        ), by = .(NACE_BR, NACE_2d_BR, year)]
    iy_w   <- iy_w[n_pat >= min_patents_ind]
    feols(rev_realloc ~ nonconversion_rate | NACE_BR + year,
          data = iy_w, cluster = ~NACE_2d_BR)
  }),
  paste0("Window = ", conversion_windows, "yr")
)

modelsummary(
  models_windows,
  output   = paste0(output_dir, "reallocation_nonconversion_windows.tex"),
  stars    = TRUE,
  title    = "Robustness: Alternative Conversion Windows",
  coef_rename = c("nonconversion_rate" = "Non-Conv. Rate (incumbents)"),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = "NACE 4-digit and year FEs. Clustered SEs at NACE 2-digit."
)

# --- 9. Cross-Sectional Scatter: Industry Non-Conversion vs. Reallocation -----
# Average non-conversion rate and average reallocation rate per industry
industry_avg <- industry_year[, .(
  avg_rev_realloc    = mean(rev_realloc,        na.rm = TRUE),
  avg_nonconv        = mean(nonconversion_rate,  na.rm = TRUE),
  avg_nonconv_large  = mean(nonconversion_rate_large, na.rm = TRUE),
  n_years            = .N,
  NACE_2d_BR         = first(NACE_2d_BR)
), by = NACE_BR]

p_scatter <- ggplot(industry_avg[!is.na(avg_nonconv)],
                    aes(x = avg_nonconv, y = avg_rev_realloc, label = NACE_2d_BR)) +
  geom_point(colour = "#1B9E77", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", colour = "#D95F02", fill = "#D95F02", alpha = 0.15) +
  ggrepel::geom_text_repel(size = 2.5, max.overlaps = 15) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_classic() +
  labs(
    title    = "Industry Non-Conversion Rate vs. Revenue Reallocation Rate",
    subtitle = "Each point = NACE 4-digit industry; averages over 2010-2022",
    x        = "Average Incumbent Non-Conversion Rate",
    y        = "Average Within-Industry Revenue Reallocation Rate"
  )
ggsave(paste0(output_dir, "industry_nonconv_vs_reallocation_scatter.png"),
       p_scatter, width = 8, height = 7)
