# =============================================================================
# Module 10: Markup, Market Power, and Patent Non-Conversion
# =============================================================================
# Objective: Test whether high-markup firms strategically avoid converting
#            patents to products — i.e. whether market power suppresses the
#            innovative output of patenting activity.
#
# Theoretical prediction (rent-preservation motive):
#   A firm with high markups faces a cannibalization cost from launching new
#   products that compete with its existing portfolio. It therefore has less
#   incentive to convert patents into market-ready products. This effect should
#   be amplified in concentrated industries (where the firm's market power is
#   not competed away) and for large incumbents (who hold the most to lose).
#
# Empirical tests:
#   1. Descriptive: markup distribution by conversion status (converting vs not)
#   2. Core regression: conversion_it ~ log_markup_{t-1} + NACE_BR FE + year FE
#      → does higher prior markup predict lower conversion probability?
#   3. Heterogeneity: markup × HHI, markup × size, markup × leader status
#      → is the effect stronger when market power is structural?
#   4. Mechanism: markup growth → falling conversion
#      → is it rising markups (not levels) that suppress conversion?
#   5. Time-varying: markup × year trend
#      → has the markup-suppression relationship strengthened over time?
#   6. Rolling coefficient: 5-year window estimates over the sample period
#
# Data inputs:
#   - 7_final_firm_lvl_dta.rds          (full merged firm-year panel)
#   - 2_product_data/.../2e_...parquet  (NACE concordance conversion measure)
#
# Key variables used:
#   - mu_sepcal_TL_sBFSR_t1/t8/t10     (Translog markups; TL baseline)
#   - mu_sepcal_CD_sBFSR_t1            (Cobb-Douglas robustness)
#   - num_pat_families, prod_added      (patenting and product creation)
#   - HHI_industry, leader, log_empl_bar (market structure)
#   - consolidated_birth_year           (incumbent flag)
# =============================================================================

source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

output_dir <- paste0(output_dir, "10_markup_nonconversion/")
output_dir_creator(output_dir)

# --- Parameters ---------------------------------------------------------------
baseline_window  <- 2
conversion_windows <- c(0, 2, 3, 5)
markup_tl        <- "mu_sepcal_TL_sBFSR_t1"   # Translog, structural, baseline
markup_cd        <- "mu_sepcal_CD_sBFSR_t1"   # Cobb-Douglas robustness
markup_tl_t8     <- "mu_sepcal_TL_sBFSR_t8"   # Translog, size threshold 8
prod_start       <- 2010

# --- 1. Data Loading ----------------------------------------------------------
firm_data <- as.data.table(read_rds("7_final_firm_lvl_dta.rds"))
firm_data <- firm_data[NACE_2d_BR %in% prodcom_sectors & year >= prod_start]

nace_pat_prod <- as.data.table(read_parquet(
  paste0("2_product_data/", cpa_or_pf, "/2e_firm_lvl_patent_product_nace2d_dta.parquet")
))
nace_pat_prod <- nace_pat_prod[, !grepl("h[1345]", names(nace_pat_prod)), with = FALSE]

# --- 2. Conversion Variables --------------------------------------------------
setorder(firm_data, firmid, year)

for (w in conversion_windows) {
  firm_data[, paste0("future_products_", w) :=
    shift(frollsum(prod_added, n = w + 1, align = "left"), type = "lead", n = 0),
    by = firmid]

  firm_data[, paste0("converted_patents_", w) :=
    fifelse(num_pat_families > 0,
            pmin(1, num_pat_families, get(paste0("future_products_", w))),
            NA_real_)]

  firm_data[, paste0("nonconverted_patents_", w) :=
    fifelse(num_pat_families > 0,
            1 - get(paste0("converted_patents_", w)),
            NA_real_)]
}

firm_data <- merge(firm_data, nace_pat_prod, by = c("firmid", "year"),
                   all.x = TRUE, sort = FALSE)

# --- 3. Markup Preprocessing --------------------------------------------------
# Keep only positive markups (negative values are estimation artefacts)
firm_data <- firm_data[get(markup_tl) > 0 | is.na(get(markup_tl))]
firm_data[, log_markup    := log(get(markup_tl))]
firm_data[, log_markup_cd := log(get(markup_cd))]
firm_data[, log_markup_t8 := log(get(markup_tl_t8))]

# Winsorise at 1st / 99th percentiles to limit influence of extreme values
p01 <- quantile(firm_data$log_markup, 0.01, na.rm = TRUE)
p99 <- quantile(firm_data$log_markup, 0.99, na.rm = TRUE)
firm_data[, log_markup_w := pmax(p01, pmin(p99, log_markup))]

# Lagged markup: treat as determined before the patenting/conversion decision
firm_data[, log_markup_l   := shift(log_markup_w, 1, type = "lag"), by = firmid]
firm_data[, log_markup_cd_l := shift(log_markup_cd, 1, type = "lag"), by = firmid]

# Markup growth: change in market power over time
firm_data[, markup_growth := log_markup_w - log_markup_l]

# Incumbent flag
firm_data[, incumbent := as.integer(consolidated_birth_year < year)]

# --- 4. Analysis Sample -------------------------------------------------------
# Restrict to patenting incumbent firm-years with valid markup
pat_data <- firm_data[
  num_pat_families > 0 &
    incumbent == 1 &
    !is.na(log_markup_l) &
    !is.na(converted_patents_2)
]

cat(sprintf("Patenting incumbent firm-years with valid markup: %d\n", nrow(pat_data)))
cat(sprintf("Unique firms: %d\n", uniqueN(pat_data$firmid)))
cat(sprintf("Year range: %d - %d\n", min(pat_data$year), max(pat_data$year)))

# --- 5. Descriptive Statistics ------------------------------------------------
# 5.1 Markup by conversion status
desc_conv <- pat_data[, .(
  n_obs          = .N,
  mean_markup    = mean(get(markup_tl), na.rm = TRUE),
  median_markup  = median(get(markup_tl), na.rm = TRUE),
  sd_markup      = sd(get(markup_tl), na.rm = TRUE),
  mean_log_markup = mean(log_markup_w, na.rm = TRUE)
), by = .(conversion_status = fifelse(converted_patents_2 >= 0.5,
                                      "Converting", "Non-Converting"))]

create_latex_table(
  data         = desc_conv,
  caption      = "Markup by Patent Conversion Status (2-year window, incumbent patenting firms)",
  output_dir   = output_dir,
  include_preamble = FALSE,
  digits       = 4,
  filename     = "markup_by_conversion_status"
)

# 5.2 Markup gradient over time: converting vs. non-converting
markup_by_year <- pat_data[!is.na(converted_patents_2), .(
  converting    = weighted.mean(log_markup_w[converted_patents_2 >= 0.5],
                                nq_bar[converted_patents_2 >= 0.5], na.rm = TRUE),
  nonconverting = weighted.mean(log_markup_w[converted_patents_2 < 0.5],
                                nq_bar[converted_patents_2 < 0.5], na.rm = TRUE)
), by = year]

markup_yr_long <- melt(markup_by_year, id.vars = "year",
                        variable.name = "status", value.name = "mean_log_markup")
markup_yr_long[, status := factor(status,
  levels = c("converting", "nonconverting"),
  labels = c("Converting", "Non-Converting"))]

p_trend <- ggplot(markup_yr_long, aes(x = year, y = mean_log_markup,
                                       colour = status, linetype = status)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Converting" = "#1B9E77", "Non-Converting" = "#D95F02")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.title.x = element_blank()) +
  labs(
    title    = "Revenue-Weighted Average Markup: Converting vs. Non-Converting Patenting Firms",
    subtitle = "Incumbent firms that filed patents; 2-year conversion window",
    y        = "Mean Log Markup (Translog, winsorised)"
  )
ggsave(paste0(output_dir, "markup_converting_vs_nonconverting_trend.png"),
       p_trend, width = 9, height = 6)

# 5.3 Conversion rate by markup quintile (summary gradient)
pat_data[, markup_quintile := cut(
  log_markup_w,
  breaks = quantile(log_markup_w, probs = seq(0, 1, 0.2), na.rm = TRUE),
  labels = paste0("Q", 1:5),
  include.lowest = TRUE
)]

conv_by_quintile <- pat_data[!is.na(markup_quintile), .(
  mean_conv    = weighted.mean(converted_patents_2, w = nq_bar, na.rm = TRUE),
  mean_nonconv = weighted.mean(nonconverted_patents_2, w = nq_bar, na.rm = TRUE),
  n            = .N
), by = markup_quintile]

p_gradient <- ggplot(conv_by_quintile,
                     aes(x = markup_quintile, y = mean_conv, group = 1)) +
  geom_col(fill = "#1B9E77", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(
    ymin = mean_conv - 1.96 * sqrt(mean_conv * (1 - mean_conv) / n),
    ymax = mean_conv + 1.96 * sqrt(mean_conv * (1 - mean_conv) / n)
  ), width = 0.25) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_classic() +
  labs(
    title    = "Patent Conversion Rate by Markup Quintile",
    subtitle = "Revenue-weighted; incumbent patenting firms; 2-year window",
    x        = "Log Markup Quintile (Q1 = lowest, Q5 = highest)",
    y        = "Conversion Rate"
  )
ggsave(paste0(output_dir, "conversion_by_markup_quintile.png"),
       p_gradient, width = 8, height = 6)

# --- 6. Core Regressions: Markup → Conversion ---------------------------------

# Spec 1: Baseline (revenue-weighted, industry + year FE)
m1 <- feols(converted_patents_2 ~ log_markup_l | NACE_BR + year,
            data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# Spec 2: Unweighted
m2 <- feols(converted_patents_2 ~ log_markup_l | NACE_BR + year,
            data = pat_data, cluster = ~NACE_2d_BR)

# Spec 3: Firm fixed effects (within-firm variation in markup and conversion)
m3 <- feols(converted_patents_2 ~ log_markup_l | firmid + year,
            data = pat_data, weights = ~nq_bar, cluster = ~firmid)

# Spec 4: Non-conversion as outcome (mirror of spec 1)
m4 <- feols(nonconverted_patents_2 ~ log_markup_l | NACE_BR + year,
            data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# Spec 5: Cobb-Douglas markup robustness
m5 <- feols(converted_patents_2 ~ log_markup_cd_l | NACE_BR + year,
            data = pat_data[!is.na(log_markup_cd_l)],
            weights = ~nq_bar, cluster = ~NACE_2d_BR)

# Spec 6: NACE concordance measure (share of patents in new products)
m6 <- feols(share_new_patent_in_new_products_h2 ~ log_markup_l | NACE_BR + year,
            data = pat_data[!is.na(share_new_patent_in_new_products_h2)],
            weights = ~nq_bar, cluster = ~NACE_2d_BR)

# Spec 7: Alternative threshold markup (t8)
m7 <- feols(converted_patents_2 ~ log_markup_t8 | NACE_BR + year,
            data = pat_data[!is.na(log_markup_t8)],
            weights = ~nq_bar, cluster = ~NACE_2d_BR)

models_core <- list(
  "Conv. (Rev.Wt.)"  = m1,
  "Conv. (Unwtd.)"   = m2,
  "Conv. (Firm FE)"  = m3,
  "Non-Conv."        = m4,
  "Conv. (CD)"       = m5,
  "Pat-Prod Share"   = m6,
  "Conv. (TL t8)"    = m7
)

modelsummary(
  models_core,
  output   = paste0(output_dir, "markup_conversion_core.tex"),
  stars    = TRUE,
  title    = "Markup and Patent-to-Product Conversion: Core Specifications",
  coef_rename = c(
    "log_markup_l"    = "Log Markup (t-1, TL-t1)",
    "log_markup_cd_l" = "Log Markup (t-1, CD-t1)",
    "log_markup_t8"   = "Log Markup (TL-t8)"
  ),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = paste0(
    "Incumbent patenting firms only (num\\_pat\\_families > 0). ",
    "Specs 1,4-7: NACE 4-digit + year FE, clustered at NACE 2-digit. ",
    "Spec 3: firm + year FE, clustered at firm level. ",
    "Outcome: converted\\_patents\\_2 = 1 if patents led to product(s) within 2 years."
  )
)

# --- 7. Heterogeneity: Markup × Market Structure ------------------------------
# Prediction: markup suppresses conversion most strongly when market power
# is structural (high HHI) and when the firm has most to lose (large/leader).

# 7.1 Markup × HHI: rent-preservation is stronger in concentrated industries
m_h1 <- feols(converted_patents_2 ~ log_markup_l * HHI_industry | NACE_BR + year,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# 7.2 Markup × firm size
m_h2 <- feols(converted_patents_2 ~ log_markup_l * log_empl_bar | NACE_BR + year,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# 7.3 Markup × leader status
m_h3 <- feols(converted_patents_2 ~ log_markup_l * leader | NACE_BR + year,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# 7.4 Markup × HHI, controlling for size
m_h4 <- feols(converted_patents_2 ~ log_markup_l * HHI_industry + log_markup_l * log_empl_bar | NACE_BR + year,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# 7.5 Separate regressions by size bucket
titles_size <- fread(
  "title, restriction, weight_flag, additional_controls
  M. Small,  age_size_bucket==\"mature_small\",  T,
  M. Medium, age_size_bucket==\"mature_medium\", T,
  M. Large,  age_size_bucket==\"mature_large\",  T,
  Y. Small,  age_size_bucket==\"young_small\",   T,
  Y. NonSmall, age_size_bucket==\"young_large\", T,
  All, NA, T, log_empl_bar + HHI_industry"
)

formulas_markup <- create_formulas(
  ys       = "converted_patents_2",
  xs       = "log_markup_l",
  controls = "",
  fe       = "NACE_BR + year"
)

regression_innovation_growth(
  regression_name         = "markup_x_conversion_by_size",
  data                    = pat_data,
  formulas                = formulas_markup,
  titles_and_restrictions = titles_size,
  types                   = "all_firms",
  subsets                 = "all",
  graphs                  = TRUE
)

models_het <- list(
  "Markup × HHI"          = m_h1,
  "Markup × Size"         = m_h2,
  "Markup × Leader"       = m_h3,
  "Markup × HHI + Size"   = m_h4
)

modelsummary(
  models_het,
  output   = paste0(output_dir, "markup_conversion_heterogeneity.tex"),
  stars    = TRUE,
  title    = "Markup and Non-Conversion: Heterogeneity by Market Structure",
  coef_rename = c(
    "log_markup_l"              = "Log Markup (t-1)",
    "HHI_industry"              = "HHI",
    "log_empl_bar"              = "Log Employment",
    "leader"                    = "Leader (top 1)",
    "log_markup_l:HHI_industry" = "Log Markup × HHI",
    "log_markup_l:log_empl_bar" = "Log Markup × Log Employment",
    "log_markup_l:leader"       = "Log Markup × Leader"
  ),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = paste0(
    "Incumbent patenting firms. Revenue-weighted. ",
    "NACE 4-digit + year FE. Clustered SEs at NACE 2-digit."
  )
)

# Coefficient plot: markup × HHI interaction across HHI terciles
pat_data[, HHI_tercile := cut(HHI_industry,
                               breaks = quantile(HHI_industry, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                               labels = c("Low HHI", "Mid HHI", "High HHI"),
                               include.lowest = TRUE)]

models_hhi_tercile <- setNames(
  lapply(c("Low HHI", "Mid HHI", "High HHI"), function(t) {
    feols(converted_patents_2 ~ log_markup_l | NACE_BR + year,
          data = pat_data[HHI_tercile == t], weights = ~nq_bar, cluster = ~NACE_2d_BR)
  }),
  c("Low Concentration", "Mid Concentration", "High Concentration")
)

tidy_tercile <- imap_dfr(models_hhi_tercile, function(m, nm) {
  tidy(m, conf.int = TRUE) %>% mutate(group = nm)
}) %>% filter(term == "log_markup_l")
setDT(tidy_tercile)
tidy_tercile[, group := factor(group,
  levels = c("Low Concentration", "Mid Concentration", "High Concentration"))]

p_hhi_coef <- ggplot(tidy_tercile,
                     aes(x = group, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(colour = "#1B9E77", size = 0.8) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  labs(
    title    = "Effect of Log Markup on Conversion Rate by Industry Concentration Tercile",
    subtitle = "Revenue-weighted OLS; NACE 4-digit + year FE; 95% CI",
    y        = "Coefficient on Log Markup (t-1)"
  )
ggsave(paste0(output_dir, "markup_conversion_by_HHI_tercile.png"),
       p_hhi_coef, width = 8, height = 5)

# --- 8. Mechanism: Markup Growth → Falling Conversion ------------------------
# Tests whether it is rising markups (accumulating market power), not just
# the level, that suppresses conversion.

pat_data2 <- firm_data[
  num_pat_families > 0 &
    incumbent == 1 &
    !is.na(markup_growth) &
    !is.na(converted_patents_2)
]

m_g1 <- feols(converted_patents_2 ~ markup_growth | NACE_BR + year,
              data = pat_data2, weights = ~nq_bar, cluster = ~NACE_2d_BR)

m_g2 <- feols(converted_patents_2 ~ markup_growth * HHI_industry | NACE_BR + year,
              data = pat_data2, weights = ~nq_bar, cluster = ~NACE_2d_BR)

m_g3 <- feols(converted_patents_2 ~ markup_growth * leader | NACE_BR + year,
              data = pat_data2, weights = ~nq_bar, cluster = ~NACE_2d_BR)

# Joint test of level + growth
m_g4 <- feols(converted_patents_2 ~ log_markup_l + markup_growth | NACE_BR + year,
              data = pat_data2[!is.na(log_markup_l)], weights = ~nq_bar, cluster = ~NACE_2d_BR)

models_growth <- list(
  "Conv. ~ Markup Growth"         = m_g1,
  "Conv. ~ MG × HHI"             = m_g2,
  "Conv. ~ MG × Leader"          = m_g3,
  "Conv. ~ Level + Growth"       = m_g4
)

modelsummary(
  models_growth,
  output   = paste0(output_dir, "markup_growth_conversion.tex"),
  stars    = TRUE,
  title    = "Markup Growth and Patent-to-Product Conversion",
  coef_rename = c(
    "markup_growth"              = "Markup Growth",
    "log_markup_l"               = "Log Markup (t-1)",
    "HHI_industry"               = "HHI",
    "leader"                     = "Leader",
    "markup_growth:HHI_industry" = "Markup Growth × HHI",
    "markup_growth:leader"       = "Markup Growth × Leader"
  ),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = paste0(
    "Incumbent patenting firms. Revenue-weighted. ",
    "NACE 4-digit + year FE. Clustered SEs at NACE 2-digit. ",
    "Markup Growth = Δ log markup (Translog, winsorised)."
  )
)

# --- 9. Time-Varying Relationship: Has Markup-Suppression Strengthened? -------
pat_data[, year_trend := year - min(year, na.rm = TRUE)]

m_t1 <- feols(converted_patents_2 ~ log_markup_l * year_trend | NACE_BR,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

m_t2 <- feols(converted_patents_2 ~ log_markup_l * year_trend + HHI_industry | NACE_BR,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

m_t3 <- feols(converted_patents_2 ~ log_markup_l * year_trend * leader | NACE_BR,
              data = pat_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)

modelsummary(
  list("Markup × Trend" = m_t1, "Markup × Trend + HHI" = m_t2, "Markup × Trend × Leader" = m_t3),
  output   = paste0(output_dir, "markup_conversion_time_trend.tex"),
  stars    = TRUE,
  title    = "Time-Varying Relationship: Markup and Patent-to-Product Conversion",
  coef_rename = c(
    "log_markup_l"                    = "Log Markup (t-1)",
    "year_trend"                      = "Year Trend",
    "leader"                          = "Leader",
    "HHI_industry"                    = "HHI",
    "log_markup_l:year_trend"         = "Log Markup × Trend",
    "log_markup_l:leader"             = "Log Markup × Leader",
    "year_trend:leader"               = "Trend × Leader",
    "log_markup_l:year_trend:leader"  = "Log Markup × Trend × Leader"
  ),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = "NACE 4-digit FE. Year trend enters as continuous regressor. Clustered SEs at NACE 2-digit."
)

# --- 10. Rolling Coefficient: 5-Year Window -----------------------------------
# Tracks how the markup → conversion coefficient evolves over time.
rolling_coef <- rbindlist(lapply(sort(unique(pat_data$year)), function(y) {
  window_data <- pat_data[year %in% (y - 4):y]
  if (nrow(window_data) < 100 || uniqueN(window_data$firmid) < 20) return(NULL)
  tryCatch({
    mod <- feols(converted_patents_2 ~ log_markup_l | NACE_BR,
                 data = window_data, weights = ~nq_bar, cluster = ~NACE_2d_BR)
    data.table(
      year = y,
      coef = coef(mod)["log_markup_l"],
      se   = se(mod)["log_markup_l"]
    )
  }, error = function(e) NULL)
}))

p_rolling <- ggplot(rolling_coef, aes(x = year, y = coef)) +
  geom_ribbon(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
              fill = "#1B9E77", alpha = 0.2) +
  geom_line(colour = "#1B9E77", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  labs(
    title    = "Rolling 5-Year Coefficient: Log Markup → Patent Conversion Rate",
    subtitle = "Revenue-weighted OLS, NACE 4-digit FE; shaded band = 95% CI",
    y        = "Coefficient on Log Markup (t-1)"
  )
ggsave(paste0(output_dir, "markup_conversion_rolling_coef.png"),
       p_rolling, width = 9, height = 6)

# --- 11. Robustness: Alternative Conversion Windows --------------------------
models_windows <- setNames(
  lapply(conversion_windows, function(w) {
    temp <- pat_data[!is.na(get(paste0("converted_patents_", w)))]
    feols(as.formula(paste0("converted_patents_", w, " ~ log_markup_l | NACE_BR + year")),
          data = temp, weights = ~nq_bar, cluster = ~NACE_2d_BR)
  }),
  paste0("Window = ", conversion_windows, "yr")
)

modelsummary(
  models_windows,
  output   = paste0(output_dir, "markup_conversion_windows_robustness.tex"),
  stars    = TRUE,
  title    = "Robustness: Markup and Conversion Rate by Conversion Window",
  coef_rename = c("log_markup_l" = "Log Markup (t-1, TL)"),
  gof_omit = "Std.Errors|R2 Within Adj.|AIC|BIC|Log.Lik.",
  notes    = "Incumbent patenting firms. Revenue-weighted. NACE 4-digit + year FE. Clustered at NACE 2-digit."
)

# --- 12. Summary: Implied Effect Sizes ----------------------------------------
# Compute the magnitudes implied by the main specification (m1) for the paper.
beta_m1 <- coef(m1)["log_markup_l"]
sd_markup <- sd(pat_data$log_markup_l, na.rm = TRUE)
mean_conv  <- mean(pat_data$converted_patents_2, na.rm = TRUE)

cat("\n=== Implied Effect Sizes (baseline spec m1) ===\n")
cat(sprintf("Coefficient on log markup (t-1):  %.4f (SE = %.4f)\n",
            beta_m1, se(m1)["log_markup_l"]))
cat(sprintf("1 SD increase in log markup → Δ conversion: %.4f pp (%.2f%% of mean)\n",
            beta_m1 * sd_markup * 100, beta_m1 * sd_markup / mean_conv * 100))
cat(sprintf("10th→90th pctile markup → Δ conversion: %.4f pp\n",
            beta_m1 *
              diff(quantile(pat_data$log_markup_l, c(0.10, 0.90), na.rm = TRUE)) * 100))
