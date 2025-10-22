#' ------------------------------------------------------------------------------
#' Script: Knowledge diffusion, market power, productivity dispersion, and dynamism
#' Author: OpenAI ChatGPT
#' Date: 2025-05-11
#' Description: Implements empirical diagnostics to assess the slowdown in knowledge
#' diffusion in France and its relationship with market power, productivity
#' dispersion, and business dynamism using firm-level (1994-2022), product-level
#' (2009-2022) and patent-level datasets constructed in prior steps of the project.
#' Where possible, the script reuses pre-defined helper functions and variables from
#' `Main.R` and the Tools directory.
#' ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# 0. Setup and helper functions                                                #
# ---------------------------------------------------------------------------- #
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))
output_dir <- paste0(output_dir, "2025/Export 11.05/knowledge_diffusion/")
output_dir_creator(output_dir)

library(arrow)
library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(modelsummary)
library(purrr)
library(stringr)

# Helper: quiet mean/median to avoid NA warnings
safe_mean <- function(x) ifelse(all(is.na(x)), NA_real_, mean(x, na.rm = TRUE))
safe_var  <- function(x) ifelse(sum(!is.na(x)) <= 1, NA_real_, var(x, na.rm = TRUE))
safe_quant <- function(x, probs) ifelse(all(is.na(x)), NA_real_, quantile(x, probs, na.rm = TRUE, type = 7))

# ---------------------------------------------------------------------------- #
# 1. Load core datasets                                                        #
# ---------------------------------------------------------------------------- #

# Utility to load datasets that may be stored either as parquet or RDS
load_dataset <- function(path_candidates, reader, ...) {
  existing_path <- purrr::detect(path_candidates, file.exists)
  if (is.null(existing_path)) {
    stop(paste("None of the candidate files exist:", paste(path_candidates, collapse = ", ")))
  }
  reader(existing_path, ...)
}

firm_data <- load_dataset(
  c("1_firm_yr_lvl_br_dta.parquet", "firm_yr_lvl_br_dta.parquet"),
  read_parquet
) %>%
  as.data.table()

product_firm_panel <- load_dataset(
  c("product_firm_data_pre_high_growth.RDS", "product_firm_data.RDS"),
  readRDS
) %>%
  as.data.table()

product_detail <- load_dataset(
  c("product_data_10_digit_all_prodfra_.RDS", "product_level_growth_prodfra_.RDS"),
  readRDS
) %>%
  as.data.table()

patent_firm_panel <- load_dataset(
  c("patenting_products_firm_level.RDS", "4_firm_lvl_patent_dta.parquet"),
  function(path) if (stringr::str_detect(path, "parquet$")) read_parquet(path) else readRDS(path)
) %>%
  as.data.table()

ipcr_cumulative <- load_dataset(
  c("ipcr_cumulative.RDS", "4a_ipcr_cumulative.RDS"),
  readRDS
) %>%
  as.data.table()

# Harmonise firm identifiers across datasets
firm_data[, firmid := as.character(firmid)]
product_firm_panel[, firmid := as.character(firmid)]
product_detail[, firmid := as.character(firmid)]
patent_firm_panel[, firmid := as.character(firmid)]
ipcr_cumulative[, firmid := as.character(firmid)]

# Ensure consistent year coverage
firm_data <- firm_data[year >= 1994 & year <= 2022]
product_firm_panel <- product_firm_panel[year >= 2009 & year <= 2022]
product_detail <- product_detail[year >= 2009 & year <= 2022]
ipcr_cumulative <- ipcr_cumulative[year >= 1994 & year <= 2022]

# Bring core firm characteristics for later merges
firm_core_vars <- firm_data[, .(firmid, year, NACE_BR, NACE_2d, size, young,
                                nq, nq_bar, empl, empl_bar, tfp, tfp_bar,
                                within_industry_rev_share, within_economy_rev_share_BR,
                                firm_birth_year = if ("firm_birth_year" %in% names(firm_data)) firm_birth_year else NA_integer_)]

# ---------------------------------------------------------------------------- #
# 2. Patent-based knowledge diffusion metrics                                  #
# ---------------------------------------------------------------------------- #

# Expand list column of new IPCR codes into event-level adoption dataset
ipcr_events <- ipcr_cumulative[
  !is.na(firmid) & !is.null(new_ipcr),
  .(ipcr_code = unlist(new_ipcr)),
  by = .(firmid, year)
][!is.na(ipcr_code)]

if (nrow(ipcr_events) == 0) {
  stop("No IPCR adoption events detected. Verify ipcr_cumulative object.")
}

setorder(ipcr_events, ipcr_code, year, firmid)

# Identify pioneer firm-year for each IPCR code
ipcr_pioneers <- ipcr_events[, .SD[1L], by = ipcr_code]
setnames(ipcr_pioneers, c("firmid", "year"), c("pioneer_firmid", "pioneer_year"))

ipcr_events <- merge(ipcr_events, ipcr_pioneers, by = "ipcr_code", all.x = TRUE)
ipcr_events[, adoption_lag := year - pioneer_year]
ipcr_events[, external_adoption := firmid != pioneer_firmid]

# Attach industry for adopter and pioneer
ipcr_events <- merge(ipcr_events, firm_data[, .(firmid, year, NACE_BR)],
                     by = c("firmid", "year"), all.x = TRUE)

ipcr_events <- merge(ipcr_events,
                     unique(firm_data[, .(pioneer_firmid = firmid, pioneer_year = year, pioneer_NACE = NACE_BR)]),
                     by = c("pioneer_firmid", "pioneer_year"), all.x = TRUE)

ipcr_events[, cross_industry := pioneer_NACE != NACE_BR]
ipcr_events[, pioneer_flag := as.integer(adoption_lag == 0)]

# Summaries by year and by industry-year
ipcr_diffusion_year <- ipcr_events[, .(
  total_new_codes = .N,
  share_pioneers   = safe_mean(pioneer_flag),
  share_external   = safe_mean(external_adoption),
  share_cross_ind  = safe_mean(cross_industry),
  mean_adoption_lag = safe_mean(adoption_lag),
  p95_adoption_lag  = safe_quant(adoption_lag, 0.95)
), by = year][order(year)]

ipcr_diffusion_industry <- ipcr_events[, .(
  total_new_codes = .N,
  share_external   = safe_mean(external_adoption),
  share_cross_ind  = safe_mean(cross_industry),
  mean_adoption_lag = safe_mean(adoption_lag),
  median_adoption_lag = safe_quant(adoption_lag, 0.5)
), by = .(NACE_BR, year)]

ipcr_cols_to_rename <- setdiff(names(ipcr_diffusion_industry), c('NACE_BR', 'year'))
setnames(ipcr_diffusion_industry, ipcr_cols_to_rename, paste0('ipcr_', ipcr_cols_to_rename))

# Save outputs
fwrite(ipcr_diffusion_year, file.path(output_dir, "ipcr_diffusion_year.csv"))
fwrite(ipcr_diffusion_industry, file.path(output_dir, "ipcr_diffusion_industry.csv"))

# Plot the evolution of adoption lags
lag_plot <- ggplot(ipcr_diffusion_year, aes(x = year, y = mean_adoption_lag)) +
  geom_line(color = "#1f78b4", linewidth = 1) +
  geom_point(color = "#1f78b4") +
  geom_ribbon(aes(ymin = 0, ymax = p95_adoption_lag), alpha = 0.1, fill = "#1f78b4") +
  labs(title = "Average lag to adopt new IPCR codes",
       subtitle = "Mean and 95th percentile of adoption lags",
       x = "Year", y = "Years since pioneer adoption") +
  theme_minimal()

ggsave(file.path(output_dir, "ipcr_adoption_lag_trend.png"), lag_plot, width = 7, height = 4)

# ---------------------------------------------------------------------------- #
# 3. Product-level diffusion metrics                                          #
# ---------------------------------------------------------------------------- #

# Identify first introduction of products (10-digit prodfra codes)
product_events <- product_detail[
  first_introduction == 1,
  .(firmid, prodfra_plus, year)
]

if (nrow(product_events) == 0) {
  warning("No product introduction events found in product_detail. Check data availability.")
} else {
  product_pioneers <- product_events[, .SD[1L], by = prodfra_plus]
  setnames(product_pioneers, c("firmid", "year"), c("pioneer_firmid", "pioneer_year"))

  product_events <- merge(product_events, product_pioneers, by = "prodfra_plus", all.x = TRUE)
  product_events[, adoption_lag := year - pioneer_year]
  product_events[, pioneer_flag := as.integer(adoption_lag == 0)]
  product_events[, firmid := as.character(firmid)]
  product_events[, pioneer_firmid := as.character(pioneer_firmid)]

  product_events <- merge(product_events, firm_core_vars[, .(firmid, year, NACE_BR)],
                          by = c("firmid", "year"), all.x = TRUE)
  product_events <- merge(product_events,
                          unique(firm_core_vars[, .(pioneer_firmid = firmid, pioneer_year = year, pioneer_NACE = NACE_BR)]),
                          by = c("pioneer_firmid", "pioneer_year"), all.x = TRUE)
  product_events[, cross_industry := pioneer_NACE != NACE_BR]

  product_diffusion_year <- product_events[, .(
    total_new_products = .N,
    share_pioneers = safe_mean(pioneer_flag),
    share_cross_ind = safe_mean(cross_industry),
    mean_adoption_lag = safe_mean(adoption_lag),
    p95_adoption_lag = safe_quant(adoption_lag, 0.95)
  ), by = year][order(year)]

  product_diffusion_industry <- product_events[, .(
    total_new_products = .N,
    share_cross_ind = safe_mean(cross_industry),
    mean_adoption_lag = safe_mean(adoption_lag)
  ), by = .(NACE_BR, year)]

  prod_cols_to_rename <- setdiff(names(product_diffusion_industry), c('NACE_BR', 'year'))
  setnames(product_diffusion_industry, prod_cols_to_rename, paste0('product_', prod_cols_to_rename))

  fwrite(product_diffusion_year, file.path(output_dir, "product_diffusion_year.csv"))
  fwrite(product_diffusion_industry, file.path(output_dir, "product_diffusion_industry.csv"))

  product_lag_plot <- ggplot(product_diffusion_year, aes(x = year, y = mean_adoption_lag)) +
    geom_line(color = "#33a02c", linewidth = 1) +
    geom_point(color = "#33a02c") +
    geom_ribbon(aes(ymin = 0, ymax = p95_adoption_lag), alpha = 0.1, fill = "#33a02c") +
    labs(title = "Average lag to adopt new products",
         subtitle = "10-digit Prodfra codes",
         x = "Year", y = "Years since first market appearance") +
    theme_minimal()

  ggsave(file.path(output_dir, "product_adoption_lag_trend.png"), product_lag_plot, width = 7, height = 4)
}

# ---------------------------------------------------------------------------- #
# 4. Productivity dispersion and convergence proxies                          #
# ---------------------------------------------------------------------------- #

firm_productivity <- firm_data[!is.na(tfp), .(
  firmid, year, NACE_BR, tfp
)]

productivity_dispersion <- firm_productivity[, .(
  tfp_var = safe_var(tfp),
  tfp_iqr = safe_quant(tfp, 0.75) - safe_quant(tfp, 0.25),
  tfp_gap_90_10 = safe_quant(tfp, 0.90) - safe_quant(tfp, 0.10),
  tfp_frontier = safe_quant(tfp, 0.95),
  tfp_median = safe_quant(tfp, 0.50)
), by = .(NACE_BR, year)]

productivity_dispersion[, frontier_gap := tfp_frontier - tfp_median]
fwrite(productivity_dispersion, file.path(output_dir, "productivity_dispersion.csv"))

# ---------------------------------------------------------------------------- #
# 5. Market power measures                                                     #
# ---------------------------------------------------------------------------- #

# Industry concentration (HHI and top shares)
industry_sales <- firm_data[!is.na(nq), .(industry_sales = sum(nq, na.rm = TRUE)), by = .(NACE_BR, year)]
firm_sales <- merge(firm_data[, .(firmid, year, NACE_BR, nq)], industry_sales,
                    by = c("NACE_BR", "year"), all.x = TRUE)
firm_sales[, market_share := fifelse(industry_sales > 0, nq / industry_sales, NA_real_)]

industry_concentration <- firm_sales[, .(
  HHI = sum(market_share^2, na.rm = TRUE),
  top4_share = sum(sort(market_share, decreasing = TRUE)[seq_len(min(.N, 4))], na.rm = TRUE),
  firm_count = .N
), by = .(NACE_BR, year)]

# Markup proxy: use within-industry revenue share if markups unavailable
markup_var <- intersect(c("markup", "mark_up", "markup_dw", "markup_translog"), names(firm_data))
if (length(markup_var) > 0) {
  markup_proxy <- firm_data[, .(markup = get(markup_var[1]), firmid, year, NACE_BR)]
} else {
  markup_proxy <- firm_data[, .(markup = within_industry_rev_share, firmid, year, NACE_BR)]
}

markup_dispersion <- markup_proxy[!is.na(markup), .(
  mean_markup = safe_mean(markup),
  markup_iqr = safe_quant(markup, 0.75) - safe_quant(markup, 0.25)
), by = .(NACE_BR, year)]

fwrite(industry_concentration, file.path(output_dir, "industry_concentration.csv"))
fwrite(markup_dispersion, file.path(output_dir, "markup_dispersion.csv"))

# ---------------------------------------------------------------------------- #
# 6. Business dynamism metrics                                                 #
# ---------------------------------------------------------------------------- #

# Entry/exit indicators using firm birth and observed panel boundaries
firm_data[, firm_min_year := min(year, na.rm = TRUE), by = firmid]
firm_data[, firm_max_year := max(year, na.rm = TRUE), by = firmid]

sample_min_year <- min(firm_data$year, na.rm = TRUE)
sample_max_year <- max(firm_data$year, na.rm = TRUE)

firm_data[, entry := as.integer(year == firm_min_year & firm_min_year > sample_min_year)]
firm_data[, exit := as.integer(year == firm_max_year & firm_max_year < sample_max_year)]
firm_data[, high_growth := as.integer(!is.na(empl_growth) & empl_growth >= 0.20)]
firm_data[, shrinking := as.integer(!is.na(empl_growth) & empl_growth <= -0.20)]

business_dynamism <- firm_data[, .(
  entry_rate = safe_mean(entry),
  exit_rate = safe_mean(exit),
  share_high_growth = safe_mean(high_growth),
  share_shrinking = safe_mean(shrinking),
  job_reallocation = safe_mean(abs(empl_growth)),
  sales_reallocation = safe_mean(abs(nq_growth))
), by = .(NACE_BR, year)]

fwrite(business_dynamism, file.path(output_dir, "business_dynamism.csv"))

# ---------------------------------------------------------------------------- #
# 7. Link diffusion to outcomes: panel regressions                             #
# ---------------------------------------------------------------------------- #

panel_components <- list(
  ipcr_diffusion_industry,
  if (exists('product_diffusion_industry')) product_diffusion_industry else NULL,
  productivity_dispersion,
  industry_concentration,
  markup_dispersion,
  business_dynamism
) %>% purrr::compact()

if (length(panel_components) == 0) {
  stop('No components available to build the analysis panel.')
}

analysis_panel <- Reduce(
  function(dt_left, dt_right) merge(dt_left, dt_right, by = c('NACE_BR', 'year'), all = TRUE),
  panel_components
)

if (nrow(analysis_panel) == 0) {
  stop('Merged analysis panel is empty. Verify that component datasets overlap in (NACE_BR, year).')
}

if ('ipcr_share_external' %in% names(analysis_panel)) {
  analysis_panel[, diffusion_external_share := ipcr_share_external]
}
if ('ipcr_share_cross_ind' %in% names(analysis_panel)) {
  analysis_panel[, diffusion_cross_share := ipcr_share_cross_ind]
}
if ('product_share_cross_ind' %in% names(analysis_panel)) {
  analysis_panel[, product_diffusion_cross := product_share_cross_ind]
}
if ('ipcr_mean_adoption_lag' %in% names(analysis_panel)) {
  analysis_panel[, ipcr_diffusion_lag := ipcr_mean_adoption_lag]
}
if ('product_mean_adoption_lag' %in% names(analysis_panel)) {
  analysis_panel[, product_diffusion_lag := product_mean_adoption_lag]
}
analysis_panel[, firm_count := fifelse(is.na(firm_count), 1, firm_count)]
if (!'diffusion_external_share' %in% names(analysis_panel)) {
  analysis_panel[, diffusion_external_share := NA_real_]
}
if (!'ipcr_diffusion_lag' %in% names(analysis_panel)) {
  analysis_panel[, ipcr_diffusion_lag := NA_real_]
}

# Regressions: market power, productivity dispersion, entry rate
reg_market_power <- feols(HHI ~ diffusion_external_share + ipcr_diffusion_lag | NACE_BR + year,
                          data = analysis_panel, weights = ~ firm_count)

reg_productivity <- feols(frontier_gap ~ diffusion_external_share + ipcr_diffusion_lag | NACE_BR + year,
                          data = analysis_panel, weights = ~ firm_count)

reg_dynamism <- feols(entry_rate ~ diffusion_external_share + ipcr_diffusion_lag | NACE_BR + year,
                      data = analysis_panel, weights = ~ firm_count)

modelsummary(
  list(MarketPower = reg_market_power, ProductivityGap = reg_productivity, EntryRate = reg_dynamism),
  output = file.path(output_dir, "diffusion_regressions.html"),
  gof_omit = "IC|Log"
)

# ---------------------------------------------------------------------------- #
# 8. Save intermediate objects for reuse                                      #
# ---------------------------------------------------------------------------- #

saveRDS(list(
  ipcr_events = ipcr_events,
  product_events = if (exists("product_events")) product_events else NULL,
  ipcr_diffusion_year = ipcr_diffusion_year,
  product_diffusion_year = if (exists("product_diffusion_year")) product_diffusion_year else NULL,
  productivity_dispersion = productivity_dispersion,
  industry_concentration = industry_concentration,
  markup_dispersion = markup_dispersion,
  business_dynamism = business_dynamism,
  analysis_panel = analysis_panel,
  regressions = list(
    market_power = reg_market_power,
    productivity = reg_productivity,
    dynamism = reg_dynamism
  )
), file = file.path(output_dir, "knowledge_diffusion_workspace.RDS"))

message("Knowledge diffusion diagnostics completed and saved to ", output_dir)