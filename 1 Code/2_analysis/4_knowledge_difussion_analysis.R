# ------------------------------------------------------------------------------
# Script: Knowledge diffusion and industry outcomes
# Goal:  Build knowledge diffusion metrics from patent data and relate them to
#        industry concentration, factor shares, entry, dynamism, and growth
#        dispersion. Designed to run after the cleaning scripts in 1_cleaning.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Setup and helper utilities
# ------------------------------------------------------------------------------

source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

analysis_output_dir <- file.path(output_dir, "knowledge_diffusion", format(Sys.Date(), "%Y%m%d"))
output_dir_creator(analysis_output_dir)

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(dplyr)
  library(fixest)
  library(modelsummary)
  library(purrr)
  library(stringr)
})

safe_mean <- function(x) ifelse(all(is.na(x)), NA_real_, mean(x, na.rm = TRUE))
safe_median <- function(x) ifelse(all(is.na(x)), NA_real_, median(x, na.rm = TRUE))
safe_sd <- function(x) ifelse(sum(!is.na(x)) <= 1, NA_real_, sd(x, na.rm = TRUE))
safe_quant <- function(x, probs) ifelse(all(is.na(x)), NA_real_, quantile(x, probs, na.rm = TRUE, type = 7))
safe_ratio <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, num / den)

load_dataset <- function(path_candidates, reader, ...) {
  existing_path <- purrr::detect(path_candidates, file.exists)
  if (is.null(existing_path)) {
    stop(paste("None of the candidate files exist:", paste(path_candidates, collapse = ", ")))
  }
  reader(existing_path, ...)
}

to_dt <- function(obj) as.data.table(obj)

# ------------------------------------------------------------------------------
# 1. Load core datasets (paths relative to Main.R working directory)
# ------------------------------------------------------------------------------

firm_data <- load_dataset(
  c("1_firm_yr_lvl_br_dta.parquet"),
  read_parquet
) |> to_dt()


firm_data <- read_parquet("1_firm_yr_lvl_br_dta.parquet") 
industry_data <- load_dataset(
  c("5_industry_yr_lvl_dta.parquet"),
  read_parquet
) |> to_dt()

patent_data <- load_dataset(
  c("3_patent_lvl_patent_dta.parquet", "patent_data/3_patent_lvl_patent_dta.parquet"),
  read_parquet
) |> to_dt()

citation_summary <- load_dataset(
  c("3b_patent_citation_summary.RDS", "patent_data/3b_patent_citation_summary.RDS"),
  readRDS
) |> to_dt()

ipcr_cumulative <- load_dataset(
  c("4a_ipcr_cumulative.RDS"),
  readRDS
) |> to_dt()

# ------------------------------------------------------------------------------
# 2. Firm-year controls and lookup tables
# ------------------------------------------------------------------------------

firm_core <- firm_data[, .(
  firmid = as.character(firmid),
  year,
  NACE_BR,
  nq,
  nq_bar,
  empl,
  empl_bar,
  labor_cost,
  capital,
  raw_materials,
  young,
  born,
  died,
  empl_growth,
  nq_growth,
  empl_reallocation_weighted,
  nq_reallocation_weighted,
  tfpr
)]

firm_core[, firmid := as.character(firmid)]
firm_core <- firm_core[!is.na(NACE_BR)]

# ------------------------------------------------------------------------------
# 3. Patent citation-based diffusion metrics
# ------------------------------------------------------------------------------

patent_lookup <- unique(patent_data[, .(
  patent_family,
  firmid = as.character(firmid),
  filing_year = as.integer(filing_year)
)])

citation_panel <- merge(
  citation_summary,
  patent_lookup,
  by = "patent_family",
  all.x = TRUE
)

citation_panel <- merge(
  citation_panel,
  firm_core[, .(firmid, year, NACE_BR)],
  by.x = c("firmid", "filing_year"),
  by.y = c("firmid", "year"),
  all.x = TRUE
)

citation_panel <- citation_panel[!is.na(NACE_BR) & !is.na(filing_year)]
setnames(citation_panel, "filing_year", "year")

citation_industry <- citation_panel[
  ,
  .(
    patents_with_citations = .N,
    citation_external_share = safe_ratio(sum(n_external_citations, na.rm = TRUE), sum(n_prior_art, na.rm = TRUE)),
    citation_self_share = safe_ratio(sum(n_self_citations, na.rm = TRUE), sum(n_prior_art, na.rm = TRUE)),
    mean_distinct_prior_art = safe_mean(n_distinct_prior_art),
    median_distinct_prior_art = safe_median(n_distinct_prior_art)
  ),
  by = .(NACE_BR, year)
]

citation_flows <- citation_panel[
  ,
  .(prior_firmid = unlist(firmid_prior_list)),
  by = .(NACE_BR, year, patent_family)
][!is.na(prior_firmid) & prior_firmid != "unknown"]

citation_diversity <- citation_flows[
  ,
  {
    counts <- as.numeric(table(prior_firmid))
    total <- sum(counts)
    hhi <- if (total == 0) NA_real_ else sum((counts / total)^2)
    entropy <- if (total == 0) NA_real_ else -sum((counts / total) * log(pmax(counts / total, .Machine$double.eps)))
    list(
      citation_unique_prior_firms = uniqueN(prior_firmid),
      citation_inflow_hhi = hhi,
      citation_inflow_entropy = entropy
    )
  },
  by = .(NACE_BR, year)
]

knowledge_diffusion_patent <- merge(
  citation_industry,
  citation_diversity,
  by = c("NACE_BR", "year"),
  all = TRUE
)

# ------------------------------------------------------------------------------
# 4. IPCR-based diffusion metrics
# ------------------------------------------------------------------------------

ipcr_events <- ipcr_cumulative[
  lengths(new_ipcr) > 0,
  .(ipcr_code = unlist(new_ipcr)),
  by = .(firmid = as.character(firmid), year)
]

if (nrow(ipcr_events) > 0) {
  ipcr_events <- merge(
    ipcr_events,
    firm_core[, .(firmid, year, NACE_BR)],
    by = c("firmid", "year"),
    all.x = TRUE
  )

  ipcr_pioneers <- ipcr_events[order(year, firmid)]
  ipcr_pioneers <- ipcr_pioneers[
    ,
    .SD[1L],
    by = ipcr_code
  ][
    ,
    .(ipcr_code, pioneer_firmid = firmid, pioneer_year = year, pioneer_NACE = NACE_BR)
  ]

  ipcr_events <- merge(
    ipcr_events,
    ipcr_pioneers,
    by = "ipcr_code",
    all.x = TRUE
  )

  ipcr_events[, adoption_lag := ifelse(is.na(pioneer_year), NA_real_, year - pioneer_year)]
  ipcr_events[, external_adoption := pioneer_firmid != firmid]
  ipcr_events[, cross_industry := pioneer_NACE != NACE_BR]
}

ipcr_intensity <- merge(
  ipcr_cumulative[, .(firmid = as.character(firmid), year, ipcr_creat, n_ipcr_growth, n_NACE_growth)],
  firm_core[, .(firmid, year, NACE_BR)],
  by = c("firmid", "year"),
  all.x = TRUE
)

ipcr_industry <- ipcr_intensity[
  !is.na(NACE_BR),
  .(
    share_new_ipcr_firms = safe_mean(ipcr_creat),
    mean_ipcr_growth = safe_mean(n_ipcr_growth),
    mean_nace_growth = safe_mean(n_NACE_growth)
  ),
  by = .(NACE_BR, year)
]

if (exists("ipcr_events") && nrow(ipcr_events) > 0) {
  ipcr_diffusion <- ipcr_events[
    !is.na(NACE_BR),
    .(
      ipcr_adoptions = .N,
      ipcr_external_share = safe_mean(external_adoption),
      ipcr_cross_industry_share = safe_mean(cross_industry),
      ipcr_mean_adoption_lag = safe_mean(adoption_lag),
      ipcr_median_adoption_lag = safe_median(adoption_lag)
    ),
    by = .(NACE_BR, year)
  ]
} else {
  ipcr_diffusion <- data.table()
}

knowledge_diffusion_ipcr <- merge(
  ipcr_industry,
  ipcr_diffusion,
  by = c("NACE_BR", "year"),
  all = TRUE
)

# ------------------------------------------------------------------------------
# 5. Industry outcomes of interest
# ------------------------------------------------------------------------------

share_capital_costs <- 0.08

industry_outcomes <- firm_core[
  !is.na(NACE_BR),
  .(
    firm_count = .N,
    labor_share = safe_ratio(sum(labor_cost, na.rm = TRUE), sum(nq, na.rm = TRUE)),
    profit_share = safe_ratio(
      sum(nq - labor_cost - raw_materials - share_capital_costs * capital, na.rm = TRUE),
      sum(nq, na.rm = TRUE)
    ),
    entry_rate = safe_mean(as.integer(born)),
    exit_rate = safe_mean(as.integer(died)),
    young_employment_share = safe_ratio(
      sum(empl_bar * (young == 1), na.rm = TRUE),
      sum(empl_bar, na.rm = TRUE)
    ),
    gross_job_reallocation = sum(empl_reallocation_weighted, na.rm = TRUE),
    gross_sales_reallocation = sum(nq_reallocation_weighted, na.rm = TRUE),
    sales_growth_dispersion = safe_sd(nq_growth),
    employment_growth_dispersion = safe_sd(empl_growth)
  ),
  by = .(NACE_BR, year)
]

tfpr_stats <- firm_core[
  !is.na(tfpr),
  .(
    tfpr_frontier = safe_quant(tfpr, 0.95),
    tfpr_median = safe_quant(tfpr, 0.50)
  ),
  by = .(NACE_BR, year)
]
tfpr_stats[, frontier_laggard_gap := tfpr_frontier - tfpr_median]

industry_outcomes <- merge(
  industry_outcomes,
  tfpr_stats[, .(NACE_BR, year, frontier_laggard_gap)],
  by = c("NACE_BR", "year"),
  all.x = TRUE
)

industry_outcomes <- merge(
  industry_outcomes,
  industry_data[, .(NACE_BR, year, HHI_industry, CR4, CR10, gini, leader_rev_share, tech_level, high_tech)],
  by = c("NACE_BR", "year"),
  all.x = TRUE
)

# ------------------------------------------------------------------------------
# 6. Assemble analysis panel
# ------------------------------------------------------------------------------

panel_components <- list(
  knowledge_diffusion_patent,
  knowledge_diffusion_ipcr,
  industry_outcomes
) |> purrr::compact()

analysis_panel <- Reduce(
  function(dt_left, dt_right) merge(dt_left, dt_right, by = c("NACE_BR", "year"), all = TRUE),
  panel_components
)

firm_counts <- firm_core[, .(firm_count = .N), by = .(NACE_BR, year)]
analysis_panel <- merge(
  analysis_panel,
  firm_counts,
  by = c("NACE_BR", "year"),
  all.x = TRUE,
  suffixes = c("", "_from_firms")
)

analysis_panel[, firm_count := fifelse(!is.na(firm_count_from_firms), firm_count_from_firms, firm_count)]
analysis_panel[, firm_count := fifelse(is.na(firm_count) | firm_count <= 0, 1, firm_count)]
analysis_panel[, firm_count_from_firms := NULL]

setorder(analysis_panel, NACE_BR, year)
analysis_panel[
  ,
  `:=`(
    citation_external_lag = shift(citation_external_share, 1),
    citation_entropy_lag = shift(citation_inflow_entropy, 1),
    ipcr_cross_lag = shift(ipcr_cross_industry_share, 1),
    ipcr_external_lag = shift(ipcr_external_share, 1),
    ipcr_lag_mean = shift(ipcr_mean_adoption_lag, 1),
    new_ipcr_share_lag = shift(share_new_ipcr_firms, 1)
  ),
  by = NACE_BR
]

diffusion_vars <- c(
  "citation_external_lag",
  "ipcr_cross_lag",
  "new_ipcr_share_lag",
  "ipcr_lag_mean"
)

analysis_panel[, diffusion_index := rowMeans(.SD, na.rm = TRUE), .SDcols = diffusion_vars]

fwrite(analysis_panel, file.path(analysis_output_dir, "knowledge_diffusion_panel.csv"))

# ------------------------------------------------------------------------------
# 7. Empirical analysis
# ------------------------------------------------------------------------------

regressions <- list(
  Concentration_HHI = feols(
    HHI_industry ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Concentration_CR4 = feols(
    CR4 ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Profit_Share = feols(
    profit_share ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Labor_Share = feols(
    labor_share ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Entry_Rate = feols(
    entry_rate ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Young_Employment = feols(
    young_employment_share ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Frontier_Gap = feols(
    frontier_laggard_gap ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Job_Reallocation = feols(
    gross_job_reallocation ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  ),
  Growth_Dispersion = feols(
    employment_growth_dispersion ~ citation_external_lag + ipcr_cross_lag + new_ipcr_share_lag + ipcr_lag_mean + diffusion_index
    | NACE_BR + year,
    data = analysis_panel,
    weights = ~ firm_count
  )
)

modelsummary(
  regressions,
  output = file.path(analysis_output_dir, "knowledge_diffusion_regressions.html"),
  gof_omit = "IC|Log|Theta",
  stars = TRUE
)

saveRDS(
  list(
    analysis_panel = analysis_panel,
    regressions = regressions,
    knowledge_diffusion_patent = knowledge_diffusion_patent,
    knowledge_diffusion_ipcr = knowledge_diffusion_ipcr,
    industry_outcomes = industry_outcomes
  ),
  file = file.path(analysis_output_dir, "knowledge_diffusion_workspace.RDS")
)

message("Knowledge diffusion analysis completed. Results stored in ", analysis_output_dir)
