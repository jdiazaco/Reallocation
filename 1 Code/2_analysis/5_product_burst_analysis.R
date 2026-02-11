#' ------------------------------------------------------------------------------
#' Script: Product Burst Analysis with Patent and Trademark Data
#' Author: Julián Díaz-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' Description: Analyzes product creation bursts and their relationship with
#'              patenting and trademark activity
#' ------------------------------------------------------------------------------

# Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 1) Load Data -------------------------------------------------------

start <- 2009
end <- 2022

patenting_products <- read_rds("temp/patenting_products_firm_level.RDS")
patent_data <- read_rds("4b_patenting_products_firm_level.rds")
tm_data <- read_rds("4c_tm_firm_level.rds")

# # 2) Select and Filter Variables ----------------------------------------
# 
# selected_names <- c(
#   # Identifiers
#   "firmid", "year",
#   # Industry / classification
#   "NACE_BR", "NACE_2d_BR",
#   # Employment / inputs
#   "empl", "empl_l", "empl_bar", "log_empl_bar", "empl_growth", "empl_reallocation",
#   "capital", "capital_bar", "capital_growth",
#   "nq", "nq_bar", "nq_growth", "nq_reallocation",
#   "labor_cost", "raw_materials",
#   # NACE patent info
#   "NACE_cum", "NACE_cum_l", "new_NACE",
#   "n_NACE", "n_NACE_bar", "n_NACE_growth",
#   # Firm demographics & age/size buckets
#   "young", "firm_age", "log_firm_age",
#   "age_leader", "age_size_bucket", "age_size_quartile", "age_size_decile",
#   "age_size_percentile", "age_size_1000tile", "age_top_4_leaders", "age_top_10_leaders",
#   "leader", "top_4_leaders", "top_10_leaders",
#   # Size measures and ranks
#   "size", "size_quartile", "size_decile", "size_percentile", "size_1000tile",
#   "rank_within_industry", "n_firms_in_industry",
#   # Revenue / shares
#   "nq_bar", "nq_growth", "rev_bar", "rev_growth",
#   # Product portfolio & dynamics
#   "number_of_products", "prod_creat", "prod_destr", "prod_added", "prod_removed",
#   "new_products", "first_introduction",
#   "net_product_creat", "net_product_creat_window",
#   "net_product_destr", "net_product_destr_window",
#   names(patenting_products)[grep("new_", names(patenting_products))],
#   # Patents & filing
#   "pat_families_dummy", "pat_families_dummy_window", "ever_patent",
#   "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
#   "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth",
#   "product_innovative_pat_filings_window", "product_innovative_pat_families_window",
#   # Trademarks
#   "tm_dummy", "tm_dummy_window", "ever_tm"
# )
# 
# in_firm_data <- selected_names[selected_names %in% names(patenting_products)]
# not_in_firm_data <- selected_names[!(selected_names %in% names(patenting_products))]
# 
# patenting_products <- patenting_products %>%
#   select(in_firm_data)

# 3) Create Burst Variables -----------------------------------------------

burst_threshold <- 2
burst_thresholds <- c(2, 5, 10)

patenting_products <- patenting_products[, burst := ifelse(prod_added > burst_threshold, 1, 0)] %>%
  group_by(firmid) %>%
  setDT() %>%
  mutate(ever_burst = any(prod_added >= burst_threshold))

for (i in burst_thresholds) {
  patenting_products[, (paste0("burst_", i)) := ifelse(prod_added >= i, 1, 0)]
}

# 4) Analyze Bursts per Year -----------------------------------------------

bursts_per_year <- patenting_products[, lapply(.SD, mean, na.rm = TRUE),
  .SDcols = grepl("burst_", names(patenting_products)), by = year
] %>%
  pivot_longer(starts_with("burst_"), names_prefix = "burst_") %>%
  setDT() %>%
  .[, mean := mean(value), by = .(name)] %>%
  .[, dev_from_mean := (value - mean) / mean]

vars_labs <- fread("var, lab
                   dev_from_mean, Dev. from Av. Firms with Bursts/Total Firms per Year
                   value, Firms with Bursts/Total Firms per Year")

for (i in 1:nrow(vars_labs)) {
  var <- vars_labs[i]$var
  lab <- vars_labs[i]$lab

  ggplot(bursts_per_year, aes(x = year, y = .data[[var]], color = name)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      # limits = c(min(bursts_per_year$year) + 1, max(bursts_per_year$year)),
      breaks = scales::breaks_pretty(5)
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 100),
      limits = c(min(0, min(bursts_per_year[[var]])), max(bursts_per_year[[var]]))
    ) +
    labs(x = "Year", y = lab, color = "Burst Threshold") +
    theme_classic()

  ggsave(paste0(output_dir, var, "_per_year.png"), width = 9, height = 5)
}

burst_size_analysis <- patenting_products %>%
  select(firmid, year, prod_creat, burst, size_quartile, size_decile, size_percentile, size_1000tile) %>%
  lead_lag_creator("size_decile", 5, to_dummy = FALSE) %>%
  # lead_lag_creator("nq_growth", 5, to_dummy = FALSE) %>%
  setDT() %>%
  data.table::melt(
    id.vars = c("firmid", "year", "burst", "size_quartile", "size_decile", "size_percentile", "size_1000tile"),
    measure.vars = patterns("^size_decile"),
    variable.name = "period",
    value.name = "size_decile"
  ) %>%
  .[, period := gsub("size_decile", "", period)] %>%
  .[, period := ifelse(grepl("_lag", period), -as.numeric(gsub("_lag", "", period)), as.numeric(gsub("_lead", "", period)))] %>%
  .[, period := ifelse(is.na(period), 0, period)] %>%
  .[!is.na(size_decile), .(
    size_decile = mean(size_decile, na.rm = TRUE),
    sd = sd(size_decile, na.rm = TRUE),
    n = .N
  ), by = .(burst, period)] %>%
  .[, `:=`(
    se = sd / sqrt(n))] %>%
  .[, `:=`(
    ci_lower = size_decile - 1.96 * se,
    ci_upper = size_decile + 1.96 * se
  )]

ggplot(burst_size_analysis[!is.na(burst)], aes(x = period, y = size_decile, color = factor(burst), group = burst)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = factor(burst)), alpha = 0.2, color = NA) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "gray", "1" = "darkblue"), labels = c("0" = "No Burst", "1" = "Burst")) +
  scale_fill_manual(values = c("0" = "gray", "1" = "darkblue"), labels = c("0" = "No Burst", "1" = "Burst")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = range(burst_size_analysis$period, na.rm = TRUE)) +
  labs(x = "Year Relative to Product Expansion", y = "Average Size Decile", color = "Firm Status", fill = "Firm Status") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
ggsave(paste0(output_dir, "burst_size_decile_analysis.png"), width = 9, height = 5)
# 5) Merge Trademark and Patent Data ----------------------------------------

tm_patenting_products <- patenting_products %>%
  select(firmid, year, burst, ever_burst, prod_creat, size) %>%
  merge(tm_data, by = c("firmid", "year"), all = TRUE) %>%
  merge(patent_data, by = c("firmid", "year"), all = TRUE)

patent_tm_data_names <- c(
  "ever_patent",
  "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings",
  "total_pat_filings", "total_pat_filings_growth",
  "pat_families_dummy", "pat_families_dummy_window", "num_pat_families",
  "total_pat_families", "total_pat_families_growth",
  "num_tm", "total_tm", "tm_dummy", "total_tm_growth", "tm_dummy_window", "ever_tm"
)

tm_patenting_products[, (patent_tm_data_names) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = patent_tm_data_names
]

# 6) Dynamic Regression Analysis ----------------------------------------

window <- 5

tm_patenting_products <- tm_patenting_products %>% rename(ipcr_creat_dummy=ipcr_creat)
for (x in c("pat_families_dummy", "tm_dummy", "ipcr_creat_dummy")) {
  patenting_bursts <- dynamic_reg_reallocation(
    data = tm_patenting_products,
    y = "burst",
    x = x,
    fix_eff = "",
    weight_var = NULL,
    disag_var = "size",
    n_lags_bw = window,
    n_lags_fw = window
  )
  dynamic_reg_graphs(
    patenting_bursts, "", output_dir,
    paste0(x, "_series"), -window, window
  )
}

# 7) Prepare Data for Burst Analysis -----------------------------------------

burst_patenting_plots <- function(title, dt){
  for(x in unique(dt$var)){
    ggplot(dt[var == x], aes(x = name, y = value, color = series)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(
        labels = scales::label_percent(scale = 100)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Year Relative to Burst", y = toTitleCase( paste0(remove_special_chars(x), " Likelihood")) , color = "Series") +
      theme_classic() +
      theme(legend.position = "bottom")
    
    ggsave(paste0(output_dir, x, "_", title, ".png"), width = 9, height = 5)
  }
}

# Extract patent and trademark dummy variable names
pat_tm_vars_to_mean <- names(tm_patenting_products)[
  grepl("pat_families_dummy|tm_dummy|ipcr_creat_dummy", names(tm_patenting_products))
]

# Define function to aggregate and reshape burst data for plotting
process_burst_data <- function(data, group_vars, series_label) {
  data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pat_tm_vars_to_mean, by = group_vars] %>%
    pivot_longer(matches("pat_families_dummy|tm_dummy|ipcr_creat_dummy")) %>%
    setDT() %>%
    .[, c("var", "name") := tstrsplit(name, "dummy")] %>%
    .[, var := sub("_$", "", var)] %>%
    .[, name := str_replace_all(name, c("_lead" = "", "_lag" = "-", "^$" = "0"))] %>%
    .[name != "_window"] %>%
    .[, name := as.numeric(name)] %>%
    .[, series := eval(series_label)]
}

# Process and plot patenting patterns by burst status and product creation
series_patenting_burst <- process_burst_data(
  tm_patenting_products[!is.na(burst) & !is.na(ever_burst) & !is.na(prod_creat)],
  c("burst", "ever_burst", "prod_creat"),
  quote(paste0(ifelse(ever_burst == 1, "Ever Burst", "No Burst"), " - ",
    ifelse(prod_creat > 0, "Product Created", "No Product Created"), " - ",
    ifelse(burst == 1, "Burst", "No Burst")))
)
burst_patenting_plots("series", series_patenting_burst)

# Process and plot patenting patterns by firm size among bursting firms
series_patenting_burst_size <- process_burst_data(
  tm_patenting_products[burst == 1],
  "size",
  quote(size)
)
burst_patenting_plots("size_series", series_patenting_burst_size)
# 8) Product and patent burst rank analysis ------------------------------------
var_labels <- list(
  prod_added = "Products Added",
  num_pat_families = "Patent Families Added",
  number_of_products = "Total Products"
)

for(var in c("prod_added", "num_pat_families", "number_of_products")){
  
  patenting_products[, `:=`(
  rank = frank(-get(var), ties.method = "average", na.last = "keep"),
  n_firms_patenting_products = .N
  ), by = year] %>%
  .[, rank_share := rank / n_firms_patenting_products]
  
  test <- patenting_products[
  year == 2019 & !is.na(get(var)) & get(var) > 0 & !is.na(rank_share) & rank_share > 0,
  .(rank_share = mean(rank_share, na.rm = TRUE)),
  keyby = var
  ]
  
  ggplot(test, aes(x = .data[[var]], y = rank_share)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(
    trans = "log2",
    breaks = scales::breaks_log(base = 2)
  ) +
  scale_y_continuous(
    trans = "log10",
    breaks = scales::breaks_log(base = 10)
  ) +
  labs(
    x = paste0(var_labels[[var]]),
    y = "Rank Share"
  ) +
  theme_classic()
  
  ggsave(paste0(output_dir, var, "_rank_share.png"))

}
 
# 8) Citation analysis ------------------------------------

patent_records <- read_parquet("3_patent_lvl_patent_dta.parquet")
patent_citation <- readRDS("3b_patent_citation_summary.rds") %>% setDT()
product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "2a_product_yr_lvl_dta.parquet"))
ipcr_data <- readRDS("4a_ipcr_cumulative.RDS")

subset_firms <- patenting_products[burst == 1 & ever_patent == 1] %>%
  select(firmid, year, new_0, new_1, new_2, new_4, new_6, new_8, burst, NACE_cum, new_NACE, num_pat_families, number_of_products, prod_added) %>%
  merge(product_data, by = c("firmid", "year"), all.x = T)
length(unique(subset_firms$prodcom))

burst_events_industry_granular <- patenting_products[, .(
  burst_industry = max(burst, na.rm = TRUE),
  burst_leader = max(burst * leader, na.rm = TRUE),
  patent_industry = sum(num_pat_families, na.rm = TRUE),
  patent_leader = sum(num_pat_families * leader, na.rm = TRUE),
  num_tm = sum(num_tm, na.rm = TRUE),
  tm_leader = sum(num_tm * leader, na.rm = TRUE),
  number_of_products = sum(number_of_products, na.rm = TRUE),
  number_of_products_bursting = sum(number_of_products * burst, na.rm = TRUE),
  number_of_products_not_bursting = sum(number_of_products * (1 - burst), na.rm = TRUE),
  prod_added = sum(prod_added, na.rm = TRUE),
  prod_added_bursting = sum(prod_added * burst, na.rm = TRUE),
  prod_added_not_bursting = sum(prod_added * (1 - burst), na.rm = TRUE),
  prod_destroyed = sum(prod_destr, na.rm = TRUE),
  prod_destroyed_bursting = sum(prod_destr * burst, na.rm = TRUE),
  prod_destroyed_not_bursting = sum(prod_destr * (1 - burst), na.rm = TRUE),
  n_firms = n_distinct(firmid),
  n_patenting_firms = sum(ever_patent == 1, na.rm = TRUE),
  n_tm_firms = sum(ever_tm == 1, na.rm = TRUE)
), by = .(NACE_BR, year)] %>%
  .[, `:=`(
    product_creation_rate = prod_added / number_of_products,
    product_creation_rate_bursting = prod_added_bursting / number_of_products_bursting,
    product_creation_rate_not_bursting = prod_added_not_bursting / number_of_products_not_bursting,
    patent_intensity = patent_industry / number_of_products,
    patent_per_firm = patent_industry / n_firms,
    patent_adoption_rate = n_patenting_firms / n_firms,
    tm_intensity = num_tm / number_of_products,
    tm_per_firm = num_tm / n_firms,
    tm_adoption_rate = n_tm_firms / n_firms,
    burst_event = burst_industry == 1
  )] %>%
  merge(
    .[burst_event == TRUE, .(event_year = year), by = NACE_BR],
    by = "NACE_BR",
    allow.cartesian = TRUE
  ) %>%
  .[, years_from_event := year - event_year]

burst_events_industry_aggregated <- burst_events_industry_granular[, .(
  product_creation_rate = mean(product_creation_rate, na.rm = TRUE),
  product_creation_rate_bursting = mean(product_creation_rate_bursting, na.rm = TRUE),
  product_creation_rate_not_bursting = mean(product_creation_rate_not_bursting, na.rm = TRUE),
  patent_intensity = mean(patent_intensity, na.rm = TRUE),
  patent_per_firm = mean(patent_per_firm, na.rm = TRUE),
  patent_adoption_rate = mean(patent_adoption_rate, na.rm = TRUE),
  tm_intensity = mean(tm_intensity, na.rm = TRUE),
  tm_per_firm = mean(tm_per_firm, na.rm = TRUE),
  tm_adoption_rate = mean(tm_adoption_rate, na.rm = TRUE)
), by = years_from_event]

ggplot(burst_events_industry_aggregated, aes(x = years_from_event)) +
  geom_line(aes(y = product_creation_rate, color = "Overall")) +
  geom_point(aes(y = product_creation_rate, color = "Overall")) +
  geom_line(aes(y = product_creation_rate_bursting, color = "Bursting Firms")) +
  geom_point(aes(y = product_creation_rate_bursting, color = "Bursting Firms")) +
  geom_line(aes(y = product_creation_rate_not_bursting, color = "Non-Bursting Firms")) +
  geom_point(aes(y = product_creation_rate_not_bursting, color = "Non-Bursting Firms")) +
  labs(x = "Years from Burst Event", y = "Product Creation Rate", color = "Firm Type") +
  theme_classic()

ggsave(paste0(output_dir, "burst_creation_rate_event_study.png"), width = 10, height = 6)

ggplot(burst_events_industry_aggregated, aes(x = years_from_event)) +
  geom_line(aes(y = patent_intensity, color = "Patent Intensity")) +
  geom_point(aes(y = patent_intensity, color = "Patent Intensity")) +
  geom_line(aes(y = tm_intensity, color = "TM Intensity")) +
  geom_point(aes(y = tm_intensity, color = "TM Intensity")) +
  labs(x = "Years from Burst Event", y = "IP Intensity", color = "Measure") +
  theme_classic()

ggsave(paste0(output_dir, "ip_intensity_event_study.png"), width = 10, height = 6)

ggplot(burst_events_industry_aggregated, aes(x = years_from_event)) +
  geom_line(aes(y = patent_adoption_rate, color = "Patent Adoption")) +
  geom_point(aes(y = patent_adoption_rate, color = "Patent Adoption")) +
  geom_line(aes(y = tm_adoption_rate, color = "TM Adoption")) +
  geom_point(aes(y = tm_adoption_rate, color = "TM Adoption")) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  labs(x = "Years from Burst Event", y = "Adoption Rate", color = "Measure") +
  theme_classic()

ggsave(paste0(output_dir, "ip_adoption_event_study.png"), width = 10, height = 6)

event_study <- feols(
  product_creation_rate ~ sunab(event_year, years_from_event) | NACE_BR + year,
  data = burst_events_industry_granular,
  vcov = ~ NACE_BR + year
)

# 2. Extract and visualize coefficients with confidence intervals
event_coefs <- tidy(event_study) %>%
  setDT() %>%
  .[grepl("years_from_event", term)] %>%
  .[, years_from_event := as.numeric(gsub(".*::", "", term))] %>%
  .[, `:=`(
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error
  )] %>%
  setorder(years_from_event)

# 3. Create comprehensive event study plot
ggplot(event_coefs, aes(x = years_from_event, y = estimate)) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(x = "Years from Burst Event", 
       y = "Effect on Product Creation Rate",
       title = "Event Study: Industry Burst Impact on Product Creation Rate") +
  theme_classic()

ggsave(paste0(output_dir, "event_study_creation_rate.png"), width = 10, height = 6)

# 4. Test alternative outcomes
for (outcome in c("product_creation_rate_bursting", "product_creation_rate_not_bursting")) {
  est <- feols(
    as.formula(paste0(outcome, " ~ sunab(event_year, years_from_event) | NACE_BR + year")),
    data = burst_events_industry_granular,
    vcov = ~ NACE_BR + year
  )
  cat("\n", outcome, ":\n")
  print(est)
}



burst_events_industry_growth <- growth_creator(burst_events_industry_aggregated, c("number_of_products"))

patenting_products <- merge(patenting_products, burst_events_industry_granular, by = c("NACE_BR", "year"), all.x = T)

window <- 5

for (x in c("burst_industry", "burst_leader", "patent_industry", "patent_leader")) {
  for( y in c("burst", "prod_added", "pat_families_dummy", "num_pat_families", "tm_dummy", "num_tm", "ipcr_creat_dummy")){
    patenting_bursts <- dynamic_reg_reallocation(
      data = tm_patenting_products,
      y = y,
      x = x,
      fix_eff = "NACE_BR + year",
      weight_var = NULL,
      disag_var = "size",
      n_lags_bw = window,
      n_lags_fw = window
    )
    dynamic_reg_graphs(
      patenting_bursts, "", output_dir,
      paste0(x, "_", y, "_series"), -window, window
    )
  }
}







patent_records <- merge(patent_records,
                         patent_citation[, .(patent_family, share_self_citation, share_external_citation)],
                         by="patent_family",
                         all.x=T)


patent_records <- patent_records[, .(share_self_citation=mean(share_self_citation, na.rm=T),
                                     share_external_citation=mean(share_external_citation, na.rm=T)), by=.(firmid, filing_year)]

# Bring in citation information
tm_patenting_products <- merge(tm_patenting_products,
                              patent_records,
                              by.x=c("firmid", "year"),
                              by.y=c("firmid", "filing_year"),
                              all.x=T)



ggplot(patenting_products[year==2019], aes(x=log(number_of_products, base=2) , y=log(rank_share, base=10))) + 
  geom_point(alpha=0.1)

test <- patenting_products[year==2019, .(rank_share=mean(rank_share, na.rm=T)), by=number_of_products]
ggplot(test, aes(x=log(number_of_products, base=2) , y=log(rank_share, base=10))) + 
  geom_point()

test <- patenting_products[, .(rank_share=mean(rank_share, na.rm=T)), by=prod_added]
ggplot(test, aes(x=log(prod_added, base=2) , y=log(rank_share, base=10))) + 
  geom_point()


test <- patenting_products[, .(num_pat_families=mean(num_pat_families, na.rm=T)), by=prod_added]

ggplot(test, aes(x=log(prod_added, base=2) , y=log(num_pat_families, base=2))) + 
  geom_point() + 
  geom_smooth(method="lm")

# 9) Difference-in-Differences (DiD) Event Study Analysis -----------------------
# Analysis: Does an industry-level burst lead to higher product introduction rates?
# Treatment: Industry experiences a burst event
# Outcome: Firm-level product introduction rate (prod_added)

# Identify industry-year burst events
industry_burst_events <- patenting_products[, .(
  industry_burst = max(burst, na.rm = TRUE),
  num_bursting_firms = sum(burst, na.rm = TRUE),
  total_firms = .N,
  bursting_firm_share = sum(burst, na.rm = TRUE) / .N
), by = .(NACE_BR, year)] %>%
  .[industry_burst == 1] %>%  # Only keep years with at least one burst
  .[, .(first_burst_year = min(year)), by = NACE_BR]

# Create treatment indicator: 1 if industry-year is post-burst, 0 otherwise
did_data <- patenting_products %>%
  merge(industry_burst_events, by = "NACE_BR", all.x = TRUE) %>%
  setDT() %>%
  .[, `:=`(
    first_burst_year = fifelse(is.na(first_burst_year), Inf, first_burst_year),
    treat = fifelse(!is.na(first_burst_year) & first_burst_year != Inf, 1, 0),
    years_to_burst = year - first_burst_year
  )] %>%
  # Define event window (e.g., t-3 to t+5)
  .[, event_window := fifelse(years_to_burst >= -3 & years_to_burst <= 5 & treat == 1, 1, 0)]

# Create leads and lags for event study (years before and after burst)
did_data <- did_data %>%
  .[, `:=`(
    # Pre-event periods
    pre_3 = fifelse(years_to_burst == -3 & treat == 1, 1, 0),
    pre_2 = fifelse(years_to_burst == -2 & treat == 1, 1, 0),
    pre_1 = fifelse(years_to_burst == -1 & treat == 1, 1, 0),
    # Treatment period (year of burst and after)
    post_0 = fifelse(years_to_burst == 0 & treat == 1, 1, 0),
    post_1 = fifelse(years_to_burst == 1 & treat == 1, 1, 0),
    post_2 = fifelse(years_to_burst == 2 & treat == 1, 1, 0),
    post_3 = fifelse(years_to_burst == 3 & treat == 1, 1, 0),
    post_4 = fifelse(years_to_burst == 4 & treat == 1, 1, 0),
    post_5 = fifelse(years_to_burst == 5 & treat == 1, 1, 0)
  )]

# Remove infinite values for visualization
did_data <- did_data[first_burst_year != Inf]

# 9.1) Plot Event Study Graph - Average Treatment Effect
event_study_data <- did_data[event_window == 1 | treat == 0, .(
  prod_added_mean = mean(prod_added, na.rm = TRUE),
  prod_added_se = sd(prod_added, na.rm = TRUE) / sqrt(.N),
  prod_added_n = .N,
  new_products_mean = mean(new_products, na.rm = TRUE),
  new_products_se = sd(new_products, na.rm = TRUE) / sqrt(.N),
  pat_families_mean = mean(num_pat_families, na.rm = TRUE),
  pat_families_se = sd(num_pat_families, na.rm = TRUE) / sqrt(.N)
), by = years_to_burst] %>%
  .[!is.na(years_to_burst) & years_to_burst >= -3 & years_to_burst <= 5] %>%
  setorder(years_to_burst)

# Add confidence intervals
event_study_data <- event_study_data[, `:=`(
  prod_added_ci_lower = prod_added_mean - 1.96 * prod_added_se,
  prod_added_ci_upper = prod_added_mean + 1.96 * prod_added_se,
  new_products_ci_lower = new_products_mean - 1.96 * new_products_se,
  new_products_ci_upper = new_products_mean + 1.96 * new_products_se,
  pat_families_ci_lower = pat_families_mean - 1.96 * pat_families_se,
  pat_families_ci_upper = pat_families_mean + 1.96 * pat_families_se
)]

# Event study visualization: Product addition
ggplot(event_study_data, aes(x = years_to_burst, y = prod_added_mean)) +
  geom_ribbon(aes(ymin = prod_added_ci_lower, ymax = prod_added_ci_upper), 
              alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = -0.5, y = Inf, label = "Burst Event", hjust = 1.1, vjust = 1.5, color = "red") +
  scale_x_continuous(
    breaks = -3:5,
    labels = paste0("t", ifelse(-3:5 >= 0, "+", ""), -3:5)
  ) +
  labs(
    x = "Years Relative to Industry Burst",
    y = "Average Products Added per Firm",
    title = "Event Study: Industry Burst Impact on Product Introduction"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggsave(paste0(output_dir, "event_study_prod_added.png"), width = 10, height = 6)

# Event study visualization: Patent families
ggplot(event_study_data, aes(x = years_to_burst, y = pat_families_mean)) +
  geom_ribbon(aes(ymin = pat_families_ci_lower, ymax = pat_families_ci_upper), 
              alpha = 0.2, fill = "darkgreen") +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_x_continuous(
    breaks = -3:5,
    labels = paste0("t", ifelse(-3:5 >= 0, "+", ""), -3:5)
  ) +
  labs(
    x = "Years Relative to Industry Burst",
    y = "Average Patent Families per Firm",
    title = "Event Study: Industry Burst Impact on Patenting"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggsave(paste0(output_dir, "event_study_patents.png"), width = 10, height = 6)

# 9.2) DiD Regression: Basic Specification
# Model: prod_added = β0 + β1*treat + β2*post + β3*(treat*post) + FE + ε
# where treat = 1 for industries that experience a burst
#       post = 1 for years after the first burst

did_data_reg <- did_data[event_window == 1 | treat == 0, .(
  prod_added, new_products, num_pat_families, num_tm,
  pat_families_dummy, tm_dummy, ipcr_creat_dummy,
  firmid, NACE_BR, year, treat, years_to_burst, size, young, firm_age
)] %>%
  .[, `:=`(
    post = fifelse(years_to_burst >= 0 & treat == 1, 1, 0)
  )]

# Basic DiD specification
did_basic <- feols(
  prod_added ~ treat * post | NACE_BR + year,
  data = did_data_reg,
  vcov = "twoway~NACE_BR+year"
)

# DiD with firm fixed effects
did_firm_fe <- feols(
  prod_added ~ treat * post | firmid + year,
  data = did_data_reg,
  vcov = "twoway~NACE_BR+year"
)

# DiD with controls (size, age)
did_controls <- feols(
  prod_added ~ treat * post + log(firm_age + 1) + I(size > 0) | NACE_BR + year,
  data = did_data_reg,
  vcov = "twoway~NACE_BR+year"
)

# Display results
modelsummary(
  list("Basic DiD" = did_basic, "Firm FE" = did_firm_fe, "With Controls" = did_controls),
  output = paste0(output_dir, "did_regression_results.txt"),
  title = "DiD Event Study: Industry Burst Impact on Product Introduction"
)

cat("\n=== DID REGRESSION RESULTS ===\n")
cat("Model 1: Basic DiD (Industry and Year FE)\n")
print(did_basic)
cat("\nModel 2: DiD with Firm FE\n")
print(did_firm_fe)
cat("\nModel 3: DiD with Controls\n")
print(did_controls)

# 9.3) Event Study DiD Regression (Dynamic Effects)
# Include separate coefficients for each lead and lag

did_data_dynamic <- did_data[event_window == 1 | treat == 0] %>%
  setDT()

formula_str <- paste0(
  "prod_added ~ ",
  paste(paste0(c("pre_3", "pre_2", "pre_1", "post_0", "post_1", 
                 "post_2", "post_3", "post_4", "post_5")), collapse = " + "),
  " | NACE_BR + year"
)

did_dynamic <- feols(
  as.formula(formula_str),
  data = did_data_dynamic
)

# Extract coefficients for visualization
did_dynamic_coefs <- tidy(did_dynamic) %>%
  setDT() %>%
  .[grepl("pre_|post_", term)] %>%
  .[, `:=`(
    period_num = as.numeric(gsub("pre_|post_", "", term)),
    period_type = ifelse(grepl("pre_", term), -1, 1),
    years_from_event = ifelse(grepl("pre_", term), -as.numeric(gsub("pre_", "", term)), as.numeric(gsub("post_", "", term)))
  )] %>%
  .[, `:=`(
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error
  )] %>%
  setorder(years_from_event)

# Add baseline (period before treatment) for visualization
baseline <- data.table(
  term = "pre_1_baseline",
  estimate = 0,
  std.error = 0,
  years_from_event = -1,
  conf_low = 0,
  conf_high = 0
)

did_dynamic_coefs <- rbind(did_dynamic_coefs, baseline, fill = TRUE)

# Dynamic event study plot
ggplot(did_dynamic_coefs[!is.na(years_from_event)], aes(x = years_from_event, y = estimate)) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_x_continuous(
    breaks = -3:5,
    labels = paste0("t", ifelse(-3:5 >= 0, "+", ""), -3:5)
  ) +
  labs(
    x = "Years Relative to Industry Burst",
    y = "Coefficient Estimate (Products Added)",
    title = "Dynamic DiD: Year-by-Year Treatment Effects on Product Introduction"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

ggsave(paste0(output_dir, "dynamic_did_coefficients.png"), width = 10, height = 6)

# 9.4) Heterogeneous Effects by Firm Characteristics
# Test if treatment effects differ by firm size and age

# By firm size
did_data_reg <- did_data_reg[, `:=`(
  small_firm = fifelse(size <= quantile(size, 0.25, na.rm = TRUE), 1, 0),
  large_firm = fifelse(size > quantile(size, 0.75, na.rm = TRUE), 1, 0)
)]

did_by_size <- feols(
  prod_added ~ treat * post * small_firm + treat * post * large_firm | NACE_BR + year,
  data = did_data_reg,
  vcov = "twoway~NACE_BR+year"
)

# By firm age
did_by_age <- feols(
  prod_added ~ treat * post * young + treat * post * I(firm_age > median(firm_age, na.rm = TRUE)) | NACE_BR + year,
  data = did_data_reg[!is.na(young)],
  vcov = "twoway~NACE_BR+year"
)

cat("\n=== HETEROGENEOUS TREATMENT EFFECTS ===\n")
cat("By Firm Size:\n")
print(did_by_size)
cat("\nBy Firm Age:\n")
print(did_by_age)

# 9.5) Alternative Outcomes
# Test effects on other innovation metrics

did_outcomes <- list()

for (outcome in c("new_products", "num_pat_families", "num_tm", "pat_families_dummy", "tm_dummy")) {
  if (outcome %in% names(did_data_reg)) {
    did_outcomes[[outcome]] <- feols(
      as.formula(paste0(outcome, " ~ treat * post | NACE_BR + year")),
      data = did_data_reg,
      vcov = "twoway~NACE_BR+year"
    )
  }
}

modelsummary(
  did_outcomes,
  output = paste0(output_dir, "did_alternative_outcomes.txt"),
  title = "DiD Analysis: Alternative Outcome Variables"
)

cat("\n=== ALTERNATIVE OUTCOMES ===\n")
print(modelsummary(did_outcomes, output = "markdown"))

# 9.6) Parallel Trends Test
# Check if treated and control industries had parallel trends pre-event

parallel_trends_data <- did_data[years_to_burst < 0] %>%
  setDT() %>%
  .[, .(prod_added_mean = mean(prod_added, na.rm = TRUE)), by = .(treat, years_to_burst)] %>%
  .[!is.na(years_to_burst)]

ggplot(parallel_trends_data, aes(x = years_to_burst, y = prod_added_mean, color = factor(treat), linetype = factor(treat))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "gray", "1" = "darkblue"), labels = c("0" = "Control (No Burst)", "1" = "Treated (Burst Event)")) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"), labels = c("0" = "Control (No Burst)", "1" = "Treated (Burst Event)")) +
  labs(
    x = "Years Before Burst",
    y = "Average Products Added per Firm",
    title = "Parallel Trends Test: Pre-Event Dynamics",
    color = "Group",
    linetype = "Group"
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave(paste0(output_dir, "parallel_trends_test.png"), width = 10, height = 6)

# 9.7) Summary Statistics Table
summary_stats <- did_data_reg[, .(
  mean_prod_added = mean(prod_added, na.rm = TRUE),
  sd_prod_added = sd(prod_added, na.rm = TRUE),
  mean_new_products = mean(new_products, na.rm = TRUE),
  sd_new_products = sd(new_products, na.rm = TRUE),
  mean_patents = mean(num_pat_families, na.rm = TRUE),
  sd_patents = sd(num_pat_families, na.rm = TRUE),
  n_obs = .N
), by = .(treat, post)] %>%
  .[order(treat, post)]

print(summary_stats)
write.csv(summary_stats, paste0(output_dir, "did_summary_statistics.csv"), row.names = FALSE)

cat("\n=== EVENT STUDY SUMMARY COMPLETE ===\n")
cat("Output files saved to:", output_dir, "\n")



