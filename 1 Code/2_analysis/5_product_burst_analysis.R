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




patenting_products[, `:=`(
  rank = frank(-number_of_products, ties.method = "average", na.last = "keep"),
  n_firms_patenting_products  = .N
), by=year] %>%
  .[, rank_share:=rank/n_firms_patenting_products]


for(var in c("prod_added", "num_pat_families")){
  test <- patenting_products[year==2019,
                             .(rank_share = mean(rank_share, na.rm=TRUE)),
                             keyby = var
  ]
  
  ggplot(test, aes_string(x=paste0("log(", var, ", base=2)"), y="log(rank_share, base=10)")) + 
    geom_point()
  
}

test <- patenting_products[, .(rank_share=mean(rank_share, na.rm=T)), by=prod_added]

ggplot(test, aes(x=log(prod_added, base=2) , y=log(rank_share, base=10))) + 
  geom_point()

  
# 8) Citation analysis ------------------------------------


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



ggsave(paste0(output_dir, ""))



