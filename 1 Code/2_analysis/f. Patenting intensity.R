#' ------------------------------------------------------------------------------
#' Script: Patent Data Cleaning and Cumulative IPCR/NACE Generation
#' Author: Juli?n D?az-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' ------------------------------------------------------------------------------

# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 8) IPC analysis -----------------

# Load all necessary data
firm_data <- read_parquet("1_firm_yr_lvl_dta.parquet")  # Firm-year level data (from b.)
product_data <- read_parquet("2b_product_lvl_dta.parquet") # Product information aggregated at the firm level (from e.)
industry_data <- read_parquet("5_industry_yr_lvl_dta.parquet") # Industry data (from c.)
patent_data <- read_rds("4_firm_lvl_patent_dta.parquet") # Patent and trademark data (from f.)

start=2009
# Keep only continuers and from 2009
firm_data <- firm_data[year>=start & abs(empl_growth) != 2 & abs(nq_growth) != 2]



  %>%
  select(
    firmid, year, NACE_BR, NACE_2d, young,
    patent, patent_window, ever_patent,
    size, young, firm_age, number_of_products,
    # superstar, superstar_cr4, superstar_tfp_99, superstar_tfp_90,
    prod_creat, prod_destr,
    within_economy_rev_share_BR, within_industry_rev_share,
    tm, tm_window, ever_tm,
    new_products, first_introduction,
    net_product_creat, net_product_creat_window,
    net_product_destr, net_product_destr_window,
    empl_bar, empl_l, empl_growth, nq_growth, nq_bar,
    capital, capital_growth, capital_bar,
    tfp_growth, tfp_bar, rev_growth, rev_bar,
    rank_within_industry, n_firms_in_industry,
    size_quartile, size_decile, size_percentile, size_1000tile, leader,
    age_size_bucket, age_size_quartile, age_size_decile, age_size_percentile, age_size_1000tile, age_leader,
    ipcr_cum, NACE_cum, n_ipcr, n_NACE, n_NACE_bar, n_ipcr_bar, n_NACE_growth,
    n_ipcr_growth, ipcr_cum_l, NACE_cum_l, new_ipcr, new_NACE, ipcr_creat
  )

names(patenting_products)

# Identify firms that have patented at some point
ipcr_firmids <- unique(ipcr_cumulative$firmid) # length(ipcr_firmids) 118129
prod_firmids <- unique(patenting_products$firmid) # length(prod_firmids) 70354
patenting_prod_firmids <- intersect(ipcr_firmids, prod_firmids) # length(patenting_prod_firmids) 9109
fwrite(data.frame(patenting_prod_firmids=unlist(patenting_prod_firmids)), paste0("firm_lists/patenting_firms_in_BR_no_outliers.csv"))

# Create  variables and clean data
patenting_products[, `:=`(ipcr_creat = fifelse(is.na(ipcr_creat), 0, ipcr_creat))] %>% # Treat NA as 0 for ipcr_creat
  window_var_cretor("firmid", "year", "ipcr_creat", 2, 0, "ipcr_creat_window", na_rm = F) %>% # Create 2-year window for ipcr_creat
  .[, `:=`(
    log_firm_age = log(firm_age),
    log_empl_bar = log(empl_bar)
  )] %>%
  .[, product_innovative_patent_window := net_product_creat_window * patent_window] %>% # Interaction between product creation and patenting
  .[, product_strategic_patent_window := (1 - net_product_creat_window) * patent_window] # Interaction between lack of product creation and patenting

# Create revenue share growth variables
rev_share_growth <- growth_creator(patenting_products, c("within_economy_rev_share_BR", "within_industry_rev_share"), 1) %>%
  select(
    firmid, year,
    within_economy_rev_share_BR_growth, within_economy_rev_share_BR_l,
    within_industry_rev_share_growth, within_industry_rev_share_l, within_industry_rev_share_bar
  )

# Merge all data
patenting_products <- merge(patenting_products, rev_share_growth, by = c("firmid", "year"), all.x = T) %>%
  merge(industry_data, by = c("NACE_BR", "year"), all.x = T)
rm(rev_share_growth, industry_data, ipcr_cumulative); gc()

# Save final data
write_rds(patenting_products, "patenting_products_firm_level.RDS")


# 8a) Growth, patenting and trademarking -----------------

patenting_products <- read_rds("patenting_products_firm_level.RDS")

# threshold_young <- 5
# output_dir_og <- output_dir
# patenting_products_og <- patenting_products
graphs <- T
regression_name<-"growth_x_innovation"
ys <- c("nq_growth", "empl_growth", "rev_growth", "within_industry_rev_share_growth") # , "rev_growth")
xs <- c("tm_window", "tm_window*net_product_creat_window", "patent_window", "patent_window*net_product_creat_window", "net_product_destr_window", "net_product_creat_window", "patent_window + ipcr_creat_window")
controls <- c("log(empl_l)")
fe <- c("", "firmid + year", "NACE_BR + year", "NACE_BR^year")

formulas <- create_formulas(ys, xs, controls, fe)
titles_and_restrictions <- fread(
  'title, restriction, weight_flag, additional_controls
  M. Small, age_size_bucket=="mature_small", T,
  M. Medium, age_size_bucket=="mature_medium", T,
  M. Large, age_size_bucket=="mature_large", T,
  Y. Small, age_size_bucket=="young_small", T,
  Y. NonSmall, age_size_bucket=="young_large", T,
  All, NA, log_age'
)

# types<- c("all_firms") # , "patenting_firms"
# subsets<- c("all") # "young", "mature"

regression_innovation_growth(regression_name, patenting_products, formulas, titles_and_restrictions, graphs=graphs)

# 8b) Patenting Graphs -----------------

setDT(patenting_products)

start_year <- 1991
end_year <- 2022

#' Create firm typology, distinguishing firms that never patent,
#' that have patented at some point in the same ipc cluster,
#' and that have patented at some point in new ipc codes
firm_typology <- patenting_products[, .(
  sum_patenting = sum(patent, na.rm = T),
  sum_ipcr_creat = sum(ipcr_creat, na.rm = T)
),
by = .(firmid)
][
  , pat_firm_type := fifelse(
    sum_patenting == 0, "never patent",
    fifelse(sum_ipcr_creat > 0, "new ipcr", "same ipcr")
  )
][
  , c("firmid", "pat_firm_type")
]
patenting_products <- merge(patenting_products, firm_typology, by = "firmid", all.x = T)

#' Create product introduction rates by
patenting_year <- patenting_products[, .(
  net_product_creat_wt = weighted.mean(net_product_creat, nq_bar, na.rm = T),
  net_product_creat = mean(net_product_creat, na.rm = T)
), by = .(year, pat_firm_type)]

unweighted <- ggplot(patenting_year, aes(x = year, y = net_product_creat, color = pat_firm_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(patenting_products$year), max(patenting_products$year), by = 1)) +
  labs(
    title = "Net Product Creation Over Time by Firm Type",
    subtitle = "Unweigthed Average",
    x = "Year",
    y = "Net Product Creation",
    color = "Firm Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.25))

weighted <- ggplot(patenting_year, aes(x = year, y = net_product_creat_wt, color = pat_firm_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(patenting_products$year), max(patenting_products$year), by = 1)) +
  labs(
    subtitle = "Weighted by Average Revenue",
    x = "Year",
    y = "Net Product Creation",
    color = "Firm Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.25))

plot <- unweighted + theme(legend.position = "none") + weighted
print(unweighted)
ggsave(paste0(output_dir, "net_product_creat_time_pat_firm_type.png"), width = 6, height = 4, dpi = 300)

ipcr_year <- ipcr_cumulative[, .(ipcr_creat = mean(ipcr_creat, na.rm = T)), by = .(year)][year %in% start_year:end_year]

ggplot(ipcr_year, aes(x = year, y = ipcr_creat)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(ipcr_year$year), max(ipcr_year$year), by = 1)) +
  labs(
    title = "Share of Patenting Firms Expanding into New IPC Codes",
    x = "Year",
    y = "Share of Patenting Firms",
    color = "Firm Type"
  ) +
  theme_minimal() +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.3)

ggsave(paste0(output_dir, "share_pat_firms_new_ipc.png"), height = 5, width = 9)

# Graphs for patenting and tm by age and size--------------------------------

patent_tm_data <- readRDS("patent_tm_clean.RDS")
firm_data_select <- readRDS("sbs_br_combined_cleaned.rds")

size_age <- firm_data_select[, c(
  "firmid", "year", "NACE_BR", "size", "young",
  "superstar", "superstar_cr4", "superstar_tfp_99", "superstar_tfp_90"
)]
size_age <- merge(size_age, patent_tm_data[, c("firmid", "year", "num_patent", "num_tm")], by = c("firmid", "year"), all.x = T)

size_age[, patent := ifelse(num_patent <= 0 | is.na(num_patent), 0, 1)]
size_age[, tm := ifelse(num_tm <= 0 | is.na(num_tm), 0, 1)]

# Number and share of patents and trademarks per year
patent_tm_year <- size_age[, .(
  patent = sum(num_patent, na.rm = T),
  tm = sum(num_tm, na.rm = T),
  patent_share = mean(patent, na.rm = T),
  tm_share = mean(tm, na.rm = T),
  n_firms = .N
), by = .(year)]
patent_tm_year <- patent_tm_year[, `:=`(
  patent_per_firm = patent / n_firms,
  tm_per_firm = tm / n_firms
)]
for (var in c("patent", "tm", "patent_share", "tm_share", "patent_per_firm", "tm_per_firm")) {
  ggplot(patent_tm_year, aes(x = year, y = .data[[var]])) +
    geom_line()
  ggsave(paste0(output_dir, var, "_year.png"), height = 4, width = 8)
}

# Number and share of patents and trademarks per year and size

for (diff_var in c("size", "young")) {
  patent_tm_year <- size_age[, .(
    patent = sum(num_patent, na.rm = T),
    tm = sum(num_tm, na.rm = T),
    patent_share = mean(patent, na.rm = T),
    tm_share = mean(tm, na.rm = T),
    n_firms = .N
  ), by = .(year, get(diff_var))]
  # names(patent_tm_year)[names(patent_tm_year)=="get"]<-diff_var

  patent_tm_year <- patent_tm_year[, `:=`(
    patent_per_firm = patent / n_firms,
    tm_per_firm = tm / n_firms
  )]
  patent_tm_year <- patent_tm_year[!is.na(get)]


  for (var in c("patent", "tm", "patent_share", "tm_share", "patent_per_firm", "tm_per_firm")) {
    ggplot(patent_tm_year, aes(x = year, y = .data[[var]], color = as.factor(get))) +
      geom_line() +
      labs(color = diff_var)
    ggsave(paste0(output_dir, var, "_", diff_var, "_year.png"), height = 4, width = 8)
  }

  unique_vals <- unique(patent_tm_year$get)

  for (unique_val in unique_vals) {
    for (var in c("patent", "tm", "patent_share", "tm_share", "patent_per_firm", "tm_per_firm")) {
      ggplot(patent_tm_year[get == unique_val], aes(x = year, y = .data[[var]], color = as.factor(get))) +
        geom_line() +
        labs(color = diff_var)
      ggsave(paste0(output_dir, var, "_", diff_var, "_", unique_val, "_year.png"), height = 4, width = 8)
    }
  }
}

# 9) Patenting trademarking analysis -----------------

patenting_products <- readRDS("patenting_products_firm_level.RDS")
threshold_young <- 5
output_dir_og <- output_dir
patenting_products_og <- patenting_products
graphs <- T
regression_name<-"patent_tm_x_size_concentration"
ys <- c("patent", "patent_window", "empl_growth", "nq_growth", "capital_growth") # , "rev_growth")
xs <- c("log_empl_bar*HHI_industry", "within_industry_rev_share*HHI_industry")
controls <- c("log_firm_age")
fe <- c("", "firmid + year", "NACE_BR + year", "NACE_BR^year")


formulas <- create_formulas(ys, xs, controls, fe)
titles_and_restrictions_fixed_buckets <- fread(
  'title, restriction, weight_flag, additional_controls
  All, NA, T,
  M. Small, age_size_bucket=="mature_small", T,
  M. Medium, age_size_bucket=="mature_medium", T,
  M. Large, age_size_bucket=="mature_large", T,
  Y. Small, age_size_bucket=="young_small", T,
  Y. NonSmall, age_size_bucket=="young_large", T,'
)

disag_reg_parameters <- data.table(
  y = "patent_window",
  x = "log_empl_bar*HHI_industry",
  controls = "log_firm_age",
  fe = "firmid + year",
  weight = "nq_bar",
  restriction = "age_size_bucket=='mature_large'",
  disag_var = c("NACE_2d", "NACE_BR")
)

types <- c("all_firms", "patenting_firms")
subsets<- c("all") # "young", "mature"

regression_innovation_growth(regression_name,
  patenting_products, formulas, titles_and_restrictions_fixed_buckets,
  types = types, subsets = subsets, graphs = graphs, disag_reg_parameters = disag_reg_parameters
)

titles_and_restrictions_quantiles <- fread(
  'title, restriction, weight_flag, additional_controls
    All, NA, T, .[x]*(leader)*(young)
  M. Q1, age_size_quartile=="mature_q1", T,
  M. Q4, age_size_quartile=="mature_q4", T,
  M. Q10, age_size_decile=="mature_d10", T,
  M. Q100, age_size_decile=="mature_p99" | age_size_decile=="mature_p100", T,
  M. Q1000, age_size_1000tile=="mature_k999" | age_size_1000tile=="mature_k1000", T,
  M. Q1000, age_size_1000tile=="mature_k999" | age_size_1000tile=="mature_k1000", T,
  M. Leader, age_leader=="mature_leader", T,'
)

regression_name<-"patent_tm_x_size_concentration_quantiles"

regression_innovation_growth(regression_name,
  patenting_products, formulas, titles_and_restrictions_quantiles,
  types = types, subsets = subsets, graphs = graphs, disag_reg_parameters = disag_reg_parameters
)


