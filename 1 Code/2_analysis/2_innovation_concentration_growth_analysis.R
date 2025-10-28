#' ------------------------------------------------------------------------------
#' Script: Patent Data Cleaning and Cumulative IPCR/NACE Generation
#' Author: Juli?n D?az-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' ------------------------------------------------------------------------------

# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 8) IPC analysis -----------------

# Load all necessary data


start=2009
end=2022

patenting_products <- read_rds("6_final_firm_lvl_dta.rds")  # Firm-year level data (from b.)

# Find which names are in firm_data and which aren't
selected_names <- c(
  # Identifiers
  "firmid", "year",

  # Industry / classification
  "NACE_BR", "NACE_2d_BR",
  "NACE_cum", "NACE_cum_l", "new_NACE",
  "n_NACE", "n_NACE_bar", "n_NACE_growth",

  # Firm demographics & age/size buckets
  "young", "firm_age", "log_firm_age",
  "age_leader", "age_size_bucket", "age_size_quartile", "age_size_decile", "age_size_percentile", "age_size_1000tile",
  "leader",

  # Size measures and ranks
  "size", "size_quartile", "size_decile", "size_percentile", "size_1000tile",
  "rank_within_industry", "n_firms_in_industry",

  # Employment / inputs
  "empl_bar", "empl_l", "log_empl_bar",
  "empl_growth", "nq_bar", "nq_growth",
  "capital", "capital_bar", "capital_growth",

  # Revenue / shares
  "rev_bar", "rev_growth",
  "within_economy_rev_share_BR", "within_economy_rev_share_BR_l", "within_economy_rev_share_BR_growth",
  "within_industry_rev_share", "within_industry_rev_share_l", "within_industry_rev_share_bar", "within_industry_rev_share_growth",

  # Productivity
  "tfp_bar", "tfp_growth",

  # Product portfolio & dynamics
  "number_of_products",
  "prod_creat", "prod_destr", "prod_added", "prod_removed",
  "new_products", "first_introduction",
  "net_product_creat", "net_product_creat_window",
  "net_product_destr", "net_product_destr_window",

  # Patents & filings
  "patent", "patent_window", "ever_patent",
  "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
  "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth",
  "product_innovative_pat_filings_window", "product_strategic_pat_filings_window", "product_innovative_pat_families_window", "product_strategic_pat_families_window",

  # IPC expansion / IPCR measures
  "ipcr_cum", "ipcr_cum_l", "ipcr_creat", "ipcr_creat_window", "new_ipcr",
  "n_ipcr", "n_ipcr_bar", "n_ipcr_growth",

  # Trademarks
  "tm", "tm_window", "ever_tm"
)

in_firm_data <- selected_names[selected_names %in% names(patenting_products)]
not_in_firm_data <- selected_names[!selected_names %in% names(patenting_products)]
# Keep only continuers and from 2009
patenting_products <- patenting_products[year>=start & abs(empl_growth) != 2 & abs(nq_growth) != 2]  %>%
  select(in_firm_data)


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


