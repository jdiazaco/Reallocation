# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ----------------------------------------------------------------

end <- 2022

lifi_data <- as.data.table(read_parquet("C:/Users/Public/1. Microprod/0. Raw data processing/Codes/Preparation_LIFI/LIFI.parquet"))
lifi_data[, year:=as.numeric(year)]
firm_data <- as.data.table(read_parquet("1_firm_yr_lvl_br_dta.parquet"))
setkey(lifi_data, firmid, year)
setkey(firm_data, firmid, year)
firm_data <- lifi_data[firm_data]
rm(lifi_data); gc()

product_data <- as.data.table(read_parquet(paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet")))
patent_data <- as.data.table(read_rds("4b_patenting_products_firm_level.RDS"))
Vieindustry_data <- as.data.table(read_parquet("5_industry_yr_lvl_dta.parquet")) %>% select(-c("n_firms_in_industry", "leader_rev_share", "diff_leader_vs_2nd"))


# Make lists of firms
firm_list_names <- c("prodcom", "br", "patenting")
firm_list_sources <- list(
  prodcom = product_data,
  br = firm_data,
  patenting = patent_data
)

dir.create("firm_lists", showWarnings = FALSE)
for (name in firm_list_names) {
  dt <- firm_list_sources[[name]]
  cols <- intersect(c("firmid", "firmid_char"), names(dt))
  if( length(cols)==0L) {
    warning(sprintf("No firm id columns found for list '%s' - skipping", name))
    next
  }
  firms <- unique(dt[, ..cols])
  write.csv(firms, file = paste0("firm_lists/", name, "_firms.csv"), row.names = FALSE)
}

firm_data <- firm_data %>% setkey(firmid, year, consolidated_birth_year, legal_birth_year, economic_birth_year, economic_death_year)
product_data <- product_data %>% setkey(firmid, year, consolidated_birth_year, legal_birth_year, economic_birth_year, economic_death_year)
patent_data <- patent_data %>% setkey(firmid, year) 
setkey(industry_data, NACE_BR, year)

firm_data <- product_data[firm_data] %>% setkey(firmid, year, consolidated_birth_year, legal_birth_year, economic_birth_year, economic_death_year)
firm_data <- patent_data[firm_data] %>% setkey(NACE_BR, year)
firm_data <- industry_data[firm_data] %>% setkey(firmid, year)
rm(product_data, patent_data, industry_data); gc()

vars_to_log <- c("firm_age", "empl_bar")
# Create log variables
for(var in vars_to_log){
  log_var <- paste0("log_", var)
  firm_data[, (log_var) := asinh(get(var))]
}

firm_data <- firm_data[, product_innovative_pat_filings_window := net_product_creat_window * pat_filings_dummy_window] %>% # Interaction between product creation and patenting
  .[, product_strategic_pat_filings_window := (1 - net_product_creat_window) * pat_filings_dummy_window] %>%
  .[, product_innovative_pat_families_window := net_product_creat_window * pat_families_dummy_window] %>% # Interaction between product creation and patenting
  .[, product_strategic_pat_families_window := (1 - net_product_creat_window) * pat_families_dummy_window]  %>% setkey(firmid, year)
# Create revenue share growth variables
rev_share_growth <- growth_creator(firm_data, c("within_economy_rev_share_BR", "within_industry_rev_share"), 1) %>%
  select(
    firmid, year,
    within_economy_rev_share_BR_growth, within_economy_rev_share_BR_l,
    within_industry_rev_share_growth, within_industry_rev_share_l, within_industry_rev_share_bar
  ) %>% setkey(firmid, year)

# Merge all data
firm_data <- rev_share_growth[firm_data]
rm(rev_share_growth); gc()

# Adjust patent variables
patent_data_names <- c("ever_patent",
                       "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
                       "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth")
firm_data[, (patent_data_names):=lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = patent_data_names]

write_rds(firm_data, "7_final_firm_lvl_dta.rds")
rm(list=ls()); gc()

