# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ----------------------------------------------------------------
firm_data <- as.data.table(read_parquet("1_firm_yr_lvl_br_dta.parquet"))
product_data <- as.data.table(read_parquet(paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet")))
patent_data <- as.data.table(read_rds("4b_patenting_products_firm_level.RDS"))
industry_data <- as.data.table(read_parquet("5_industry_yr_lvl_dta.parquet"))


# Make lists of firms
firm_list_names <- c("prodcom", "br", "patenting")
firm_list_sources <- list(
  prodcom = product_data,
  br = firm_data,
  patenting = patent_data
)

dir.create("firm_lists", showWarnings = FALSE)
for (name in firm_list_names) {
  firms <- unique(firm_list_sources[[name]]$firmid)
  write.csv(data.frame(firmid = firms), file = paste0("firm_lists/", name, "_firms.csv"), row.names = FALSE)
}

firm_data <- merge(firm_data, product_data,
    by = c("firmid", "year", "consolidated_birth_year", "legal_birth_year", "economic_birth_year", "economic_death_year"), 
    all.x = TRUE
  )
  firm_data <- merge(firm_data, patent_data, by = c("firmid", "year"), all.x = TRUE)
  firm_data <- merge(firm_data, industry_data, by = c("NACE_BR", "year"), all.x = TRUE)
rm(product_data, patent_data, industry_data); gc()


# ## add variables for later analysis 
# sbs_data$birth_year<-as.numeric(sbs_data$birth_year)
# sbs_data = sbs_data[, superstar := ifelse(is.na(NACE_BR), NA, nq /sum(nq, na.rm =T)> .01), by =.(year,NACE_BR)]

# # Alternative superstar definitions (These were added before saving sbs_data to avoid running all these code twice)
# sbs_data[,group_size:=.N, by=.(year, NACE_BR)][, superstar_cr4:=F]
# sbs_data[group_size>=10, .SD[order(-nq)][1:4], by=.(year, NACE_BR)][, superstar_cr4:= T][, c("firmid", "year", "NACE_BR", "superstar_cr4")] -> top4
# sbs_data[top4, on=.(year, NACE_BR, firmid), superstar_cr4:=i.superstar_cr4]
# sbs_data[, superstar_tfp_99:= F]
# sbs_data[group_size>=10, tfp_p99:=quantile(tfp,0.99, na.rm=T), by=.(year, NACE_BR)][group_size>=10 & tfp >=tfp_p99, superstar_tfp_99:=T]
# sbs_data[, superstar_tfp_90:= F]
# sbs_data[group_size>=10, tfp_p90:=quantile(tfp,0.90, na.rm=T), by=.(year, NACE_BR)][group_size>=10 & tfp >=tfp_p90, superstar_tfp_90:=T]
# top4<-NULL; sbs_data$group_size<-NULL; sbs_data$superstar_tfp<-NULL; sbs_data$tfp_p90<-NULL; sbs_data$tfp_p99<-NULL

# ## Filter by prodcom firms or sectors
# firm_yr_lvl_br_dta_prodcom_firms<-firm_yr_lvl_br_dta[firmid %in% prodcom_firms]
# firm_yr_lvl_br_dta_prodcom_sectors <- firm_yr_lvl_br_dta[ sector_NACE %in% prodcom_sectors]

# # Remove outliers
# firm_yr_lvl_br_dta <- outliers_remove(firm_yr_lvl_br_dta, 0.01)
# firm_yr_lvl_br_dta_prodcom_firms <- outliers_remove(firm_yr_lvl_br_dta_prodcom_firms, 0.01)
# # firm_yr_lvl_br_dta_prodcom_sectors <- outliers_remove(firm_yr_lvl_br_dta_prodcom_sectors, 0.01)

# # Save firm_data with only relevant variables and firms in industries covered by prodcom
# save_parquet(firm_yr_lvl_br_dta, "firm_yr_lvl_br_dta.parquet")
# save_parquet(firm_yr_lvl_br_dta_prodcom_firms, "firm_yr_lvl_br_dta_prodcom_firms.parquet")
# # save_parquet(firm_yr_lvl_br_dta_prodcom_sectors, 'firm_yr_lvl_br_dta_prodcom_sectors.parquet')

# Adjust windows for left-censoring
# firm_data[, pat_filings_window_psum := fifelse(pat_filings_window_psum == 0 & pat_filings_window == 1 & !is.na(pat_filings_window), 1, pat_filings_window_psum)]
# firm_data[, pat_families_window_psum := fifelse(pat_families_window_psum == 0 & pat_families_window == 1 & !is.na(pat_families_window), 1, pat_families_window_psum)]

vars_to_log <- c("firm_age", "empl_bar")
# Create log variables
for(var in vars_to_log){
  log_var <- paste0("log_", var)
  firm_data[, (log_var) := asinh(get(var))]
}

# firm_data <- firm_data[product_innovative_patent_window := net_product_creat_window * patent_window] %>% # Interaction between product creation and patenting
#   .[, product_strategic_patent_window := (1 - net_product_creat_window) * patent_window] # Interaction between lack of product creation and patenting

# # Create revenue share growth variables
# rev_share_growth <- growth_creator(patenting_products, c("within_economy_rev_share_BR", "within_industry_rev_share"), 1) %>%
#   select(
#     firmid, year,
#     within_economy_rev_share_BR_growth, within_economy_rev_share_BR_l,
#     within_industry_rev_share_growth, within_industry_rev_share_l, within_industry_rev_share_bar
#   )

write_parquet(firm_data, "6_final_firm_lvl_dta.parquet")

