# This script creates the final firm level dataset used in the analysis. It merges in NACE codes from the BR data and creates some additional variables for later analysis
# Creates variable of firm positions within industry using information from the industry and firm level datasets



NACE_data = readRDS('sbs_br_combined.rds') %>% select(c('firmid', 'year', 'NACE_BR'))


## the NACE data reported in the BR prior to 2008 uses NAF revision 1 (see ficus documentation) which is equivalent to NACE
## through the first three digits. Since those are the only ones we need to match we're golden 
NACE_data[, NACE_version := ifelse(year<2008, 1.1, 2)]

## add in the the sector (manufac/ services) and tech_level
NACE_data[, industry := as.numeric(str_sub(NACE_BR,1,2))]
#Juli?n: add the by argument, otherwise there is error
NACE_data = merge(NACE_data, division_levels, all.x = T, by=c("industry", "NACE_version"))

#fix the random 3 digit cases from NACE 1.1
NACE_data[ as.numeric(str_sub(NACE_BR,1,3)) == 244 & NACE_version == 1.1, tech_level := 1]
NACE_data[ as.numeric(str_sub(NACE_BR,1,3)) == 353 & NACE_version == 1.1, tech_level := 1]
NACE_data[ as.numeric(str_sub(NACE_BR,1,3)) == 351 & NACE_version == 1.1, tech_level := 3]

# categorize high tech-low tech 
NACE_data[,high_tech := tech_level <3]
NACE_data[,low_tech := !high_tech]
## export
saveRDS(NACE_data,'firm_NACE_BR.rds')


## add variables for later analysis 
sbs_data$birth_year<-as.numeric(sbs_data$birth_year)
sbs_data = sbs_data[, superstar := ifelse(is.na(NACE_BR), NA, nq /sum(nq, na.rm =T)> .01), by =.(year,NACE_BR)]

# Alternative superstar definitions (These were added before saving sbs_data to avoid running all these code twice)
sbs_data[,group_size:=.N, by=.(year, NACE_BR)][, superstar_cr4:=F]
sbs_data[group_size>=10, .SD[order(-nq)][1:4], by=.(year, NACE_BR)][, superstar_cr4:= T][, c("firmid", "year", "NACE_BR", "superstar_cr4")] -> top4
sbs_data[top4, on=.(year, NACE_BR, firmid), superstar_cr4:=i.superstar_cr4]
sbs_data[, superstar_tfp_99:= F]
sbs_data[group_size>=10, tfp_p99:=quantile(tfp,0.99, na.rm=T), by=.(year, NACE_BR)][group_size>=10 & tfp >=tfp_p99, superstar_tfp_99:=T]
sbs_data[, superstar_tfp_90:= F]
sbs_data[group_size>=10, tfp_p90:=quantile(tfp,0.90, na.rm=T), by=.(year, NACE_BR)][group_size>=10 & tfp >=tfp_p90, superstar_tfp_90:=T]
top4<-NULL; sbs_data$group_size<-NULL; sbs_data$superstar_tfp<-NULL; sbs_data$tfp_p90<-NULL; sbs_data$tfp_p99<-NULL

# generate dummies for each sector 
sbs_data[, `:=`(manufacturing = !is.na(sector) & sector == 1,
                services = !is.na(sector) & sector == 2,
                other_sector = is.na(sector) & !is.na(NACE_BR))]

## combine employment bucket + sector
sbs_data[, sector_labor_bucket := ifelse(is.na(empl_bar), NA,
    ifelse(manufacturing, paste0("manufacturing:", as.character(labor_bucket)),
        ifelse(services, paste0("services:", labor_bucket),
            ifelse(other_sector, paste0("other sector:", labor_bucket), NA)
        )
    )
)]

# Create market share measures
firm_lvl_br_dta[, within_industry_rev_share :=  nq_bar/ sum(nq_bar, na.rm = T),
             by = .(NACE_BR, year)]
firm_lvl_br_dta[, within_economy_rev_share_BR := nq_bar / sum(nq_bar, na.rm = T),
    by = .(year)
]

### Size quantiles and age brackets
# 1) Compute quartile/decile/percentile within (year, NACE_BR)
firm_yr_lvl_br_dta <- firm_yr_lvl_br_dta %>%
  .[, `:=`(
    size_quartile = as.integer(ifelse(n_firms_in_industry > 4, pmin(4L, ceiling(4 * rank_within_industry / n_firms_in_industry)), NA)),
    size_decile = as.integer(ifelse(n_firms_in_industry > 10, pmin(10L, ceiling(10 * rank_within_industry / n_firms_in_industry)), NA)),
    size_percentile = as.integer(ifelse(n_firms_in_industry > 100, pmin(100L, ceiling(100 * rank_within_industry / n_firms_in_industry)), NA)),
    size_1000tile = as.integer(ifelse(n_firms_in_industry > 1000, pmin(1000L, ceiling(1000 * rank_within_industry / n_firms_in_industry)), NA)),
    leader = ifelse(rank_within_industry == n_firms_in_industry, 1L, 0L)
  )] %>%
  # .[, c("rank_within_industry", "n_firms_in_industry") := NULL] %>%
  # 2) Build the categorical buckets
  #   a) young/mature x small/medium/large
  .[, age_size_bucket :=
    fcase(
      young == 1 & size == "small",  "young_small",
      young == 1 & size != "small",  "young_large",
      young == 0 & size == "small",  "mature_small",
      young == 0 & size == "medium", "mature_medium",
      young == 0 & size == "large",  "mature_large",
      default = NA_character_
    )] %>%
  #   b) young/mature x quartile (compact construction)
  .[, age_size_quartile := ifelse(is.na(young) | is.na(size_quartile), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_q", size_quartile)
  )] %>%
  #   c) young/mature x decile (compact construction)
  .[, age_size_decile := ifelse(is.na(young) | is.na(size_decile), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_d", size_decile)
  )] %>%
  #   d) young/mature x percentile (compact construction)
  .[, age_size_percentile := ifelse(is.na(young) | is.na(size_percentile), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_p", size_percentile)
  )] %>%
  #   e) young/mature x 1000tile (compact construction)
  .[, age_size_1000tile := ifelse(is.na(young) | is.na(size_1000tile), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_k", size_1000tile)
  )] %>%
  #   f) young/mature x top firm (compact construction)
  .[, age_leader := ifelse(is.na(young) | is.na(leader), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_", fifelse(leader == 1, "leader", "follower"))
  )]


## Filter by prodcom firms or sectors
firm_yr_lvl_br_dta_prodcom_firms<-firm_yr_lvl_br_dta[firmid %in% prodcom_firms]
firm_yr_lvl_br_dta_prodcom_sectors <- firm_yr_lvl_br_dta[ sector_NACE %in% prodcom_sectors]

# Remove outliers
firm_yr_lvl_br_dta <- outliers_remove(firm_yr_lvl_br_dta, 0.01)
firm_yr_lvl_br_dta_prodcom_firms <- outliers_remove(firm_yr_lvl_br_dta_prodcom_firms, 0.01)
# firm_yr_lvl_br_dta_prodcom_sectors <- outliers_remove(firm_yr_lvl_br_dta_prodcom_sectors, 0.01)

# Save firm_data with only relevant variables and firms in industries covered by prodcom
save_parquet(firm_yr_lvl_br_dta, "firm_yr_lvl_br_dta.parquet")
save_parquet(firm_yr_lvl_br_dta_prodcom_firms, "firm_yr_lvl_br_dta_prodcom_firms.parquet")
# save_parquet(firm_yr_lvl_br_dta_prodcom_sectors, 'firm_yr_lvl_br_dta_prodcom_sectors.parquet')


# Adjust windows for left-censoring
product_summary[, pat_filings_window_psum := fifelse(pat_filings_window_psum == 0 & pat_filings_window == 1 & !is.na(pat_filings_window), 1, pat_filings_window_psum)]
product_summary[, pat_families_window_psum := fifelse(pat_families_window_psum == 0 & pat_families_window == 1 & !is.na(pat_families_window), 1, pat_families_window_psum)]


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
  merge(br_industry_HHI, by = c("NACE_BR", "year"), all.x = T)
rm(rev_share_growth, br_industry_HHI, ipcr_cumulative); gc()

