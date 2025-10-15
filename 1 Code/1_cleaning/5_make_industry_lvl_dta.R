# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ----------------------------------------------------------------
firm_yr_lvl_br_dta <- as.data.table(read_parquet("1_firm_yr_lvl_br_dta.parquet"))

division_levels = as.data.table(read_excel("C:/Users/Public/1. Microprod/Reallocation_work/2 Data/product_harmonization_output/division_to_tech_level.xlsx", 
                                           sheet = "division_to_tech_level"))

# make industry-year level data                                    
industry_tech <- firm_yr_lvl_br_dta[, .(NACE_BR = unique(NACE_BR)), by = .(year)]
## the NACE data reported in the BR prior to 2008 uses NAF revision 1 (see ficus documentation) which is equivalent to NACE
## through the first three digits. Since those are the only ones we need to match we're golden 
industry_tech[, NACE_version := ifelse(year<2008, 1.1, 2)]

## add in the the sector (manufac/ services) and tech_level
industry_tech[, industry := as.numeric(str_sub(NACE_BR,1,2))]
#Juli?n: add the by argument, otherwise there is error
industry_tech = merge(industry_tech, division_levels, all.x = T, by=c("industry", "NACE_version"))

#fix the random 3 digit cases from NACE 1.1
industry_tech[ as.numeric(str_sub(NACE_BR,1,3)) == 244 & NACE_version == 1.1, tech_level := 1]
industry_tech[ as.numeric(str_sub(NACE_BR,1,3)) == 353 & NACE_version == 1.1, tech_level := 1]
industry_tech[ as.numeric(str_sub(NACE_BR,1,3)) == 351 & NACE_version == 1.1, tech_level := 3]

# categorize high tech-low tech 
industry_tech[,high_tech := tech_level <3]
industry_tech[,low_tech := !high_tech]

# summarize br variables to industry-year level 
industry_summary <- firm_yr_lvl_br_dta[, .(
  n_firms_in_industry  = .N,
  total_nq = sum(nq, na.rm=T),
  total_empl = sum(empl, na.rm=T),
  av_firm_size_empl = mean(empl, na.rm = TRUE),
  median_firm_size_empl = median(empl, na.rm = TRUE),
  av_firm_size_nq = mean(nq, na.rm = TRUE),
  median_firm_size_nq = median(nq, na.rm = TRUE),
  leader_rev_share = sum(within_industry_rev_share[leader == 1], na.rm = TRUE),
  CR4 = sum(within_industry_rev_share[top_4_leaders == 1], na.rm = TRUE),
  CR10 = sum(within_industry_rev_share[top_10_leaders == 1], na.rm = TRUE),
  diff_leader_vs_2nd = 
    {
      # Find the second highest nq_bar within each (year, NACE_BR) group
      nq_bars <- nq_bar[order(-nq_bar)]
      if (length(nq_bars) > 1 && !is.na(nq_bars[2]) && nq_bars[1] != 0) {
        (nq_bars[1] - nq_bars[2]) / nq_bars[1]
      } else {
        NA_real_
      }
    }
), by = .(year, NACE_BR)]

#Calculate industry HHI and Gini
firm_yr_lvl_br_dta[, market_share_squared := within_industry_rev_share^2]
hhi <- firm_yr_lvl_br_dta[, .(
  HHI_industry = sum(market_share_squared, na.rm = TRUE),
  gini = ineq::Gini(within_industry_rev_share, na.rm = TRUE)
), by = .(year, NACE_BR)]

# Merge industry summary with HHI and tech level
industry_data <- merge(industry_summary, hhi, by=c("year", "NACE_BR"))
industry_data <- merge(industry_data, industry_tech, by = c("NACE_BR", "year"))

# output parquet
write_parquet(industry_data, "5_industry_yr_lvl_dta.parquet")


