# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ----------------------------------------------------------------
firm_yr_lvl_br_dta <- as.data.table(read_parquet("1_firm_yr_lvl_br_dta.parquet"))

division_levels = as.data.table(read_excel("Ancillary datasets/division_to_tech_level.xlsx", 
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
industry_summary <- firm_yr_lvl_br_dta[
  !is.na(NACE_BR),
  .(
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

# Create industry dynamism measures
share_capital_costs <- 0.08
industry_outcomes <- firm_yr_lvl_br_dta[
  !is.na(NACE_BR),
  .(
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
    gross_job_reallocation = (sum(empl_reallocation_weighted, na.rm = TRUE) / sum(empl_share, na.rm=TRUE)),
    gross_sales_reallocation = (sum(nq_reallocation_weighted, na.rm = TRUE) / sum(nq_share, na.rm=TRUE)),
    markup = (sum(mu_sepcal_TL_sNoFSR_t10 * within_economy_rev_share_BR, na.rm = TRUE) / sum(within_economy_rev_share_BR, na.rm=TRUE)),
    
    sales_growth_dispersion = safe_sd(nq_growth),
    employment_growth_dispersion = safe_sd(empl_growth)
  ),
  by = .(NACE_BR, year)
]


#Calculate industry HHI and Gini
firm_yr_lvl_br_dta[, market_share_squared := within_industry_rev_share^2]
hhi <- firm_yr_lvl_br_dta[, .(
  HHI_industry = sum(market_share_squared, na.rm = TRUE),
  gini = ineq::Gini(within_industry_rev_share, na.rm = TRUE)
), by = .(year, NACE_BR)]


# Compact leader/top-N lookups and merge
leader_top_lookup <- firm_yr_lvl_br_dta[
  ,
  {
    l <- as.character(firmid[leader == 1])
    t4 <- as.character(unique(firmid[top_4_leaders == 1]))
    t10 <- as.character(unique(firmid[top_10_leaders == 1]))
    competitors_BR <- as.character(unique(firmid))
    list(
      leader_BR = if (length(l)) l[1] else NA_character_,
      top_4_BR = list(if (length(t4)) t4 else NA_character_),
      top_10_BR = list(if (length(t10)) t10 else NA_character_),
      competitors_BR = list(if (length(t10)) competitors_BR else NA_character_)
    )
  },
  by = .(NACE_BR, year)
]

# Merge industry summary with HHI and tech level
industry_data <- merge(industry_summary, hhi, by=c("year", "NACE_BR"), all.x = TRUE)
industry_data <- merge(industry_data, industry_tech, by = c("NACE_BR", "year"), all.x = TRUE)
industry_data <- merge(industry_data, industry_outcomes, by = c("NACE_BR", "year"), all.x = TRUE)
industry_data <- merge(industry_data, leader_top_lookup, by = c("NACE_BR", "year"), all.x = TRUE)


industry_data <- industry_data[substr(NACE_BR, 1, 2) %in% prodcom_sectors]

# output parquet
write_parquet(industry_data, "5_industry_yr_lvl_dta.parquet")

# ggplot(industry_data[n_firms_in_industry>1], aes(x=n_firms_in_industry, y=gross_job_reallocation)) + geom_point(alpha=0.1)
