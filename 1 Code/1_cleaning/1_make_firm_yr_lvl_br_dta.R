# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

#0) Import Business Registry / SBS data -------------------------------------------------------------------------
# import both datasets then filter BR data by whether it is in SBS,
# which underwent additional cleaning to ensure panel without breaks 

start = 1994
end = 2022
filter_min_n_employees <- 1
# raw_dir<-raw_dir_public

if (grepl("nb|Users/lse", dirname(rstudioapi::getActiveDocumentContext()$path))) {
  firm_yr_lvl_br_dta = readRDS("sbs_br_combined.rds") %>%
    rename(
      economic_birth_year = birth_year, economic_death_year = death_year,
      legal_birth_year = firm_birth_year
    )
} else {
  ## import BR data
  br_data = rbindlist(lapply(c(start:end), function(yr) {
    print(yr)
    ## import BR data
    br_path = paste0(raw_dir, "br/br", yr, ".csv")
    br_data_temp = fread(br_path, select = c("ENT_ID", "year", "empl", "NACE_M", "Start_Ent", "LEGAL"))
    br_data_temp[, NACE_BR := NACE_M]
    br_data_temp[, `:=`(firmid = as.character(ENT_ID))]
    br_data_temp = br_data_temp %>%
      rename(lfo = LEGAL) %>%
      select(firmid, year, empl, NACE_BR, Start_Ent)
  }), fill = T)


  ## import SBS data
  interest_vars = c("ENT_ID", "year", "SBS_12110")
  sbs_data = rbindlist(lapply(c(start:end), function(yr) {
    print(yr)
    ## import SBS
    path = paste0(raw_dir, "sbs/sbs", yr, ".csv")
    sbs_data_temp <- fread(path, nrows = 1)
    available_vars = intersect(interest_vars, colnames(sbs_data_temp))
    sbs_data_temp <- fread(path, select = available_vars)
    sbs_data_temp[, ENT_ID := as.character(ENT_ID)]
    sbs_data_temp = sbs_data_temp %>% select(intersect(interest_vars, colnames(sbs_data_temp)))
  }), use.names = T, fill = T)
  sbs_data = sbs_data %>% select(interest_vars)
  colnames(sbs_data) = c("firmid", "year", "nq")

  ## Juli?n: import BS data to get capital measures
  interest_vars = c("ENT_ID", "year", "capital", "turnover", "raw_materials", "labor_cost")
  bs_data = rbindlist(lapply(c(start:end), function(yr) {
    print(yr)
    ## import BS
    path = paste0(raw_dir, "bs/bs", yr, ".csv")
    bs_data_temp <- fread(path, nrows = 1)
    available_vars = intersect(interest_vars, colnames(bs_data_temp))
    bs_data_temp <- fread(path, select = available_vars)
    bs_data_temp[, ENT_ID := as.character(ENT_ID)]
    bs_data_temp = bs_data_temp %>% select(intersect(interest_vars, colnames(bs_data_temp)))
  }), use.names = T, fill = T)
  bs_data = bs_data %>% select(interest_vars)
  colnames(bs_data) = c("firmid", "year", "capital", "turnover", "raw_materials", "labor_cost")

  # There are some firmid codes with less than 9-digits that are 9-digit codes in the other database
  # This happens because they start with 0 in the other database
  # Here I add an initial 0 to all firmids that have less than 9 digits

  br_data$firmid <- str_pad(br_data$firmid, width = 9, side = "left", pad = "0")
  bs_data$firmid <- str_pad(bs_data$firmid, width = 9, side = "left", pad = "0")
  sbs_data$firmid <- str_pad(sbs_data$firmid, width = 9, side = "left", pad = "0")

  ## merge data and remove those not in sbs / without labor
  firm_yr_lvl_br_dta = merge(sbs_data, br_data, all.x = T)
  firm_yr_lvl_br_dta = merge(firm_yr_lvl_br_dta, bs_data, all.x = T)
  rm(sbs_data, br_data, bs_data)
  gc()

  setorder(firm_yr_lvl_br_dta, firmid, year)

  # Adjust NACE variables
  firm_yr_lvl_br_dta[, `:=`(NACE_BR = zoo::na.locf(NACE_BR, na.rm = F)), by = firmid]
  firm_yr_lvl_br_dta[, `:=`(
    NACE_BR = str_pad(NACE_BR, 4, side = "left", pad = "0"),
    NACE_2d = substr(NACE_BR, 1, 2)
  )]

  ## define active firms as those with post employees;
  ## define firm birth/death as first/last time
  ## with positive employees in the data
  firm_yr_lvl_br_dta = firm_yr_lvl_br_dta[empl > filter_min_n_employees]
  firm_yr_lvl_br_dta[, `:=`(
    economic_birth_year = min(year),
    economic_death_year = max(year)
  ), by = firmid]

  # ## Q: CHECK WHETHER THIS IS AN ISSUE (IN CONCORDANCE WITH AGE VARIABLE DEFINITION BELOW)
  # ## A: I THINK THIS IS AN ISSUE, BECAUSE 1994 FIRMS WILL NOT HAVE A BIRTH YEAR, BUT THEY ARE CLEARLY OLD AND
  # ## IF HAVE LASTED THIS LONG ARE PROBABLY IMPORTANT. BUT THEN I REALIZED I DO DEAL WITH THIS BELOW,
  # ## SO I AM JUST BRINGING THAT CODE UP
  # firm_yr_lvl_br_dta[economic_birth_year == start, economic_birth_year := NA]
  # firm_yr_lvl_br_dta[economic_death_year == end, economic_death_year := NA]

  # Juli?n: Create variable with the year the firm was created according to br
  source(paste0(tools_dir, "firm_birth_year_creator.R")) # CAUTION: This can take a long time
  firm_yr_lvl_br_dta$legal_birth_year <- as.integer(firm_yr_lvl_br_dta$firm_birth_year)
  firm_yr_lvl_br_dta[, firm_birth_year := NULL]
  hist(firm_yr_lvl_br_dta$legal_birth_year, breaks = 121)
}

# Create age and young indicators
firm_yr_lvl_br_dta[, legal_birth_year := ifelse(legal_birth_year == 0, NA, legal_birth_year)]
firm_yr_lvl_br_dta[, consolidated_birth_year := fifelse(
  economic_birth_year == start,
  fifelse(legal_birth_year < economic_birth_year, legal_birth_year, economic_birth_year), economic_birth_year
)]
hist(firm_yr_lvl_br_dta$consolidated_birth_year, breaks = 121)
firm_yr_lvl_br_dta[, firm_age := year - consolidated_birth_year]


firm_yr_lvl_br_dta[, young := ifelse(is.na(firm_age), NA, ifelse(firm_age <= 5, 1, 0))]
firm_yr_lvl_br_dta <- deflate(firm_yr_lvl_br_dta, "NACE_BR", c("nq", "capital", "turnover", "raw_materials", "labor_cost"), 2009)

share_capital_costs <- 0.08
firm_yr_lvl_br_dta[, `:=`(sum_costs = (labor_cost + (capital * share_capital_costs) + raw_materials))] %>%
  .[, `:=`(
    t_l = labor_cost / sum_costs,
    t_k = (capital * share_capital_costs) / sum_costs,
    t_m = (raw_materials) / sum_costs,
    nq_capital = ifelse(capital != 0, nq / capital, 0),
    nq_empl = ifelse(empl != 0, nq / empl, 0),
    nq_raw_materials = ifelse(raw_materials != 0, nq / raw_materials, 0)
  )]

## generate lagged variables Juli?n: add capital
normal_cols = c("nq", "empl", "capital", "raw_materials", "labor_cost") #, "tfp"
firm_yr_lvl_br_dta_temp <- growth_creator(data=firm_yr_lvl_br_dta, 
normal_cols=normal_cols, 
n_lag = 1, 
by_vars = c("firmid", "year"), 
create_born_died = T
)
firm_yr_lvl_br_dta <- merge(firm_yr_lvl_br_dta, firm_yr_lvl_br_dta_temp, by = c("year", "firmid"), all = T)
firm_yr_lvl_br_dta[, status := ifelse(born, "born", ifelse(died, "died", "survived"))]

## generate employment buckets (I use the divisions present in the ICT data)
breaks = c(-Inf, 10, 50, 250, Inf)
categories = c("<10", "10-50", "50-250", "250+")
firm_yr_lvl_br_dta[, labor_bucket := cut(empl_bar, breaks = breaks, labels = categories, right = F)]

# Juli?n Create size variables for regressions
firm_yr_lvl_br_dta[, size := case_when(
  empl_bar < 50 ~ "small",
  empl_bar >= 50 & empl_bar < 250 ~ "medium",
  empl_bar >= 250 ~ "large"
)]
table(firm_yr_lvl_br_dta$size, useNA = "always")

# Bring in Alex's linkedin data
linkedin = read_parquet("french_affiliated_firm_roles_collapsed_clean.parquet") %>%
  select(-c(rcid, `__index_level_0__`)) 
firm_yr_lvl_br_dta <- merge(firm_yr_lvl_br_dta, linkedin, by = c("firmid", "year"), all.x = T)
firm_yr_lvl_br_dta <- firm_yr_lvl_br_dta[, log_emp_rnd:=log(emp_rnd)]
firm_yr_lvl_br_dta[, log_emp_rnd := ifelse(is.nan(log_emp_rnd) | is.infinite(log_emp_rnd), NA_real_, log_emp_rnd)]

firm_data_select[, `:=`(
  rank_within_industry = frank(nq_bar, ties.method = "average", na.last = "keep"),
  n_firms_in_industry  = .N
), by = .(year, NACE_BR)] %>%
  .[, `:=`(
    size_quartile = as.integer(ifelse(n_firms_in_industry > 4, pmin(4L, ceiling(4 * rank_within_industry / n_firms_in_industry)), NA)),
    size_decile = as.integer(ifelse(n_firms_in_industry > 10, pmin(10L, ceiling(10 * rank_within_industry / n_firms_in_industry)), NA)),
    size_percentile = as.integer(ifelse(n_firms_in_industry > 100, pmin(100L, ceiling(100 * rank_within_industry / n_firms_in_industry)), NA)),
    size_1000tile = as.integer(ifelse(n_firms_in_industry > 1000, pmin(1000L, ceiling(1000 * rank_within_industry / n_firms_in_industry)), NA)),
    leader = ifelse(n_firms_in_industry > 1 & rank_within_industry == n_firms_in_industry, 1L, 0L),
    top_4_leaders = ifelse(n_firms_in_industry > 4 & rank_within_industry > (n_firms_in_industry - 4), 1L, 0L),
    top_10_leaders = ifelse(n_firms_in_industry > 10 & rank_within_industry > (n_firms_in_industry - 10), 1L, 0L)
  )] %>%
  # Add a measure of how far away leaders are from the rest of the distribution, share of leaders (top1, top4, top10) in total industry revenue
  .[, `:=`(
    leader_rev_share = sum(within_industry_rev_share[leader == 1], na.rm = TRUE),
    top_4_leaders_rev_share = sum(within_industry_rev_share[top_4_leaders == 1], na.rm = TRUE),
    top_10_leaders_rev_share = sum(within_industry_rev_share[top_10_leaders == 1], na.rm = TRUE),
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
  ), by = .(year, NACE_BR)] %>%

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
  )] %>%
  #  g) young/mature x top 4 firms (compact construction)
  .[, age_top_4_leaders := ifelse(is.na(young) | is.na(top_4_leaders), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_", fifelse(top_4_leaders == 1, "top_4", "not_top_4"))
  )] %>%
  #  h) young/mature x top 10 firms (compact construction)
  .[, age_top_10_leaders := ifelse(is.na(young) | is.na(top_10_leaders), NA,
    paste0(fifelse(young == 1, "young", "mature"), "_", fifelse(top_10_leaders == 1, "top_10", "not_top_10"))
  )]

setorder(firm_data_select, NACE_BR, year, rank_within_industry)
  View(firm_data_select %>% select(
    firmid, year, nq, NACE_BR, rank_within_industry, n_firms_in_industry, 
    size_quartile, size_decile, size_percentile, size_1000tile, leader, top_4_leaders, top_10_leaders, 
    leader_rev_share, top_4_leaders_rev_share, top_10_leaders_rev_share, diff_leader_vs_2nd,
    age_size_bucket, age_size_quartile, age_size_decile, age_size_percentile, age_size_1000tile, age_leader
  ))

## Filter by prodcom firms or sectors
firm_data_select <-firm_data_select[firmid %in% prodcom_firms]

# Remove outliers
firm_data_select <- outliers_remove(firm_data_select, 0.01)

# Juli?n: Only firmid, year, nace information
NACE_BR_data <- firm_yr_lvl_br_dta[, c("firmid", "year", "NACE_BR", "NACE_2d")]

# Save firm_data with only relevant variables and firms in industries covered by prodcom
save_parquet(firm_yr_lvl_br_dta, "1_firm_yr_lvl_br_dta.parquet")
save_parquet(NACE_BR_data, 'ancillary_datasets/NACE_BR_data.parquet')

