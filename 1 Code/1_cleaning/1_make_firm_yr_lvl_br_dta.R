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
  firm_yr_lvl_br_dta = setDT(readRDS("sbs_br_combined.rds")) %>%
    rename(
      economic_birth_year = birth_year, economic_death_year = death_year,
      legal_birth_year = firm_birth_year
    ) %>%
    .[, `:=`(economic_birth_year = min(year), economic_death_year = max(year)), by = firmid] %>%
    .[, NACE_2d_BR := substr(NACE_BR, 1, 2)]
  
  patent_data <- load_dataset(
    c("3_patent_lvl_patent_dta.parquet", "patent_data/3_patent_lvl_patent_dta.parquet"),
    read_parquet
  ) |> to_dt()
  
  # Documentation: Create a side-by-side comparison table of unique firm IDs from two sources
  # - Extract unique firm IDs from firm_yr_lvl_br_dta and patent_data, convert to character, sort, and remove NA
  # - Determine n as the maximum length of the two unique-ID vectors
  # - Pad the shorter vector(s) with NA so both vectors have length n
  # - Construct a data.table 'test' with:
  #     * firmid_product_data: numeric coercion of v1 (as.numeric(v1))
  #     * firmid_patent_data: v2 (character)
  # - Purpose: align and compare firm identifiers from product-level and patent datasets side-by-side
  # - Note: converting v1 to numeric will yield NA for any non-numeric IDs
  br_firmids_char <- unique(as.character(firm_yr_lvl_br_dta$firmid)) %>% sort() %>% na.omit()
  patent_firmids_char <- unique(as.character(patent_data$firmid)) %>% sort() %>% na.omit()
  pad_len <- max(length(br_firmids_char), length(patent_firmids_char))
  br_firmids_padded <- c(br_firmids_char, rep(NA_character_, pad_len - length(br_firmids_char)))
  patent_firmids_padded <- c(patent_firmids_char, rep(NA_character_, pad_len - length(patent_firmids_char)))
  firmid_comparison_table <- data.table(
    firmid_br_numeric = as.numeric(br_firmids_padded),
    firmid_patent_string = patent_firmids_padded
  )
  
  # Map BR firmids to patent firmids where possible; replace BR firmid with patent id when available
  firm_yr_lvl_br_dta <- merge(
    firm_yr_lvl_br_dta,
    firmid_comparison_table,
    by.x = "firmid",
    by.y = "firmid_br_numeric",
    all.x = TRUE
  ) %>%
    .[, firmid := as.character(firmid_patent_string)] %>%
    .[, firmid_patent_string := NULL]
  
} else {
  ## import BR data
  br_data = rbindlist(lapply(c(start:end), function(yr) {
    print(yr)
    ## import BR data
    br_path = paste0(raw_dir, "br/br", yr, ".csv")
    br_data_temp = fread(br_path, select = c("ENT_ID", "year", "NACE_M", "Start_Ent", "LEGAL"))
    br_data_temp[, NACE_BR := NACE_M]
    br_data_temp[, `:=`(firmid = as.character(ENT_ID))]
    br_data_temp = br_data_temp %>%
      rename(lfo = LEGAL) %>%
      select(firmid, year, NACE_BR, Start_Ent)
  }), fill = T)
  
  ## import SBS data
  interest_vars = c("ENT_ID", "year", "SBS_12110", "SBS_16110")
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
  colnames(sbs_data) = c("firmid", "year", "nq", "empl")
  
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
  
  setkey(br_data, firmid, year)
  setkey(bs_data, firmid, year)
  setkey(sbs_data, firmid, year)
  
  # firmid_keys <- data.table(firmid= c(unique(br_data$firmid),
  #                                          unique(bs_data$firmid),
  #                                          unique(bs_data$firmid)))
  # 
  # firmid_keys <- unique(firmid_keys) %>% arrange(firmid, desc=T) %>%  .[, firmid:=.I]
  # fwrite(firmid_keys, "firm_lists/firmid_keys.csv")
  
  # br_data[, firmid:=NULL]
  # bs_data[, firmid:=NULL]
  # sbs_data[, firmid:=NULL]
  
  # br_data <- merge(br_data, firmid_keys, by=c("firmid"), all.x=T)
  # bs_data <- merge(bs_data, firmid_keys, by=c("firmid"), all.x=T)
  # sbs_data <- merge(sbs_data, firmid_keys, by=c("firmid"), all.x=T)
  
  ## merge data and remove those not in sbs / without labor
  firm_yr_lvl_br_dta = sbs_data[br_data]
  setkey(firm_yr_lvl_br_dta, firmid, year)
  firm_yr_lvl_br_dta <- bs_data[firm_yr_lvl_br_dta]
  rm(sbs_data, br_data, bs_data)
  gc()
  setkey(firm_yr_lvl_br_dta, firmid, year)
  
  ## define active firms as those with post employees;
  ## define firm birth/death as first/last time
  ## with positive employees in the data
  firm_yr_lvl_br_dta = firm_yr_lvl_br_dta[empl >= filter_min_n_employees]
  firm_yr_lvl_br_dta[, `:=`(
    economic_birth_year = min(year),
    economic_death_year = max(year)
  ), by = firmid]
  
  # Juli?n: Create variable with the year the firm was created according to br
  source(paste0(tools_dir, "firm_birth_year_creator.R")) # CAUTION: This can take a long time
  firm_yr_lvl_br_dta$legal_birth_year <- as.integer(firm_yr_lvl_br_dta$firm_birth_year)
  firm_yr_lvl_br_dta[, firm_birth_year := NULL]
  hist(firm_yr_lvl_br_dta$legal_birth_year, breaks = 121)
  
  {
    firm_yr_lvl_br_dta <- read_csv("ficusfare_profil_wof_19942019_v0222.csv")
    test <- fread("1_temp.csv")
    
  }
  
}


fwrite(firm_yr_lvl_br_dta, "1_temp.csv")
firm_yr_lvl_br_dta <- fread("1_temp.csv")
setkey(firm_yr_lvl_br_dta, firmid, year)

# Create age and young indicators
firm_yr_lvl_br_dta[, legal_birth_year := ifelse(legal_birth_year == 0, NA, legal_birth_year)]
firm_yr_lvl_br_dta[, consolidated_birth_year := fifelse(
  economic_birth_year == start,
  fifelse(legal_birth_year < economic_birth_year & legal_birth_year>=1901, legal_birth_year, economic_birth_year), economic_birth_year
)]
hist(firm_yr_lvl_br_dta$consolidated_birth_year, breaks = 121)
firm_yr_lvl_br_dta[, firm_age := year - consolidated_birth_year]

firm_yr_lvl_br_dta[, young := ifelse(is.na(firm_age), NA, ifelse(firm_age <= 5, 1, 0))]
firm_yr_lvl_br_dta <- deflate(firm_yr_lvl_br_dta, "NACE_BR", c("nq", "capital", "turnover", "raw_materials", "labor_cost"), 2009) %>%
  rename(DEFind_BR = DEFind)

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

# Calculate median cost shares by industry-year
cost_share_medians <- firm_yr_lvl_br_dta[, .(
  elas_t_l = median(t_l, na.rm = TRUE),
  elas_t_k = median(t_k, na.rm = TRUE),
  elas_t_m = median(t_m, na.rm = TRUE)
), by = .(NACE_BR, year)]

# Merge medians back to main data
firm_yr_lvl_br_dta <- merge(firm_yr_lvl_br_dta, cost_share_medians, by = c("NACE_BR", "year"), all.x = TRUE)

# Estimate cost-share production function by industry-year and compute TFP residuals
firm_yr_lvl_br_dta[, tfpr := nq - (elas_t_l * asinh(empl) + elas_t_k * asinh(capital) + elas_t_m * asinh(raw_materials))]

## generate lagged variables Juli?n: add capital
normal_cols = c("nq", "empl", "capital", "raw_materials", "labor_cost", "tfpr") #, "tfp"
firm_yr_lvl_br_dta <- growth_creator(
  data = firm_yr_lvl_br_dta,
  normal_cols = normal_cols,
  n_lag = 1,
  by_vars = c("firmid", "year"),
  create_born_died = T
)
print("The following should be only 1: ")
table(firm_yr_lvl_br_dta[, .(n=.N), by=.(firmid, year)]$n)

# Adjust NACE variables
setorder(firm_yr_lvl_br_dta, firmid, year)
firm_yr_lvl_br_dta[, `:=`(NACE_BR = zoo::na.locf(NACE_BR, na.rm = F)), by = firmid]
firm_yr_lvl_br_dta[, `:=`(
  NACE_BR = str_pad(NACE_BR, 4, side = "left", pad = "0"),
  NACE_2d_BR = substr(NACE_BR, 1, 2)
)]

firm_yr_lvl_br_dta[, status := ifelse(born, "born", ifelse(died, "died", "survived"))]

# Generate sector dummies
firm_yr_lvl_br_dta[, `:=`(
  manufacturing = !is.na(NACE_2d_BR) & substr(NACE_2d_BR, 1, 1) == 1,
  services = !is.na(NACE_2d_BR) & substr(NACE_2d_BR, 1, 1) == 2,
  other_sector = is.na(NACE_2d_BR) & !is.na(NACE_BR)
)]

## generate employment buckets (I use the divisions present in the ICT data)
breaks = c(-Inf, 10, 50, 250, Inf)
categories = c("<10", "10-50", "50-250", "250+")
firm_yr_lvl_br_dta[, labor_bucket := cut(empl_bar, breaks = breaks, labels = categories, right = F)]

## combine employment bucket + sector
firm_yr_lvl_br_dta[, sector_labor_bucket := ifelse(is.na(empl_bar), NA,
    ifelse(manufacturing, paste0("manufacturing:", as.character(labor_bucket)),
        ifelse(services, paste0("services:", labor_bucket),
            ifelse(other_sector, paste0("other sector:", labor_bucket), NA)
        )
    )
)]


# Juli?n Create size variables for regressions
firm_yr_lvl_br_dta[, size := case_when(
  empl_bar < 50 ~ "small",
  empl_bar >= 50 & empl_bar < 250 ~ "medium",
  empl_bar >= 250 ~ "large"
)]
table(firm_yr_lvl_br_dta$size, useNA = "always")

# firm_yr_lvl_br_dta <- firm_yr_lvl_br_dta %>% rename(firmid=firmid) %>% 
#   .[, firmid:=as.numeric(firmid)]


# Create market share measures
firm_yr_lvl_br_dta[, within_industry_rev_share := nq_bar / sum(nq_bar, na.rm = T),
                   by = .(NACE_BR, year)
]
firm_yr_lvl_br_dta[, within_economy_rev_share_BR := nq_bar / sum(nq_bar, na.rm = T),
                   by = .(year)
]

# 1) Create size measures: rank within industry, size buckets (quartile, decile, percentile, 1000tile), leader and top_4_leaders
firm_yr_lvl_br_dta[, `:=`(
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

# setorder(firm_yr_lvl_br_dta, NACE_BR, year, rank_within_industry)
# View(firm_yr_lvl_br_dta %>% select(
#   firmid, year, nq, NACE_BR, rank_within_industry, n_firms_in_industry, 
#   size_quartile, size_decile, size_percentile, size_1000tile, leader, top_4_leaders, top_10_leaders, 
#   leader_rev_share, top_4_leaders_rev_share, top_10_leaders_rev_share, diff_leader_vs_2nd,
#   age_size_bucket, age_size_quartile, age_size_decile, age_size_percentile, age_size_1000tile, age_leader
# ))

# ## Filter by prodcom firms or sectors
# firm_yr_lvl_br_dta <- firm_yr_lvl_br_dta[firmid %in% prodcom_firms]

# # Remove outliers
# firm_yr_lvl_br_dta <- outliers_remove(firm_yr_lvl_br_dta, 0.01)

# Juli?n: Only firmid, year, nace information
NACE_BR_data <- firm_yr_lvl_br_dta[, c("firmid", "year", "NACE_BR", "NACE_2d_BR", "DEFind_BR")]

print("The following should be only 1: ")
table(firm_yr_lvl_br_dta[, .(n=.N), by=.(firmid, year)]$n)

if (!grepl("nb|Users/lse", dirname(rstudioapi::getActiveDocumentContext()$path))) {
  
  
  # Bring in Alex's linkedin data
  linkedin = read_parquet("linkedin_data/french_affiliated_firm_roles_collapsed_clean.parquet") %>%
    select(-c(rcid, `__index_level_0__`))  %>% setDT(.)%>% setkey(firmid, year)
  firm_yr_lvl_br_dta <- linkedin[firm_yr_lvl_br_dta]
  rm(linkedin); gc()
  firm_yr_lvl_br_dta <- firm_yr_lvl_br_dta[, log_emp_rnd:=log(emp_rnd)]
  firm_yr_lvl_br_dta[, log_emp_rnd := ifelse(is.nan(log_emp_rnd) | is.infinite(log_emp_rnd), NA_real_, log_emp_rnd)]
  setkey(firm_yr_lvl_br_dta, firmid, year)
  
  # Bring in NUTS information
  nuts<-fread("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Ancillary datasets/NUTS/nuts_soe_addition.csv") 
  nuts_conc<-fread("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Ancillary datasets/NUTS/nuts_conc.csv")
  
  # # Clean it and merge it to product_summary
  nuts[, firmid:=str_pad(as.character(siren), 9, side="left", pad="0")]
  # nuts<-nuts[firmid %in% unique(product_summary$firmid)]
  nuts<-nuts[, c("firmid", "year", "nuts3")]
  nuts<-merge(nuts, nuts_conc, by.x="nuts3", by.y="nuts2013", all.x=T)
  nuts[, nuts3:=fifelse(!is.na(nuts2016), nuts2016, nuts3)]
  nuts[, nuts3:=fifelse(nuts3=="",NA_character_, nuts3)]
  nuts<-nuts[, c("firmid", "year", "nuts3")] %>% setkey(firmid, year) # firmid==445045537 
  # firm_yr_lvl_br_dta<- merge(firm_yr_lvl_br_dta, nuts, by=c("firmid", "year"), all.x = T)
  firm_yr_lvl_br_dta <- nuts[firm_yr_lvl_br_dta]
}

# Bring in markup information
if(grepl("Drive", getwd())){
  # create variable mu_sepcal_TL_sNoFSR_t10 sampling from 0.5 to 2
  firm_yr_lvl_br_dta[, mu_sepcal_TL_sNoFSR_t10 := runif(.N, min = 0.5, max = 2)]
  firm_yr_lvl_br_dta[, mu_sepcal_TL_sBFSR_t10 := runif(.N, min = 0.5, max = 2)]
  firm_yr_lvl_br_dta[, firmid:=str_pad(firmid, 9, side="left", "0")]
  
} else {
  markup_data <- read_dta("dta_mu_sepcal_ficusfare_reduced_sec_year_firm_win020_p_XTABOND_andSTATA_DGM.dta") 
  setDT(markup_data)
  markup_data[, firmid:=str_pad(siren, 9, side="left", "0")]
  markup_data[, siren:=NULL]
  firm_yr_lvl_br_dta <- merge(firm_yr_lvl_br_dta, markup_data, by=c("firmid", "year"), all.x=T)
}

# Save firm_data with only relevant variables and firms in industries covered by prodcom
write_parquet(firm_yr_lvl_br_dta, "1_firm_yr_lvl_br_dta.parquet")
write_parquet(NACE_BR_data, 'Ancillary datasets/NACE_BR_data.parquet')
# 
# firm_yr_lvl_br_dta <- read_parquet("1_firm_yr_lvl_br_dta.parquet")
# firm_yr_lvl_br_dta[, c("firmid.x", "firmid.y"):=NULL]
# firmid_keys<-fread("firm_lists/firmid_keys.csv")
# 
# firm_yr_lvl_br_dta <- merge(firm_yr_lvl_br_dta, firmid_keys, all.x=TRUE)
# write_parquet(firm_yr_lvl_br_dta, "1_firm_yr_lvl_br_dta.parquet")

