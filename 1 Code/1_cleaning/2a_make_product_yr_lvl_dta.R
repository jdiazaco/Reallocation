# This script processes Prodcom data to create a firm-product-year level dataset.
# It aggregates product data to various classification levels, calculates growth metrics, and prepares the data for analysis.
# The script is designed to be flexible, allowing for different levels of product aggregation and filtering options.

# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))
dummy <- FALSE

#3) extract product level data from Prodcom  ----------------------------------------------

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
parameters(prodfra_or_pcc8, only_prodfra_in_prodcom)

## import supplementary data
harmonized_prodfra = fread(paste0("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/product_harmonization_output/harmonized codes/prodfra_harmonized_2009to2023_", prodfra_or_pcc8, ".csv"))
active_firm_list = read_parquet("firm_lists/active_firm_list.parquet")
birth_death = read_parquet("firm_lists/firm_birth_death.parquet")
NACE_BR_data <- read_parquet("Ancillary datasets/NACE_BR_data.parquet")
# firmid_keys<-fread("firm_lists/firmid_keys.csv")
# nace_DEFind <- fread("nace_DEFind.conc", colClasses = c("character"))

#Juli?n: Pc8_entry_year
PC8_entry_year <- fread('PC8_years_entry/PC8_prodfra_years_entry.csv', select=c("codes", "code_entry_year"))
unit_collapsed <- fread("Ancillary datasets/unit_collapsed.csv")
unit_collapsed_M2<-unit_collapsed %>% mutate(unit_obv="M2")
unit_collapsed<-unique(rbind(unit_collapsed, unit_collapsed_M2)); rm(unit_collapsed_M2);gc()

## import the prodcom data + harmonize product codes 
start = 2009
end = 2022

product_data = rbindlist(lapply(c(start:end),function(yr){
  # yr<-2018
  print(yr)
  # import product data 
  # filepath = paste0(raw_dir,'prodcom/prodcom',yr,'.csv')
  filepath = paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Raw_data/Data/prodcom/new_2025/eap',yr, '_pp.csv')
  dta_temp = fread(filepath)
  dta_temp[, firmid := as.character(firmid)]
  dta_temp[, pcc8 := as.character(pcc8)]
  dta_temp$firmid<-str_pad(dta_temp$firmid, width = 9, side="left", pad="0")
  # dta_temp[, firmid:=NULL]
  # dta_temp <- merge(dta_temp, firmid_keys, by="firmid", all.x=T) 
  
  #Juli?n: Replace NAs with 0s
  vars<-c("rev", "prod_q", "sold_q")
  for(var in vars){
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
    dta_temp[[var]][is.na(dta_temp[[var]])]<-0
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
  }
  
  #keep product values that aren't dropped in harmonization
  prodfra_var = paste0('prodfra_',yr)
  prodfra_codes =  unique(harmonized_prodfra %>% select(prodfra_var,prodfra_plus) %>% filter(!is.na(get(prodfra_var))))
  dta_temp = merge(dta_temp, prodfra_codes,by.x = prodfra_or_pcc8, by.y = prodfra_var)
  
  #keep firms that employ labor, except if the latest prodcom available exceeds FARE/FICUS coverage
  if(yr<=br_end){
    dta_temp = merge(dta_temp, active_firm_list, by = c('year','firmid'))
  }
  
  # define active as positive rev or quantities, keep only active products
  dta_temp = dta_temp[(rev + sold_q + prod_q)>0]
  
  #Juli?n: Merge information about the year of codes' first appearance in harmonized PC8 tables
  dta_temp = merge(dta_temp, PC8_entry_year, by.x = 'pcc8', by.y = "codes", all.x = T)

  # collapse data to firm-prodfra_plus level
  dta_temp = dta_temp[, .(rev = sum(rev, na.rm = T), 
                          code_entry_year=min(code_entry_year), 
                          sold_q=sum(sold_q, na.rm = T)),
                      by= .(firmid, firmid, prodfra_plus, year, unit)]#, code_entry_year)]

  #generate product-HHI variable
  dta_temp[, total_rev := sum(rev, na.rm = T), by = firmid]
  dta_temp[,HHI := ifelse(total_rev > 0, sum((rev/total_rev)^2),NA), by = firmid]
  dta_temp[, total_rev:=NULL]
  
  ## generate active status marker
  dta_temp[,active := 1]
}))


# Adjust units 
product_data[, unit_collapsed:=paste(unique(unit[!is.na(unit) & unit!="M2"]), collapse=","), by=.(firmid,prodfra_plus)]
product_data<-merge(product_data, unit_collapsed, by.x=c("unit_collapsed", "unit"), by.y=c("unit_collapsed", "unit_obv"), all.x = T)
product_data[, sold_q:=sold_q*factor]

# # Bring in product_data
# # product_data <- readRDS(paste0("product_data_", filter_indicator, "_.RDS"))
# product_data <- setDT(readRDS(paste0("product_data_10_digit_all_prodfra_.RDS")))

if(grepl("Drive", getwd())){
  product_data <- readRDS(paste0("product_data_", filter_indicator, "_.RDS"))
  patent_data <- load_dataset(
    c("3_patent_lvl_patent_dta.parquet", "patent_data/3_patent_lvl_patent_dta.parquet"),
    read_parquet
  ) |> to_dt()
  
  v1 <- unique(as.character(product_data$firmid)) %>% sort() %>% na.omit()
  v2 <- unique(as.character(patent_data$firmid)) %>% sort() %>% na.omit()
  n <- max(length(v1), length(v2))
  v1 <- c(v1, rep(NA, n - length(v1)))
  v2 <- c(v2, rep(NA, n - length(v2)))
  test <- data.table(
    firmid_product_data = as.numeric(v1),
    firmid_patent_data = v2
  )
  
  product_data <- merge(product_data, test, by.x="firmid", by.y="firmid_product_data", all.x=TRUE) %>%
    .[, firmid:=firmid_patent_data] %>% .[ , firmid_patent_data:=NULL]
}


# Create product codes at different aggregation levels
product_data[, `:=`(
  prodcom = substr(prodfra_plus, 1, 8),
  cpa = substr(prodfra_plus, 1, 6),
  NACE_4d_pf = substr(prodfra_plus, 1, 4),
  NACE_2d_pf = substr(prodfra_plus, 1, 2)
)]

# # Bring in DEFind information based on NACE_4d_pf
# product_data <- merge(product_data, nace_DEFind, by.x = "NACE_4d_pf", by.y = "nace", all.x = T) %>%
#   select(firmid, year, cpa_or_pf, rev, everything()) %>%
#   rename(DEFind_pf = DEFind)


setcolorder(product_data, c(
  "firmid", "year", "prodfra_plus", "prodcom", "cpa", "NACE_4d_pf", "NACE_2d_pf", "rev",
  setdiff(names(product_data), c("firmid", "year", "prodfra_plus", "prodcom", "cpa", "NACE_2d_pf", "NACE_4d_pf", "rev"))
))

# Aggregate revenue and sold_q by firmid, year, and the variable stored in cpa_or_pf
# Keep product variables that are more aggregated than cpa_or_pf, but not less aggregated
# Define product aggregation hierarchy
product_vars <- c("prodfra_plus", "prodcom", "cpa", "NACE_4d_pf", "NACE_2d_pf")
agg_levels <- setNames(seq_along(product_vars), product_vars)

product_data_og <- copy(product_data) # Keep a copy of the original data


for (cpa_or_pf in product_vars) {
  
  print(paste0("Aggregating to ", cpa_or_pf, " level"))

  product_data <- copy(product_data_og) # Reset product_data to original data for each iteration

  # Get current aggregation level
  current_level <- agg_levels[[cpa_or_pf]]

  # Keep product variables that are more aggregated (higher index) than cpa_or_pf
  vars_to_keep <- setdiff(product_vars[agg_levels >= current_level], cpa_or_pf)

  # Always keep firmid, year, active, rev, sold_q
  base_vars <- c("firmid", "year", cpa_or_pf, "active")
  vars_to_select <- unique(c(base_vars, vars_to_keep, "rev", "sold_q"))

  # Deflate prodcom revenue data using industry deflators
  product_data <- deflate(product_data, "NACE_4d_pf", "rev", start) %>%
    rename(DEFind_pf = DEFind)

  # Aggregate revenue and quantities sold by product category at the specified aggregation level
  product_data <- product_data[, .(
    rev = sum(rev, na.rm = TRUE),
    sold_q = sum(sold_q, na.rm = TRUE)
  ), by = c("DEFind_pf", base_vars, vars_to_keep)]


  product_data <- growth_creator(
    data = product_data,
    normal_cols = c("rev", "active", "sold_q"),
    n_lag = 1,
    by_vars = c("firmid", vars_to_keep, "DEFind_pf", cpa_or_pf, "year"),
    create_born_died = TRUE,
    data_type = "survey"
  )
  
  # Remove variables that start with "active" or "sold_q", but leave "active", "active_l", "sold_q" and "sold_q_l"
  product_data <- product_data %>% select(-starts_with("active"), -starts_with("sold_q"), active, active_l, sold_q, sold_q_l)

  # Bring in BR NACE info to remove excluded sectors both at firm and product level
  product_data <- merge(product_data, NACE_BR_data, by = c("firmid", "year"), all.x = T)

  # Exclude firms and product lines in industries if the parameter is set above
  product_data <- product_data %>% filter(
    !(NACE_2d_pf %in% ind_to_exclude),
    !(NACE_2d_BR %in% ind_to_exclude)
  )

  product_data[, status := ifelse(born, "born", ifelse(died, "died", "survived"))]

  product_data[, active_year := ifelse(active, year, NA)]
  product_data <- product_data[order(firmid, get(cpa_or_pf), year)]
  product_data[, `:=`(first_year=min(year, na.rm = TRUE),
                      last_year=max(active_year, na.rm = TRUE)),
                 by=firmid]
  product_data[, forward_year := shift(year, type = "lead"), by = .(firmid, get(cpa_or_pf))]
  product_data[, lag_year := shift(year, type = "lag"), by = .(firmid, get(cpa_or_pf))]
  product_data[, gap := fifelse(
    (forward_year == (year + 1) & lag_year == (year - 1)) |
      (forward_year == year | lag_year == year),
    0, 1
  )]
  product_data[is.na(gap), gap := 0]

  # #Juli?n: Change gap years rev_growth=0, rev_reallocation=0 and rev_bar=0
  product_data[, rev_growth := ifelse(gap == 1, 0, rev_growth)]
  product_data[, rev_reallocation := ifelse(gap == 1, 0, rev_reallocation)]
  product_data[, rev_bar := ifelse(gap == 1, 0, rev_bar)]
  product_data[, within_firm_rev_share := rev_bar / sum(rev_bar, na.rm = T),
    by = .(firmid, year)
  ]
  product_data[is.nan(within_firm_rev_share), within_firm_rev_share := 0]
  product_data[, within_economy_rev_share := rev_bar / sum(rev_bar, na.rm = T),
    by = .(year)
  ]

  ## generate product status variables
  ## Juli?n: Add paused status
  product_data[, `:=`(
    first_introduction = year == min(active_year, na.rm = T),
    discontinued = year > max(active_year, na.rm = T) & !active
  ), by = .(firmid, get(cpa_or_pf))]
  product_data[, `:=`(
    reintroduced = !first_introduction & active & !active_l,
    paused = !discontinued & active_l & !active,
    incumbent = active_l & active
  )]

  ## export the data
  product_data = product_data %>%
    arrange(firmid, cpa_or_pf, year) %>%
    select(
      "firmid", "year", "DEFind_BR", "NACE_2d_BR", "NACE_BR", "DEFind_pf", setdiff(vars_to_select, c("active")), "status", "gap",
      "first_year", "last_year", "first_introduction", "discontinued",
      "reintroduced", "paused", "incumbent", everything()
    )
  
  dir.create(paste0("2_product_data/", cpa_or_pf), showWarnings = FALSE, recursive = TRUE)
  write_parquet(product_data, paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet"))

  print(paste0("Saved product_data/", cpa_or_pf, "/2_product_yr_lvl_dta.parquet"))
}
