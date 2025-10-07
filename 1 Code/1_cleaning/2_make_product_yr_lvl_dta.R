# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))
dummy <- FALSE

#3) extract product level data from Prodcom  ----------------------------------------------

# We can:
# 1. Use 8-digit (prodcom) or 10-digit (prodfra) product codes. Based on this, we bring in 8 or 10 digit product code harmonization to create the Prodcom database
# 2. EAP has three different categorizations for prodfra codes: prodfra nested in prodcon, prodfra outside of prodcom or prodfra not nested, but with Prodcom information.
# We can decide to use only prodfra codes nested in prodcom or use all prodfra codes
# 3. Exclude certain industries (e.g. public utilities industries)
# We do the above by setting parameters below. The name of the files we will save changes depending on those parameters

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
parameters(prodfra_or_pcc8, only_prodfra_in_prodcom)

## import supplementary data
harmonized_prodfra = fread(paste0("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/product_harmonization_output/harmonized codes/prodfra_harmonized_2009to2023_", prodfra_or_pcc8, ".csv"))
active_firm_list = readRDS("active_firm_list.rds")
birth_death = readRDS("firm_birth_death.rds")
NACE_BR_data <- readRDS("NACE_BR_data.rds")
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c("character"))

#Juli?n: Pc8_entry_year
PC8_entry_year <- fread('PC8_years_entry/PC8_prodfra_years_entry.csv', select=c("codes", "code_entry_year"))
unit_collapsed <- fread("~/Reallocation/6 Publish/1 Code/Ancillary datasets/unit_collapsed.csv")
unit_collapsed_M2<-unit_collapsed %>% mutate(unit_obv="M2")
unit_collapsed<-unique(rbind(unit_collapsed, unit_collapsed_M2)); rm(unit_collapsed_M2);gc()

## import the prodcom data + harmonize product codes 
start = 2009
end = 2023

product_data = rbindlist(lapply(c(start:end),function(yr){
  # yr<-2022
  print(yr)
  # import product data 
  # filepath = paste0(raw_dir,'prodcom/prodcom',yr,'.csv')
  filepath = paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Raw_data/Data/prodcom/new_2025/eap',yr, '_pp.csv')
  dta_temp = fread(filepath)
  dta_temp[, firmid := as.character(firmid)]
  dta_temp[, pcc8 := as.character(pcc8)]
  dta_temp$firmid<-str_pad(dta_temp$firmid, width = 9, side="left", pad="0")
  
  #Juli?n: Replace NAs with 0s
  vars<-c("rev", "prod_q", "sold_q")
  for(var in vars){
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
    dta_temp[[var]][is.na(dta_temp[[var]])]<-0
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
  }
  
  #keep product values that aren't dropped in harmonization
  prodfra_var = paste0('prodfra_',yr)
  prodfra_codes =  unique(harmonized_prodfra %>% select(prodfra_var,prodfra_plus) %>% filter(!is.na(prodfra_var)))
  dta_temp = merge(dta_temp, prodfra_codes,by.x = prodfra_or_pcc8, by.y = prodfra_var)
  
  #keep firms that employ labor
  dta_temp = merge(dta_temp, active_firm_list, by = c('year','firmid'))
  
  # define active as positive rev or quantities, keep only active products
  dta_temp = dta_temp[(rev + sold_q + prod_q)>0]
  
  #Juli?n: Merge information about the year of codes' first appearance in harmonized PC8 tables
  dta_temp = merge(dta_temp, PC8_entry_year, by.x = 'pcc8', by.y = "codes", all.x = T)

  # collapse data to firm-prodfra_plus level
  dta_temp = dta_temp[, .(rev = sum(rev, na.rm = T), 
                          code_entry_year=min(code_entry_year), 
                          sold_q=sum(sold_q, na.rm = T)),
                      by= .(firmid, prodfra_plus, year, unit)]#, code_entry_year)]

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

# # Bring in BR NACE information
# product_data<-merge(product_data, NACE_BR_data, by=c("firmid", "year"), all.x=T)

# # Create alternative NACE information: Take the first 4 digits of the prodfra_plus code
# product_data<-product_data %>% mutate(NACE_prodfra=substr(prodfra_plus, 1, 4))

# # Juli?n: Exclude firms and product lines in excluded industries if the parameter is set above
# exclude_industries<-T
# parameters(prodfra_or_pcc8, only_prodfra_in_prodcom, exclude_industries)
# # These are industries that are not continuously covered by prodcom (specifically, they were not covered in 2009)
# if(exclude_industries){
#   ind_to_exclude <- c(19, 35,36,37,38,39,46) 
  
#   # Exclude firms and product lines in industries if the parameter is set above
#   product_data<-product_data %>% filter(!(substr(NACE, 1, 2) %in% ind_to_exclude),
#                                         !(NACE_2d %in% ind_to_exclude))
# }

# # Deflate prodcom revenue data using industry deflators
# # if(!dummy){
# #   product_data<-deflate(product_data, "NACE", "rev", start)
# # }
# # make_summary_stats(product_data, c("rev", "HHI"), "full_sample", "product_data.xlsx")
# # description("product_data.xlsx", "Summary statistics on revenue and within-firm concentration using PRODCOM data.\n")
# # make_summary_stats(product_data, c("rev", "HHI"), "DEFind", "product_data_industry.xlsx")
# # description("product_data_industry.xlsx", "Summary statistics on revenue and within-firm concentration using PRODCOM data, per NACE industry.\n")

# ## generate lags
# normal_cols = c('active', 'rev')
# lag_cols = paste0(normal_cols, '_l')
# product_data_l = product_data[year<end, ] %>% mutate(year = year + 1) %>%
#   select(firmid,year, prodfra_plus, normal_cols)
# colnames(product_data_l)[names(product_data_l) %in% normal_cols] = lag_cols

# product_data = merge(product_data, product_data_l, by=c("firmid", "year", "prodfra_plus"), all = T)

# # Juli?n: Fix NAs in DEFind 
# nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
# product_data$DEFind<-NULL
# product_data <- merge(product_data, nace_DEFind, by.x = "NACE_prodfra", by.y = "nace", all.x = T) %>% select(firmid, year, prodfra_plus, rev, everything())
# #Juli?n: add code_entry_year
# columns = c('firmid', 'prodfra_plus', 'year', "code_entry_year")
# for (i in seq_along(normal_cols)){
#   product_data[is.na(get(normal_cols[i])), normal_cols[i]:=0]
#   product_data[is.na(get(lag_cols[i])), lag_cols[i]:=0]
#   columns = c(columns, lag_cols[i], normal_cols[i])
# }
# product_data[,`:=`(active = active==1, active_l = active_l==1)]

# ## add in birth/death status,
# product_data = merge(product_data,birth_death, by=c('firmid'))
# product_data[, `:=`(born = !is.na(birth_year) & birth_year == year,
#                     died = !is.na(death_year) & death_year < year)]
# product_data[, status:= ifelse(born, 'born', ifelse(died, 'died', 'survived'))]

# ## fix first / last year of data 
# product_data[born == T, lag_cols:= 0]
# product_data[died == T, normal_cols := 0] 

# setorder(product_data, firmid, prodfra_plus, year)
# product_data<-product_data %>% group_by(firmid) %>% mutate( first_year=min(year), last_year=max(year) )
# product_data <- product_data %>% group_by(firmid, prodfra_plus) %>% mutate(forward_year=dplyr::lead(year), lag_year=dplyr::lag(year))
# #Juli?n: Create prodcom coverage gap variables
# product_data <- product_data %>% group_by(firmid, prodfra_plus) %>% 
#   mutate(gap=ifelse(forward_year==(year+1) & lag_year==(year-1), 0, ifelse(forward_year==year | lag_year==year, 0, 1)))
# product_data <- product_data %>% mutate(gap=ifelse(is.na(gap),0,gap))

# product_data <- product_data %>% select(firmid, prodfra_plus, year, first_year, last_year, gap, forward_year, lag_year, everything())
# product_data<-as.data.table(product_data)
# product_data[, rev_bar := .5*(rev + rev_l)]
# #Juli?n: delete absolute values to have actual revenue growth (not reallocation)
# product_data[, rev_growth := ifelse(rev_bar != 0, (rev - rev_l)/rev_bar, 0)]
# product_data[, rev_reallocation := abs(ifelse(rev_bar != 0, abs(rev - rev_l)/rev_bar, 0))]
# #Juli?n: Change gap years rev_growth=0, rev_reallocation=0 and rev_bar=0
# product_data[, rev_growth := ifelse(gap==1,0, rev_growth)]
# product_data[, rev_reallocation := ifelse(gap==1,0, rev_reallocation)]
# product_data[, rev_bar := ifelse(gap==1,0, rev_bar)]
# product_data[, within_firm_rev_share :=  rev_bar/ sum(rev_bar, na.rm = T),
#              by = .(firmid,year)]
# product_data[is.nan(within_firm_rev_share), within_firm_rev_share := 0]
# product_data[, within_economy_rev_share :=  rev_bar/ sum(rev_bar, na.rm = T),
#              by = .(year)]

# ## generate product status variables 
# ## Juli?n: Add paused status

# product_data[, active_year := ifelse(active, year, NA)]
# product_data[, `:=`(first_introduction = year == min(active_year, na.rm = T),
#                     discontinued = year>max(active_year, na.rm = T) & !active), by= .(firmid, prodfra_plus)]
# product_data[, `:=`(reintroduced = !first_introduction & active & !active_l,
#                     paused= !discontinued & active_l & !active,
#                     incumbent = active_l & active)]

# ## export the data 
# product_data = product_data %>% arrange(firmid, prodfra_plus, year) %>% select(columns, everything())

# # make_summary_stats(product_data, c("rev", "rev_l", "gap", "HHI", "rev_growth"), "year", "product_data2_year")
# # description("product_data2_DEFind.xlsx", 
# #             "Summary statistics on revenue and within-firm concentration using PRODCOM data excluding utilities, per NACE 2 digit codes. \n")

# saveRDS(product_data, paste0("product_level_growth_", filter_indicator,  "_.RDS"))

# #4cpa) clean product level data from Prodcom  ----------------------------------------------

# # Set parameters for prodfra-pcc8 and excluded industries
# prodfra_or_pcc8<-"prodfra" # Although this is the cpa aggregation, this should be prodfra since we are bringing the prodfra (not pcc8) data from step 3
# only_prodfra_in_prodcom<-FALSE
# parameters(prodfra_or_pcc8, only_prodfra_in_prodcom)

# # Set start and end years
# start = 2009
# end = 2022

# # Bring in product_data
# # product_data <- readRDS(paste0("product_data_", filter_indicator, "_.RDS"))
product_data <- setDT(readRDS(paste0("product_data_10_digit_all_prodfra_.RDS")))

# Create product codes at different aggregation levels
product_data[, `:=`(
  prodcom = substr(prodfra_plus, 1, 8),
  cpa = substr(prodfra_plus, 1, 6),
  NACE_4d_pf = substr(prodfra_plus, 1, 4),
  NACE_2d_pf = substr(prodfra_plus, 1, 2)
)]

# Bring in DEFind information based on NACE_4d_pf
product_data <- merge(product_data, nace_DEFind, by.x = "NACE_4d_pf", by.y = "nace", all.x = T) %>% select(firmid, year, cpa_or_pf, rev, everything())


setcolorder(product_data, c(
  "firmid", "year", "prodfra_plus", "prodcom", "cpa", "NACE_4d_pf", "NACE_2d_pf", "rev",
  setdiff(names(product_data), c("firmid", "year", "prodfra_plus", "prodcom", "cpa", "NACE_2d_pf", "NACE_4d_pf", "rev"))
))

# Aggregate revenue and sold_q by firmid, year, and the variable stored in cpa_or_pf
# Keep product variables that are more aggregated than cpa_or_pf, but not less aggregated
# Define product aggregation hierarchy
product_vars <- c("prodfra_plus", "prodcom", "cpa", "NACE_4d_pf", "NACE_2d_pf")
agg_levels <- setNames(seq_along(product_vars), product_vars)


for (cpa_or_pf in product_vars) {

  print(paste0("Aggregating to ", cpa_or_pf, " level"))

  # Get current aggregation level
  current_level <- agg_levels[[cpa_or_pf]]

  # Keep product variables that are more aggregated (higher index) than cpa_or_pf
  vars_to_keep <- setdiff(product_vars[agg_levels >= current_level], cpa_or_pf)

  # Always keep firmid, year, active, rev, sold_q
  base_vars <- c("firmid", "year", cpa_or_pf, "active")
  vars_to_select <- unique(c(base_vars, vars_to_keep, "rev", "sold_q"))

  # Aggregate revenue and quantities sold by product category at the specified aggregation level
  product_data <- product_data[, .(
    rev = sum(rev, na.rm = TRUE),
    sold_q = sum(sold_q, na.rm = TRUE)
  ), by = c(base_vars, vars_to_keep)]

  # Bring in BR NACE info to remove excluded sectors both at firm and product level
  product_data <- merge(product_data, NACE_BR_data, by = c("firmid", "year"), all.x = T)

  # Exclude firms and product lines in industries if the parameter is set above
  product_data <- product_data %>% filter(
    !(substr(NACE_2d_pf, 1, 2) %in% ind_to_exclude),
    !(NACE_2d %in% ind_to_exclude)
  )

  # Deflate prodcom revenue data using industry deflators
  # product_data <- deflate(product_data, "NACE", "rev", start)

  product_data_temp <- growth_creator(
    data = product_data,
    normal_cols = c("rev", "active"),
    n_lag = 1,
    by_vars = c("firmid", cpa_or_pf, "year"),
    create_born_died = TRUE
  )

  product_data <- merge(product_data, product_data_temp, by = c("firmid", cpa_or_pf, "year"), all = TRUE)
  rm(product_data_temp); gc()

  # ## generate lags
  # normal_cols = c('active', 'rev')
  # lag_cols = paste0(normal_cols, '_l')
  # product_data_l = product_data[year<end, ] %>% mutate(year = year + 1) %>%
  #   select(firmid,year, cpa, normal_cols)
  # colnames(product_data_l)[names(product_data_l) %in% normal_cols] = lag_cols

  # product_data = merge(product_data, product_data_l, by=c("firmid", "year", "cpa"), all = T)

  # # Fix NAs in NACE
  # product_data<-product_data %>% mutate(NACE=substr(cpa, 1, 4))

  # Juli?n: Fix NAs in DEFind

  # #Juli?n: add code_entry_year
  # columns = c('firmid', 'cpa', 'year')
  # for (i in seq_along(normal_cols)){
  #   product_data[is.na(get(normal_cols[i])), normal_cols[i]:=0]
  #   product_data[is.na(get(lag_cols[i])), lag_cols[i]:=0]
  #   columns = c(columns, lag_cols[i], normal_cols[i])
  # }
  # product_data[,`:=`(active = active==1, active_l = active_l==1)]

  # ## add in birth/death status,
  # product_data = merge(product_data,birth_death, by=c('firmid'))
  # product_data[, `:=`(born = !is.na(birth_year) & birth_year == year,
  #                     died = !is.na(death_year) & death_year < year)]
  # product_data[, status:= ifelse(born, 'born', ifelse(died, 'died', 'survived'))]

  # ## fix first / last year of data
  # product_data[born == T, lag_cols:= 0]
  # product_data[died == T, normal_cols := 0]

  # setorder(product_data, firmid, cpa, year)
  # product_data<-product_data %>% group_by(firmid) %>% mutate( first_year=min(year), last_year=max(year) )
  product_data <- product_data[order(firmid, get(cpa_or_pf), year)]
  product_data[, forward_year := shift(year, type = "lead"), by = .(firmid, get(cpa_or_pf))]
  product_data[, lag_year := shift(year, type = "lag"), by = .(firmid, get(cpa_or_pf))]
  product_data[, gap := fifelse(
    (forward_year == (year + 1) & lag_year == (year - 1)) |
      (forward_year == year | lag_year == year),
    0, 1
  )]
  product_data[is.na(gap), gap := 0]

  # product_data <- product_data %>% select(firmid, cpa_or_pf, year, first_year, last_year, gap, forward_year, lag_year, everything())
  # product_data<-as.data.table(product_data)
  # product_data[, rev_bar := .5*(rev + rev_l)]
  # #Juli?n: delete absolute values to have actual revenue growth (not reallocation)
  # product_data[, rev_growth := ifelse(rev_bar != 0, (rev - rev_l)/rev_bar, 0)]
  # product_data[, rev_reallocation := abs(ifelse(rev_bar != 0, abs(rev - rev_l)/rev_bar, 0))]
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

  product_data[, active_year := ifelse(active, year, NA)]
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
    select("firmid", "year", vars_to_select, everything())

  # make_summary_stats(product_data, c("rev", "rev_l", "gap", "HHI", "rev_growth"), "year", "product_data2_year")
  # description("product_data2_DEFind.xlsx",
  #             "Summary statistics on revenue and within-firm concentration using PRODCOM data excluding utilities, per NACE 2 digit codes. \n")

  # Up to here filter_indicator would be "10_digit_all_prodfra_exclude_industries", but this is not actually 10_digit, this is 6_digit
  # prodfra_or_pcc8<-"cpa"
  # parameters(prodfra_or_pcc8, only_prodfra_in_prodcom, exclude_industries)
  write_parquet(product_data, paste0("2_product_yr_lvl_dta_", cpa_or_pf, ".parquet"))

  print(paste0("Saved 2_product_yr_lvl_dta_", cpa_or_pf, ".parquet"))
}
