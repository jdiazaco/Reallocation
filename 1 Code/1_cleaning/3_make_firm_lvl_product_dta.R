# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

#5) prepare firm level decompositions of product data ------------------------------------
product_data <- read_parquet(paste0("2_product_yr_lvl_dta_", cpa_or_pf, ".parquet"))

##5.1) calculate extensive margin product growth/reallocation rate -------------------
firm_data_ex = product_data[, .(products_l = sum(active_l),
                                products = sum(active),
                                prod_added = sum(active & !active_l),
                                prod_removed = sum(!active & active_l)),
                            by = .(firmid, year)]
firm_data_ex[, `:=`(entrance_growth = prod_added / products,
                    exit_growth = prod_removed / products_l,
                    entrance_share = products/sum(products),
                    exit_share = products_l / sum(products_l)),
             by =year]

firm_data_ex[status == 'born', exit_growth:=0]
firm_data_ex[status == 'died', entrance_growth:=0]
firm_data_ex[,status:= NULL]
firm_data_ex[, `:=`(entrance_growth_weighted  = entrance_growth * entrance_share,
                    exit_growth_weighted      = exit_growth     * exit_share)] 

vars<-c("entrance", "exit")
for (var in vars){
  firm_data_ex[[paste0(var, "_reallocation")]]<-firm_data_ex[[paste0(var, "_growth")]]
  firm_data_ex[[paste0(var, "_reallocation_weighted")]]<-firm_data_ex[[paste0(var, "_growth_weighted")]]
}

firm_data_ex = firm_data_ex %>% select(-c('products_l', 'products', 'prod_added', 'prod_removed'))
saveRDS(firm_data_ex, 'firm_level_ex_margin.rds')

# make_summary_stats(firm_data_ex, c("entrance_growth", "exit_growth", "entrance_share", "exit_share"), "year", "ex_margin_year.xls")
# description("ex_margin_year.csv", "Summary statistics for average entrance growth, exit growth, entrance share and exit share per year.\n")
# make_summary_stats(firm_data_ex, c("entrance_growth", "exit_growth", "entrance_share", "exit_share"), "full_sample", "ex_margin.xls")
# description("ex_margin.csv", "Summary statistics for average entrance growth, exit growth, entrance share and exit share.\n")

##5.2) calculate intensive margin product reallocation rate -------------------
product_data = product_data %>% select(firmid, prodfra_plus, year, rev_l, rev, rev_bar,
                                       rev_reallocation, within_firm_rev_share, everything())
firm_data_in = product_data[, .(rev=sum(rev),
                                rev_l=sum(rev_l),
                                rev_bar = .5*(sum(rev_bar)),#Why times 0.5?
                                rev_reallocation = sum(rev_reallocation*within_firm_rev_share)),
                            by = .(firmid, year,birth_year, death_year)] #Here aggregating by firm and year, deleting product-lines.
firm_data_in[, rev_share :=  rev_bar /sum(rev_bar), by = year] #Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
firm_data_in[, `:=`(rev_reallocation_weighted = rev_reallocation * rev_share, rev_bar =NULL)]
saveRDS(firm_data_in, 'firm_level_reallocation_in_margin.rds')

# make_summary_stats(firm_data_in, c("rev_reallocation"), "year", "in_margin_reallocation_year.xls")
# description("in_margin_reallocation_year.xls", "Summary statistics for average revenue reallocation rate. Source: Prodcom.\n")

##5.3) calculate intensive margin product growth rate -------------------

product_data = product_data %>% select(firmid, prodfra_plus, year, rev_l, rev, rev_bar,
                                       rev_growth, within_firm_rev_share, everything())
firm_data_in = product_data[, .(rev=sum(rev),
                                rev_l=sum(rev_l),
                                rev_bar = .5*(sum(rev_bar)),#Why times 0.5?
                                rev_growth = sum(rev_growth*within_firm_rev_share)),
                            by = .(firmid, year,birth_year, death_year)] #Here aggregating by firm and year, deleting product-lines.
firm_data_in[, rev_share :=  rev_bar /sum(rev_bar), by = year] #Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
firm_data_in[, `:=`(rev_growth_weighted = rev_growth * rev_share, rev_bar =NULL)]
saveRDS(firm_data_in, 'firm_level_growth_in_margin.rds')

# make_summary_stats(firm_data_in, c("rev_growth"), "year", "in_margin_growth_year.xls")
# description("in_margin_growth_year.xls", "Summary statistics for average groeth reallocation rate, per year. Source: Prodcom.\n")


#6) generate final firm Level dataset - growth and reallocation --------------------------------------------------------
# rm(list = ls())
# gc()
# setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/')

## import data
sbs_data = readRDS('sbs_br_combined_cleaned.rds')
firm_data_ex = readRDS('firm_level_ex_margin.rds')
firm_data_in_reallocation = readRDS('firm_level_reallocation_in_margin.rds')
firm_data_in_growth = readRDS('firm_level_growth_in_margin.rds')


## merge together 
product_data = merge(firm_data_in_reallocation,firm_data_ex, all = T) %>% mutate(in_prodcom = T)
combined_data = merge(sbs_data, product_data, by = c('firmid', 'year', 'birth_year', 'death_year'), all = T)
combined_data[is.na(in_prodcom ), in_prodcom := F]
combined_data[,full_sample:= 1]
saveRDS(combined_data, 'combined_sbs_br_prodcom_data.rds')

## merge together 
product_data = merge(firm_data_in_growth,firm_data_ex, all = T) %>% mutate(in_prodcom = T)
combined_data = merge(sbs_data, product_data, by = c('firmid', 'year', 'birth_year', 'death_year'), all = T)
combined_data[is.na(in_prodcom ), in_prodcom := F]
combined_data[,full_sample:= 1]
saveRDS(combined_data, 'combined_sbs_br_prodcom_data_growth.rds')

#Bring in necessary firm and product information
firm_data_select<-readRDS("sbs_br_data_prodcom_firms.RDS") %>% filter(year>2009)
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
product_data<-readRDS(paste0("product_level_growth_", filter_indicator, "_.RDS"))

#Set the number of digits of the different product aggregation levels

# Check only prodcom firms are present in the firm_data_select sample
if(length(setdiff(unique(firm_data_select$firmid), unique(product_data$firmid)))!=0){
  stop("Firm data does not contain only prodcom firms. Check modules a and c and come back")
}

# 1) Identification and tracking of core and new product categories across aggregation levels  --------------------

# This script analyzes product reallocation and firm dynamics by identifying 
# and tracking core products across multiple levels of product aggregation 
# (1, 2, 4, 6 digits for CPA; optionally up to 10 for ProdFra+). 
# It:
#   - Loads firm and product-level data
#   - Verifies consistency of input datasets
#   - Defines and tracks core product categories within firms
#   - Calculates product entries and exits by aggregation level
#   - Outputs datasets augmented with core product and transition metrics


# Ensure product categories are exhaustive and mutually exclusive
# Each row should fall into exactly one category
if (nrow(product_data[, .(first_introduction, reintroduced, discontinued, incumbent, paused)]) != nrow(product_data)) {
  stop("Product categories are not exhaustive and/or mutually exclusive.")
}

# Extract relevant code substrings based on classification system
if (cpa_or_pf == "cpa") {
  product_data[, NACE_2d_pf := substr(cpa, 1, 2)]
} else if (cpa_or_pf == "prodfra_plus") {
  product_data[, `:=`(
    prodcom = substr(prodfra_plus, 1, 8),
    cpa = substr(prodfra_plus, 1, 6),
    NACE_2d_pf = substr(prodfra_plus, 1, 2)
  )]
} else if( cpa_or_pf == "prodcom"){
  product_data[, `:=`(
    cpa = substr(prodcom, 1, 6),
    NACE_2d_pf = substr(prodcom, 1, 2)
  )]
} else {
  stop("Invalid classification system specified.")
}

# Define a function to identify the core products within each firm, by category aggregation level
core_switch_product <- function(data, n_digits) {
  pf <- paste0("pf_", n_digits)
  core <- paste0("core_", pf)
  switch <- paste0("switch_", pf)
  n_core <- paste0("n_core_", pf)
  share_core <- paste0("share_core_", pf)
  share_runup <- paste0("share_runup_", pf)
  
  setDT(data)
  
  # Define category based on number of digits
  data[[pf]] <- if (n_digits == 1) substr(data$DEFind, 1, 1) else substr(data[[cpa_or_pf]], 1, n_digits)
  
  # Aggregate revenue by firm-year-category
  data <- data[, .(rev = sum(rev, na.rm = TRUE)), by = .(firmid, year, cat = get(pf))]
  
  # Compute revenue share within each firm-year
  data[, share := rev / sum(rev, na.rm = TRUE), by = .(firmid, year)]
  
  # Rank products by revenue and extract top 2 categories
  data <- data[order(firmid, year, -rev)]
  data[, rank := frank(-rev, ties.method = "first"), by = .(firmid, year)]
  data[, `:=`(
    share_core = ifelse(rank == 1, share, NA_real_),
    share_runup = ifelse(rank == 2, share, NA_real_)
  ), by = .(firmid, year)]
  
  data[, `:=`(
    share_core = max(share_core, na.rm = TRUE),
    share_runup = max(share_runup, na.rm = TRUE)
  ), by = .(firmid, year)]
  
  # Keep only top ranked categories
  data <- data[ , .SD[rev == max(rev, na.rm = TRUE) & rev > 0], by = .(firmid, year)]
  
  # Summarize core categories
  data <- data[, .(
    n_core = uniqueN(cat),
    cat = paste(unique(cat), collapse = ", "),
    share_core = unique(share_core),
    share_runup = unique(share_runup)
  ), by = .(firmid, year)]
  
  # Replace infinite values
  data[, share_runup := ifelse(is.infinite(share_runup), NA, share_runup)]
  
  setorder(data, firmid, year)
  
  # Flag switching core category
  data<-data[, switch:=!(ifelse(is.na(dplyr::lag(cat, 1)), NA,  str_detect(dplyr::lag(cat), cat))), by=.(firmid)] # Flag if there has been a switch in category  
  
  # Rename output columns  
  setnames(data, c("cat", "switch", "n_core", "share_core", "share_runup"), c(core, switch, n_core, share_core, share_runup)) # Adjust names
  
  return(data)
}


# Loop through aggregation levels to generate core category information for each level
levels_agg <- digits[digits != 0]
product_core <- NULL

for (i in seq_along(levels_agg)) {
  print(levels_agg[i])
  data <- core_switch_product(product_data, levels_agg[i])
  
  if (i == 1) {
    product_core <- data
  } else {
    if (nrow(product_core) != nrow(data)) stop("Mismatch in row count across aggregation levels.")
    product_core <- merge(product_core, data, by = c("firmid", "year"), all.x = TRUE)
  }
}


# Compute lagged values for core metrics for comparison across years and merge with original product data
setorder(product_core, firmid, year)
share_cores<-names(product_core)[grepl("^core|^share_core|^share_runup", names(product_core))]
for(i in share_cores){
  product_core<-product_core[, (paste0("lag_", i)):=dplyr::lag(get(i), 1), by=firmid]
}
product_data<-merge(product_data, product_core, by=c("firmid", "year"))
setorder(product_data, firmid, year, -rev)

# Compute Entry and Exit Flags by Aggregation Level
digits_inv <- sort(digits, decreasing = TRUE)

# Loop through each level to create entry and exit variables dynamically
for (i in seq_along(digits_inv)) {
  
  # i<-1
  digit <- digits_inv[i]
  
  # Define previous levels to exclude in the condition for the current level
  if(i==1){
    prev_digits<-NULL
  }else{
    prev_digits <- digits_inv[1:(i - 1)]
  }
  
  # Entry (new_*) conditions based on the digit level
  product_data <- product_data[, paste0("new_", digit) := 
                                 first_introduction &
                                 # Dynamically match conditions based on the digit level
                                 (if (digit == 10) prodfra_plus==lag_core_pf_10
                                  else if (digit == 8) prodcom==lag_core_pf_8 
                                  else if (digit == 6) cpa == lag_core_pf_6
                                  else if (digit == 4) NACE == lag_core_pf_4
                                  else if (digit == 2) NACE_2d_pf == lag_core_pf_2
                                  else if (digit == 1) substr(DEFind, 1, 1) == lag_core_pf_1
                                  else if (digit == 0) substr(DEFind, 1, 1) != lag_core_pf_1) &  # for digit 0 or any other cases
                                 
                                 # Ensure no overlap with previous levels
                                 !(if (length(prev_digits) > 0) 
                                   Reduce(`|`, lapply(prev_digits, function(d) get(paste0("new_", d))))
                                   else FALSE)]
  
  # Exit (exit_*) conditions based on the digit level
  product_data <- product_data[, paste0("exit_", digit) := 
                                 discontinued &
                                 # Dynamically match conditions based on the digit level
                                 (if (digit == 10) prodfra_plus==lag_core_pf_10
                                  else if (digit == 8) prodcom==lag_core_pf_8 
                                  else if (digit == 6) cpa == lag_core_pf_6
                                  else if (digit == 4) NACE == lag_core_pf_4
                                  else if (digit == 2) NACE_2d_pf == lag_core_pf_2
                                  else if (digit == 1) substr(DEFind, 1, 1) == lag_core_pf_1
                                  else if (digit == 0) substr(DEFind, 1, 1) != lag_core_pf_1) &  # for digit 0 or any other cases
                                 
                                 # Ensure no overlap with previous levels
                                 # Ensure no overlap with previous levels
                                 !(if (length(prev_digits) > 0) 
                                   Reduce(`|`, lapply(prev_digits, function(d) get(paste0("exit_", d))))
                                   else FALSE)]
}



# Reorder and filter columns
product_data <- product_data %>%
  select(firmid, year, cpa_or_pf, rev, first_introduction, starts_with("new_"),
         discontinued, starts_with("exit_"), starts_with("core_"), starts_with("lag_"), everything())

# Save results
write_rds(product_data, paste0("product_level_growth_", filter_indicator, "_new_core_analysis", ext, ".RDS"))
write_rds(product_core, paste0("product_core", ext, ".RDS"))


# 2) Data prep for different measures of product entry and exit  --------------------
# 2.1) Create measures of number of products by product status and entry/exit by pre post exit/entry status -----

###############################################################################
#
# Description:
# This script processes product-level data to construct a firm-year panel 
# summarizing product dynamics. It aggregates variables related to product 
# entry, exit, and status, and creates indicators for key product transitions 
# such as reintroduction, discontinuation, and pausing.
#
# Main steps:
# 1. Load firm-year-product-level data and core firm-year metadata.
# 2. Aggregate product-level variables to the firm-year level.
# 3. Handle edge cases:
#    - Replace values in a firm's first observed year with NAs to avoid spurious entries.
#    - Drop firm-year observations with zero revenue (i.e., likely not active).
# 4. Construct counts of products by status: new, reintroduced, paused, discontinued.
# 5. Create indicators for product dynamics:
#    - Entry following exit, exit following entry
#    - Entry or exit in previous or future years
#    - Entry and exit occurring in the same year
# 6. Remove false negatives due to short firm histories (e.g., near first/last year).
# 7. Finalize the dataset with cleaned and labeled columns, including dummy variables
#    for product creation (prod_creat) and destruction (prod_destr).
#
# Note: The script uses a combination of data.table for aggregation and dplyr 
# for sequential lag/lead calculations and filtering.
###############################################################################


product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_new_core_analysis", ext, ".RDS"))
product_core<-readRDS(paste0("product_core", ext, ".RDS"))

# Define working variables
vars<-c("first_introduction", names(product_data)[grep("^new", names(product_data))], 
        # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2",
        names(product_data)[grep("^exit", names(product_data))], 
        "reintroduced", "discontinued", "incumbent", "paused", "rev")
unique_vars<-c("first_year", "last_year")

#Aggregate variables at the firm-year level from the firm-year-product level
product_summary_sums<-product_data[, lapply(.SD, sum, na.rm=T), .SDcols=vars, by=.(firmid, year)]
rev_growth<-growth_creator(product_summary_sums, "rev", 1) %>% select(firmid, year, rev_l, rev_bar, rev_growth)
product_summary_sums<-merge(product_summary_sums, rev_growth, by=c("firmid", "year"), all.x = T)
product_summary_years<-product_data[, lapply(.SD, unique, na.rm=T), .SDcols=unique_vars, by=.(firmid, year)]
product_summary<-merge(product_summary_sums, product_summary_years, by=c("firmid", "year"), all.x=T)
rm(product_summary_sums, product_summary_years, rev_growth); gc()

product_summary<- merge(product_summary, product_core, by=c("firmid", "year"), all.x = T)

setDT(product_summary)

# Define number of products in a year
product_summary[, `:=`(number_of_products=first_introduction+reintroduced+incumbent)]
# product_summary<-product_summary[, number_of_years:=n_distinct(year), by=firmid]

# Set firm's first year observations to NAs, since we don't know actual product status that year (everything is spuriously first_introduction)
# We still leave the observations, because the information on number of products is still useful
new<-grep("^new", names(product_summary), value=T)
switch<-grep("^switch", names(product_summary), value=T)
product_summary<-product_summary[year==first_year,`:=`(number_of_products=first_introduction+reintroduced+incumbent,
                                                       first_introduction=NA,
                                                       paused=NA,
                                                       reintroduced=NA,
                                                       incumbent=NA,
                                                       discontinued=NA)  ]
product_summary<-product_summary[year==first_year, (new):=NA  ]
product_summary<-product_summary[year==first_year, (switch):=NA  ]

# We drop last years observations by taking off all observations that do not have revenue in that year
# since everything is spuriously discontinued and we don't actually know the product composition that year
product_summary<-product_summary[rev!=0]

# Number of products by product status
product_summary[, `:=`(new_products=first_introduction,
                       paused_products=paused,
                       reintroduced_products=reintroduced,
                       destroyed_products=discontinued,
                       number_of_products=ifelse(year!=first_year, first_introduction+reintroduced+incumbent, number_of_products ))]

# Create measures of entry post pre exit and exit post pre entry
product_summary <- product_summary %>% group_by(firmid) %>% mutate(entry_year=ifelse(new_products!=0, year, NA),
                                                                   exit_year=ifelse(destroyed_products!=0, year, NA)) %>% 
  mutate(exit_post_entry=case_when(is.na(entry_year) ~ NA_real_,
                                   # destroyed_products!=0 ~ 1,
                                   dplyr::lead(destroyed_products, 1)!=0 ~ 1,
                                   dplyr::lead(destroyed_products, 2)!=0 ~ 1,
                                   # dplyr::lead(destroyed_products, 3)!=0 ~ 1,
                                   # dplyr::lead(destroyed_products, 4)!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(entry_post_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   # new_products!=0 ~ 1,
                                   dplyr::lead(new_products, 1)!=0 ~ 1,
                                   dplyr::lead(new_products, 2)!=0 ~ 1,
                                   # dplyr::lead(new_products, 3)!=0 ~ 1,
                                   # dplyr::lead(new_products, 4)!=0 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(exit_pre_entry=case_when(is.na(entry_year) ~ NA_real_,
                                  # destroyed_products!=0 ~ 1,
                                  dplyr::lag(destroyed_products, 1)!=0 ~ 1,
                                  dplyr::lag(destroyed_products, 2)!=0 ~ 1,
                                  # dplyr::lag(destroyed_products, 3)!=0 ~ 1,
                                  # dplyr::lag(destroyed_products, 4)!=0 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(entry_pre_exit=case_when(is.na(exit_year) ~ NA_real_,
                                  # new_products!=0 ~ 1,
                                  dplyr::lag(new_products, 1)!=0 ~ 1,
                                  dplyr::lag(new_products, 2)!=0 ~ 1,
                                  # dplyr::lag(new_products, 3)!=0 ~ 1,
                                  # dplyr::lag(new_products, 4)!=0 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(exit_with_entry=case_when(is.na(entry_year) ~ NA_real_,
                                   destroyed_products!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(entry_with_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   new_products!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  
  ungroup()

# Delete false negatives, stemming from pre variable not being able to capture introduction if the firm has 
product_summary <- product_summary %>% group_by(firmid) %>% 
  mutate(exit_post_entry=ifelse(last_year-year>2 | exit_post_entry==1, exit_post_entry, NA),
         entry_post_exit=ifelse(last_year-year>2| entry_post_exit==1, entry_post_exit, NA), # Here the inequality is not strict because we have a "dummy" last year
         exit_pre_entry=ifelse(year-first_year>=2 | exit_pre_entry==1, exit_pre_entry, NA), 
         entry_pre_exit=ifelse(year-first_year>=2| entry_pre_exit==1, entry_pre_exit, NA))

# Arrange variables
product_summary <- product_summary %>% select(firmid, year, number_of_products, new_products, destroyed_products, paused_products, reintroduced_products,
                                              exit_post_entry, exit_pre_entry, entry_post_exit, entry_pre_exit, everything())
# Create dummy variable with 
product_summary <- product_summary %>% mutate(prod_creat=case_when( new_products>0 ~ 1, 
                                                                    is.na(new_products) ~ NA, 
                                                                    TRUE ~ 0),
                                              prod_destr=case_when( destroyed_products>0 ~ 1, 
                                                                    is.na(destroyed_products) ~ NA, 
                                                                    TRUE ~ 0))


saveRDS(product_summary, paste0("product_creation_destruction_eppe", ext, ".RDS"))

# 2.2) Lag and lead product entry and exit variables -----

###############################################################################
# Script: Product Entry/Exit Lag and Lead Variable Generator
#
# Description:
# This script loads a firm-year panel of product creation and destruction events, 
# and constructs lagged and lead indicators for each firm across multiple years. 
# These indicators are used to capture the temporal dynamics of product entry 
# (creation) and exit (destruction) behavior, including how product changes today 
# relate to past or future firm actions.
#
# Main components:
# 1. Load pre-processed data (`product_summary`) that includes product creation 
#    and destruction indicators for each firm-year.
#
# 2. Define the `lead_lag_creator()` function, which:
#    - Accepts a data.table, two variable names (creation/destruction), and the 
#      number of lags/leads to compute.
#    - Computes binary indicators for each `n`-year lag and lead of the specified 
#      variables using `dplyr::lag()` and `dplyr::lead()` inside a grouped `data.table`.
#
# 3. Apply the `lead_lag_creator()` function:
#    - First for general product creation and destruction (`prod_creat`, `prod_destr`)
#    - Then in a loop over all digit-level new/exit indicators (`new_1`, `exit_1`, etc.)
#
# 4. Generate a one-period lag of `number_of_products` for each firm.
#
# 5. Save the extended dataset with all created lag/lead features to an `.RDS` file.
#
# Notes:
# - This script combines the performance of `data.table` with the flexibility of 
#   `dplyr::lead()` and `dplyr::lag()` to compute time-relative indicators.
# - Assumes `digits` is a predefined vector containing product code levels (e.g., 1:5).
###############################################################################


product_summary<- readRDS(paste0("product_creation_destruction_eppe", ext, ".RDS"))


lead_lag_creator<-function(data, creat_var, destr_var, n_lags){
  # data<-product_summary
  # creat_var<-"exit_1"
  # destr_var<-"exit_1"
  # n_lags<-4
  setorder(data, firmid, year)
  setDT(data)
  
  for(i in 1:n_lags){
    x_creat_lag<-paste0(creat_var, "_lag", i)
    x_destr_lag<-paste0(destr_var, "_lag", i)
    x_creat_lead<-paste0(creat_var, "_lead", i)
    x_destr_lead<-paste0(destr_var, "_lead", i)
    
    vars<-c(x_creat_lag, x_destr_lag, x_creat_lead, x_destr_lead)
    
    data[, (x_creat_lag):=ifelse( is.na(dplyr::lag(get(creat_var), i)), NA, ifelse(dplyr::lag(get(creat_var), i)>0, 1,0)), by=.(firmid)]
    data[, (x_creat_lead):=ifelse( is.na(dplyr::lead(get(creat_var), i)), NA, ifelse(dplyr::lead(get(creat_var), i)>0, 1,0)), by=.(firmid)]
    data[, (x_destr_lag):=ifelse( is.na(dplyr::lag(get(destr_var), i)), NA, ifelse(dplyr::lag(get(destr_var), i)>0, 1,0)), by=.(firmid)]
    data[, (x_destr_lead):=ifelse( is.na(dplyr::lead(get(destr_var), i)), NA, ifelse(dplyr::lead(get(destr_var), i)>0, 1,0)), by=.(firmid)]
    
  }
  
  return(data)
}

product_summary<-lead_lag_creator(product_summary, "prod_creat", "prod_destr", n_lags=4)

for (i in digits) {
  
  new<-paste0("new_", i)
  exit<-paste0("exit_", i)
  
  product_summary<-lead_lag_creator(product_summary, new, exit, n_lags=4)
  
}

product_summary<-product_summary[, lag_number_of_products:=ifelse( is.na(dplyr::lag(number_of_products, 1)), NA, dplyr::lag(number_of_products, 1)), by=.(firmid)]


saveRDS(product_summary, paste0("product_creation_destruction", ext, ".RDS"))