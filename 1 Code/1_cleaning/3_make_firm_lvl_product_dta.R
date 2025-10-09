# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 1) prepare firm level decompositions of product data ------------------------------------
product_data <- read_parquet(paste0("2_product_yr_lvl_dta_", cpa_or_pf, ".parquet"))
firm_data <- read_parquet("1_firm_yr_lvl_br_dta.parquet")

## 5.1) calculate extensive margin product growth/reallocation rate -------------------
firm_data_ex = product_data[, .(
  products_l = sum(active_l),
  products = sum(active),
  prod_added = sum(active & !active_l),
  prod_removed = sum(!active & active_l)
),
by = .(firmid, year, consolidated_birth_year, economic_death_year, status)
]

firm_data_ex[, `:=`(
  entrance_growth = prod_added / products,
  exit_growth = prod_removed / products_l,
  entrance_share = products / sum(products),
  exit_share = products_l / sum(products_l)
),
by = year
]

firm_data_ex[status == 'born', exit_growth:=0]
firm_data_ex[status == 'died', entrance_growth:=0]
firm_data_ex[,status:= NULL]
firm_data_ex[, `:=`(entrance_growth_weighted  = entrance_growth * entrance_share,
                    exit_growth_weighted      = exit_growth     * exit_share)] 

vars<-c("entrance", "exit")
for (var in vars){
  firm_data_ex[[paste0(var, "_reallocation")]] <- firm_data_ex[[paste0(var, "_growth")]]
  firm_data_ex[[paste0(var, "_reallocation_weighted")]] <- firm_data_ex[[paste0(var, "_growth_weighted")]]
}
##5.2) calculate intensive margin product reallocation rate -------------------
product_data = product_data %>% select(firmid, cpa_or_pf, year, rev_l, rev, rev_bar,
                                       rev_reallocation, rev_growth, within_firm_rev_share, everything())
firm_data_in = product_data[, .(
  rev = sum(rev),
  rev_l = sum(rev_l),
  rev_bar = 0.5 * (sum(rev_bar)), # Why times 0.5?
  rev_reallocation = sum(rev_reallocation * within_firm_rev_share),
  rev_growth = sum(rev_growth * within_firm_rev_share)
),
by = .(firmid, year, consolidated_birth_year, economic_death_year)
] # Here aggregating by firm and year, deleting product-lines.

firm_data_in[, rev_share := rev_bar / sum(rev_bar), by = year]#Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
firm_data_in[, `:=`(
  rev_reallocation_weighted = rev_reallocation * rev_share,
  rev_growth_weighted = rev_growth * rev_share,
  rev_bar = NULL
)]

## merge together
combined_data = merge(firm_data_in, firm_data_ex, all = T) %>%
  mutate(in_prodcom = T)
combined_data = merge(firm_data, combined_data, by = c('firmid', 'year', 'consolidated_birth_year', 'economic_death_year'), all = T)
combined_data[is.na(in_prodcom), in_prodcom := F]
combined_data[, full_sample := 1]
# saveRDS(combined_data, 'combined_sbs_br_prodcom_data.rds')

# 2) Identification and tracking of core and new product categories across aggregation levels  --------------------

# This script analyzes product reallocation and firm dynamics by identifying 
# and tracking core products across multiple levels of product aggregation 
# (1, 2, 4, 6 digits for CPA; optionally up to 10 for ProdFra+). 
# It:
#   - Loads firm and product-level data
#   - Verifies consistency of input datasets
#   - Defines and tracks core product categories within firms
#   - Calculates product entries and exits by aggregation level
#   - Outputs datasets augmented with core product and transition metrics


# # Ensure product categories are exhaustive and mutually exclusive
# # Each row should fall into exactly one category
# if (nrow(product_data[, .(first_introduction, reintroduced, discontinued, incumbent, paused)]) != nrow(product_data)) {
#   stop("Product categories are not exhaustive and/or mutually exclusive.")
# }

# # Extract relevant code substrings based on classification system
# if (cpa_or_pf == "cpa") {
#   product_data[, NACE_2d_pf := substr(cpa, 1, 2)]
# } else if (cpa_or_pf == "prodfra_plus") {
#   product_data[, `:=`(
#     prodcom = substr(prodfra_plus, 1, 8),
#     cpa = substr(prodfra_plus, 1, 6),
#     NACE_2d_pf = substr(prodfra_plus, 1, 2)
#   )]
# } else if( cpa_or_pf == "prodcom"){
#   product_data[, `:=`(
#     cpa = substr(prodcom, 1, 6),
#     NACE_2d_pf = substr(prodcom, 1, 2)
#   )]
# } else {
#   stop("Invalid classification system specified.")
# }



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
share_cores <- names(product_core)[grepl("^core|^share_core|^share_runup", names(product_core))]
product_core_l <- copy(product_core) %>%
  select(firmid, year, all_of(share_cores)) %>%
  .[, year := year + 1] # Shift year forward for lag merge
names(product_core_l)[-c(1, 2)] <- paste0("lag_", names(product_core_l)[-c(1, 2)])
product_core <- merge(product_core, product_core_l, by = c("firmid", "year"), all.x = TRUE)


# Compute Entry and Exit Flags by Aggregation Level
digits_inv <- sort(digits, decreasing = TRUE)

# Loop through each level to create entry and exit variables dynamically
# Helper function to get code column and lag column for each digit
get_code_and_lag <- function(digit) {
  switch(as.character(digit),
    "10" = list(code = "prodfra_plus", lag = "lag_core_pf_10"),
    "8"  = list(code = "prodcom",      lag = "lag_core_pf_8"),
    "6"  = list(code = "cpa",          lag = "lag_core_pf_6"),
    "4"  = list(code = "NACE_4d_pf",         lag = "lag_core_pf_4"),
    "2"  = list(code = "NACE_2d_pf",   lag = "lag_core_pf_2"),
    "1"  = list(code = "DEFind_pf",       lag = "lag_core_pf_1"),
    NULL
  )
}

for (i in seq_along(digits_inv)) {
  digit <- digits_inv[i]
  prev_digits <- if (i > 1) digits_inv[1:(i - 1)] else integer(0)
  code_info <- get_code_and_lag(digit)

  # Entry condition
  product_data[, paste0("new_", digit) := {
    cond <- first_introduction
    if (!is.null(code_info)) {
      if (digit == 1) {
        cond <- cond & substr(get(code_info$code), 1, 1) == get(code_info$lag)
      } else {
        cond <- cond & get(code_info$code) == get(code_info$lag)
      }
    }
    # Exclude overlap with previous levels
    if (length(prev_digits) > 0) {
      cond <- cond & !Reduce(`|`, lapply(prev_digits, function(d) get(paste0("new_", d))))
    }
    cond
  }]

  # Exit condition
  product_data[, paste0("exit_", digit) := {
    cond <- discontinued
    if (!is.null(code_info)) {
      if (digit == 1) {
        cond <- cond & substr(get(code_info$code), 1, 1) == get(code_info$lag)
      } else {
        cond <- cond & get(code_info$code) == get(code_info$lag)
      }
    }
    # Exclude overlap with previous levels
    if (length(prev_digits) > 0) {
      cond <- cond & !Reduce(`|`, lapply(prev_digits, function(d) get(paste0("exit_", d))))
    }
    cond
  }]
}



# Reorder and filter columns
product_data <- product_data %>%
  select(firmid, year, cpa_or_pf, rev, first_introduction, starts_with("new_"),
         discontinued, starts_with("exit_"), starts_with("core_"), starts_with("lag_"), everything())

# # Save results
# write_rds(product_data, paste0("product_level_growth_", filter_indicator, "_new_core_analysis", ext, ".RDS"))
# write_rds(product_core, paste0("product_core", ext, ".RDS"))


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
vars <- c(
  "first_introduction", names(product_data)[grep("^new", names(product_data))],
  # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2",
  names(product_data)[grep("^exit", names(product_data))],
  "reintroduced", "discontinued", "incumbent", "paused", "rev"
)
unique_vars <- c("first_year", "last_year")

#Aggregate variables at the firm-year level from the firm-year-product level
product_summary_sums <- product_data[, lapply(.SD, sum, na.rm=T), .SDcols=vars, by=.(firmid, year)]
rev_growth <- growth_creator(product_summary_sums, "rev", 1) %>% select(firmid, year, rev_l, rev_bar, rev_growth)
product_summary_sums <- merge(product_summary_sums, rev_growth, by=c("firmid", "year"), all.x = T)
product_summary_years <- product_data[, lapply(.SD, unique, na.rm=T), .SDcols=unique_vars, by=.(firmid, year)]
product_summary <- merge(product_summary_sums, product_summary_years, by=c("firmid", "year"), all.x=T)
rm(product_summary_sums, product_summary_years, rev_growth); gc()

product_summary <- merge(product_summary, product_core, by=c("firmid", "year"), all.x = T)

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

# Create dummy variable with 
product_summary <- product_summary %>% mutate(prod_creat=case_when( new_products>0 ~ 1, 
                                                                    is.na(new_products) ~ NA, 
                                                                    TRUE ~ 0),
                                              prod_destr=case_when( destroyed_products>0 ~ 1, 
                                                                    is.na(destroyed_products) ~ NA, 
                                                                    TRUE ~ 0))


saveRDS(product_summary, paste0("product_creation_destruction_eppe", ext, ".RDS"))

