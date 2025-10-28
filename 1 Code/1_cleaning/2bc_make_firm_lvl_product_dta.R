# ------------------------------------------------------------------------------
# Title: Product Entry/Exit and Core Metrics Aggregation Pipeline
#
# Description:
# This script processes product-level data for firms, computing entry and exit
# flags at multiple aggregation levels, generating lagged core metrics, and
# summarizing product activity at the firm-year level. It is designed for
# longitudinal analysis of product portfolios, including tracking introductions,
# discontinuations, and core product metrics over time.
#
# Main Steps:
# 1. Load product-level data from parquet files.
# 2. Compute lead and lagged values for core metrics across aggregation levels.
# 3. Create lagged variables for entry/exit analysis.
# 4. Merge core metrics back into product-level data.
# 5. Dynamically compute entry and exit flags for each aggregation level.
# 6. Aggregate product-level variables to the firm-year level.
# 7. Generate growth metrics and merge summary variables.
# 8. Rename variables for clarity and create a count of products per firm-year.
# 9. Create lagged product count variable.
# 10. Set first-year observations to NA for product status variables.
# 11. Filter out last-year observations with zero revenue.
#
# Key Variables:
# - Entry/Exit Flags: new_{digit}, exit_{digit}
# - Core Metrics: core_*, lag_core_*
# - Product Status: prod_added, prod_removed, prod_incumbent, prod_paused, prod_reintroduced
# - Aggregated Metrics: number_of_products, rev
#
# Dependencies:
# - data.table, dplyr, rstudioapi, arrow (for read_parquet)
# - Custom functions: core_switch_product, growth_creator, create_lags
#
# Usage:
# Source this script after defining required functions and loading necessary
# libraries. Ensure input data files and parameters (e.g., digits, cpa_or_pf)
# are set appropriately.
# ------------------------------------------------------------------------------
# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))
start=2009
end=2022


# 1) Load data  ------------------------------------
product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet"))

# 2) Compute lead and lagged values for core metrics  ------------------------------------
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

share_cores <- names(product_core)[grepl("^core|^share_core|^share_runup", names(product_core))]
product_core <- create_lags(product_core, share_cores, id_vars = c("firmid"), time_var = "year")

write_parquet(product_core, paste0("2_product_data/", cpa_or_pf, "/2b_product_core_dta.parquet"))

# 3) Create lagged core variables for entry/exit analysis --------------------------------

product_core <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2b_product_core_dta.parquet"))
# share_cores <- names(product_core)[grepl("^lag_core|^lag_share_core|^lag_share_runup", names(product_core))]
# product_core <- product_core %>% select(-share_cores)

# Merge core variables back into product-level data
product_data<-merge(product_data, product_core, by=c("firmid", "year"), all.x=TRUE)


# Compute Entry and Exit Flags by Aggregation Level
digits_inv <- sort(digits, decreasing = TRUE)


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
                                  else if (digit == 4) NACE_4d_pf == lag_core_pf_4
                                  else if (digit == 2) NACE_2d_pf == lag_core_pf_2
                                  else if (digit == 1) substr(DEFind_pf, 1, 1) == lag_core_pf_1
                                  else if (digit == 0) substr(DEFind_pf, 1, 1) != lag_core_pf_1) &  # for digit 0 or any other cases
                                 
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
                                  else if (digit == 4) NACE_4d_pf == lag_core_pf_4
                                  else if (digit == 2) NACE_2d_pf == lag_core_pf_2
                                  else if (digit == 1) substr(DEFind_pf, 1, 1) == lag_core_pf_1
                                  else if (digit == 0) substr(DEFind_pf, 1, 1) != lag_core_pf_1) &  # for digit 0 or any other cases
                                 
                                 # Ensure no overlap with previous levels
                                 # Ensure no overlap with previous levels
                                 !(if (length(prev_digits) > 0) 
                                   Reduce(`|`, lapply(prev_digits, function(d) get(paste0("exit_", d))))
                                   else FALSE)]
}


# # Loop through each level to create entry and exit variables dynamically
# # Helper function to get code column and lag column for each digit
# get_code_and_lag <- function(digit) {
#   switch(as.character(digit),
#     "10" = list(code = "prodfra_plus", lag = "lag_core_pf_10"),
#     "8"  = list(code = "prodcom",      lag = "lag_core_pf_8"),
#     "6"  = list(code = "cpa",          lag = "lag_core_pf_6"),
#     "4"  = list(code = "NACE_4d_pf",         lag = "lag_core_pf_4"),
#     "2"  = list(code = "NACE_2d_pf",   lag = "lag_core_pf_2"),
#     "1"  = list(code = "DEFind_pf",       lag = "lag_core_pf_1"),
#     NULL
#   )
# }
# 
# product_data_og <- product_data
# 
# for (i in seq_along(digits_inv)) {
#   digit <- digits_inv[i]
#   prev_digits <- if (i > 1) digits_inv[1:(i - 1)] else integer(0)
#   code_info <- get_code_and_lag(digit)
# 
#   # Entry condition
#   product_data[, paste0("new_", digit) := {
#     cond <- first_introduction
#     if (!is.null(code_info)) {
#       if (digit == 1) {
#         cond <- cond & substr(get(code_info$code), 1, 1) == get(code_info$lag)
#       } else {
#         cond <- cond & get(code_info$code) == get(code_info$lag)
#       }
#     }
#     # Exclude overlap with previous levels
#     if (length(prev_digits) > 0) {
#       cond <- cond & !Reduce(`|`, lapply(prev_digits, function(d) get(paste0("new_", d))))
#     }
#     cond
#   }]
# 
#   # Exit condition
#   product_data[, paste0("exit_", digit) := {
#     cond <- discontinued
#     if (!is.null(code_info)) {
#       if (digit == 1) {
#         cond <- cond & substr(get(code_info$code), 1, 1) == get(code_info$lag)
#       } else {
#         cond <- cond & get(code_info$code) == get(code_info$lag)
#       }
#     }
#     # Exclude overlap with previous levels
#     if (length(prev_digits) > 0) {
#       cond <- cond & !Reduce(`|`, lapply(prev_digits, function(d) get(paste0("exit_", d))))
#     }
#     cond
#   }]
# }


# Reorder and filter columns
product_data <- product_data %>%
  select(firmid, year, cpa_or_pf, rev, first_introduction, starts_with("new_"),
         discontinued, starts_with("exit_"), starts_with("core_"), starts_with("lag_"), everything())


# Define working variables
vars <- c(
  "first_introduction", names(product_data)[grep("^new", names(product_data))],
  # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2",
  names(product_data)[grep("^exit", names(product_data))],
  "reintroduced", "discontinued", "incumbent", "paused", "rev"
)
unique_vars <- c("consolidated_birth_year", "economic_death_year", "legal_birth_year", "economic_birth_year", "first_year", "last_year", "status")

#Aggregate variables at the firm-year level from the firm-year-product level
agg_product_data <- product_data[, lapply(.SD, sum, na.rm=T), .SDcols=vars, by=.(firmid, year)]
product_summary_years <- product_data[, lapply(.SD, unique, na.rm=T), .SDcols=unique_vars, by=.(firmid, year)]
agg_product_data <- merge(agg_product_data, product_summary_years, by = c("firmid", "year"), all.x = T)
agg_product_data <- agg_product_data %>% .[!(rev==0 & status!="died")] 

agg_product_data_temp <- growth_creator(agg_product_data, "rev", 1, create_born_died=T, data_type="survey") %>%
  select("firmid", "year", names(.)[grepl("rev_", names(.))])
agg_product_data <- merge(agg_product_data, agg_product_data_temp, by=c("firmid", "year"), all.x=T)
rm(product_summary_years); gc()

# Rename variables
setnames(agg_product_data,
    old = c("first_introduction", "paused", "reintroduced", "discontinued", "incumbent"),
    new = c("prod_added", "prod_paused", "prod_reintroduced", "prod_removed", "prod_incumbent")
)

# Create dummies and windows for creation and destruction variables
agg_product_data[, c("prod_creat","prod_destr","net_product_creat","net_product_destr") := .(
  as.integer(prod_added > 0),
  as.integer(prod_removed > 0),
  pmax(prod_added - prod_removed, 0),
  pmax(prod_removed - prod_added, 0)
)]

# Create windows
for(var in c("prod_creat","prod_destr","net_product_creat","net_product_destr")){
  agg_product_data <- window_var_cretor(agg_product_data, "firmid", "year", 
                                        var, 
                                        2, 0, 
                                        paste0(var, "_window"), 
                                        na_rm = F) 
}

# Create number_of_products column
agg_product_data[, number_of_products := prod_added + prod_reintroduced + prod_incumbent]

agg_product_data <- create_lags(agg_product_data,
    c("number_of_products"),
    id_vars = c("firmid"), time_var = "year", all=F
)

# Set firm's first year observations to NAs, since we don't know actual product status that year
new_cols <- grep("^new", names(agg_product_data), value = TRUE)
switch_cols <- grep("^switch", names(agg_product_data), value = TRUE)

agg_product_data[year == first_year, (c(
    "prod_added", "prod_paused", "prod_reintroduced", "prod_removed", "prod_incumbent", new_cols
)) := NA]

agg_product_data[year == first_year, (new_cols) := NA]
# agg_product_data[year == first_year, (switch_cols) := NA]

# Drop last year observations with zero revenue
# agg_product_data <- agg_product_data[rev != 0]

## 5.1) calculate extensive margin product growth/reallocation rate -------------------
agg_product_data <- agg_product_data[, `:=`(
    entrance_growth = prod_added / number_of_products,
    exit_growth = prod_removed / lag_number_of_products,
    entrance_share = number_of_products / sum(number_of_products, na.rm=T),
    exit_share = lag_number_of_products / sum(lag_number_of_products, na.rm=T)
),
by = year
]

agg_product_data[status == 'born', exit_growth:=0]
agg_product_data[status == 'died', entrance_growth:=0]
agg_product_data[,status:= NULL]
agg_product_data[, `:=`(entrance_growth_weighted  = entrance_growth * entrance_share,
                    exit_growth_weighted      = exit_growth     * exit_share)] 

vars<-c("entrance", "exit")
for (var in vars){
  agg_product_data[[paste0(var, "_reallocation")]] <- agg_product_data[[paste0(var, "_growth")]]
  agg_product_data[[paste0(var, "_reallocation_weighted")]] <- agg_product_data[[paste0(var, "_growth_weighted")]]
}

## 5.2) calculate intensive margin product reallocation rate -------------------
# agg_product_data[, rev_share2 := rev_bar / sum(rev_bar, na.rm = TRUE), by = year]#Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
agg_product_data[, `:=`(
    rev_reallocation_weighted = rev_reallocation * rev_share,
    rev_growth_weighted = rev_growth * rev_share
)]

# This should be only 1
table(agg_product_data[, .(n=.N), by=.(firmid, year)]$n)

# Export firm-level product data
write_parquet(agg_product_data, paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet"))

