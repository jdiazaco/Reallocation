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

# 2) Compute lead and lagged values for core metrics  ------------------------------------
product_data <- read_parquet(paste0("2_product_data/prodfra_plus/2a_product_yr_lvl_dta.parquet")) # Using prodfra_plus because it contains all cpa_or_pf possibilites
levels_agg <- digits[digits != 0]
product_core <- NULL

for (i in seq_along(levels_agg)) {
  print(levels_agg[i])
  data <- core_switch_product(product_data, levels_agg[i])
  setkey(data, firmid, year)

  if (i == 1) {
    product_core <- data
    setkey(product_core, firmid, year)
  } else {
    if (nrow(product_core) != nrow(data)) stop("Mismatch in row count across aggregation levels.")
    setkey(product_core, firmid, year)
    setkey(data, firmid, year)
    product_core <- data[product_core]
    
  }
}

share_cores <- names(product_core)[grepl("^core|^share_core|^share_runup", names(product_core))]
product_core <- create_lags(product_core, share_cores, id_vars = c("firmid"), time_var = "year")

write_parquet(product_core, paste0("2_product_data/2b_product_core_dta.parquet"))

# 3) Create lagged core variables for entry/exit analysis --------------------------------

product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet")) # Using prodfra_plus because it contains all cpa_or_pf possibilites
product_core <- read_parquet(paste0("2_product_data/2b_product_core_dta.parquet"))
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

# Rename variables
setnames(agg_product_data,
         old = c("first_introduction", "paused", "reintroduced", "discontinued", "incumbent"),
         new = c("prod_added", "prod_paused", "prod_reintroduced", "prod_removed", "prod_incumbent")
)

# Create dummies and windows for creation and destruction variables
agg_product_data[, c("prod_creat", "prod_destr", "net_product_creat", "net_product_destr") := .(as.integer(prod_added>0),
                                                                                                   as.integer(prod_removed>0),
                                                                                                   pmax(prod_added-prod_removed, 0),
                                                                                                   pmax(prod_removed - prod_added, 0))]

for(var in c("prod_creat", "prod_destr", "net_product_creat", "net_product_destr")){
  agg_product_data<-window_var_cretor(agg_product_data, "firmid", "year",
                                      var,
                                      2, 0 ,
                                      paste0(var, "_window"),
                                      na_rm=F)
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

## 3.1) calculate extensive margin product growth/reallocation rate -------------------
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

## 3.2) calculate intensive margin product reallocation rate -------------------
# agg_product_data[, rev_share2 := rev_bar / sum(rev_bar, na.rm = TRUE), by = year]#Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
agg_product_data[, `:=`(
  rev_reallocation_weighted = rev_reallocation * rev_share,
  rev_growth_weighted = rev_growth * rev_share
)]

# This should be only 1
table(agg_product_data[, .(n=.N), by=.(firmid, year)]$n)

# Export firm-level product data
write_parquet(agg_product_data, paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet"))

# 4) Create firm-level product-market competitors information ------------------

# load data 
product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet"))

# 1) Create size measures: rank within product categories, size buckets (quartile, decile, percentile, 1000tile), leader and top_4_leaders
product_data[, `:=`(
  rank_within_pc = frank(rev_bar, ties.method = "average", na.last = "keep"),
  n_firms_in_pc  = .N
), by = .(year, get(cpa_or_pf))] %>%
  .[, `:=`(
    size_quartile = as.integer(ifelse(n_firms_in_pc > 4, pmin(4L, ceiling(4 * rank_within_pc / n_firms_in_pc)), NA)),
    size_decile = as.integer(ifelse(n_firms_in_pc > 10, pmin(10L, ceiling(10 * rank_within_pc / n_firms_in_pc)), NA)),
    size_percentile = as.integer(ifelse(n_firms_in_pc > 100, pmin(100L, ceiling(100 * rank_within_pc / n_firms_in_pc)), NA)),
    size_1000tile = as.integer(ifelse(n_firms_in_pc > 1000, pmin(1000L, ceiling(1000 * rank_within_pc / n_firms_in_pc)), NA)),
    leader = ifelse(n_firms_in_pc > 1, ifelse( rank_within_pc == n_firms_in_pc, 1L, 0L), NA),
    top_4_leaders = ifelse(n_firms_in_pc > 8, ifelse(rank_within_pc > (n_firms_in_pc - 4), 1L, 0L), NA), 
    top_10_leaders = ifelse(n_firms_in_pc > 20, ifelse(rank_within_pc > (n_firms_in_pc - 10), 1L, 0L), NA)
  )] %>%
  # Add a measure of how far away leaders are from the rest of the distribution, share of leaders (top1, top4, top10) in total industry revenue
  .[, `:=`(
    # leader_rev_share = sum(within_industry_rev_share[leader == 1], na.rm = TRUE),
    # top_4_leaders_rev_share = sum(within_industry_rev_share[top_4_leaders == 1], na.rm = TRUE),
    # top_10_leaders_rev_share = sum(within_industry_rev_share[top_10_leaders == 1], na.rm = TRUE),
    diff_leader_vs_2nd = 
      {
        # Find the second highest rev_bar within each (year, NACE_BR) group
        rev_bars <- rev_bar[order(-rev_bar)]
        if (length(rev_bars) > 1 && !is.na(rev_bars[2]) && rev_bars[1] != 0) {
          (rev_bars[1] - rev_bars[2]) / rev_bars[1]
        } else {
          NA_real_
        }
      }
  ), by = .(year, get(cpa_or_pf))] 
setDT(product_data)

# Check that all is well
View(product_data[prodcom=="24511340"] %>% select(firmid, year, cpa_or_pf, rev, rev_bar, 
                                                  rank_within_pc, n_firms_in_pc, size_quartile, 
                                                  size_decile, size_percentile, size_1000tile, 
                                                  leader, top_4_leaders, top_10_leaders, 
                                                  diff_leader_vs_2nd)%>% 
       setorder(n_firms_in_pc, prodcom, year, rank_within_pc)
) 

# Make product maket level leaders and players dataset
product_mkt_data <- product_data[, .(leader=list(unique(firmid[leader==1])),
                                     top_4_leaders=list(unique(firmid[top_4_leaders==1])),
                                     top_10_leaders=list(unique(firmid[top_10_leaders==1])),
                                     firms_in_pc=list(unique(firmid)),
                                     n_firms_in_pc = n_distinct(firmid),
                                     diff_leader_vs_2nd = 
                                       {
                                         # Find the second highest rev_bar within each (year, NACE_BR) group
                                         rev_bars <- rev_bar[order(-rev_bar)]
                                         if (length(rev_bars) > 1 && !is.na(rev_bars[2]) && rev_bars[1] != 0) {
                                           (rev_bars[1] - rev_bars[2]) / rev_bars[1]
                                         } else {
                                           NA_real_
                                         }
                                       }), by=.(get(cpa_or_pf), year)] %>%
  .[, (cpa_or_pf):=get] %>% .[, get:=NULL]

# Bring product market level information to firm-product level data
product_data <- merge(product_data %>% select(-c(leader, top_4_leaders, top_10_leaders, n_firms_in_pc, diff_leader_vs_2nd)), 
                      product_mkt_data, by=c(cpa_or_pf, "year"), all.x = T)
# CAUTION: this next step can take a long time (hours)
product_firm_data <- product_data[, .(leaders=list(unique(unlist(leader))),
                                      top_4_leaders=list(unique(unlist(top_4_leaders))),
                                      top_10_leaders=list(unique(unlist(top_10_leaders))),
                                      firms_in_pc=list(unique(unlist(firms_in_pc)))), by=.(firmid, year)]

write_rds(product_firm_data, paste0("2_product_data/", cpa_or_pf, "/2d_firm_lvl_product_competitors_dta.RDS"))

# 5) Create firm-year patent-product NACE2 overlap dataset (2e) ------------------
# Goal: combine NACE2d patenting information with product sectors at the firm-year level

{
  # --- 5.1 Load patent-level data (module 3 output) --------------------------
  patent_lvl_data <- read_parquet("3_patent_lvl_patent_dta.parquet") %>% setDT(.)
  patent_lvl_data <- patent_lvl_data[!is.na(firmid)]
  patent_lvl_data[, firmid := as.character(firmid)]
  patent_lvl_data[, year := filing_year]

  # --- 5.2 Map patents to NACE2d using IPC concordance (same as module 4) -----
  patent_lvl_data[, ipcr_upd := gsub(" ", "", ipc_subclasses_symbol)]
  patent_lvl_data[, ipcr_list := strsplit(ipcr_upd, ";")]
  patent_ipcr <- patent_lvl_data[, .(ipcr = unlist(ipcr_list)), by = .(firmid, filing_year, patent_family)]

  special_codes <- c("A61K8", "B65F1", "B65F3", "B65F5", "B65F7", "B65F9")
  patent_ipcr[, ipcr4 := substr(toupper(ipcr), 1, 5)]
  patent_ipcr[, ipcr4 := ifelse(ipcr4 %in% special_codes, ipcr4, substr(ipcr4, 1, 4))]
  patent_ipcr <- unique(patent_ipcr[, .(firmid, filing_year, patent_family, ipcr4)])

  nace_ipc_concord <- unique(fread("Ancillary datasets/IPC V8_NACE Rev 2.txt"))
  nace_ipc_concord[, NACE2 := substr(as.character(NACE2), 1, 2)]
  patent_nace <- merge(
    patent_ipcr, nace_ipc_concord[, .(IPCV2015, NACE2)],
    by.x = "ipcr4", by.y = "IPCV2015", all.x = TRUE
  )
  # patent_nace <- patent_nace[!is.na(NACE2)]
  patent_nace[, year := filing_year]

  # --- 5.3 Load firm-level patent totals (module 4 output) -------------------
  firm_lvl_patent_data <- read_rds("4b_patenting_products_firm_level.rds") %>% setDT(.)
  firm_lvl_patent_data[, firmid := as.character(firmid)]
  firm_lvl_patent_data <- firm_lvl_patent_data[, .(
    firmid, year,
    total_patents = total_pat_families,
    new_patents = num_pat_families
  )]

  # --- 5.4 Load product-level data and define NACE2d product sets -------------
  product_data_2a <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2a_product_yr_lvl_dta.parquet")) %>% setDT(.)
  product_data_2a[, firmid := as.character(firmid)]

  # Current products: active product lines in the year (first_introduction/reintroduced/incumbent)
  product_current <- product_data_2a[
    (first_introduction | reintroduced | incumbent) & !is.na(NACE_2d_pf),
    .(NACE2 = unique(NACE_2d_pf)),
    by = .(firmid, prod_year = year)
  ]

  # New products: first introduction in the year
  product_new <- product_data_2a[
    first_introduction & !is.na(NACE_2d_pf),
    .(NACE2 = unique(NACE_2d_pf)),
    by = .(firmid, prod_year = year)
  ]

  # --- 5.5 Bring in firm-level product data to merge in the end -------------
  firm_lvl_product_data <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet")) %>% setDT(.)
  firm_lvl_product_data[, firmid := as.character(firmid)]


  # --- 5.6 Count patents matching product sectors ----------------------------
  # Use merge() and then filter by year conditions to avoid non-equi data.table joins

  # Current products: stock (filing_year <= prod_year)
  patent_current_merge <- merge(
    patent_nace, product_current,
    by = c("firmid", "NACE2"), allow.cartesian = TRUE
  )
  patent_current_stock <- patent_current_merge[
    year <= prod_year,
    .(patents_stock_in_current_products = uniqueN(patent_family)),
    by = .(firmid, year = prod_year)
  ]

  # Current products: new patents in same year
  patent_current_new <- patent_current_merge[
    year == prod_year,
    .(new_patents_in_current_products = uniqueN(patent_family)),
    by = .(firmid, year = prod_year)
  ]

  # New products: horizons t, t+1, ..., t+5
  base_years <- firm_lvl_patent_data[, .(firmid, year)]
  horizons <- 0:5
  newprod_stock_list <- list()
  newprod_new_list <- list()

  for (h in horizons) {
    # Product sector set for new products in [t, t+h]
    product_new_window <- merge(base_years, product_new, by = "firmid", allow.cartesian = TRUE)
    product_new_window <- product_new_window[
      prod_year >= year & prod_year <= year + h,
      .(NACE2 = unique(NACE2)),
      by = .(firmid, year)
    ]

    # Merge patent sectors with windowed new-product sectors
    patent_newprod_merge <- merge(
      patent_nace %>% rename(pat_year=year), product_new_window,
      by = c("firmid", "NACE2"), allow.cartesian = TRUE
    )

    # Stock of patents (pat_year <= year) that match new product sectors in window
    newprod_stock_list[[paste0("h", h)]] <- patent_newprod_merge[
      pat_year <= year,
      .(patents_stock = uniqueN(patent_family)),
      by = .(firmid, year)
    ][, horizon := h]

    # New patents in year that match new product sectors in window
    newprod_new_list[[paste0("h", h)]] <- patent_newprod_merge[
      pat_year == year,
      .(new_patents = uniqueN(patent_family)),
      by = .(firmid, year)
    ][, horizon := h]
  }

  newprod_stock_all <- rbindlist(newprod_stock_list, use.names = TRUE, fill = TRUE)
  newprod_new_all <- rbindlist(newprod_new_list, use.names = TRUE, fill = TRUE)

  newprod_stock_wide <- dcast(
    newprod_stock_all,
    firmid + year ~ horizon,
    value.var = "patents_stock",
    fill = 0
  )
  newprod_new_wide <- dcast(
    newprod_new_all,
    firmid + year ~ horizon,
    value.var = "new_patents",
    fill = 0
  )

  for (h in horizons) {
    setnames(newprod_stock_wide, as.character(h), paste0("patents_stock_in_new_products_h", h))
    setnames(newprod_new_wide, as.character(h), paste0("new_patents_in_new_products_h", h))
  }

  # --- 5.6 Merge counts and compute shares -----------------------------------
  patent_product_nace2d <- merge(firm_lvl_patent_data, patent_current_stock, by = c("firmid", "year"), all.x = TRUE)
  # patent_product_nace2d <- merge(firm_lvl_patent_data, 
  #                                firm_lvl_product_data %>%
  #                                  select(firmid, year, number_of_products, 
  #                                         names(firm_lvl_product_data)[grepl("new_|exit_[0-9]", names(firm_lvl_product_data))]),
  #                                by = c("firmid", "year"),
  #                                all.x = TRUE)
  patent_product_nace2d <- merge(patent_product_nace2d, patent_current_new, by = c("firmid", "year"), all.x = TRUE)
  patent_product_nace2d <- merge(patent_product_nace2d, newprod_stock_wide, by = c("firmid", "year"), all.x = TRUE)
  patent_product_nace2d <- merge(patent_product_nace2d, newprod_new_wide, by = c("firmid", "year"), all.x = TRUE)

  # Replace NAs with 0 for counts
  patent_product_nace2d <- merge(patent_product_nace2d, firm_lvl_product_data %>% select(firmid, year, prod_added, number_of_products), 
                                 by=c("firmid", "year"),
                                 all.x = T)
  
  count_vars_current_prods <- c(
    "patents_stock_in_current_products", "new_patents_in_current_products")
  
  for (v in count_vars_current_prods){
    patent_product_nace2d[!is.na(number_of_products) & number_of_products > 0, (v) := fifelse(is.na(get(v)), 0, get(v))]
  }
  
  count_vars_new_prods <- c(
    paste0("patents_stock_in_new_products_h", horizons),
    paste0("new_patents_in_new_products_h", horizons)
  )
  for (v in count_vars_new_prods){
    patent_product_nace2d[!is.na(prod_added) & prod_added > 0, (v) := fifelse(is.na(get(v)), 0, get(v))]
    patent_product_nace2d[prod_added == 0, (v) := NA_real_]
    
  }

  # Shares: handle zero denominators
  patent_product_nace2d[, `:=`(
    share_stock_patent_in_current_products = fifelse(total_patents > 0, patents_stock_in_current_products / total_patents, NA_real_),
    share_new_patent_in_current_products = fifelse(new_patents > 0, new_patents_in_current_products / new_patents, NA_real_)
  )]

  # Shares for new products by horizon
  for (h in horizons) {
    stock_col <- paste0("patents_stock_in_new_products_h", h)
    new_col <- paste0("new_patents_in_new_products_h", h)
    share_stock_col <- paste0("share_stock_patent_in_new_products_h", h)
    share_new_col <- paste0("share_new_patent_in_new_products_h", h)

    patent_product_nace2d[, (share_stock_col) := fifelse(
      total_patents > 0 , get(stock_col) / total_patents, NA_real_
    )]
    patent_product_nace2d[, (share_new_col) := fifelse(
      new_patents > 0, get(new_col) / new_patents, NA_real_
    )]
  }
  
  # delete intermediate variables to save memory
  patent_product_nace2d[, c("prod_added", "number_of_products") := NULL]
  
  # --- Enforce full-window definition (match script 7) -------------------------
  # Any horizon variable ending in _h{k} is set to NA if year+k is not observed for that firm.
  setDT(patent_product_nace2d)
  setorder(patent_product_nace2d, firmid, year)
  patent_product_nace2d[, max_year_firm := max(year, na.rm = TRUE), by = firmid]
  
  h_cols <- grep("_h[0-9]+$", names(patent_product_nace2d), value = TRUE)
  if (length(h_cols) > 0) {
    h_vals <- sort(unique(as.integer(sub(".*_h([0-9]+)$", "\\1", h_cols))))
    for (h in h_vals) {
      cols_h <- h_cols[grepl(paste0("_h", h, "$"), h_cols)]
      agg_product_data[year > (max_year_firm - h), (cols_h) := NA]
    }
  }
  
  # If a direct 2-year future-products variable exists, censor it too
  if ("future_products_2" %in% names(agg_product_data)) {
    agg_product_data[year > (max_year_firm - 2), future_products_2 := NA_real_]
  }
  
  agg_product_data[, max_year_firm := NULL]
  

  # --- 5.7 Save dataset 2e ---------------------------------------------------
  dir.create(paste0("2_product_data/", cpa_or_pf), showWarnings = FALSE, recursive = TRUE)
  write_parquet(
    patent_product_nace2d,
    paste0("2_product_data/", cpa_or_pf, "/2e_firm_lvl_patent_product_nace2d_dta.parquet")
  )
}


