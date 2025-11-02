# setup
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 1) Make IPCR cumulative data ----------------------------------------------


{
# a) Load patent level data------------------------------------------------
patent_data <- read_parquet("3_patent_lvl_patent_dta.parquet")
setDT(patent_data)
patent_data <- patent_data[!is.na(firmid)]

# b) Expand multiple firm IDs per patent ---------------------------------------
# patent_data[, siren_list := strsplit(siren, ",")]
# patent_data <- patent_data[
#   , .(firmid = unlist(siren_list)),
#   by = setdiff(names(patent_data), "siren_list")
# ]

# c) Process IPCR codes ------------------------------------------------------
patent_data[, `:=`(
  ipcr_upd = gsub(" ", "", ipc_subclasses_symbol),
  patent_id = .I
)]

patent_data[, ipcr_list := strsplit(ipcr_upd, ";")]
test <- patent_data[, .(ipcr = unlist(ipcr_list)), by = .(firmid, filing_year, patent_id)]

# Normalize IPCR to 4 digits + handle special codes
special_codes <- c("A61K8", "B65F1", "B65F3", "B65F5", "B65F7", "B65F9")
test[, ipcr4 := substr(toupper(ipcr), 1, 5)]
test[, ipcr4 := ifelse(ipcr4 %in% special_codes, ipcr4, substr(ipcr4, 1, 4))]

test <- unique(test[, .(firmid, filing_year, patent_id, ipcr4)])

# d) Add NACE codes via concordance --------------------------------------------
nace_ipc_concord <- unique(fread("Ancillary datasets/IPC V8_NACE Rev 2.txt"))
test <- merge(test, nace_ipc_concord[, .(IPCV2015, NACE2)],
  by.x = "ipcr4", by.y = "IPCV2015", all.x = TRUE
)

# e) Create full firm-year panel ------------------------------------------------
start <- 1990
end <- 2024
years_per_firm <- test[, .(min_year = start, max_year = end), by = firmid]
all_years <- years_per_firm[, .(filing_year = min_year:max_year), by = firmid]

test_expanded <- merge(all_years, test, by = c("firmid", "filing_year"), all = TRUE)
rm(test, years_per_firm, patent_data_upd, nace_ipc_concord)
gc()
setorder(test_expanded, firmid, filing_year)

# f) Efficient cumulative IPCR4 and NACE2 sets ----------------------------------
cum_data <- test_expanded[
  order(firmid, filing_year),
  .(ipcr4 = list(ipcr4), NACE2 = list(NACE2)),
  by = .(firmid, filing_year)
]
#   cum_data <- cum_data[
#   , `:=`(
#     ipcr_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), ipcr4, accumulate = TRUE),
#     NACE_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), NACE2, accumulate = TRUE)
#   ),
#   by = firmid
# ]

cum_data <- cum_data[
  , c("ipcr_cum", "NACE_cum") :={
    ipcr_cum_list <- vector("list", .N)
    NACE_cum_list <- vector("list", .N)
    ipcr_seen<- character()
    NACE_seen <- character()
    for(i in seq_len(.N)){
      ipcr_new <- ipcr4[[i]]
      NACE_new <- NACE2[[i]]
      if(!all(is.na(ipcr_new))) ipcr_seen <- unique(c(ipcr_seen, ipcr_new[!is.na(ipcr_new)]))
      if(!all(is.na(NACE_new))) NACE_seen <- unique(c(NACE_seen, NACE_new[!is.na(NACE_new)]))
      ipcr_cum_list[[i]] <- ipcr_seen
      NACE_cum_list[[i]] <- NACE_seen
    }
    list(ipcr_cum_list, NACE_cum_list)
  },
  by = firmid
]


  cum_data <- cum_data [
  , .(firmid, filing_year, ipcr_cum, NACE_cum)
] 
  cum_data <- cum_data[
  , `:=`(
    ipcr_cum = lapply(ipcr_cum, unique),
    NACE_cum = lapply(NACE_cum, unique),
    year = filing_year
  )
] 
  cum_data <- cum_data[, `:=`(
  n_ipcr = sapply(ipcr_cum, function(x) sum(!is.na(x))),
  n_NACE = sapply(NACE_cum, function(x) sum(!is.na(x)))
)]

# cum_data[, firmid_char:=str_pad(firmid, 9, side="left", pad="0")]
# cum_data[, firmid:=NULL]
# cum_data <- merge(cum_data, firmid_keys, by="firmid_char", all.x=T)

# cum_data <- cum_data[, firmid := as.integer(.GRP), by = firmid] %>% .[, firmid := as.numeric(firmid)] 
growth <- growth_creator(data=cum_data, 
                         normal_cols = c("n_NACE", "n_ipcr"), 
                         by_vars = c("firmid", "year"),
                         n_lag=1)[, c("firmid", "year", "n_NACE_bar", "n_ipcr_bar", "n_NACE_growth", "n_ipcr_growth")]
# cum_data_og <- copy(cum_data)
cum_data <- merge(cum_data, growth, by = c("firmid", "year"), all.x = T)

# Shift forward for lag comparison
cum_data_fwd <- copy(cum_data)[, filing_year := filing_year + 1][, c("firmid", "filing_year", "ipcr_cum", "NACE_cum")]
setnames(cum_data_fwd, c("ipcr_cum", "NACE_cum"), c("ipcr_cum_l", "NACE_cum_l"))

# Merge current and lagged cumulative data
ipcr_cumulative <- merge(cum_data, cum_data_fwd, by = c("firmid", "filing_year"), all.x = TRUE)

# g) Identify new IPCR/NACE codes per year -------------------------------------
ipcr_cumulative[, `:=`(
  new_ipcr = Map(setdiff, ipcr_cum, ipcr_cum_l),
  new_NACE = Map(setdiff, NACE_cum, NACE_cum_l)
)]
ipcr_cumulative[, ipcr_creat := sapply(new_ipcr, function(x) ifelse(length(x) > 0, 1, 0))]
ipcr_cumulative[, ipcr_creat := ifelse(ipcr_creat == 1 & is.na(n_NACE_growth), 0, ipcr_creat)]

saveRDS(ipcr_cumulative, "4a_ipcr_cumulative.RDS")
}

# 2) Firm level patent and trademark database creation-------------------

# a) Load firm data and product data -----------------

{
  start <- 1980
  end <- 2022

  patent_lvl_data <- as.data.table(read_parquet("3_patent_lvl_patent_dta.parquet"))
  ipcr_cumulative <- as.data.table(readRDS("4a_ipcr_cumulative.RDS"))
  # if (grepl("Drive", getwd())) {
  #   patent_lvl_data[, firmid := as.character(firmid)]
  #   ipcr_cumulative[, firmid := as.character(firmid)]
  # }else{
  #   patent_lvl_data <- patent_lvl_data %>% rename(firmid=firmid) %>% merge(firmid_keys, by="firmid", all.x=T)
  # }
  
  ipcr_cumulative <- ipcr_cumulative[, .(firmid, year, n_ipcr, n_NACE, n_NACE_growth, n_ipcr_growth, ipcr_creat, 
                                         ipcr_cum, NACE_cum, n_NACE_bar, n_ipcr_bar,
                                         ipcr_cum_l, NACE_cum_l, new_ipcr, new_NACE)]
  window_length <- 2

  filings_per_year <- patent_lvl_data[, .(num_pat_filings = n_distinct(control1)), by = .(firmid, filing_year)]
  families_per_year <- patent_lvl_data[, .(num_pat_families = uniqueN(patent_family)), by = .(firmid, filing_year)]
  
  # Create all combinations of firmid and year (complete panel)
  full_panel <- CJ(firmid = unique(patent_lvl_data$firmid), filing_year = max(1980, min(patent_lvl_data$filing_year)):2022)
  
  # Merge with the original data to fill in missing years with NA
  firm_lvl_patent_data <- merge(full_panel, filings_per_year, by.x = c("firmid", "filing_year"), by.y = c("firmid", "filing_year"), all.x = TRUE)
  firm_lvl_patent_data <- merge(firm_lvl_patent_data, families_per_year, by.x = c("firmid", "filing_year"), by.y = c("firmid", "filing_year"), all.x = TRUE)
  
  # Order by firm and year
  setorder(firm_lvl_patent_data, firmid, filing_year)
  
  # Compute cumulative sums, treating NA as 0
  firm_lvl_patent_data[, num_pat_filings := fifelse(is.na(num_pat_filings), 0, num_pat_filings)]
  firm_lvl_patent_data[, num_pat_families := fifelse(is.na(num_pat_families), 0, num_pat_families)]
  firm_lvl_patent_data[, total_pat_families := cumsum(num_pat_families), by = firmid]
  firm_lvl_patent_data[, total_pat_filings := cumsum(num_pat_filings), by = firmid]
  
  # saveRDS(firm_lvl_patent_data, "patent_tm_clean.RDS")
  # Growth and window measures for pat_filings and pat_families (concise)
  firm_lvl_patent_data <- firm_lvl_patent_data[
    !is.na(firmid)
  ][
    , `:=`(
      pat_filings_dummy = as.integer(num_pat_filings > 0),
      pat_families_dummy = as.integer(num_pat_families > 0)
    )
  ]
  
  # firm_lvl_patent_data[, firmid:=.GRP, by=firmid] %>% .[, firmid:=as.numeric(firmid)]
  firm_lvl_patent_data <- firm_lvl_patent_data %>% rename(year=filing_year)
  
  for (var in c("total_pat_filings", "total_pat_families")) {
    growth <- growth_creator(firm_lvl_patent_data, var, 1)[, .(firmid, year, growth = get(paste0(var, "_growth")))]
    setnames(growth, "growth", paste0(var, "_growth"))
    firm_lvl_patent_data <- merge(firm_lvl_patent_data, growth, by = c("firmid", "year"), all.x = TRUE)
  }

  names(firm_lvl_patent_data)
  hist(firm_lvl_patent_data$total_pat_filings_growth)
  hist(firm_lvl_patent_data$total_pat_families_growth)
  
  for (var in c("pat_filings_dummy", "pat_families_dummy")) {
    win_name <- paste0(var, "_window")
    firm_lvl_patent_data <- window_var_cretor(firm_lvl_patent_data, "firmid", "year", var, window_length, 0, win_name, na_rm = TRUE)
  }
  
  # product_summary <- merge(product_summary, firm_lvl_patent_data, by = c("firmid", "year"), all.x = TRUE)
  # for (var in c("total_pat_filings_growth", "total_pat_families_growth")) {
  #   product_summary[, (var) := fifelse(is.na(get(var)), 0, get(var))]
  # }
  #
  # for (var in c("pat_filings_dummy", "pat_families_dummy")) {
  #   win_name <- paste0(var, "_window_psum")
  #   product_summary <- window_var_cretor(product_summary, "firmid", "year", var, window_length, 0, win_name, na_rm = TRUE)
  #   orig_win <- paste0(var, "_window")
  #   product_summary[, (win_name) := fifelse(get(win_name) == 0 & get(orig_win) == 1 & !is.na(get(orig_win)), 1, get(win_name))]
  # }

  # Bring in IPCR information, clean it and merdge it to product_summary
  # ipcr_cumulative[, firmid := as.integer(.GRP), by = firmid] %>% .[, firmid := as.numeric(firmid)]
  firm_lvl_patent_data <- merge(firm_lvl_patent_data, ipcr_cumulative, by = c("firmid", "year"), all.x = TRUE)
  firm_lvl_patent_data[, ipcr_creat := fifelse(is.na(ipcr_creat), 0, ipcr_creat)]
  firm_lvl_patent_data <- window_var_cretor(firm_lvl_patent_data, "firmid", "year", "ipcr_creat", window_length, 0, "ipcr_creat_window", na_rm = FALSE)
  firm_lvl_patent_data[, ever_patent := as.numeric(any(pat_filings_dummy)), by = firmid]
  
  firm_lvl_patent_data <- firm_lvl_patent_data %>% # Treat NA as 0 for ipcr_creat
    window_var_cretor("firmid", "year", "ipcr_creat", window_length, 0, "ipcr_creat_window", na_rm = TRUE) # Create 2-year window for ipcr_creat
  
  # Save final data
  write_rds(firm_lvl_patent_data, "4b_patenting_products_firm_level.rds")
}

