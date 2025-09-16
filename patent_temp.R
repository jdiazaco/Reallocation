#' ------------------------------------------------------------------------------
#' Script: Patent Data Cleaning and Cumulative IPCR/NACE Generation
#' Author: Juli?n D?az-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' ------------------------------------------------------------------------------


### patent_tm_clean.RDS

patent_and_tm<-read_parquet("C:/Users/Public/Documents/Big data Project/1) data/tm_patent/siren_level_patent_and_tm_final.parquet")

# Adjust variables
patent_and_tm<-patent_and_tm %>% rename(firmid=siren, year=application_year)
patent_and_tm$`__index_level_0__`<-NULL
setorder(patent_and_tm, firmid, year)

# Create all combinations of firmid and year (complete panel)
full_panel <- CJ(firmid = unique(patent_and_tm$firmid), year = min(patent_and_tm$year):2022)

# Merge with the original data to fill in missing years with NA
patent_and_tm <- merge(full_panel, patent_and_tm, by = c("firmid", "year"), all.x = TRUE)

# Order by firm and year
setorder(patent_and_tm, firmid, year)

# Compute cumulative sums, treating NA as 0
patent_and_tm[, num_patent := fifelse(is.na(num_patent), 0, num_patent)]
patent_and_tm[, num_tm := fifelse(is.na(num_tm), 0, num_tm)]
patent_and_tm[, total_patent := cumsum(num_patent), by = firmid]
patent_and_tm[, total_tm := cumsum(num_tm), by = firmid]

saveRDS(patent_and_tm, "patent_tm_clean.RDS")


### ipcr_cumulative.RDS

# 0) Setup ----------------------------------------------------------------------
source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name <- ""
output_dir<-paste0(output_dir, "2025/Export 04.09/")
output_dir_creator(output_dir)

# 1) Load and clean patent data ------------------------------------------------
patent_data_upd <- read_parquet("tm_patent/patent_record_level_final.parquet")
setDT(patent_data_upd)

# 2) Expand multiple firm IDs per patent ---------------------------------------
patent_data_upd[, siren_list := strsplit(applicant_name_cleaned, ",")]
# Check how many rows have more than one siren
#patent_data_upd[, num_sirens := lengths(siren_list)]
#View(patent_data_upd[num_sirens > 1])
#patent_data_upd[, num_sirens := NULL]  # Remove the temporary column

patent_data_upd <- patent_data_upd[
  , .(firmid = unlist(siren_list)), by = setdiff(names(patent_data_upd), "siren_list")
]

# 3) Expand multiple IPCR codes per patent --------------------------------------------------------
patent_data_upd[, `:=`(
  ipcr_upd = gsub(" ", "", ipcr),
  patent_id = .I
)]
patent_data_upd[, ipcr_list := strsplit(ipcr_upd, ",")]
test <- patent_data_upd[, .(ipcr = unlist(ipcr_list)), by = .(firmid, application_year, patent_id)]

# Normalize IPCR to 4 digits + handle special codes
special_codes <- c("A61K8", "B65F1", "B65F3", "B65F5", "B65F7", "B65F9")
test[, ipcr4 := substr(toupper(ipcr), 1, 5)]
test[, ipcr4 := ifelse(ipcr4 %in% special_codes, ipcr4, substr(ipcr4, 1, 4))]

test <- unique(test[, .(firmid, application_year, patent_id, ipcr4)])

# 4) Add NACE codes via concordance --------------------------------------------
nace_ipc_concord <- unique(fread("IPC V8_NACE Rev 2.txt"))
test <- merge(test, nace_ipc_concord[, .(IPCV2015, NACE2)], 
              by.x = "ipcr4", by.y = "IPCV2015", all.x = TRUE)

# 5) Create full firm-year panel ------------------------------------------------
start<-1990; end<-2024
years_per_firm <- test[, .(application_year = start:end), by = firmid]
test_expanded <- merge(years_per_firm, test, by = c("firmid", "application_year"), all = TRUE)
rm(test, years_per_firm, patent_data_upd, nace_ipc_concord); gc()
setorder(test_expanded, firmid, application_year)

# 6) Efficient cumulative IPCR4 and NACE2 sets ----------------------------------
cum_data <- test_expanded[
  order(firmid, application_year),
  .(ipcr4 = list(ipcr4), NACE2 = list(NACE2)), by = .(firmid, application_year)
][
  , `:=`(
    ipcr_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), ipcr4, accumulate = TRUE),
    NACE_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), NACE2, accumulate = TRUE)
  ), by = firmid
][
  , .(firmid, application_year, ipcr_cum, NACE_cum)
][
  , `:=`(
    ipcr_cum = lapply(ipcr_cum, unique),
    NACE_cum = lapply(NACE_cum, unique),
    year=application_year
  )
][, `:=`(
  n_ipcr = sapply(ipcr_cum, function(x) sum(!is.na(x))),
  n_NACE = sapply(NACE_cum, function(x) sum(!is.na(x)))
)]
growth<-growth_creator(cum_data, c("n_NACE", "n_ipcr"), 1)[, c("firmid", "year", "n_NACE_bar", "n_ipcr_bar", "n_NACE_growth", "n_ipcr_growth")]
cum_data_og<-copy(cum_data)
cum_data<-merge(cum_data, growth, by=c("firmid", "year"), all.x = T)

# Shift forward for lag comparison
cum_data_fwd <- copy(cum_data)[, application_year := application_year + 1][, c("firmid", "application_year", "ipcr_cum", "NACE_cum")]
setnames(cum_data_fwd, c("ipcr_cum", "NACE_cum"), c("ipcr_cum_l", "NACE_cum_l"))

# Merge current and lagged cumulative data
ipcr_cumulative <- merge(cum_data, cum_data_fwd, by = c("firmid", "application_year"), all.x = TRUE)

# 7) Identify new IPCR/NACE codes per year -------------------------------------
ipcr_cumulative[, `:=`(
  new_ipcr = Map(setdiff, ipcr_cum, ipcr_cum_l),
  new_NACE = Map(setdiff, NACE_cum, NACE_cum_l)
)]
ipcr_cumulative[, ipcr_creat:=sapply(new_ipcr, function(x) ifelse(length(x)>0, 1, 0))]
ipcr_cumulative[, ipcr_creat:=ifelse(ipcr_creat==1 & is.na(n_NACE_growth), 0, ipcr_creat)]

saveRDS(ipcr_cumulative, "ipcr_cumulative.RDS")


### product_firm_data_pre_high_growth.RDS

# 0) setup -------------------------------------------------------------------

source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name<-""
output_dir<-paste0(output_dir, "2025/Export 24.04/high_growth_firms_adjust/")
output_dir_creator(output_dir)

cpa_or_pf<-"prodfra_plus"
if(cpa_or_pf=="cpa"){
  ext<-"_cpa"
  digits<-c(0, 1, 2, 4, 6)
  exit_digit<-"exit_6"
}else{
  if(cpa_or_pf=="prodfra_plus")
    ext<-""
  digits<-c(0, 1, 2, 4, 6, 8, 10)
  exit_digit<-"exit_10"
}

# 0) Clean data --------------------------------------------------------------

#Bring in necessary firm and product information
firm_data_select<-readRDS("sbs_br_data_prodcom_firms.RDS") %>% filter(year>2009) # Coming from code c. 
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
product_data<-readRDS(paste0("product_level_growth_", filter_indicator, ext, "_.RDS")) # Coming from code d. 

#' Bring in product, patent and trademark data.
product_summary<-readRDS(paste0("product_creation_destruction", ext, ".RDS"))
patent_tm_data<-readRDS("patent_tm_clean.RDS")
setDT(patent_tm_data)

# Check only prodcom firms are present in the firm_data_select sample
if(length(setdiff(unique(firm_data_select$firmid), unique(product_data$firmid)))!=0){
  stop("Firm data does not contain only prodcom firms. Check modules a and c and come back")
}

window_length<-2

# Create growth measures for patent and tm variables
patent_tm_data<-patent_tm_data[firmid %in% unique(firm_data_select$firmid) | firmid %in% unique(product_data$firmid)]
patent_growth<-growth_creator(patent_tm_data, "total_patent", window_length) %>% select(firmid, year, total_patent_l, total_patent_bar, total_patent_growth)
tm_growth<-growth_creator(patent_tm_data, "total_tm", window_length) %>% select(firmid, year, total_tm_l, total_tm_bar, total_tm_growth)
patent_tm_data<-merge(patent_tm_data, patent_growth, by=c("firmid", "year"), all.x=T)
patent_tm_data<-merge(patent_tm_data, tm_growth, by=c("firmid", "year"), all.x=T)

#' Create a two year time window for patenting and trademark after transforming the p and tm info into dummies.
patent_tm_data[, patent:=ifelse(num_patent<=0 | is.na(num_patent), 0, 1)]
patent_tm_data<-window_var_cretor(patent_tm_data, "firmid", "year", "patent", window_length, 0, "patent_window_temp", na_rm=T)
patent_tm_data[, tm:=ifelse(num_tm<=0 | is.na(num_tm), 0, 1)]
patent_tm_data<-window_var_cretor(patent_tm_data, "firmid", "year", "tm", window_length,0, "tm_window_temp", na_rm=T)

#' Bring this information into the product data. Create an independent dummy and two year time window for p and tm.
product_summary<-merge(product_summary, patent_tm_data, by=c("firmid", "year"), all.x = T)
setDT(product_summary)
product_summary[, total_patent_growth:=ifelse(is.na(total_patent_growth), 0, total_patent_growth)]
product_summary[, total_tm_growth:=ifelse(is.na(total_tm_growth), 0, total_tm_growth)]
product_summary[, patent:=ifelse(num_patent<=0 | is.na(num_patent), 0, 1)]
product_summary<-window_var_cretor(product_summary, "firmid", "year", "patent", window_length, 0, "patent_window", na_rm=T)
product_summary[, tm:=ifelse(num_tm<=0 | is.na(num_tm), 0, 1)]
product_summary<-window_var_cretor(product_summary, "firmid", "year", "tm", window_length, 0, "tm_window", na_rm=T)


#' Because some p and tm is left out of the product data when merging (all.x=T), the new time window may
#' be overlooking p and tms granted before the start of the product panel. Adjust patent_window for this.
table(product_summary$patent_window, useNA = "always") # This should be only 1 or 0, no NAs
table(product_summary$tm_window, useNA = "always") # This should be only 1 or 0, no NAs
product_summary[, patent_window:=ifelse(patent_window==0 & patent_window_temp==1 & !is.na(patent_window_temp), 1, patent_window)]
product_summary[, tm_window:=ifelse(tm_window==0 & tm_window_temp==1 & !is.na(tm_window_temp), 1, tm_window)]
product_summary[, log_n_products:=log(number_of_products)]

# Adjust product creation and destruction measures as net variables (new-old)
product_summary[, `:=`(net_product_change=new_products-destroyed_products)]
product_summary[, `:=`(net_product_creat=ifelse(net_product_change>0, 1, 0),
                       net_product_destr=ifelse(net_product_change<0, 1, 0))]

#' Create windows for product creation and destruction variables
#' emember that our product data is left censored, so time windows that go to years before our first data point should be NAs (na_rm=F, )
product_summary<-window_var_cretor(product_summary, "firmid", "year", "net_product_creat", window_length, 0, "net_product_creat_window", na_rm=F) 
product_summary<-window_var_cretor(product_summary, "firmid", "year", "net_product_destr", window_length, 0, "net_product_destr_window", na_rm=F)

# Adjust firm age and merge firm with firmdata select
# firm_data<-readRDS('sbs_br_combined_cleaned.rds') #Coming from "a. Data preparation.R" part 2
# firm_age<-firm_data[, .(birth_year_adj = min(year)), by = .(firmid)]
# saveRDS(firm_age, "BR_earliest_year_firm_birth.RDS")
# firm_data_select<-merge(firm_data_select, firm_age, by="firmid", all.x = T)
# firm_data_select<-firm_data_select[, birth_year_adj:=ifelse(birth_year_adj==)]
# firm_data_select<-firm_data_select[, firm_age:=(year-birth_year_adj)]
product_summary<-merge(product_summary, firm_data_select, by=c("firmid", "year"), all.x = T)

# Bring in NUTS information
nuts<-fread("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Ancillary datasets/NUTS/nuts_soe_addition.csv")
nuts_conc<-fread("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Ancillary datasets/NUTS/nuts_conc.csv")

# Clean it and merge it to product_summary
nuts[, firmid:=str_pad(as.character(siren), 9, side="left", pad="0")]
nuts<-nuts[firmid %in% unique(product_summary$firmid)]
nuts<-nuts[, c("firmid", "year", "nuts3")]
nuts<-merge(nuts, nuts_conc, by.x="nuts3", by.y="nuts2013", all.x=T)
nuts[, nuts3:=fifelse(!is.na(nuts2016), nuts2016, nuts3)]
nuts[, nuts3:=fifelse(nuts3=="",NA_character_, nuts3)]
nuts<-nuts[, c("firmid", "year", "nuts3")]# firmid==445045537
product_summary<-merge(product_summary, nuts, by=c("firmid", "year"), all.x = T)

# Bring in IPCR information, clean it and merdge it to product_summary
ipcr_cumulative<-readRDS("ipcr_cumulative.RDS")
product_summary<-merge(product_summary, ipcr_cumulative, by=c("firmid", "year"), all.x=T)
product_summary[, `:=`(ipcr_creat=fifelse(is.na(ipcr_creat), 0, ipcr_creat))]
product_summary<-window_var_cretor(product_summary, "firmid", "year", "ipcr_creat", 2, 0, "ipcr_creat_window", na_rm=F)
rev_growth<-growth_creator(product_summary, "rev", 1) %>% select(firmid, year,rev_l, rev_bar, rev_growth)
product_summary<-merge(product_summary, rev_growth, by=c("firmid", "year"), all.x = T)
product_summary[, ever_patent:=as.numeric(any(patent)), by=firmid]
product_summary[, ever_tm:=as.numeric(any(tm)), by=firmid]

saveRDS(product_summary, "product_firm_data_pre_high_growth.RDS")


