"
# This script analyzes product reallocation and firm dynamics by identifying 
# and tracking core products across multiple levels of product aggregation 
# (1, 2, 4, 6 digits for CPA; optionally up to 10 for ProdFra+). 
# It:
#   - Loads firm and product-level data
#   - Verifies consistency of input datasets
#   - Defines and tracks core product categories within firms
#   - Calculates product entries and exits by aggregation level
#   - Outputs datasets enriched with core product and transition metrics

Author: Juli?n D?az-Acosta
Last update: 27/02/2025
"

# 0) setup -------------------------------------------------------------------
source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name<-""
output_dir<-paste0(output_dir, "2025/Export 10.04/")
output_dir_creator(output_dir)
dummy<-T

# 4.2) Patent data creation updated-------------------

# Bring in new version of the patent dataset, turn it into a data table and remove 
# records with NA siren codes (probably these were no matched by the algorithm)
patent_data_upd<-read_parquet("tm_patent/patent_record_level_final.parquet")
setDT(patent_data_upd)
patent_data_upd<- patent_data_upd[!(is.na(siren))]

# Some patents are held by multiple firms. Siren codes are pasted together separated by commas
# Create a function to split siren codes and expand them into new rows
expand_siren_codes<-function(data, firm_var){
  data<-copy(patent_data_upd)
  
  # Split siren codes into a list
  setDT(data)
  data[, siren_list:=strsplit(get(firm_var), ",")]

  # Expand rows: One row per siren
  data <- data[, .(firmid=unlist(siren_list),
                   original_siren_count = length(siren_list[[1]])),
               by=setdiff(names(data), "siren_list")]
  
  return(data)
}

patent_data_upd<-expand_siren_codes(patent_data_upd, "siren")

# # Some patents appear twice in the dataset because they can be WO, FR and EP datasets 
# # Assume that all records that share title, siren code and application year are a single patent
# patent_data_upd<-patent_data_upd[, .(collection=paste(collection, collapse = ", ")), by=.(title, firmid, application_year, ipcr)] 
# patent_data_upd<-patent_data_upd[, .(n_applications=.N), by=.(firmid, application_year)]
# patent_data_upd<-patent_data_upd %>% rename(year=application_year)
# setorder(patent_data_upd, firmid, year)




# Clean and process patent data
test <- patent_data_upd[, `:=`(
  # Remove all spaces from the 'ipcr' field and assign to a new column 'ipcr_upd'
  ipcr_upd = gsub(" ", "", ipcr),
  
  # Create a unique sequential ID for each row (used later to unnest IPCR codes)
  patent_id = .I  
)] %>%
  
  # Add a column with the number of characters in the cleaned 'ipcr_upd' field
  .[, nchar_upd := nchar(ipcr_upd)] %>%
  
  # Split the cleaned IPCR string by commas into a list column
  .[, ipcr_list := strsplit(ipcr_upd, ",")] %>%
  
  # Unnest the list of IPCR codes into separate rows
  .[, .(ipcr = unlist(ipcr_list)),
    by = .(firmid, application_year, patent_id)
  ]
# test<-unique(test[, ipcr4:=substr(ipcr, 1, 4)][, -"ipcr"])

n_ipc<-test[, .(n_patents=n_distinct(patent_id)), by=.(ipcr)][, share_patents:=(n_patents/sum(n_patents))*100]

nace_ipc_concord<-fread("IPC V8_NACE Rev 2.txt")

test2<-test[firmid=="005520242"]

A61K8
A61K
B65F
B65F1
B65F3
B65F5
B65F7
B65F9



# Step 1: Create all firm-year combinations up to their max year
years_per_firm <- test2[, .(min_year = min(application_year), max_year = max(application_year)), by = firmid]
all_years <- years_per_firm[
  , .(application_year = min_year:max_year), by = firmid
]

# Step 2: Merge with original test data
test_expanded <- merge(all_years, test, by = c("firmid", "application_year"), all.x = TRUE)



# Step 3: For each firm-year, get all IPCR codes up to that year
setorder(test_expanded, firmid, application_year)

# Step 4: Compute cumulative IPCRs
ipcr_cumulative <- test_expanded[
  , .(year = application_year, ipcr4), by = .(firmid)
][
  , .(application_year = unique(year)), by = firmid
][
  , .(application_year = application_year,
      ipcr_cum = lapply(application_year, function(yr) {
        unique(test[firmid == .BY$firmid & application_year <= yr, ipcr4])
      })),
  by = firmid
]

ipcr_cumulative_fwd<-copy(ipcr_cumulative)
ipcr_cumulative_fwd[, application_year:=application_year+1]
normal_cols<-"ipcr_cum"
lag_cols<-paste0(normal_cols, "_l")
colnames(ipcr_cumulative_fwd)[names(ipcr_cumulative_fwd) %in% normal_cols] = lag_cols
ipcr_cumulative <- merge(ipcr_cumulative, ipcr_cumulative_fwd, by=c("firmid", "application_year"), all.x=T) 
ipcr_cumulative<-ipcr_cumulative %>% mutate(new_ipcr = map2(ipcr_cum, ipcr_cum_l, setdiff))

ipcr_cumulative[, ipcr_cum_unlist:=paste(unlist(ipcr_cum), collapse = ",")]


n_ipc<-test[, .(n_patents=n_distinct(patent_id)), by=.(ipcr4)]

saveRDS(patent_data_upd, "patent_apps.RDS")
