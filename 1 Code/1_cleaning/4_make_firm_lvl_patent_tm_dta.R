# setup
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# Set parameters ------------------------------------------------------------------
patent_folder <- "patent_data/PatentSight_Export_2025-10-01-15-38-299"


# List all CSV files in the folder
csv_files <- list.files(
    patent_folder,
    pattern = "\\.csv$", full.names = TRUE
)

# Read all CSV files, skipping first 8 lines, and process control1 columns
patent_list <- lapply(csv_files, function(f) {
    dt <- as.data.table(read_csv(f, skip = 8))
    dt[, control1 := fifelse(
        control1 == "<unknown>" & control1_round2 != "<unknown>", control1_round2,
        fifelse(control1 == "<unknown>" & control1_round2 == "<unknown>", NA_character_, control1)
    )]
    dt[, control1_round2 := NULL]
    return(dt)
})

# Combine all patent data and remove duplicates
patents <- rbindlist(patent_list, use.names = TRUE, fill = TRUE)
patents <- unique(patents)

# Read and process deposants data
deposants <- as.data.table(read_csv(paste0("patent_data/deposants-des-brevets_clean2.csv")))
deposants <- unique(deposants[, .(key_appln_nr, siren)])

# Merge datasets
patents_deposants <- merge(
    patents, deposants,
    by.x = "control1", by.y = "key_appln_nr", all.x = TRUE
)

# Clean column names
setnames(patents_deposants, tolower(gsub(" ", "_", names(patents_deposants))))

# Count number of filings, publications, and grants per firm and year
n_filings <- patents_deposants[, .(n_applications = n_distinct(patent_family)), by = .(siren, filing_year)] %>% rename(year = filing_year)
n_publications <- patents_deposants[, .(n_publications = n_distinct(patent_family)), by = .(siren, publication_year)] %>% rename(year = publication_year)
n_grants <- patents_deposants[, .(n_grants = n_distinct(patent_family)), by = .(siren, grant_year)] %>% rename(year = grant_year)

# Merge results
output <- merge(n_filings, n_publications, by = c("siren", "year"), all = TRUE) %>% merge(n_grants, by = c("siren", "year"), all = TRUE)

# Save output
fwrite(output, "4_firm_yr_lvl_patent_dta.csv")
