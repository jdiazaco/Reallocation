# setup
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# Set parameters ------------------------------------------------------------------
patentsight_inpi_folder <- "patent_data/PatentSight_Export_2025-10-01-15-38-299"
patentsight_alex_folder <- "patent_data/alex_patents"
ipc_info_folder <- "patent_data/ipc_information"

# 1) INPI patent data cleaning ------------------------------------------------------

inpi_patent_records <- fread("G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/2 Data/patent_data/deposants-des-brevets_clean2.csv")


# List all CSV files in the folder
csv_files <- list.files(
    patentsight_inpi_folder,
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

# Check how many distinct control1 obs (patent ids) are within a family
deposants<- merge(deposants, patents[, .(control1, `Patent Family`)], by.x = "key_appln_nr", by.y = "control1", all.x = TRUE)
patents_per_family <- deposants[, .(n_distinct_control1 = uniqueN(key_appln_nr)), by = `Patent Family`][!is.na(`Patent Family`)]
patents_deposants <- merge(patents_deposants, patents_per_family, by="Patent Family", all.x=TRUE)
setnames(patents_deposants, tolower(gsub(" ", "_", names(patents_deposants))))

write_parquet(patents_deposants, "patent_data/a_patents_deposants.parquet", compression = "snappy")

patents_per_family <- patents_deposants[, .(n_distinct_control1 = uniqueN(control1)), by =.(siren, patent_family)]#[, .N, by = n_distinct_control1]
# patents_deposants <- merge(patents_deposants, patents_per_family, by = "patent_family", all.x = TRUE)

# Create summary statistics of n_distict_control1 and plot histogram
ggplot(patents_per_family, aes(x = n_distinct_control1)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(1, max(patents_per_family$n_distinct_control1, na.rm = TRUE), by = 1)) +
  labs(title = "Distribution of Patent Records per Patent Family",
       x = "Number of Patent Recors per Patent Family",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x axis labels
  scale_x_continuous(breaks = seq(0, max(patents_per_family$n_distinct_control1, na.rm = TRUE), by = 10)) +  
  theme_minimal()
ggsave("patent_data/patents_per_family_histogram_inpi.png", width = 10, height = 6)

summary_stats <- patents_per_family_alex[, .(
  mean = mean(n_distinct_control1, na.rm = TRUE),
  median = median(n_distinct_control1, na.rm = TRUE),
  sd = sd(n_distinct_control1, na.rm = TRUE),
  min = min(n_distinct_control1, na.rm = TRUE),
  max = max(n_distinct_control1, na.rm = TRUE),
  n = .N
)]

#and export well-formatted for publication in Excel format
writexl::write_xlsx(summary_stats, "patent_data/patents_per_family_summary_stats_alex_inpi.xlsx")

# Clean column names
setnames(patents_deposants, tolower(gsub(" ", "_", names(patents_deposants))))
# Count number of filings, publications, and grants per firm and year
n_filings <- patents_deposants[, .(n_applications = n_distinct(patent_family)), by = .(siren, filing_year)] %>% rename(year = filing_year)
n_publications <- patents_deposants[, .(n_publications = n_distinct(patent_family)), by = .(siren, publication_year)] %>% rename(year = publication_year)
n_grants <- patents_deposants[, .(n_grants = n_distinct(patent_family)), by = .(siren, grant_year)] %>% rename(year = grant_year)

# Merge results
output <- merge(n_filings, n_publications, by = c("siren", "year"), all = TRUE) %>% merge(n_grants, by = c("siren", "year"), all = TRUE) 

# Save output
# fwrite(output, "4_firm_yr_lvl_patent_dta.csv")

# 2) Alex patent data cleaning -------------------------------------------------------

alex_patents_scrapped <- as.data.table(read_parquet(paste0("patent_data/patent_record_level_final.parquet"))) 

# Prepare data for lexisnexis --------------------

alex_patents_lexis_info <- alex_patents_scrapped[, n_publication := paste0(collection, publication_number)] %>%
  .[, control1 := n_publication] %>%      
  .[!is.na(siren)] %>%
  .[, .(n_publication, control1, siren)] %>%
  .[, group := 1]

# Save output in chunks of 1 million rows programmatically
chunk_size <- 1000000
n_rows <- nrow(alex_patents_lexis_info)
n_chunks <- ceiling(n_rows / chunk_size)

for (i in seq_len(n_chunks)) {
    start_idx <- ((i - 1) * chunk_size) + 1
    end_idx <- min(i * chunk_size, n_rows)
    fwrite(
        alex_patents_lexis_info[start_idx:end_idx],
        sprintf("patent_data/alex_patents_with_siren_lexis_info%d.csv", i)
    )
}

alex_patents_with_siren_lexis_info2 <- read_csv("patent_data/alex_patents_with_siren_lexis_info2.csv") %>% setDT(.)
# make half the sample gorup 1 and the other group 2
#   alex_patents_with_siren_lexis_info2[, group := ifelse(.I <= nrow(.)/2, 1, 2)]
alex_patents_with_siren_lexis_info2[, group := ifelse(.I <= .N/2, 1, 2)]
fwrite(alex_patents_with_siren_lexis_info2, "patent_data/alex_patents_with_siren_lexis_info2.csv")

# After feeding PatentSight with these files and downloading the results, read them back in and combine them ------------------

# List all CSV files in the folder
csv_files_lexis_nexis_alex <- list.files(
    patentsight_alex_folder,
    pattern = "\\.csv$", full.names = TRUE, recursive = TRUE
)
# Read all CSV files, skipping first 8 lines, and process control1 columns
alex_patents_lexis_nexis_list <- lapply(csv_files_lexis_nexis_alex, function(f) {
    dt <- as.data.table(read_csv(f, skip = 8))
    # dt[, control1 := fifelse(
    #     control1 == "<unknown>" & control1_round2 != "<unknown>", control1_round2,
    #     fifelse(control1 == "<unknown>" & control1_round2 == "<unknown>", NA_character_, control1)
    # )]
    # dt[, control1_round2 := NULL]
    return(dt)
})

# Combine all patent data and remove duplicates
alex_patents_lexis_nexis <- rbindlist(alex_patents_lexis_nexis_list, use.names = TRUE, fill = TRUE) %>%
  .[, control1 := fifelse( # control 1 should be any of the columns (control1_sample3_siren, control1_sample2_siren, control1_sample1_siren) that is not NA
    !is.na(control1_sample3_siren), control1_sample3_siren,
    fifelse(
      !is.na(control1_sample2_siren), control1_sample2_siren,
      fifelse(!is.na(control1_sample1_siren), control1_sample1_siren, NA_character_)
    )
  )] # Delete the other two columns # Up to here nobs is 2672266
names(alex_patents_lexis_nexis) <- tolower(gsub(" ", "_", names(alex_patents_lexis_nexis)))
alex_patents_lexis_nexis[, c("control1_sample1_siren", "control1_sample2_siren", "control1_sample3_siren") := NULL]
alex_patents_lexis_nexis <- unique(alex_patents_lexis_nexis) # Up to here nobs is 2640294 
alex_patents_lexis_nexis[, `:=`(publication_number = sub("^[A-Za-z]{2}", "", control1), # Remove country code from publication number
                                collection = substr(control1, 1, 2)) ] # Keep only country code from publication number 
alex_patents_lexis_nexis <- alex_patents_lexis_nexis[!is.na(publication_number)] # Up to here nobs is 2640294

alex_patents_lexis_nexis <- merge(
    alex_patents_scrapped, alex_patents_lexis_nexis,
    by = c("publication_number", "collection"), all.x = TRUE
) # Up to here nobs is 9101083
alex_patents_lexis_nexis <- alex_patents_lexis_nexis[!is.na(siren)] # Keep only patents with siren and control1 (patent id) # Up to here nobs is 2666083
alex_patents_lexis_nexis <- alex_patents_lexis_nexis[!is.na(control1)] # Keep only patents with siren and control1 (patent id) # Up to here nobs is 2661609

write_parquet(alex_patents_lexis_nexis, "patent_data/b_alex_patents_lexis_nexis.parquet", compression = "snappy")

## Plots Alex  ---------------

patents_per_family_alex <- alex_patents_lexis_nexis[, .(n_distinct_control1 = uniqueN(control1)), by = .(`Patent Family`, siren)][!is.na(`Patent Family`)]

ggplot(patents_per_family_alex, aes(x = asinh(n_distinct_control1))) +
  geom_histogram(fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(1, max(asinh(patents_per_family_alex$n_distinct_control1), na.rm = TRUE), by = 1)) +
  labs(
    title = "Distribution of Patent Records per Patent Family",
    subtitle = "Alex sample",
    x = "Number of Patent Recors per Patent Family",
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x axis labels
  scale_x_continuous(breaks = seq(0, max(patents_per_family_alex$n_distinct_control1, na.rm = TRUE), by = 10)) +
  theme_minimal()
ggsave("patent_data/patents_per_family_histogram_alex.png", width = 10, height = 6)

summary_stats <- patents_per_family_alex[, .(
  mean = mean(n_distinct_control1, na.rm = TRUE),
  median = median(n_distinct_control1, na.rm = TRUE),
  sd = sd(n_distinct_control1, na.rm = TRUE),
  min = min(n_distinct_control1, na.rm = TRUE),
  max = max(n_distinct_control1, na.rm = TRUE),
  n = .N
)]

#and export well-formatted for publication in Excel format
writexl::write_xlsx(summary_stats, "patent_data/patents_per_family_summary_stats_alex.xlsx")

# Summary stats on the overlap between the two datasets ---------------------------------------------------

inpi_patent_data <- read_parquet("patent_data/a_patents_deposants.parquet") %>% setDT(.)
alex_patent_data <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet") %>% setDT(.)

# Create a data table with three columns: siren, patent_family, match (whether the patent family-siren combination is in both datasets)
inpi_unique <- unique(inpi_patent_data[!is.na(siren) & !is.na(patent_family), .(siren, patent_family, filing_year)]) %>% .[, source:="inpi"]
alex_unique <- unique(alex_patent_data[!is.na(siren) & !is.na(patent_family), .(siren, patent_family, filing_year)]) %>% .[, source:="alex"]
overlap <- merge(
  inpi_unique, alex_unique,
  by = c("siren", "patent_family"),
  all = TRUE, suffixes = c("_inpi", "_alex")
)[, match := !is.na(source_inpi) & !is.na(source_alex)
][, source := fifelse(
    !is.na(source_inpi) & !is.na(source_alex), "both",
    fifelse(!is.na(source_inpi), "inpi", "alex")
  )
]

table(overlap$filing_year_inpi, overlap$source)

# Calculate summary statistics and store them in a table
total_inpi <- nrow(inpi_unique)
total_alex <- nrow(alex_unique)
total_overlap <- nrow(overlap[source == "both"])
percent_inpi_in_alex <- (total_overlap / total_inpi) * 100
percent_alex_in_inpi <- (total_overlap / total_alex) * 100
summary_overlap <- data.table(
  total_inpi = total_inpi,
  total_alex = total_alex,
  total_overlap = total_overlap,
  percent_inpi_in_alex = percent_inpi_in_alex,
  percent_alex_in_inpi = percent_alex_in_inpi
)
# 3) Bring together and clean patent data -------------

# Create patent families per source --------------------
inpi_patent_data <- read_parquet("patent_data/a_patents_deposants.parquet") %>% setDT(.)
alex_patent_data <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet") %>% setDT(.)

patent_families <- data.table(
  patent_family = unique(c(inpi_patent_data$patent_family, alex_patent_data$patent_family))
) %>%
  .[, source := fifelse(
    patent_family %in% inpi_patent_data$patent_family,
    fifelse(patent_family %in% alex_patent_data$patent_family, "both", "inpi"),
    "alex"
  )]

write_csv(patent_families, "patent_data/patent_families_per_source.csv")

# Read IPC information files and combine them --------------------

# List all CSV files in the IPC info folder
csv_files_ipc <- list.files(
  ipc_info_folder,
  pattern = "\\.csv$", full.names = TRUE
)

# Read all CSV files and combine them
ipc_list <- lapply(csv_files_ipc, function(f) {
  dt <- as.data.table(read_csv(f, skip= 8))
  return(dt)
})

# Combine all IPC data into a single data.table
ipc_data <- rbindlist(ipc_list, use.names = TRUE, fill = TRUE)
setnames(ipc_data, tolower(gsub(" ", "_", names(ipc_data))))
ipc_data <- unique(ipc_data[, .(publication_number, collection, ipc_code)])


# Merge IPC data with patent data --------------------
common_names <- intersect(names(inpi_patent_data), names(alex_patent_data))
alex_names <- setdiff(names(alex_patent_data), common_names)
inpi_names <- setdiff(names(inpi_patent_data), common_names)
common_names_extended <- c(common_names, "title.x", "title.y")

# Then combine using rbindlist with only the common columns
final_patents <- rbindlist(
  list(
    inpi_patent_data[, c(common_names), with = FALSE],
    alex_patent_data[, c(common_names_extended), with = FALSE]
  ),
  use.names = TRUE, fill = TRUE
)

# Delete duplicate rows based on all columns except title.y
final_patents <- unique(final_patents, by = setdiff(names(final_patents), "title.y"))

# Merge with patent families to get source information
final_patents <- merge(final_patents, patent_families, by = "patent_family", all.x = TRUE)
final_patents <- merge(final_patents, ipc_data, by = "patent_family", all.x = TRUE)
setorder(final_patents, siren, filing_year, source)


# Replace () by nothing and - by _ in column names
setnames(final_patents, tolower(gsub("[()]", "", gsub("-", "_", names(final_patents)))))

# Expand multiple firm IDs per patent 
final_patents[, siren_list := strsplit(siren, ",")]
final_patents <- final_patents[
  , .(firmid = unlist(siren_list)),
  by = setdiff(names(final_patents), "siren_list")
]

# Save final patents data
write_parquet(final_patents, "4_patent_lvl_patent_dta.parquet", compression = "snappy")

# 3) Text similarity analysis to assign prodcom codes to patents --------------------
prodcom_codes <- read_delim("Ancillary datasets/prodcom codes.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% setDT(.) %>%
  .[nchar(code)>4]

test <- patents_alex[!is.na(Title) & !is.na(Abstract)] %>%
  .[1:2] %>%
  select(Title, Abstract) %>%
  rename(title = Title, abstract = Abstract)

# Sys.setenv(OPENAI_API_KEY = "sk-...") # 

similarity_method <- if (nzchar(Sys.getenv("OPENAI_API_KEY"))) "openai_embeddings" else "doc2vec"

likelihoods <- compute_patent_prodcom_likelihood(
  patents = test,
  prodcom = prodcom_codes,
  method = similarity_method,
  patent_text_cols = c("title", "abstract"),
  temperature = 0.07,
  verbose = TRUE
)

test2 <- as.data.table(t(likelihoods), keep.rownames = "prodcom_code")
test2 <- merge(test2, prodcom_codes, by.x = "prodcom_code", by.y = "code", all.x = TRUE)

row_sums <- rowSums(likelihoods)
print(row_sums)



