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
ggplot(patents_per_family, aes(x = asinh(n_distinct_control1))) +
  geom_histogram( fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(1, max(patents_per_family$n_distinct_control1, na.rm = TRUE), by = 1)) +
  labs(title = "Distribution of Patent Records per Patent Family",
       subtitle = "INPI sample",
       x = "log Number of Patent Records per Patent Family",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x axis labels
  scale_x_continuous(breaks = seq(0, max(patents_per_family$n_distinct_control1, na.rm = TRUE), by = 10)) +  
  theme_minimal()
ggsave("patent_data/patents_per_family_histogram_inpi.png", width = 10, height = 6)

summary_stats <- patents_per_family[, .(
  mean = mean(n_distinct_control1, na.rm = TRUE),
  median = median(n_distinct_control1, na.rm = TRUE),
  sd = sd(n_distinct_control1, na.rm = TRUE),
  min = min(n_distinct_control1, na.rm = TRUE),
  max = max(n_distinct_control1, na.rm = TRUE),
  n = .N
)]

#and export well-formatted for publication in Excel format
writexl::write_xlsx(summary_stats, "patent_data/patents_per_family_summary_stats_inpi.xlsx")

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

{
  # Prepare data for lexisnexis ----
  
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
  ## Plots Alex ----------------------------------------------------------
  
  alex_patents_lexis_nexis <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet")
  
  patents_per_family_alex <- alex_patents_lexis_nexis[, .(n_distinct_control1 = uniqueN(control1)), by = .(patent_family, siren)][!is.na(patent_family)]
  
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
  ## Find records in the alex dataset with same title and with the same siren but different patent family ---------------------------------
  
  alex_patents_lexis_nexis <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet")
  
  duplicate_titles <- alex_patents_lexis_nexis[!is.na(title.x) & !is.na(patent_family), .N, by = .(title.x, siren)][N > 1, title.x]
  duplicate_records <- alex_patents_lexis_nexis[title.x %in% duplicate_titles, .(title.x, patent_family, control1, siren)] 
  duplicate_records <- duplicate_records[, .(n_families=n_distinct(patent_family)), by = .(title.x, siren)][n_families>1]
  duplicate_records <- merge(duplicate_records, alex_patents_lexis_nexis, by=c("title.x", "siren"), all.x=TRUE)
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
  
  #and export well-formatted for publication in Excel format
  writexl::write_xlsx(summary_overlap, "patent_data/summary_overlap_inpi_alex.xlsx")
}
# 3) Bring together and clean patent data -------------
{
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
  
  # per source, divide into 100 groups with the same number of obs
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
  write_parquet(final_patents, "3_patent_lvl_patent_dta.parquet", compression = "snappy")
  
}
# 4) Text similarity analysis to assign prodcom codes to patents --------------------
prodcom_codes <- read_delim("Ancillary datasets/prodcom codes.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% setDT(.) %>%
  .[nchar(code)>4]

alex_patent_data <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet") %>% setDT(.)
test <- alex_patent_data[!is.na(title.y) & !is.na(abstract)] %>%
  .[patent_family=="FR2912244.A1"] %>%
  select(title.y, abstract) %>%
  rename(title = title.y, abstract = abstract)

Sys.setenv(OPENAI_API_KEY = "") #

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

openxlsx::write.xlsx(test2, "patent_data/patent_prodcom_likelihoods.xlsx")

# 5) Patent citations cleaning -------------------------------------------------------

prior_art_folder <- "patent_data/prior_art/"

# 5.1 Create citation information dataset
{
  # Read prior art data
  patent_data <- read_parquet("3_patent_lvl_patent_dta.parquet")
  
  
  # Unzip all zip files in the prior art folder
  zip_files <- list.files(prior_art_folder, pattern = "\\.zip$", full.names = TRUE)
  for (zip_file in zip_files) {
    folder_name <- file.path(prior_art_folder, tools::file_path_sans_ext(basename(zip_file)))
    if (!dir.exists(folder_name)) dir.create(folder_name, recursive = TRUE, showWarnings = FALSE)
    unzip(zip_file, exdir = folder_name)
  }
  # List all CSV files in the prior art folder (including subfolders)
  csv_files_prior_art <- list.files(
    prior_art_folder,
    pattern = "\\.csv$", full.names = TRUE, recursive = TRUE
  )
  
  # Read each csv file, and clean it in the following way:
  # 1) name columns: patent_family, prior_art
  # 2) prior_art is of the form: *text*(patent_number et al). Extract only the patent_number)
  # 3) using patent_data, bring in patent_family (key=patent_family in both datasets) firmid and prior_art firmid (key=prior_art in prior art and patent_family in patent_data)
  prior_art_list <- lapply(csv_files_prior_art, function(f) {
    print(f)
    dt <- as.data.table(read_csv(f, skip = 8))
    setnames(dt, tolower(gsub(" ", "_", names(dt))))
    # names(dt)[names(dt)=="prior_art_(owner_and_document_number)"] <- "prior_art"
    dt <- dt[, .(patent_family, `prior_art_(owner_and_document_number)`)]
    dt[, prior_art := gsub("\\s+", "", sub(".*\\(([^ ]+).*", "\\1", `prior_art_(owner_and_document_number)`))] # Extract patent number before 'et al' and remove spaces
    
    # dt <- rbindlist(prior_art_list, use.names = TRUE, fill = TRUE)
    dt <- unique(dt)
    dt <- merge(
      dt, unique(patent_data[, .(patent_family, firmid)]),
      by = "patent_family", all.x = TRUE, suffixes = c("", "_citing")
    )
    setnames(dt, "firmid", "firmid_current")
    dt <- merge(
      dt, unique(patent_data[, .(patent_family, firmid)]),
      by.x = "prior_art", by.y = "patent_family", all.x = TRUE, suffixes = c("", "_cited")
    )
    setnames(dt, "firmid", "firmid_prior")
    # Replace NA firmid_prior with "unknown"
    dt[is.na(firmid_prior), firmid_prior := "unknown"]
    
    return(dt)
    
  })
  
  
  prior_art_dt<-rbindlist(prior_art_list, use.names = TRUE, fill = TRUE)
  rm(prior_art_list); gc()
  prior_art_dt <- unique(prior_art_dt)
  
  write_rds(prior_art_dt, paste0(prior_art_folder, "consolidated_prior_art.RDS"))
  
}

# 5.2 Process citation information dataset
{
  prior_art_dt <- read_rds(paste0(prior_art_folder, "consolidated_prior_art.RDS"))
  product_competitors_data <- read_rds(paste0("2_product_data/", cpa_or_pf, "/2d_firm_lvl_product_competitors_dta.RDS"))
  firm_industry_data <- as.data.table(read_parquet("firm_lists/firmid_NACE_BR_year.parquet"))
  patent_data <- load_dataset(
    c("3_patent_lvl_patent_dta.parquet", "patent_data/3_patent_lvl_patent_dta.parquet"),
    read_parquet
  ) |> to_dt()
  industry_data <- load_dataset(
    c("5_industry_yr_lvl_dta.parquet"),
    read_parquet
  ) |> to_dt()
  
  # Bring in year information to patent citations, which doesn't have it
  prior_art_dt <- merge(
    prior_art_dt,
    unique(patent_data[, .(
      patent_family,
      year = filing_year
    )]),
    by = "patent_family",
    all.x = TRUE
  )
  
  # Bring in PC competitors information
  prior_art_dt <- merge(
    prior_art_dt, 
    product_competitors_data,
    by.x=c("firmid_current", "year"),
    by.y=c("firmid", "year"), 
    all.x=T
  )
  
  # Bring in BR competitors information
  prior_art_dt <- merge(
    prior_art_dt, 
    firm_industry_data[, .(firmid, year, NACE_BR)],
    by.x=c("firmid_current", "year"),
    by.y=c("firmid", "year"), 
    all.x=T
  )
  
  prior_art_dt <- merge(
    prior_art_dt, 
    industry_data[, .(NACE_BR, year, leader_BR, top_4_BR, top_10_BR)],
    by=c("NACE_BR", "year"), 
    all.x=T
  )
  
  write_rds(prior_art_dt, paste0(prior_art_folder, "prior_art_cite_temp.RDS"))
}

# 5.3 Process citation information dataset
{
  prior_art_dt <- as.data.table(read_rds(paste0(prior_art_folder, "prior_art_cite_temp.RDS")))
  
  # View(prior_art_dt[patent_family=="EP2549788.A1"])
  
  # prior_art_dt[, `:=`(
  #   cite_leaders_PC_dummy = mapply(a_in_b_dt, firmid_prior, leaders, USE.NAMES = FALSE),
  #   # cite_top_4_PC_dummy = mapply(a_in_b_dt, firmid_prior, top_4_leaders, USE.NAMES = FALSE),
  #   cite_top_10_PC_dummy = mapply(a_in_b_dt, firmid_prior, top_10_leaders, USE.NAMES = FALSE),
  #   cite_competitors_PC_dummy = mapply(a_in_b_dt, firmid_prior, firms_in_pc, USE.NAMES = FALSE),
  #   cite_leaders_BR_dummy = mapply(a_in_b_dt, firmid_prior, leader_BR, USE.NAMES = FALSE),
  #   # cite_top_4_BR_dummy = mapply(a_in_b_dt, firmid_prior, top_4_BR, USE.NAMES = FALSE),
  #   cite_top_10_BR_dummy = mapply(a_in_b_dt, firmid_prior, top_10_BR, USE.NAMES = FALSE)
  #   # cite_competitors_BR_dummy = mapply(a_in_b_dt, firmid_prior, competitors_BR, USE.NAMES = FALSE)
  # )]
  # 
  # write_rds(prior_art_dt, paste0(prior_art_folder, "prior_art_cite_temp2.RDS"))
  # 
  # prior_art_dt <- as.data.table(read_rds(paste0(prior_art_folder, "prior_art_cite_temp2.RDS")))
  
  # set(prior_art_dt, j=cite_leaders_PC_dummy, value=unlist(prior_art_dt[[cite_leaders_PC_dummy]], use.names=FALSE))
  # prior_art_dt[, cite_leaders_PC_dummy:=unlist(cite_leaders_PC_dummy)]
  # typeof(prior_art_dt$cite_leaders_PC_dummy)
  # prior_art_dt[, cite_top_10_PC_dummy:=unlist(cite_top_10_PC_dummy)]
  # prior_art_dt[, cite_competitors_PC_dummy:=unlist(cite_competitors_PC_dummy)]
  # prior_art_dt[, cite_leaders_BR_dummy:=unlist(cite_leaders_BR_dummy)]
  # prior_art_dt[, cite_top_10_BR_dummy:=unlist(cite_top_10_BR_dummy)]
  
  
  
  # list_cols <- sapply(prior_art_dt, is.list)
  # for(col in list_cols){
  #   set(prior_art_dt, j=col, value=unlist(prior_art_dt[[col]], use.names=FALSE))
  # }
  
  # Now collapse, in the following way:
  # for each patent_family: 
  # number of distinct prior_art, 
  # number of times firmid_current== firmid_prior (self-citations), 
  # number of times firmid_current != firmid_prior (external citations)
  # vector with unique prior_art,
  # vector with unique firmid_prior
  dt_summary <- prior_art_dt[, .(
    n_distinct_prior_art = uniqueN(prior_art),
    n_prior_art=.N,
    n_self_citations = sum(firmid_current == firmid_prior, na.rm=T),
    n_external_citations = sum(firmid_current != firmid_prior, na.rm = T)
    # n_leaders_PC_citations = sum(cite_leaders_PC_dummy, na.rm=T),
    # n_top_10_PC_citations = sum(cite_top_10_PC_dummy, na.rm=T),
    # n_competitors_PC_citations = sum(cite_competitors_PC_dummy, na.rm=T),
    # n_leaders_BR_citations = sum(cite_leaders_BR_dummy, na.rm=T),
    # n_top_10_BR_citations = sum(cite_top_10_BR_dummy, na.rm=T),
    # # n_competitors_BR_citations = sum(cite_competitors_BR_dummy, na.rm=T),
    # # prior_art_list = list(unique(prior_art)),
    # firmid_prior_list = list(unique(firmid_prior))
  ), by = .(patent_family, prior_art)]
  
  # write_rds(prior_art_dt, paste0(prior_art_folder, "prior_art_cite_temp3.RDS"))
  
  
  dt_summary[,`:=`(# self_citation_dummy=fifelse(n_self_citations>0, 1, 0),
    share_self_citation = safe_ratio(n_self_citations, n_prior_art),
    share_external_citation = safe_ratio(n_external_citations, n_prior_art)
    # share_leaders_PC_citations = safe_ratio(n_leaders_PC_citations, n_prior_art),
    # share_top_10_PC_citations = safe_ratio(n_top_10_PC_citations, n_prior_art),
    # share_competitors_PC_citations = safe_ratio(n_competitors_PC_citations, n_prior_art),
    # share_leaders_BR_citations = safe_ratio(n_leaders_BR_citations, n_prior_art),
    # share_top_10_BR_citations = safe_ratio(n_top_10_BR_citations, n_prior_art)
    # share_competitors_BR_citations = safe_ratio(n_competitors_BR_citations, n_prior_art)
  )]
  
  dt_summary_patent_family <- dt_summary[, .(
    share_self_citation = safe_mean(share_self_citation),
    share_external_citation = safe_mean(share_external_citation)
    # share_leaders_PC_citations = safe_mean(share_leaders_PC_citations),
    # share_top_10_PC_citations = safe_mean(share_top_10_PC_citations),
    # share_competitors_PC_citations = safe_mean(share_competitors_PC_citations),
    # share_leaders_BR_citations = safe_mean(share_leaders_BR_citations),
    # share_top_10_BR_citations = safe_mean(share_top_10_BR_citations),
    # # share_competitors_BR_citations = safe_mean(share_competitors_BR_citations),
    # prior_art_list = list(unique(prior_art)),
    # firmid_prior_list = list(unique(unlist(firmid_prior_list)))
  ), by = .(patent_family)]
  
  dt_summary_lists <- prior_art_dt[, .(
    prior_art_list = list(unique(prior_art)),
    firmid_prior_list = list(unique(unlist(firmid_prior)))
  ), by=.(patent_family)]
  
  dt_summary_patent_family <- merge(
    dt_summary_patent_family,
    dt_summary_lists, 
    by="patent_family", 
    all.x=T)
  
  patent_data <- read_parquet("3_patent_lvl_patent_dta.parquet")
  dt_summary_patent_family <- merge(
    dt_summary_patent_family,
    unique(patent_data[, .(patent_family, filing_year)]), 
    by="patent_family", 
    all.x=T)
  
  write_rds(dt_summary_patent_family, "3b_patent_citation_summary.RDS")
}

# 6) patent citations plots -------------------------------------------------------

dt_summary <- read_rds("3b_patent_citation_summary.RDS")

dt_summary_by_year <- dt_summary[, .(
  mean_n_distinct_prior_art = mean(n_distinct_prior_art, na.rm = TRUE),
  mean_n_prior_art = mean(n_prior_art, na.rm = TRUE),
  mean_n_self_citations = mean(n_self_citations, na.rm = TRUE),
  mean_n_external_citations = mean(n_external_citations, na.rm = TRUE),
  mean_share_self_citation = mean(share_self_citation, na.rm=TRUE),
  mean_share_external_citation = mean(share_external_citation, na.rm=TRUE),
  n= .N
), by = filing_year]

ggplot(dt_summary_by_year[filing_year>=1994 & filing_year<=2017], aes(x = filing_year)) +
  geom_line(aes(y = mean_n_distinct_prior_art, color = "Mean Distinct Prior Art")) +
  geom_line(aes(y = mean_n_prior_art, color = "Mean Prior Art")) +
  # geom_line(aes(y = mean_n_self_citations, color = "Mean Self Citations")) +
  geom_line(aes(y = mean_n_external_citations, color = "Mean External Citations")) +
  labs(
    title = "Mean Share of Self-Citations and External Citations Over Time",
    x = "Filing Year",
    y = "Mean Share",
    color = "Citation Type"
  ) +
  theme_minimal()


ggplot(dt_summary_by_year[filing_year>=1994 & filing_year<=2017], aes(x = filing_year)) +
  geom_line(aes(y = mean_share_self_citation, color = "Mean Share of Self-Citations")) +
  # geom_line(aes(y = mean_share_external_citation, color = "Mean Share of External Citations")) +
  labs(
    title = "Mean Share of Self-Citations and External Citations Over Time",
    x = "Filing Year",
    y = "Mean Share",
    color = "Citation Type"
  ) +
  theme_minimal()
prior_art_data <- rbindlist(prior_art_list, use.names = TRUE, fill = TRUE)
prior_art_data <- unique(prior_art_data)
prior_art_data <- merge(
  prior_art_data, patent_data[, .(patent_family, firmid)],
  by = "patent_family", all.x = TRUE, suffixes = c("", "_citing")
)
setnames(prior_art_data, "firmid", "firmid_citing")
prior_art_data <- merge(
  prior_art_data, patent_data[, .(patent_family, firmid)],
  by.x = "prior_art", by.y = "patent_family", all.x = TRUE, suffixes = c("", "_cited")
)
setnames(prior_art_data, "firmid", "firmid_cited")
# 7) Compare firmids-patent FAMILIES in inpi and alex data -------------------------------------------------------------------------

inpi_patent_data <- read_parquet("patent_data/a_patents_deposants.parquet") %>% setDT(.)
alex_patent_data <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet") %>% setDT(.)

inpi_patent_data <- inpi_patent_data[, .(siren=list(unique(siren))), by=patent_family]
alex_patent_data <- alex_patent_data[, .(siren=list(unique(siren))), by=patent_family]

patent_source <- merge(patent_source[source=="both", .(patent_family)], 
                       inpi_patent_data,
                       by="patent_family",
                       all.x=TRUE)
setnames(patent_source, "siren", "siren_inpi")
patent_source <- merge(patent_source, 
                       inpi_patent_data,
                       by="patent_family",
                       all.x=TRUE,
                       allow.cartesian=F)
setnames(patent_source, "siren", "siren_alex")

# helper to normalise list-columns to character vectors without NA
norm_list <- function(x) {
  if (is.null(x)) return(character(0))
  v <- unlist(x)
  v <- v[!is.na(v)]
  as.character(v)
}

a_list <- lapply(patent_source$siren_inpi, norm_list)
b_list <- lapply(patent_source$siren_alex, norm_list)

same_exact <- mapply(function(a,b) identical(a,b), a_list, b_list)
same_set   <- mapply(function(a,b) setequal(a,b), a_list, b_list)
overlap_count <- mapply(function(a,b) length(intersect(a,b)), a_list, b_list)
union_count   <- mapply(function(a,b) length(union(a,b)), a_list, b_list)
jaccard <- ifelse(union_count == 0, NA_real_, overlap_count / union_count)

patent_source[, `:=`(
  same_exact = same_exact,
  same_set = same_set,
  overlap_count = overlap_count,
  union_count = union_count,
  jaccard = jaccard
)]

# quick summaries
patent_source[, .(
  total = .N,
  n_same_exact = sum(same_exact),
  n_same_set_only = sum(same_set & !same_exact),
  n_no_overlap = sum(overlap_count == 0)
)]

# 8) Compare firmids-patent APPLICATIONS in inpi and alex data

inpi_patent_data <- read_parquet("patent_data/a_patents_deposants.parquet") %>% setDT(.)
inpi_patent_records <- as.data.table(fread("G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/2 Data/patent_data/deposants-des-brevets_clean2.csv")) %>%
  .[, siren:=str_pad(as.character(siren), 9, side="left", "0") ]
inpi_patent_records <- merge(inpi_patent_records,
                             inpi_patent_data[, .(key_appln_nr=control1, patent_family)],
                             by="key_appln_nr",
                             all.x = T) %>% unique()
inpi_patent_records <- inpi_patent_records[, .(siren_list=list(unique(siren))), by=.(key_appln_nr, patent_family)]


alex_patent_data <- read_parquet("patent_data/b_alex_patents_lexis_nexis.parquet") %>% setDT(.)
alex_patents_scrapped <- as.data.table(read_parquet(paste0("patent_data/patent_record_level_final.parquet"))) 
alex_patents_scrapped <- merge(alex_patents_scrapped[, .(publication_number=paste0(collection, publication_number), siren)] ,
                               alex_patent_data[, .(publication_number=control1, patent_family)],
                               by="publication_number",
                               all.x = T) %>% unique()
alex_patents_scrapped[, siren_list := strsplit(siren, ",")]
alex_patents_scrapped <- alex_patents_scrapped[, .(siren_list=list(unique(siren))), by=.(publication_number, patent_family)]



overlap <- merge(inpi_patent_records[, .(siren=siren_list, app_n=key_appln_nr)], 
                 alex_patents_scrapped[, .(siren=siren_list, app_n=application_number)],
                 by=c("app_n"),
                 suffix=c("_inpi", "_alex"))


patent_source <- fread("patent_data/patent_families_per_source.csv")





