# Description: Check why some patent records have missing firmid in 4_patent_lvl_patent_dta.parquet

patent_data <- read_parquet("4_patent_lvl_patent_dta.parquet")
setDT(patent_data)

# Subset to rows with missing firmid
test <- patent_data[is.na(firmid)]
alex_patent_records <- read_parquet("G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/2 Data/tm_patent/patent_record_level_final.parquet")
inpi_patent_records <- fread("G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/2 Data/patent_data/deposants-des-brevets_clean2.csv")

setDT(inpi_patent_records)
missing_patents <- unique(test$control1)

# Do key_appln_nr in missing_patents exist in inpi_patent_records?
test2 <- inpi_patent_records[key_appln_nr %in% missing_patents] 

# Per key_appln_nr, do they have at least one record with a firmid in test2?
test3<-test2[, .(has_firmid = any(!is.na(siren))), by = key_appln_nr]
View(test3[has_firmid == FALSE])

# Do key_appln_nr in test3 where has_firmid==false have non-NA siren in inpi_patent_records?
test4 <- inpi_patent_records[key_appln_nr %in% test3[has_firmid == FALSE]$key_appln_nr]
View(test4[!is.na(siren)])
