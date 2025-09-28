# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))
output_dir <- paste0(output_dir, "2025/Export 22.05/")
output_dir_creator(output_dir)
dummy <- F

##define component datasets for later use & export results 
birth_death = unique(firm_yr_lvl_br_dta %>% select(firmid, birth_year, death_year))
active_firm_list = firm_yr_lvl_br_dta[year <= death_year | is.na(death_year)] %>% select(firmid, year)

# Juli?n: Only firmid, year, nace information
NACE_BR_data <- firm_yr_lvl_br_dta[, c("firmid", "year", "NACE_BR", "NACE_2d")]
