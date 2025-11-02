# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# bring firm-year level data with birth and death years
firm_yr_lvl_br_dta <- read_parquet("1_firm_yr_lvl_br_dta.parquet")

# define component datasets for later use & export results
birth_death = unique(firm_yr_lvl_br_dta %>% select(
    firmid,
    consolidated_birth_year, economic_birth_year, legal_birth_year, economic_death_year
))

# Check that observations are unique at firm level
if (nrow(birth_death) != length(unique(birth_death$firmid))) {
  stop("There are duplicate firmids in the birth_death data.")
}

# Create active firm list for later use
active_firm_list = firm_yr_lvl_br_dta[year <= economic_death_year | is.na(economic_death_year)] %>% select(firmid, year)

# Export datasets
write_parquet(active_firm_list, "1a_active_firm_list.parquet")
write_parquet(birth_death, "1a_firm_birth_death.parquet")
