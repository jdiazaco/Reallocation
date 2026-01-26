# Create synthetic lifi_data based on the variables used in 6_make_firm_ent_lvl_dta.R

library(data.table)

set.seed(123)


firm_birth_death <- read_parquet("firm_lists/firm_birth_death.parquet")
NACE_BR <- read_parquet("Ancillary datasets/NACE_BR_data.parquet") %>% mutate(firmid = str_pad(firmid, 9, pad = "0", side = "left"))


setDT(firm_birth_death)

# ensure integer years
firm_birth_death[, consolidated_birth_year := as.integer(consolidated_birth_year)]
firm_birth_death[, economic_death_year := as.integer(economic_death_year)]

# drop firms without a birth year
firm_birth_death <- firm_birth_death[!is.na(consolidated_birth_year)]

# set missing death year to birth year (or adjust as needed)
firm_birth_death[is.na(economic_death_year), economic_death_year := consolidated_birth_year]

# ensure death >= birth
firm_birth_death[economic_death_year < consolidated_birth_year, economic_death_year := consolidated_birth_year]

# expand to one row per firm-year between birth and death
firm_years <- firm_birth_death[, .(year = seq(consolidated_birth_year, economic_death_year)), by = .(firmid)]

# firm_years <- merge(firm_years, NACE_BR, by = c("firmid", "year"), all.x = TRUE)


lifi_data <- firm_years[, `:=`(
  # Firm's highest-level group identifier
  firmid_hg = sample(paste0("GROUP_", sprintf("%03d", 1:100)), .N, replace = TRUE),
  
  # Entity identifier
  entid = sample(paste0("ENT_", sprintf("%05d", 1:200)), .N, replace = TRUE),
  
  # Entity type
  ent_type = sample(c("GET-MNE", "IND-ETR", "GFR-FRA", "GFR-MNE", "IND-FRA", "1", "2", "4"), .N, replace = TRUE),
  
  # Entity nationality
  ent_nationality = sample(c("FR", "DE", "UK", "US", "CN"), .N, replace = TRUE, prob = c(0.4, 0.15, 0.15, 0.15, 0.15)),
  
  # Firm nationality
  firm_nationality = sample(c("FR", "DE", "UK", "US", "CN"), .N, replace = TRUE, prob = c(0.4, 0.15, 0.15, 0.15, 0.15))
)]

# Sort by firmid and year
setkey(lifi_data, firmid, year)

# Display first few rows
head(lifi_data, 20)

# Display structure
str(lifi_data)

# Save as RDS for use in the main script
write_rds(lifi_data, "G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/2 Data/LIFI.rds")
