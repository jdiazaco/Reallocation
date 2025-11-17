# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 8) IPC analysis -----------------

# Load all necessary data

start=2009
end=2022

firm_data <- read_rds("6_final_firm_lvl_dta.rds")

