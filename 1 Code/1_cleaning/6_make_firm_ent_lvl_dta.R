# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# load data ----------------------------------------------------------------

end <- 2022

lifi_data <- as.data.table(read_parquet("C:/Users/Public/1. Microprod/0. Raw data processing/Codes/Preparation_LIFI/LIFI.parquet"))
firm_data <- as.data.table(fread("firm_l"))
