# libraries
rm(list = ls())
gc()

# Set global options
options(
  source.defaults = list(echo = TRUE, stop.on.error = TRUE)
)


packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'readr', 'dplyr', 
             'tidyverse', 'zoo', 'reshape2','rstudioapi', "plm", 'foreign', 
             'haven', 'stringr', 'readr', 'dplyr',
             'ggplot2', 'tidyverse', 'rstudioapi', 'zoo', 'reshape2',
             'patchwork', 'latex2exp', "RColorBrewer", "texreg", "stargazer", "modelsummary", "broom", 
             "xtable", "arrow", "tools", "marginaleffects", "purrr",
             "AER", "fixest", "lfe", "lintr", "httr", "jsonlite", "ineq")


installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Establish whether this is the final run of the code or not
set.seed(123)
final<-T

if (grepl("My Drive|Users/nb", dirname(rstudioapi::getActiveDocumentContext()$path))) {
  main_dir <- "G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/"
} else {
  if (grepl("/Users/lse", getwd())) {
    main_dir <- "/Users/lse/Library/CloudStorage/GoogleDrive-juadiazaco@gmail.com/Mi unidad/IWH/PhD/Reallocation/GitHub Infrastructure/"
  } else {
    main_dir <- "~/Reallocation/7 New infrastructure/"
  }
}

  code_dir <<- case_when(
    ## When running locally
    grepl("/Users/lse", dirname(rstudioapi::getActiveDocumentContext()$path))  ~ "/Users/lse/Documents/Reallocation/1 Code/",

    grepl("nb", dirname(rstudioapi::getActiveDocumentContext()$path))  ~ "C:/Users/nb/Documents/Reallocation/1 Code/",
    
    ## When in CASD
    T ~ "~/Reallocation/7 New infrastructure/1 Code/"
  )


#Bring tools
tools_dir <- paste0(code_dir, '/Tools/')
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))
source(paste0(tools_dir, "parameters.R"))
source(paste0(tools_dir, "output_dir_creator.R"))
source(paste0(tools_dir, "calculate_weighted_means.R"))
source(paste0(tools_dir, "growth_creator.R"))
source(paste0(tools_dir, "gen_latex.R"))
source(paste0(tools_dir, "outliers_remove.R"))
source(paste0(tools_dir, "lead_lag_creator.R"))
source(paste0(tools_dir, "window_var_creator.R"))
source(paste0(tools_dir, "regression_reallocation_growth.R"))
source(paste0(tools_dir, "dynamic_reg_reallocation.R"))
source(paste0(tools_dir, "dynamic_reg_graphs.R"))
source(paste0(tools_dir, "static_reg_reallocation.R"))
source(paste0(tools_dir, "add_stars.R"))
source(paste0(tools_dir, "export_table_latex.R"))
source(paste0(tools_dir, "age_data_filter.R"))
source(paste0(tools_dir, "create_formulas.R"))
source(paste0(tools_dir, "regression_innovation_growth.R"))
source(paste0(tools_dir, "remove_special_chars.R"))
source(paste0(tools_dir, "core_switch_product.R"))
# source(paste0(tools_dir, "patent_prodcom_contextual_similarity.R"))
source(paste0(tools_dir, "create_lags.R"))
source(paste0(tools_dir, "winsorize.R"))
# source(paste0(tools_dir, "dummy_dataset.R"))


safe_mean <- function(x) ifelse(all(is.na(x)), NA_real_, mean(x, na.rm = TRUE))
safe_median <- function(x) ifelse(all(is.na(x)), NA_real_, median(x, na.rm = TRUE))
safe_sd <- function(x) ifelse(sum(!is.na(x)) <= 1, NA_real_, sd(x, na.rm = TRUE))
safe_quant <- function(x, probs) ifelse(all(is.na(x)), NA_real_, quantile(x, probs, na.rm = TRUE, type = 7))
safe_ratio <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, num / den)

load_dataset <- function(path_candidates, reader, ...) {
  existing_path <- purrr::detect(path_candidates, file.exists)
  if (is.null(existing_path)) {
    stop(paste("None of the candidate files exist:", paste(path_candidates, collapse = ", ")))
  }
  reader(existing_path, ...)
}

to_dt <- function(obj) as.data.table(obj)

# Jaccard similarity between firmid_prior_list and leader/top lists
jaccard <- function(a, b) {
  a <- unique(as.character(unlist(a)))
  b <- unique(as.character(unlist(b)))
  a <- a[!is.na(a) & a != "" & a != "unknown"]
  b <- b[!is.na(b) & b != "" & b != "unknown"]
  if (length(a) == 0 && length(b) == 0) return(NA_real_)
  if (length(a) == 0) return(0)
  if (length(b) == 0) return(NA_real_)
  inter <- length(intersect(a, b))
  union <- length(unique(c(a, b)))
  if (union == 0) return(NA_real_)
  inter / union
}


a_in_b_dt <- function(a, b) {
  a <- unique(as.character(unlist(a)))
  b <- unique(as.character(unlist(b)))
  a <- a[!is.na(a) & a != "" & a != "unknown"]
  b <- b[!is.na(b) & b != "" & b != "unknown"]
  # if (length(a) == 0 && length(b) == 0) return(NA_real_)
  if (length(a) == 0) return(0)
  if (length(b) == 0) return(NA_real_)
  inter <- length(intersect(a, b))
  union <- length(unique(c(a, b)))
  if (union == 0) return(NA_real_)
  if (inter > 0) return(1)
}

jaccard_reallocation <- function(a, b) {
  a <- unique(as.character(unlist(a)))
  b <- unique(as.character(unlist(b)))
  a <- a[!is.na(a) & a != "" & a != "unknown"]
  b <- b[!is.na(b) & b != "" & b != "unknown"]
  if (length(a) == 0 && length(b) == 0) return(NA_real_)
  if (length(a) == 0) return(0)
  if (length(b) == 0) return(NA_real_)
  inter <- length(intersect(a, b))
  union <- length(unique(c(a, b)))
  if (union == 0) return(NA_real_)
  inter / union
}



# source("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/Rtools/nace_conc.R")

# set up directories 
raw_dir = "C:/Users/NEWPROD_J_DIAZ-AC/Documents/Raw_data/Data/"
setwd(paste0(main_dir, '2 Data/'))
output_dir<-paste0(main_dir, "3 Output/")

# Create output_dir
output_dir_creator(output_dir)
output_dir_creator(paste0(main_dir, "2 Data/firm_lists/"))

# Set parameters for prodfra-pcc8 and excluded industries
exclude_industries<-TRUE
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
parameters(prodfra_or_pcc8, only_prodfra_in_prodcom, exclude_industries)

# Set start and end years
br_start<-2009
br_end<-2022

source(paste0(code_dir, ".lintr.R"))
lint(paste0(code_dir, ".lintr.R"))

# Define level of aggregation for product level data
cpa_or_pf <- "prodcom" # Options: "cpa", "prodfra_plus", "prodcom", "NACE_4d_pf", "NACE_2d_pf"
agg_levels <- list(
  prodfra_plus  = list(ext = "_prodfra_plus",digits = c(0,1,2,4,6,8,10), exit_digit = "exit_10"),
  prodcom       = list(ext = "_prodcom",    digits = c(0,1,2,4,6,8),     exit_digit = "exit_8"),
  cpa           = list(ext = "_cpa",        digits = c(0,1,2,4,6),       exit_digit = "exit_6"),
  NACE_4d_pf    = list(ext = "_nace4d_pf",  digits = c(0,1,2,4),         exit_digit = "exit_4"),
  NACE_2d_pf    = list(ext = "_nace2d_pf",  digits = c(0,1,2),           exit_digit = "exit_2")
)

if (!cpa_or_pf %in% names(agg_levels)) stop("Unknown cpa_or_pf value")
ext        <- agg_levels[[cpa_or_pf]]$ext
digits     <- agg_levels[[cpa_or_pf]]$digits
exit_digit <- agg_levels[[cpa_or_pf]]$exit_digit

# These are oil manufacturing, public utilities, water supply, sewerage, waste management and remediation activities,
# and activities of households as employers; undifferentiated goods- and services-producing activities of households for own use; activities of extraterritorial organizations and bodies
  ind_to_exclude <- c(19,35,36,37,38,39,46) 
  
  minimum_share_in_prodcom <- 30 
  minimum_n_in_br <- 300
  threshold_jaccard_change_business_group <- 1/3
  
  # firm_industry_data <- read_parquet("firm_lists/firmid_NACE_BR_year.parquet")

  if (grepl("My Drive|Users/nb", dirname(rstudioapi::getActiveDocumentContext()$path))) {
  prodcom_sectors <- c("07", "08", "10", "13", "14", "15", "16", "17", "18", "20", "21", "22", "23", "24", "25", 
                       "26", "27", "28", "29", "30", "31", "32", "33", "42", "43", "45", "47", "58", "68", "70", 
                       "71", "72", "73", "77", "82", "95", "96")
  } else {
  prodcom_sectors <- read_csv("firm_lists/prodcom_sectors.csv")
  setDT(prodcom_sectors)
  prodcom_sectors <- unique(prodcom_sectors[share >= minimum_share_in_prodcom & in_br >= minimum_n_in_br] %>% pull(NACE_2d_BR))
  }
  
  
  
output_dir <- paste0(output_dir, "2026/Export 21.01/")
output_dir_creator(output_dir)
