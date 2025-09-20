# libraries
rm(list = ls())
gc()

# Set global options
options(
  source.defaults = list(echo = TRUE, stop.on.error = TRUE)
)


packages = c('data.table', 'haven', 'readxl', 'openxlsx','stringr', 'readr', 'dplyr', 
             'tidyverse', 'zoo', 'reshape2','rstudioapi', "plm", 'foreign', "fixest", 
             'data.table', 'haven', 'stringr', 'readr', 'dplyr',
             'ggplot2', 'tidyverse', 'rstudioapi', 'zoo', 'reshape2',
             'patchwork', 'latex2exp', "RColorBrewer", "texreg", "stargazer", "modelsummary", "broom", "fixest",
             "xtable", "arrow", "tools", "marginaleffects",
             "AER", "fixest", "lfe")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Establish whether this is the final run of the code or not
set.seed(123)
final<-T

if(grepl("My Drive", dirname(rstudioapi::getActiveDocumentContext()$path))){
  main_dir<-"G:/My Drive/IWH/PhD/Reallocation/GitHub Infrastructure/"
}else{
  main_dir<-"~/Reallocation/6 Publish/"
}
  
 


#Bring tools
tools_dir <- paste0(main_dir, '1 Code/Tools/')
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




# source("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/Rtools/nace_conc.R")


# set up directories 
raw_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
setwd(paste0(main_dir, '2 Data/'))
code_dir<-paste0(main_dir, "1 Code/")
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
start<-2009
end<-2021

options(error=recover)
