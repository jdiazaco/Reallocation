#' ------------------------------------------------------------------------------
#' Script: Patent Data Cleaning and Cumulative IPCR/NACE Generation
#' Author: Juli?n D?az-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' ------------------------------------------------------------------------------

# setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# 0) IPC analysis -----------------

# Load all necessary data

start=2009
end=2022

patenting_products <- read_rds("7_final_firm_lvl_dta.rds")

# Find which names are in firm_data and which aren't
selected_names <- c(
  
  # Identifiers
  "firmid", "year",
  
  # Industry / classification
  "NACE_BR", "NACE_2d_BR",
  
  # birth_death info
  "born", "died", "consolidated_birth_year", "economic_death_year",
  
  # Employment / inputs
  "empl", "empl_l", "empl_bar",  "log_empl_bar", "empl_growth",    "empl_reallocation",
  "capital", "capital_bar", "capital_growth",
  "nq", "nq_bar", "nq_growth",   "nq_reallocation",
  "labor_cost", "raw_materials",

  # NACE patent info
  "NACE_cum", "NACE_cum_l", "new_NACE",
  "n_NACE", "n_NACE_bar", "n_NACE_growth",
  
  # Firm demographics & age/size buckets
  "young", "firm_age", "log_firm_age",
  "age_leader", "age_size_bucket", "age_size_quartile", "age_size_decile", 
  "age_size_percentile", "age_size_1000tile", "age_top_4_leaders", "age_top_10_leaders", 
  "leader", "top_4_leaders", "top_10_leaders",
  
  # Size measures and ranks
  "size", "size_quartile", "size_decile", "size_percentile", "size_1000tile",
  "rank_within_industry", "n_firms_in_industry",
  
  # Revenue / shares
  "nq_bar", "nq_growth", "rev_bar", "rev_growth", 
  "within_economy_rev_share_BR", "within_economy_rev_share_BR_l", 
  "within_economy_rev_share_BR_growth", "within_industry_rev_share",
  "within_industry_rev_share_l", "within_industry_rev_share_bar", 
  "within_industry_rev_share_growth",
  
  # Productivity
  "tfpr", "tfpr_bar", "tfpr_growth",
  
  # Industry concentration measures
  "CR4", "CR10", "HHI_industry", "gini",
  
  # Product portfolio & dynamics
  "number_of_products", "prod_creat", "prod_destr", "prod_added", "prod_removed",
  "new_products", "first_introduction", 
  "net_product_creat", "net_product_creat_window",
  "net_product_destr", "net_product_destr_window",
  
  # Patents & filing
  "patent", "patent_window", "ever_patent",
  "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
  "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth",
  "product_innovative_pat_filings_window", "product_innovative_pat_filings_window",
  "product_innovative_pat_families_window", "product_innovative_pat_families_window",
  
  # IPC and NACE expansion / IPCR measures
  "ipcr_cum", "ipcr_cum_l", "ipcr_creat", "ipcr_creat_window", "new_ipcr",
  "n_ipcr", "n_ipcr_bar", "n_ipcr_growth",  
  "NACE_cum", "NACE_cum_l",  "new_NACE",  "n_NACE", "n_NACE_bar", "n_NACE_growth",
  
  # Trademarks
  "tm", "tm_window", "ever_tm"
)

in_firm_data <- selected_names[selected_names %in% names(patenting_products)]
not_in_firm_data <- selected_names[!(selected_names %in% names(patenting_products))]
# Keep only continuers and from 2009
# patenting_products <- patenting_products[year>=start & year!=consolidated_birth_year & year<=economic_death_year]  %>%
patenting_products <- patenting_products[year>start & abs(empl_growth) != 2 & abs(nq_growth) != 2 & abs(capital_growth) != 2]  %>%
  select(in_firm_data) %>% filter(NACE_2d_BR %in% prodcom_sectors)

# Save final data
write_rds(patenting_products, "patenting_products_firm_level.RDS")


# 1) Growth, patenting and trademarking -----------------

patenting_products <- read_rds("patenting_products_firm_level.RDS")

# threshold_young <- 5
# output_dir_og <- output_dir
# patenting_products_og <- patenting_products
graphs <- T
regression_name<-"growth_x_innovation"
ys <- c("nq_growth", "empl_growth", "capital_growth") # , , "within_industry_rev_share_growth" "rev_growth")
# xs <- c("tm_window", "tm_window*net_product_creat_window", "patent_window", "patent_window*net_product_creat_window", "net_product_destr_window", "net_product_creat_window") # , "patent_window + ipcr_creat_window"
xs <- c("pat_families_dummy_window", "total_pat_families_growth") # "pat_filings_dummy_window", "total_pat_filings_growth", "net_product_creat_window", "tm_window" , "patent_window + ipcr_creat_window"
controls <- c("")
fe <- c("NACE_BR + year")

types <- c("all_firms", "patenting_firms")
subsets<- c("all") # "young", "mature"


formulas <- create_formulas(ys, xs, controls, fe)
titles_and_restrictions <- fread(
  'title, restriction, weight_flag, additional_controls
  M. Small, age_size_bucket=="mature_small", T, log(empl_l)
  M. Medium, age_size_bucket=="mature_medium", T, log(empl_l)
  M. Large, age_size_bucket=="mature_large", T, log(empl_l)
  Y. Small, age_size_bucket=="young_small", T, log(empl_l)
  Y. NonSmall, age_size_bucket=="young_large", T, log(empl_l)
  All, NA, T, .[x]*log(firm_age) + .[x]*log(empl_bar)'
)
regression_innovation_growth(regression_name, patenting_products, formulas, titles_and_restrictions, types, subsets, graphs=graphs)

graphs <- T
regression_name<-"growth_x_innovation_+_concentration"
ys <- c("nq_growth", "empl_growth", "capital_growth") # , "within_industry_rev_share_growth" , "rev_growth")
xs <- c("pat_families_dummy_window", "total_pat_families_growth") # "pat_filings_dummy_window", "total_pat_filings_growth", "net_product_creat_window", "tm_window" , "patent_window + ipcr_creat_window"
controls <- c("HHI_industry")
fe <- c("NACE_BR + year")
formulas <- create_formulas(ys, xs, controls, fe)
titles_and_restrictions <- fread(
  'title, restriction, weight_flag, additional_controls
  M. Small, age_size_bucket=="mature_small", T, log(empl_l)
  M. Medium, age_size_bucket=="mature_medium", T, log(empl_l)
  M. Large, age_size_bucket=="mature_large", T, log(empl_l)
  Y. Small, age_size_bucket=="young_small", T, log(empl_l)
  Y. NonSmall, age_size_bucket=="young_large", T, log(empl_l)
  All, NA, T, .[x]*log(firm_age) + .[x]*log(empl_bar) + .[x]*HHI_industry'
)
regression_innovation_growth(regression_name, patenting_products, formulas, titles_and_restrictions, types, subsets, graphs=graphs)

# 2) Patenting trademarking analysis -----------------

patenting_products <- readRDS("patenting_products_firm_level.RDS") %>% as.data.table(.) 
threshold_young <- 5
output_dir_og <- output_dir
patenting_products_og <- patenting_products
graphs <- T
regression_name<-"patent_tm_x_size_concentration"
ys <- c("pat_families_dummy_window", "total_pat_families_growth") # , "rev_growth")
xs <- c("log_empl_bar*HHI_industry")
controls <- c("log_firm_age")
fe <- c("NACE_BR + year")
formulas <- create_formulas(ys, xs, controls, fe)
titles_and_restrictions_fixed_buckets <- fread(
  'title, restriction, weight_flag, additional_controls
  M. Small, age_size_bucket=="mature_small", T,
  M. Medium, age_size_bucket=="mature_medium", T,
  M. Large, age_size_bucket=="mature_large", T,
  Y. Small, age_size_bucket=="young_small", T,
  Y. NonSmall, age_size_bucket=="young_large", T,
  All, NA, T, log_firm_age*HHI_industry'
)

disag_reg_parameters <- expand.grid(
  y = c("pat_families_dummy_window", "total_pat_families_growth"), # , "rev_growth")
  x = "log_empl_bar*HHI_industry",
  controls = "log_firm_age",
  fe =  c("NACE_BR + year"),
  weight = "nq_bar",
  restriction = "age_size_bucket=='mature_large'",
  disag_var = c("NACE_2d", "NACE_BR")
) %>% as.data.table(.)

types <- c("all_firms", "patenting_firms")
subsets<- c("all") # "young", "mature"

regression_innovation_growth(regression_name,
                             patenting_products, formulas, titles_and_restrictions_fixed_buckets,
                             types = types, subsets = subsets, graphs = graphs, disag_reg_parameters = disag_reg_parameters)

titles_and_restrictions_quantiles <- read_csv(
  'title, restriction, weight_flag, additional_controls
  M. Q1, age_size_quartile=="mature_q1", T,
  M. Q4, age_size_quartile=="mature_q4", T,
  M. Q10, age_size_decile=="mature_d10", T,
  M. Q100, age_size_percentile=="mature_p99" | age_size_percentile=="mature_p100", T,
  M. Top 10, age_top_10_leaders=="mature_top_10", T, log_empl_bar*top_10_leaders_rev_share
  M. Top 4, age_top_4_leaders=="mature_top_4", T, log_empl_bar*top_4_leaders_rev_share
  M. Leader, age_leader=="mature_leader", T, log_empl_bar*leader_rev_share + diff_leader_vs_2nd
  All, NA, T, log_empl_bar*HHI_industry*(leader) + leader_rev_share + diff_leader_vs_2nd')
#   M. Q1000, age_size_1000tile=="mature_k999" | age_size_1000tile=="mature_k1000", T,

titles_and_restrictions_quantiles <- read_csv(
  'title, restriction, weight_flag, additional_controls
  M. Q1, age_size_quartile=="mature_q1", T,
  M. Q4, age_size_quartile=="mature_q4", T,
  M. Q10, age_size_decile=="mature_d10", T,
  M. Q100, age_size_percentile=="mature_p99" | age_size_percentile=="mature_p100", T,
  M. Top 10, age_top_10_leaders=="mature_top_10", T, 
  M. Top 4, age_top_4_leaders=="mature_top_4", T, 
  M. Leader, age_leader=="mature_leader", T, leader_rev_share + diff_leader_vs_2nd
  All, NA, T, log_empl_bar*HHI_industry*(leader) + leader_rev_share + diff_leader_vs_2nd')

disag_reg_parameters <- expand.grid(
  y = "patent_window",
  x = "log_empl_bar*HHI_industry",
  controls = "log_firm_age",
  fe =  c("NACE_BR + year"),
  weight = "nq_bar",
  restriction = c('age_leader=="mature_leader"', 'age_top_4_leaders=="mature_top_4"'),
  disag_var = c("NACE_2d", "NACE_BR")
) %>% as.data.table(.)

regression_name<-"patent_tm_x_size_concentration_quantiles"

regression_innovation_growth(regression_name,
                             patenting_products, formulas, titles_and_restrictions_quantiles,
                             types = types, subsets = subsets, graphs = graphs)



# 3) Leader v. followers -----------------

patenting_products <- readRDS("patenting_products_firm_level.RDS") 

# Create measures of leader and follower patenting and growth activity
# Here I want to create a dataset that per year and industry has the following variables:
# leader_patent: whether the leader patented that year
# follower_patent: whether any of the followers patented that year
# share_follower_patent: share of followers that patented that year
# n_firms: number of firms in the industry that year
# leader_growth: growth (employment, revenue and capital) of the leader that year
# follower_growth: average growth of the followers that year
# n_followers: number of followers in the industry that year
# lagged versions of the above variables

industry_year_data <- patenting_products[, .(
  leader_log_new_products = asinh(new_products[leader==1]),
  follower_log_new_products = asinh(mean(new_products[leader==0], na.rm=T)),
  leader_patent = max(patent[leader == 1], na.rm = T),
  follower_patent = ifelse(.N > 1, max(patent[leader == 0], na.rm = T), 0),
  share_follower_patent = ifelse(.N > 1, mean(patent[leader == 0], na.rm = T), 0),
  n_firms = .N,
  leader_empl_growth = ifelse(any(leader == 1), empl_growth[leader == 1][1], NA_real_),
  follower_empl_growth = ifelse(.N > 1, mean(empl_growth[leader == 0], na.rm = T), NA_real_),
  leader_nq_growth = ifelse(any(leader == 1), nq_growth[leader == 1][1], NA_real_),
  follower_nq_growth = ifelse(.N > 1, mean(nq_growth[leader == 0], na.rm = T), NA_real_),
  leader_capital_growth = ifelse(any(leader == 1), capital_growth[leader == 1][1], NA_real_),
  follower_capital_growth = ifelse(.N > 1, mean(capital_growth[leader == 0], na.rm = T), NA_real_),
  n_followers = sum(leader == 0),
  empl_bar=sum(empl_bar, na.rm=T),
  nq_bar=sum(nq_bar, na.rm=T),
  capital_bar=sum(capital_bar, na.rm = T)
), by = .(NACE_BR, year)]

# Create lagged variables
lagged_vars <- c(
  "leader_log_new_products", "follower_log_new_products",
  "leader_patent", "follower_patent", "share_follower_patent", "n_firms",
  "leader_empl_growth", "follower_empl_growth",
  "leader_nq_growth", "follower_nq_growth",
  "leader_capital_growth", "follower_capital_growth",
  "n_followers"
)
for (var in lagged_vars) {
  industry_year_data[, paste0(var, "_l") := shift(.SD[[var]], 1, type = "lag"), by = NACE_BR, .SDcols = var]
}

# regress current follower measures of growth and patenting on previous year leader patenting,
# previous year leader variable of interest, industry and year fixed effects, and control for number of firms in the industry
ys = c("follower_log_new_products", "follower_empl_growth", "follower_nq_growth", "follower_capital_growth", "share_follower_patent")
xs = c("leader_patent_l")
controls = c("n_firms_l")
fe = c("year")

# Create formulas
formulas <- create_formulas(
  ys = ys,
  xs = xs,
  controls = controls,
  fe = fe
)

formulas[, formula := mapply(
  function(formula, control, y) {
    # Replace only the control variable (as a whole word) with control + leader_lagged_y
    leader_lagged <- paste0(gsub("follower", "leader", y), "_l")
    # Use regex to match the control as a whole word
    gsub(paste0("\\b", control, "\\b"), paste0(control, " + ", leader_lagged), formula)
  },
  formula, control, y,
  SIMPLIFY = TRUE, USE.NAMES = FALSE
)]
formulas[, formula:=gsub("\\+ share_leader_patent_l", "", formula)]

models <- list()
for(i in 1:nrow(formulas)) {
  formula <- formulas$formula[i] 
  weight_temp <- formulas$weight[i]
  
  model <- feols(as.formula(formula), data = industry_year_data, weights = industry_year_data[[weight_temp]], cluster = ~ NACE_BR)
  models[[formulas$y[i]]] <- model
  
}

modelsummary(models, paste0(output_dir, "leader_follower_patent_growth.tex"), stars=TRUE)
