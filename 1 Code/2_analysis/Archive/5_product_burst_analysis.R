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

patenting_products <- read_rds("temp/patenting_products_firm_level.RDS")
patent_data <- read_rds("4b_patenting_products_firm_level.rds")
tm_data <- read_rds("4c_tm_firm_level.rds")

selected_names <- c(
  
  # Identifiers
  "firmid", "year",
  
  # Industry / classification
  "NACE_BR", "NACE_2d_BR",
  
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

  # Product portfolio & dynamics
  "number_of_products", "prod_creat", "prod_destr", "prod_added", "prod_removed",
  "new_products", "first_introduction", 
  "net_product_creat", "net_product_creat_window",
  "net_product_destr", "net_product_destr_window",
  
  # Patents & filing
  "pat_families_dummy", "pat_families_dummy_window", "ever_patent",
  "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
  "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth",
  "product_innovative_pat_filings_window", "product_innovative_pat_filings_window",
  "product_innovative_pat_families_window", "product_innovative_pat_families_window",
  
  # Trademarks
  "tm_dummy", "tm_dummy_window", "ever_tm"
)

in_firm_data <- selected_names[selected_names %in% names(patenting_products)]
not_in_firm_data <- selected_names[!(selected_names %in% names(patenting_products))]

# Filter to selected prodcom sectors


# Keep only continuers and from 2009
# patenting_products <- patenting_products[year>=start & year!=consolidated_birth_year & year<=economic_death_year]  %>%
patenting_products <- patenting_products %>% select(in_firm_data)

# View(patenting_products[, c(  # Product portfolio & dynamics
#   "firmid", "year",
#   "number_of_products", "prod_added", 
# 
#   # Patents & filing
#   "ever_patent",
#   "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
#   "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth",
#   "product_innovative_pat_filings_window", "product_innovative_pat_filings_window",
#   "product_innovative_pat_families_window", "product_innovative_pat_families_window"
# )])


# Create burst variables
burst_threshold <- 5
burst_thresholds <- c(2, 5, 10)

patenting_products <- patenting_products[, burst:=ifelse(prod_added>burst_threshold, 1, 0)] %>% 
  group_by(firmid) %>%  setDT() %>%
  mutate(ever_burst=any(prod_added>=burst_threshold)) 

for(i in burst_thresholds){
  patenting_products[, (paste0("burst_", i)):=ifelse(prod_added>=i, 1, 0)] 
}


# Analyse and plot burst episodes per year
# bursts_per_year <- patenting_products[, .(burst=mean(burst, na.rm=T)), by=year]
bursts_per_year <- patenting_products[, lapply(.SD, mean, na.rm=T), .SDcols = grepl("burst_", names(patenting_products)), by=year] %>% 
  pivot_longer(starts_with("burst_"), names_prefix="burst_") %>% setDT() %>%
  .[, mean:=mean(value), by=.(name)] %>%
  .[, dev_from_mean:=(value-mean)/mean]

vars_labs <- fread("var, lab
                   dev_from_mean, Dev. from Av. Firms with Bursts/Total Firms per Year
                   value, Firms with Bursts/Total Firms per Year")

for (i in 1:nrow(vars_labs)){
  
  var <- vars_labs[i]$var
  lab <- vars_labs[i]$lab
  ggplot(bursts_per_year, aes(x=year, y=.data[[var]], color=name)) + 
    geom_line() + geom_point()+
    # scale_size_manual(values = c(1,rep(.5,5)))+
    scale_x_continuous(limits = c(min(bursts_per_year$year)+1,max(bursts_per_year$year)), breaks = scales::breaks_pretty(5))+
    scale_y_continuous(labels = scales::label_percent(scale = 100), limits = c(min(0, min(bursts_per_year[[var]])), max(bursts_per_year[[var]]))) +
    labs(x="Year", y=lab, color="Burst Threshold") +
    theme_classic() 
  ggsave(paste0(output_dir, var, "_per_year.png"), width=9, height=5)
  
}


## Trademark analysis ----------------------------------
# First merge trademark and product information

# firms_in_patenting_products <- unique(patenting_products$firmid)
# firmis_in_tm <- unique(tm_data$firmid)
# difference <- setdiff(firms_in_patenting_products, firms_in_tm)
tm_patenting_products <- patenting_products %>% select(firmid, year, burst, ever_burst, prod_creat) %>%
  merge(tm_data, by=c("firmid", "year"), all=T) %>%
  merge(patent_data, by=c("firmid", "year"), all=T)

# tm_names <- c("num_tm", "total_tm", "tm_dummy", "total_tm_growth", "tm_dummy_window", "ever_tm")

patent_tm_data_names <- c("ever_patent",
                          "pat_filings_dummy", "pat_filings_dummy_window", "num_pat_filings", "total_pat_filings", "total_pat_filings_growth",
                          "pat_families_dummy", "pat_families_dummy_window", "num_pat_families", "total_pat_families", "total_pat_families_growth",
                          "num_tm", "total_tm", "tm_dummy", "total_tm_growth", "tm_dummy_window", "ever_tm")

firm_data[, (patent_tm_data_names):=lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = patent_tm_data_names]






# burst_firms <- patenting_products %>% group_by(firmid) %>% filter(any(prod_added>burst_threshold)) %>% setDT() 
# View(burst_firms %>% select(firmid, year, prod_added, burst, num_pat_families, total_pat_families_growth, starts_with("pat_families_dummy")))
# test <- burst_firms[, .(pat_families_dummy=mean(pat_families_dummy, na.rm=T)), by=burst]
# test <- burst_firms[, .(burst=mean(burst, na.rm=T)), by=pat_families_dummy]

window <- 5

for(x in c("pat_families_dummy", "tm_dummy")){
  patenting_bursts <- dynamic_reg_reallocation(
    data=tm_patenting_products, 
    y="burst", 
    x=x, 
    fix_eff="",
    weight_var=NULL,
    disag_var = "size",
    n_lags_bw = window,
    n_lags_fw = window)
  dynamic_reg_graphs(patenting_bursts, "", output_dir, paste0(x, "_series"), -window, window)
  
}

patenting_bursts <- dynamic_reg_reallocation(
  data=patenting_products, 
  y="burst", 
  x="pat_families_dummy", 
  fix_eff="",
  weight_var=NULL,
  disag_var = "size",
  n_lags_bw = window,
  n_lags_fw = window)
dynamic_reg_graphs(patenting_bursts, "", output_dir, "test.png", -window, window)

only_bursts <- burst_firms[burst==1]
patent_records <- read_parquet("3_patent_lvl_patent_dta.parquet")
patent_citation <- readRDS("3b_patent_citation_summary.RDS")
pat_tm_vars_to_mean <- names(tm_patenting_products)[grepl("pat_families_dummy|tm_dummy", names(tm_patenting_products))]
# View(only_bursts %>% select(pat_vars_to_mean))


# Plot likelihoods of patenting around burst event
series_patenting_burst <- patenting_products[, lapply(.SD, mean, na.rm=T), .SDcols=pat_vars_to_mean, by=.(burst, ever_burst, prod_creat)]
series_patenting_burst <- series_patenting_burst[!is.na(burst)  & !is.na(ever_burst) & !is.na(prod_creat)]
series_patenting_burst <- pivot_longer(series_patenting_burst, starts_with("pat_families_dummy"), names_prefix="pat_families_dummy") %>% setDT() %>%
  .[, name:=str_replace(name, "_lead", "")] %>%
  .[, name:=str_replace(name, "_lag", "-")] %>%
  .[, name:=fifelse(name=="", "0", name)] %>%
  .[name!="_window"] %>%
  .[, name:=as.numeric(name)] %>%
  .[, series:=paste0(ever_burst, "_", prod_creat, "_", burst)]


ggplot(series_patenting_burst, aes(x=name, y=value, color=series)) + 
  geom_line() + 
  geom_point() 
  
  

patenting_products[, `:=`(
  rank = frank(-nq, ties.method = "average", na.last = "keep"),
  n_firms_patenting_products  = .N
), by=year] %>%
  .[, rank_share:=rank/n_firms_patenting_products]

ggplot(patenting_products[year==2019], aes(x=log(number_of_products, base=2) , y=log(rank_share, base=10))) + 
  geom_point(alpha=0.1)

test <- patenting_products[year==2019, .(rank_share=mean(rank_share, na.rm=T)), by=number_of_products]
ggplot(test, aes(x=log(number_of_products, base=2) , y=log(rank_share, base=10))) + 
  geom_point()

test <- patenting_products[, .(rank_share=mean(rank_share, na.rm=T)), by=prod_added]
ggplot(test, aes(x=log(prod_added, base=2) , y=log(rank_share, base=10))) + 
  geom_point()

test <- patenting_products[, .(rank_share=mean(rank_share, na.rm=T)), by=num_pat_families]

ggplot(test, aes(x=log(num_pat_families, base=2) , y=log(rank_share, base=10))) + 
  geom_point()

test <- patenting_products[, .(num_pat_families=mean(num_pat_families, na.rm=T)), by=prod_added]

ggplot(test, aes(x=log(prod_added, base=2) , y=log(num_pat_families, base=2))) + 
  geom_point() + 
  geom_smooth(method="lm")



ggsave(paste0(output_dir, ""))



