"
This file uses firm (FARE/FICUS) and product data (EAP) to analyze the effect of product entry and exit on firm growth

Author: Juli?n D?az-Acosta
Last update: 10/10/2024
"



# 0) setup -------------------------------------------------------------------
source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main - fake.R"))
folder_name<-""
output_dir<-paste0(output_dir, "Product reallocation and firm dynamics/Export 29.11/")
output_dir_creator(output_dir)

cpa_or_pf<-"cpa"
if(cpa_or_pf=="cpa"){
  ext<-"_cpa"
  digits<-c(0, 1, 2, 4, 6)
  exit_digit<-"exit_6"
}else{
  if(cpa_or_pf=="prodfra_plus")
    ext<-""
  digits<-c(0, 1, 2, 4, 6, 8, 10)
  exit_digit<-"exit_10"
}

#Bring in necessary firm and product information
firm_data_select<-readRDS("sbs_br_data_prodcom_firms.RDS") %>% filter(year>2009)
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
product_data<-readRDS(paste0("product_level_growth_", filter_indicator, ext, "_.RDS"))

#Set the number of digits of the different product aggregation levels

# Check only prodcom firms are present in the firm_data_select sample
if(length(setdiff(unique(firm_data_select$firmid), unique(product_data$firmid)))!=0){
  stop("Firm data does not contain only prodcom firms. Check modules a and c and come back")
}

# 1) Identification and tracking of core and new product categories across aggregation levels  --------------------


# Check that product categories status variables are exhaustive and mutually exclusive
# This ensures that every product is categorized only once (first_introduction, reintroduced, discontinued, incumbent, or paused) 
# and that these categories cover all products
if(sum(product_data$first_introduction, product_data$reintroduced, 
       product_data$discontinued, product_data$incumbent, product_data$paused) != nrow(product_data)){
  stop("product categories are not exhaustive and/or mutually exlcusive")
}


# Identify firm's core products and industries at the different digits aggregation levels

if(cpa_or_pf=="cpa"){
  product_data[, `:=`(NACE_2d_pf=substr(cpa, 1,2))]
}else{
  if(cpa_or_pf=="prodfra_plus"){
    product_data[, `:=`(prodcom=substr(prodfra_plus, 1,8),
                        cpa=substr(prodfra_plus, 1,6),
                        NACE_2d_pf=substr(prodfra_plus, 1,2))]
  }
}



# Define a function to identify the core products within each firm, by category aggregation level
core_switch_product<-function(data, n_digits){
  
  # data<-product_data
  # n_digits<-6
  
  # Define variable names dynamically based on the number of digits
  pf<-paste0("pf_", n_digits)
  core<-paste0("core_", pf)
  switch<-paste0("switch_", pf)
  n_core<-paste0("n_core_", pf)
  share_core<-paste0("share_core_", pf)
  share_runup<-paste0("share_runup_", pf)
  
  
  setDT(data)
  
  # Create a category variable by truncating product codes at the specified number of digits
  data[[pf]]<-substr(data[[cpa_or_pf]], 1, n_digits) # Create the category variable (sector, industry, cpa, pcc8, prodfra)
  if(n_digits==1){
    data[[pf]]<-substr(data$DEFind, 1, n_digits) # Create the category variable (sector, industry, cpa, pcc8, prodfra)
  }else{
    data[[pf]]<-substr(data[[cpa_or_pf]], 1, n_digits) # Create the category variable (sector, industry, cpa, pcc8, prodfra)
  }
  
  data<-data[, .(rev=sum(rev, na.rm = T)), by=.(firmid, year, get(pf))] # Aggregate revenue at the category level
  
  data<-data[, share:=(ifelse(sum(rev, na.rm=T)!=0 & !is.na(rev), rev/sum(rev, na.rm = T), NA)), by=.(firmid, year)] #Calculate the share of revenue of each product within the firm
  
  
  # Sort data within each firmid and year by revenue in descending order
  data <- data[order(firmid, year, -rev)]
  
  # Rank the revenue within each firmid and year
  data <- data[, rank := frank(-rev, ties.method = "first"), by = .(firmid, year)]
  
  # Assign share_core and share_runup based on rank
  data <- data[, `:=`(
    share_core = ifelse(rank == 1, share, NA_real_),  # Highest revenue share
    share_runup = ifelse(rank == 2, share, NA_real_)  # Second-highest revenue share
  ), by = .(firmid, year)]
  
  # Fill NAs in share_core and share_runup within each firmid, year
  data[, share_core := max(share_core, na.rm = TRUE), by = .(firmid, year)]
  data[, share_runup := max(share_runup, na.rm = TRUE), by = .(firmid, year)]
  
  data<-data[, core:=(rev==max(rev, na.rm = T)), by=.(firmid, year)] # Flag the category with the highest revenue
  data<-data[core==T & rev!=0] # Keep only categories with the highest revenue and with revenue
  
  data<-data[core==T & rev!=0] # Keep only categories with the highest revenue and with revenue
  data<-data[, .(n_core=n_distinct(get), # Flag the number of tied categories in number 1
                 get=paste(unique(get), collapse=", "),
                 share_core = max(share_core, na.rm = TRUE),
                 share_runup = max(share_runup, na.rm = TRUE)), by=.(firmid, year)] # Define the set of top categories
  
  data<-data[, share_runup:=ifelse(is.infinite(share_runup), NA, share_runup)] # Replace any infinite values in share_runup with NA
  
  setorder(data, firmid, year)
  data<-data[, switch:=!(ifelse(is.na(dplyr::lag(get, 1)), NA,  str_detect(dplyr::lag(get, 1), get))), by=.(firmid)] # Flag if there has been a switch in category
  
  setnames(data, c("get", "switch", "n_core", "share_core", "share_runup"), c(core, switch, n_core, share_core, share_runup)) # Adjust names
  
  return(data)
}

# Define levels of aggregation to generate core category information for each level
levels_agg<-digits[digits!=0]

# Loop through aggregation levels to generate core category information for each level
for(i in seq_along(levels_agg)){
  print(levels_agg[i])
  data<-core_switch_product(product_data, levels_agg[i])
  
  if(i==1){
    product_core<-data
  }else{
    if(nrow(product_core)!=nrow(data)){
      stop("Differing numbers in levels of aggregation. Check this.")
    }
    
    product_core<-merge(product_core, data, by=c("firmid", "year"), all.x = T, allow.cartesian=F)
    
  }
}

# Compute lagged values for core metrics for comparison across years.
setorder(product_core, firmid, year)
share_cores<-names(product_core)[grepl("^core|^share_core|^share_runup", names(product_core))]
for(i in share_cores){
  product_core<-product_core[, (paste0("lag_", i)):=dplyr::lag(get(i), 1), by=firmid]
}

product_data<-merge(product_data, product_core, by=c("firmid", "year"))
# rm(product_prodfra_core, product_prodcom_core, product_cpa_core, product_NACE_core, product_NACE_2d_pf_core, product_core); gc()

setorder(product_data, firmid, year, -rev)

digits_inv<-sort(digits, decreasing = T)

# Loop through each level to create entry and exit variables dynamically
for (i in seq_along(digits_inv)) {
  
  # i<-1
  digit <- digits_inv[i]
  
  # Define previous levels to exclude in the condition for the current level
  if(i==1){
    prev_digits<-NULL
  }else{
    prev_digits <- digits_inv[1:(i - 1)]
  }
  
  # Entry (new_*) conditions based on the digit level
  product_data <- product_data[, paste0("new_", digit) := 
                                 first_introduction &
                                 # Dynamically match conditions based on the digit level
                                 (if (digit == 10) prodfra_plus==lag_core_pf_10
                                  else if (digit == 8) prodcom==lag_core_pf_8 
                                  else if (digit == 6) cpa == lag_core_pf_6
                                  else if (digit == 4) NACE == lag_core_pf_4
                                  else if (digit == 2) NACE_2d_pf == lag_core_pf_2
                                  else if (digit == 1) substr(DEFind, 1, 1) == lag_core_pf_1
                                  else if (digit == 0) substr(DEFind, 1, 1) != lag_core_pf_1) &  # for digit 0 or any other cases
                                 
                                 # Ensure no overlap with previous levels
                                 !(if (length(prev_digits) > 0) 
                                   Reduce(`|`, lapply(prev_digits, function(d) get(paste0("new_", d))))
                                   else FALSE)]
  
  # Exit (exit_*) conditions based on the digit level
  product_data <- product_data[, paste0("exit_", digit) := 
                                 discontinued &
                                 # Dynamically match conditions based on the digit level
                                 (if (digit == 10) prodfra_plus==lag_core_pf_10
                                  else if (digit == 8) prodcom==lag_core_pf_8 
                                  else if (digit == 6) cpa == lag_core_pf_6
                                  else if (digit == 4) NACE == lag_core_pf_4
                                  else if (digit == 2) NACE_2d_pf == lag_core_pf_2
                                  else if (digit == 1) substr(DEFind, 1, 1) == lag_core_pf_1
                                  else if (digit == 0) substr(DEFind, 1, 1) != lag_core_pf_1) &  # for digit 0 or any other cases
                                 
                                 # Ensure no overlap with previous levels
                                 # Ensure no overlap with previous levels
                                 !(if (length(prev_digits) > 0) 
                                   Reduce(`|`, lapply(prev_digits, function(d) get(paste0("exit_", d))))
                                   else FALSE)]
}



# product_data<-product_data %>% select(firmid, year, cpa, within_firm_rev_share,  new_6, new_4, new_2, new_1, new_0,
#                                       switch_pf_6,switch_pf_4, switch_pf_2,  everything())

product_data<-product_data %>% select(firmid, year, cpa_or_pf, rev, first_introduction, grep("^new", names(product_data)), 
                                      discontinued, grep("^exit", names(product_data)), grep("^core", names(product_data)), grep("^lag", names(product_data)), everything())


write_rds(product_data, paste0("product_level_growth_", filter_indicator,  "_new_core_analysis", ext, ".RDS"))
write_rds(product_core, paste0("product_core", ext, ".RDS"))

# 2) Data prep for different measures of product entry and exit  --------------------
# 2.1) Create measures of number of products by product status and entry/exit by pre post exit/entry status -----
product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_new_core_analysis", ext, ".RDS"))
product_core<-readRDS(paste0("product_core", ext, ".RDS"))

# Define working variables
vars<-c("first_introduction", names(product_data)[grep("^new", names(product_data))], 
        # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2",
        names(product_data)[grep("^exit", names(product_data))], 
        "reintroduced", "discontinued", "incumbent", "paused", "rev")
unique_vars<-c("first_year", "last_year")

#Aggregate variables at the firm-year level from the firm-year-product level
product_summary_sums<-product_data[, lapply(.SD, sum, na.rm=T), .SDcols=vars, by=.(firmid, year)]
product_summary_years<-product_data[, lapply(.SD, unique, na.rm=T), .SDcols=unique_vars, by=.(firmid, year)]
product_summary<-merge(product_summary_sums, product_summary_years, by=c("firmid", "year"), all.x=T)
rm(product_summary_sums, product_summary_years); gc()

product_summary<- merge(product_summary, product_core, by=c("firmid", "year"), all.x = T)

setDT(product_summary)

# Define number of products in a year
product_summary[, `:=`(number_of_products=first_introduction+reintroduced+incumbent)]
# product_summary<-product_summary[, number_of_years:=n_distinct(year), by=firmid]

# Set firm's first year observations to NAs, since we don't know actual product status that year (everything is spuriously first_introduction)
# We still leave the observations, because the information on number of products is still useful
new<-grep("^new", names(product_summary), value=T)
switch<-grep("^switch", names(product_summary), value=T)
product_summary<-product_summary[year==first_year,`:=`(number_of_products=first_introduction+reintroduced+incumbent,
                                                       first_introduction=NA,
                                                       paused=NA,
                                                       reintroduced=NA,
                                                       incumbent=NA,
                                                       discontinued=NA)  ]
product_summary<-product_summary[year==first_year, (new):=NA  ]
product_summary<-product_summary[year==first_year, (switch):=NA  ]

# We drop last years observations by taking off all observations that do not have revenue in that year
# since everything is spuriously discontinued and we don't actually know the product composition that year
product_summary<-product_summary[rev!=0]

# Number of products by product status
product_summary[, `:=`(new_products=first_introduction,
                       paused_products=paused,
                       reintroduced_products=reintroduced,
                       destroyed_products=discontinued,
                       number_of_products=ifelse(year!=first_year, first_introduction+reintroduced+incumbent, number_of_products ))]

# Create measures of entry post pre exit and exit post pre entry
product_summary <- product_summary %>% group_by(firmid) %>% mutate(entry_year=ifelse(new_products!=0, year, NA),
                                                                   exit_year=ifelse(destroyed_products!=0, year, NA)) %>% 
  mutate(exit_post_entry=case_when(is.na(entry_year) ~ NA_real_,
                                   # destroyed_products!=0 ~ 1,
                                   dplyr::lead(destroyed_products, 1)!=0 ~ 1,
                                   dplyr::lead(destroyed_products, 2)!=0 ~ 1,
                                   # dplyr::lead(destroyed_products, 3)!=0 ~ 1,
                                   # dplyr::lead(destroyed_products, 4)!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(entry_post_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   # new_products!=0 ~ 1,
                                   dplyr::lead(new_products, 1)!=0 ~ 1,
                                   dplyr::lead(new_products, 2)!=0 ~ 1,
                                   # dplyr::lead(new_products, 3)!=0 ~ 1,
                                   # dplyr::lead(new_products, 4)!=0 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(exit_pre_entry=case_when(is.na(entry_year) ~ NA_real_,
                                  # destroyed_products!=0 ~ 1,
                                  dplyr::lag(destroyed_products, 1)!=0 ~ 1,
                                  dplyr::lag(destroyed_products, 2)!=0 ~ 1,
                                  # dplyr::lag(destroyed_products, 3)!=0 ~ 1,
                                  # dplyr::lag(destroyed_products, 4)!=0 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(entry_pre_exit=case_when(is.na(exit_year) ~ NA_real_,
                                  # new_products!=0 ~ 1,
                                  dplyr::lag(new_products, 1)!=0 ~ 1,
                                  dplyr::lag(new_products, 2)!=0 ~ 1,
                                  # dplyr::lag(new_products, 3)!=0 ~ 1,
                                  # dplyr::lag(new_products, 4)!=0 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(exit_with_entry=case_when(is.na(entry_year) ~ NA_real_,
                                   destroyed_products!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(entry_with_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   new_products!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  
  ungroup()

# Delete false negatives, stemming from pre variable not being able to capture introduction if the firm has 
product_summary <- product_summary %>% group_by(firmid) %>% 
  mutate(exit_post_entry=ifelse(last_year-year>2 | exit_post_entry==1, exit_post_entry, NA),
         entry_post_exit=ifelse(last_year-year>2| entry_post_exit==1, entry_post_exit, NA), # Here the inequality is not strict because we have a "dummy" last year
         exit_pre_entry=ifelse(year-first_year>=2 | exit_pre_entry==1, exit_pre_entry, NA), 
         entry_pre_exit=ifelse(year-first_year>=2| entry_pre_exit==1, entry_pre_exit, NA))

# Arrange variables
product_summary <- product_summary %>% select(firmid, year, number_of_products, new_products, destroyed_products, paused_products, reintroduced_products,
                                              exit_post_entry, exit_pre_entry, entry_post_exit, entry_pre_exit, everything())
# Create dummy variable with 
product_summary <- product_summary %>% mutate(prod_creat=case_when( new_products>0 ~ 1, 
                                                                    is.na(new_products) ~ NA, 
                                                                    TRUE ~ 0),
                                              prod_destr=case_when( destroyed_products>0 ~ 1, 
                                                                    is.na(destroyed_products) ~ NA, 
                                                                    TRUE ~ 0))


saveRDS(product_summary, paste0("product_creation_destruction_eppe", ext, ".RDS"))

# 2.2) Lag and lead product entry and exit variables -----

product_summary<- readRDS(paste0("product_creation_destruction_eppe", ext, ".RDS"))


lead_lag_creator<-function(data, creat_var, destr_var, n_lags){
  # data<-product_summary
  # creat_var<-"exit_1"
  # destr_var<-"exit_1"
  # n_lags<-4
  setorder(data, firmid, year)
  setDT(data)
  
  for(i in 1:n_lags){
    x_creat_lag<-paste0(creat_var, "_lag", i)
    x_destr_lag<-paste0(destr_var, "_lag", i)
    x_creat_lead<-paste0(creat_var, "_lead", i)
    x_destr_lead<-paste0(destr_var, "_lead", i)
    
    vars<-c(x_creat_lag, x_destr_lag, x_creat_lead, x_destr_lead)
    
    data[, (x_creat_lag):=ifelse( is.na(dplyr::lag(get(creat_var), i)), NA, ifelse(dplyr::lag(get(creat_var), i)>0, 1,0)), by=.(firmid)]
    data[, (x_creat_lead):=ifelse( is.na(dplyr::lead(get(creat_var), i)), NA, ifelse(dplyr::lead(get(creat_var), i)>0, 1,0)), by=.(firmid)]
    data[, (x_destr_lag):=ifelse( is.na(dplyr::lag(get(destr_var), i)), NA, ifelse(dplyr::lag(get(destr_var), i)>0, 1,0)), by=.(firmid)]
    data[, (x_destr_lead):=ifelse( is.na(dplyr::lead(get(destr_var), i)), NA, ifelse(dplyr::lead(get(destr_var), i)>0, 1,0)), by=.(firmid)]
    
  }
  
  return(data)
}

product_summary<-lead_lag_creator(product_summary, "prod_creat", "prod_destr", n_lags=4)

for (i in digits) {
  
  new<-paste0("new_", i)
  exit<-paste0("exit_", i)
  
  product_summary<-lead_lag_creator(product_summary, new, exit, n_lags=4)
  
}

product_summary<-product_summary[, lag_number_of_products:=ifelse( is.na(dplyr::lag(number_of_products, 1)), NA, dplyr::lag(number_of_products, 1)), by=.(firmid)]


saveRDS(product_summary, paste0("product_creation_destruction", ext, ".RDS"))

# 3) Create measures of product diversification within years --------------------

HHI_firm<-product_data[!is.na(HHI), .(HHI=unique(HHI)), by=.(firmid, year)]
saveRDS(HHI_firm, "HHI_firm_year.RDS")

# 4) Patent data  ------------------------
# 4.1) Patent data creation-------------------
patent_data<-fread("patent_data.csv")
patent_data$firmid<-as.character(patent_data$siren)
patent_data$firmid<-str_pad(patent_data$firmid, 9, "left", "0")

patent_data<-patent_data[, .(min_publication_year=min(publication_year, na.rm=T),
                             max_publication_year=max(publication_year, na.rm=T),
                             collection=paste(collection, collapse = ", ")), by=.(application_year, title, inventor_name, firmid)] 

test_applications<- patent_data %>% group_by(firmid, application_year) %>% summarise(n_applications=n()) %>% rename(year=application_year)
test_granted <- patent_data %>% group_by(firmid, min_publication_year) %>% summarise(n_publications=n()) %>% rename(year=min_publication_year)

patent_apps_published<-merge(test_applications, test_granted, by=c("firmid", "year"), all=T)

saveRDS(patent_apps_published, "patent_apps_published.RDS")

# 4.2) Patent trend graph -------------------

patent_apps_published<-readRDS("patent_apps_published.RDS")


trends<-patent_data %>% select(application_year, application_number, collection, type, title, publication_year, publication_number, inventor_name, ipcr_list)
trends<-unique(trends)

publications_by_year<- trends %>%  select(publication_year, publication_number, collection)
publications_by_year<- unique(publications_by_year)
publications_by_year<- publications_by_year %>% group_by(publication_year, collection) %>% summarise(n_publications=n()) %>% 
  rename(year=publication_year) %>% pivot_wider(id_cols = "year", names_from = "collection", values_from = "n_publications", names_prefix = "pubs_")


ggplot(publications_by_year %>% filter(year %in% 1995:2022), aes(x=year)) + 
  geom_line(aes(y=pubs_FR, color="FR")) + 
  geom_line(aes(y=pubs_WO, color="WO")) +
  geom_line(aes(y=pubs_EP, color="EP")) + 
  scale_x_continuous(breaks = (seq(1995, 2022, by=1)))+
  labs(title="Patent Grants for French Firms by Geographic Scope", y="Count", x="Year", color="Type") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(paste0(output_dir, "patents_trends.png"), width=6, height=4, dpi=300)




# 5) Analysis of product innovation by firm size and age percentiles ----------------------

product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_new_core_analysis", ext, ".RDS"))
product_summary<-readRDS(paste0("product_creation_destruction", ext, ".RDS"))
product_core<-readRDS(paste0("product_core", ext, ".RDS"))

patent_apps_published<-readRDS("patent_apps_published.RDS")

setDT(patent_apps_published)
firms_patenting<-patent_apps_published[, .(n_publications=sum(n_publications, na.rm = T)), by=.(firmid)]
firms_patenting<-unique(patent_apps_published[n_publications>0]$firmid)

product_summary<-merge(product_summary, patent_apps_published, by=c("firmid", "year"), all.x = T)
# product_summary[, n_publications:=ifelse(is.na(n_publications),0, n_publications)]

firm_age<-firm_data_select[, c("firmid", "year", "firm_age")]
product_data<-merge(product_data, firm_age, by=c("firmid", "year"), all.x = T)

rev_limit<-0
threshold_firms_per_category<-50
n_bins<-50
weight_var<-"rev_share"

# innovation_vars<-c("first_introduction", "new_10", "new_8", "new_6", "new_4", "new_2", "new_1", "new_0",
#                    "discontinued", "exit_10", "exit_8", "exit_6", "exit_4", "exit_2", "exit_1", "exit_0")
#                            # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2")

# digits<-c(0, 1, 2, 4, 6, 8, 10)
innovation_vars<-c("all", digits)

labels_codes<-c("No Common Root", paste0(digits[digits!=0], " Digits"))

if (cpa_or_pf == "cpa"){
  category_vars<-c("DEFind", "NACE_2d", "NACE", "cpa")
} else {
  if (cpa_or_pf == "prodfra_plus"){
    category_vars<-c("DEFind", "NACE_2d", "NACE", "cpa", "prodcom", "cpa")
  }
}

# samples<-c("all_firms", "firms_patenting", "firms_not_patenting")
samples<-c("all_firms")
distrib<-c("rev", "age")

product_data<-product_data[year!=first_year & year!=last_year]
product_data_og<-product_data

folder_name<-cpa_or_pf
output_dir<-paste0(output_dir, folder_name, "/")
output_dir_creator(output_dir)
output_dir_og<-output_dir

product_data<-product_data %>% select(firmid, year, cpa, core_pf_6, rev, first_introduction, grep("^new", names(product_data)), 
                                      discontinued, grep("^exit", names(product_data)), everything())

file_conn = file(paste0(output_dir_og, folder_name, '.tex'), 'w')
write( paste0("\\section{Product Reallocation by Firm Size and Age Percentiles} \n"), paste0(output_dir_og, folder_name, '.tex'), append=T)


for(x in distrib){
  
  # x<-"rev"
  
  if (x=="rev"){
    distrib_text<-"Size"
  }else{
    if(x=="age"){
      distrib_text<-"Age"
    }
  }
  
  output_dir_creator(paste0(output_dir_og), temp = T, sub_folder = x)
  
  # if(!dir.exists(paste0(output_dir_og, x, "/"))){
  #   output_dir<-paste0(output_dir_og, x, "/")
  #   output_dir_creator(output_dir)
  # }else{
  #   output_dir<-paste0(output_dir_og, x, "/")
  # }
  
  
  for(k in samples){
    # k<-"all_firms"
    
    output_dir_creator(paste0(output_dir_og), temp = T, sub_folder = paste0(x, "/", k))
    
    
    # if(!dir.exists(paste0(output_dir_og, x, "/", k, "/"))){
    #   output_dir<-paste0(output_dir_og, x, "/", k, "/")
    #   output_dir_creator(output_dir)
    # }else{
    #   output_dir<-paste0(output_dir_og, x, "/", k, "/")
    # }
    
    if(k=="all_firms"){
      product_data<-product_data_og
    }
    
    if(k=="firms_patenting"){
      product_data<-product_data_og[firmid %in% firms_patenting]
    }
    
    if(k=="firms_not_patenting"){
      product_data<-product_data_og[!(firmid %in% firms_patenting)]
    }
    for(j in category_vars){
      
      # Create the data table that will store results
      all_results<-data.table(bin=numeric(), weighted_avg_in_rate=numeric(), weighted_avg_ex_rate=numeric(),  innovation_var=numeric())
      
      
      for(i in innovation_vars){
        # i<-"6"
        # j<-"NACE"
        
        
        output_dir_creator(paste0(output_dir_og), temp = T, sub_folder = paste0(x, "/", k, "/", j))
        
        # if(!dir.exists(paste0(output_dir_og, x, "/", k, "/", j, "/"))){
        #   output_dir<-paste0(output_dir_og, x, "/", k, "/",j, "/")
        #   output_dir_creator(output_dir)
        # }else{
        #   output_dir<-paste0(output_dir_og, x, "/", k, "/",j, "/")
        # }
        
        if(i =="all"){
          new_var<-"first_introduction"
          exit_var<-"discontinued"
        }else{
          new_var<-paste0("new_", i)
          exit_var<-paste0("exit_", i)
        }
        
        # Argente et al.: "For each firm x product catergory, we compute average sales, the average product
        # innovation rate (new products/number of products sold)... 
        
        # Define dynamic column names based on the value of cpa_or_pf
        core_pf_var <- if (cpa_or_pf == "cpa") "lag_share_core_pf_6" else "lag_share_core_pf_10"
        runup_pf_var <- if (cpa_or_pf == "cpa") "lag_share_runup_pf_6" else "lag_share_runup_pf_10"
        
        
        # Calculate the test table without using := inside .()
        test <- product_data[, {
          # Compute values for dynamic columns without naming them directly
          core_pf_value <- mean(get(core_pf_var)[get(exit_var) & year != last_year], na.rm = TRUE)
          runup_pf_value <- mean(get(runup_pf_var)[get(exit_var) & year != last_year], na.rm = TRUE)
          
          .(
            n_new_products = n_distinct(cpa[get(new_var) & year != first_year & rev > rev_limit], na.rm = TRUE),
            new_codes = paste(unique(cpa[get(new_var) & year != first_year & rev > rev_limit], na.rm = TRUE), collapse = ", "),
            n_exit_products = n_distinct(cpa[get(exit_var) & year != last_year], na.rm = TRUE),
            share_exit_product = mean(within_firm_rev_share[get(exit_var) & year != last_year], na.rm = TRUE),
            
            # Assigning the temporary computed values without naming them
            core_pf_value = core_pf_value,
            runup_pf_value = runup_pf_value,
            
            exit_codes = paste(unique(cpa[get(exit_var) & year != last_year], na.rm = TRUE), collapse = ", "),
            n_products = n_distinct(cpa),
            codes = paste(unique(cpa, na.rm = TRUE), collapse = ", "),
            rev = mean(rev, na.rm = TRUE),
            age = mean(firm_age, na.rm = TRUE)
          )
        }, by = .(firmid, get(j))]
        
        # Rename columns with dynamic names using 
        test[, (core_pf_var) := core_pf_value]
        test[, (runup_pf_var) := runup_pf_value]
        test[, c("core_pf_value", "runup_pf_value") := NULL] # Clean up temporary columns
        
        
        ## Rename variable get to the category variable, since it is misnamed after the prior command 
        names(test)[names(test)=="get"]<-j
        
        ## Create product introduction and exit rates
        test<-test[, `:=`(in_rate=n_new_products/n_products,
                          ex_rate=n_exit_products/n_products) ]
        ## Count number of firms in each product category
        test<-test[, `:=`(n_firms_in_cpa=.N), by=.(get(j)) ]
        ## Keep only product categories with at least the number of firms defined beforehand
        test<-test[n_firms_in_cpa>threshold_firms_per_category]
        
        # Argente et al.: Whithin each product category, we assign firms to 50 bins of average sales ...
        # and plot the average product innovation rate for each bin.
        # distrib<-"age"
        restult<-test[, `:=`(bin=frank(get(x), ties.method = "first") %/% (max(frank(get(x), ties.method = "first"))/n_bins) +1), by=get(j)]
        
        # restult<-test[, `:=`(bin=frank(rev, ties.method = "first") %/% (max(frank(rev, ties.method = "first"))/n_bins) +1), by=get(j)]
        
        # Argente et al.: Each dot plots the averages after weighting each product category by its 
        # importance in the whole sector, as measured by the share of sales accounted for by the category
        
        restult<-restult[, `:=`(total_rev=sum(rev, na.rm=T))]
        restult<-restult[, `:=`(rev_share=rev/total_rev)]
        
        lag_share_core <- if (cpa_or_pf == "cpa") "lag_share_core_pf_6" else if (cpa_or_pf == "prodfra_plus") "lag_share_core_pf_10"
        lag_share_runup <- if (cpa_or_pf == "cpa") "lag_share_runup_pf_6" else if (cpa_or_pf == "prodfra_plus") "lag_share_runup_pf_10"
        
        
        # Define weighting variables and parameter
        variables_to_weight <- c("in_rate", "ex_rate", "share_exit_product", lag_share_core, lag_share_runup)
        group_var <- "bin"  # Grouping variable
        
        # Calculate weighted and unweighted variables
        restult <- calculate_weighted_means(data = restult, weight_var = weight_var, variables_to_weight = variables_to_weight, group_var = group_var, weighted=TRUE)
        
        # restult<-restult[, .(weighted_in_rate=sum(in_rate*rev_share, na.rm=T)/sum(rev_share, na.rm = T),
        #                      weighted_ex_rate=sum(ex_rate*rev_share, na.rm=T)/sum(rev_share, na.rm = T),
        #                      weighted_share_exit_product=sum(share_exit_product*rev_share, na.rm=T)/sum(rev_share[!is.na(share_exit_product)], na.rm = T),
        #                      weighted_lag_share_core_pf_6=sum(lag_share_core_pf_6*rev_share, na.rm=T)/sum(rev_share[!is.na(lag_share_core_pf_6)], na.rm = T),
        #                      weighted_lag_share_runup_pf_6=sum(lag_share_runup_pf_6*rev_share, na.rm=T)/sum(rev_share[!is.na(lag_share_runup_pf_6)], na.rm = T)), by=.(bin)]
        
        
        restult$innovation_var<-i
        restult[, bin:=((bin-1)*(100/n_bins))]
        
        all_results<-rbind(all_results, restult, fill=T)
        
        if(i=="all"){
          ggplot(restult, aes(x=bin, y=weighted_in_rate)) + 
            geom_point() +
            labs(x=paste0("Firm-", j, " ", distrib_text, " Percentile"), y="Weighted Average Product Entry Rate") +
            theme_minimal() + 
            scale_x_continuous(breaks=seq(0,100, by=25)) + 
            ylim(0, max(0.4, restult$weighted_in_rate))+
            ggtitle(paste0("Product Entry Rates by Firm-", j, " Size "),
                    subtitle=paste0("Product Entry Variable: ", new_var))
          ggsave(paste0(output_dir, new_var, "_entry_", x, ".png"), width = 8, height = 5, dpi = 300)
          max_weighted_in_rate<-restult$weighted_in_rate
          
          if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
            gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", new_var, "_entry_", x, ".png"),
                               paste0("cpa", "/", x, "/", k, "/", "NACE", "/", new_var, "_entry_", x, ".png")), 
                      types=c("image", "image"), 
                      captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
          }
          
          ggplot(restult, aes(x=bin, y=weighted_ex_rate)) + 
            geom_point() +
            labs(x=paste0("Firm-", j, " ", distrib_text, " Percentile"), y="Weighted Average Product Exit Rate") +
            theme_minimal() + 
            scale_x_continuous(breaks=seq(0,100, by=25)) + 
            ylim(0, max(0.4, restult$weighted_ex_rate))+
            ggtitle(paste0("Product Exit Rates by Firm-", j, " Size "),
                    subtitle=paste0("Product Exit Variable: ", exit_var))
          
          ggsave(paste0(output_dir, exit_var, "_exit_", x, ".png"), width = 8, height = 5, dpi = 300)
          max_weighted_ex_rate<-restult$weighted_ex_rate
          
          if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
            gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", exit_var, "_exit_", x, ".png"),
                               paste0("cpa", "/", x, "/", k, "/", "NACE", "/", exit_var, "_exit_", x, ".png")), 
                      types=c("image", "image"), 
                      captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
          }
          
        }
      }
      
      #Checks
      all_results<-all_results[!is.na(bin)]
      all_results<-unique(all_results)
      all_results_wide<-pivot_wider(all_results, names_from = innovation_var, values_from =   names(all_results)[grepl("^weighted|^unweighted", names(all_results))])
      
      setDT(all_results_wide)
      all_results_wide[, all_in:= rowSums(.SD, na.rm=T), .SDcols = (grep(c("in_rate_[0-9]"), names(all_results_wide)))]
      all_results_wide[, all_ex:= rowSums(.SD, na.rm=T), .SDcols = (grep(c("ex_rate_[0-9]"), names(all_results_wide)))]
      all_results_wide[, check_in:=(abs(all_in-weighted_avg_in_rate_all)<0.0000000000000001) ]
      all_results_wide[, check_ex:=(abs(all_ex-weighted_avg_ex_rate_all)<0.0000000000000001) ]
      
      all_results<- all_results[innovation_var %in% digits,]
      all_results$innovation_var<-factor(all_results$innovation_var, levels=digits)
      
      ggplot(all_results, aes(x=bin, y=weighted_in_rate, fill=innovation_var)) + 
        geom_bar(stat = "identity") + 
        scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
        ylim(0, max(0.51, max_weighted_in_rate))+
        ggtitle(paste0("Product Entry Rate by Firm-",j, " ", distrib_text))+
        labs(x=paste0(distrib_text, " Percentile"), y="Weighted Average Product Entry Rate") +
        theme_minimal()
      ggsave(paste0(output_dir, "entry_", x, ".png"), width = 8, height = 5, dpi = 300)
      
      if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
        gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", "entry_", x, ".png"),
                           paste0("cpa", "/", x, "/", k, "/", "NACE", "/", "entry_", x, ".png")), 
                  types=c("image", "image"), 
                  captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
      }
      
      
      ggplot(all_results, aes(x=bin, y=weighted_ex_rate, fill=innovation_var)) + 
        geom_bar(stat = "identity") + 
        scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
        ylim(0, max(0.4, max_weighted_ex_rate))+
        ggtitle(paste0("Product Exit Rate by Firm-",j, " ", distrib_text))+
        labs(x=paste0(distrib_text, " Percentile"), y="Weighted Average Product Exit Rate") +
        theme_minimal()
      ggsave(paste0(output_dir, "exit_", x, ".png"), width = 8, height = 5, dpi = 300)
      
      if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
        gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", "exit_", x, ".png"),
                           paste0("cpa", "/", x, "/", k, "/", "NACE", "/", "exit_", x, ".png")), 
                  types=c("image", "image"), 
                  captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
      }
      
      
      all_results<-all_results[, `:=`(norm_weighted_avg_ex_rate=weighted_ex_rate/sum(weighted_ex_rate, na.rm=T)), by=bin]
      all_results<-all_results[, `:=`(norm_weighted_avg_in_rate=weighted_in_rate/sum(weighted_in_rate, na.rm=T)), by=bin]
      
      ggplot(all_results, aes(x=bin, y=norm_weighted_avg_ex_rate, fill=innovation_var)) +
        geom_bar(stat = "identity") +
        scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
        ggtitle(paste0("Share of Product Types in Exit Rates by Firm-",j, " ", distrib_text))+
        labs(x=paste0(distrib_text, " Percentile"), y="Weighted Average Product Exit Rate") +
        theme_minimal()
      ggsave(paste0(output_dir, "share_exit_", x, ".png"), width = 8, height = 5, dpi = 300)
      
      if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
        gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", "share_exit_", x, ".png"),
                           paste0("cpa", "/", x, "/", k, "/", "NACE", "/", "share_exit_", x, ".png")), 
                  types=c("image", "image"), 
                  captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
      }
      
      
      core_pf_var <- if (cpa_or_pf == "cpa") "lag_share_core_pf_6" else "lag_share_core_pf_10"
      runup_pf_var <- if (cpa_or_pf == "cpa") "lag_share_runup_pf_6" else "lag_share_runup_pf_10"
      
      
      # Define variable names based on cpa_or_pf input
      runup_weighted_var <- if (cpa_or_pf == "cpa") "weighted_lag_share_runup_pf_6_6" else "weighted_lag_share_runup_pf_10_10"
      core_weighted_var <- if (cpa_or_pf == "cpa") "weighted_lag_share_core_pf_6_6" else "weighted_lag_share_core_pf_10_10"
      runup_unweighted_var <- if (cpa_or_pf == "cpa") "unweighted_lag_share_runup_pf_6_6" else "unweighted_lag_share_runup_pf_10_10"
      core_unweighted_var <- if (cpa_or_pf == "cpa") "unweighted_lag_share_core_pf_6_6" else "unweighted_lag_share_core_pf_10_10"
      
      # Plot with ggplot, dynamically specifying the column names
      ggplot(all_results_wide, aes(x = bin)) + 
        geom_line(aes(y = .data[[runup_weighted_var]], color = "Runner-up (w)"), size = 1) +
        geom_line(aes(y = .data[[core_weighted_var]], color = "Core (w)"), size = 1) +
        geom_line(aes(y = .data[[runup_unweighted_var]], color = "Runner-up (uw)"), size = 1) +
        geom_line(aes(y = .data[[core_unweighted_var]], color = "Core (uw)"), size = 1) +
        labs(
          x = paste0(distrib_text, " Percentile"),
          y = "Product Revenue Share",
          color = "Product Type",
          subtitle = "Comparing Runup and Core Shares across Firm Size Percentiles"
        ) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0, 100, by = 10))
      ggsave(paste0(output_dir, "revenue_share.png"), width = 8, height = 5, dpi = 300)
      
      if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
        gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", "revenue_share.png"),
                           paste0("cpa", "/", x, "/", k, "/", "NACE", "/", "revenue_share.png")), 
                  types=c("image", "image"), 
                  captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
      }
      
      
      # scale_color_manual(values = c("Runup Share" = "blue", "Core Share" = "red"))
      
      # # Original ggplot code
      # p <- ggplot(all_results, aes(x=bin, y=norm_weighted_avg_ex_rate, fill=innovation_var, 
      #                              text = paste("Weighted Share Ex:", weighted_share_ex))) + 
      #   geom_bar(stat = "identity") + 
      #   scale_fill_discrete(labels=labels_codes, name="Common Root with Core") +
      #   ggtitle(paste0("Share of Product Types in Exit Rates by Firm-", j, " Size")) +
      #   labs(x = "Size Percentile", y = "Weighted Average Product Entry Rate") +
      #   theme_minimal()
      # 
      # # Convert to interactive plotly plot
      # interactive_plot <- ggplotly(p, tooltip = "text")
      # 
      # # Display the plot (this will open an interactive viewer if running in RStudio)
      # interactive_plot
      
      ggplot(all_results, aes(x=bin, y=norm_weighted_avg_in_rate, fill=innovation_var)) + 
        geom_bar(stat = "identity") + 
        scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
        ggtitle(paste0("Share of Product Types in Entry Rates by Firm-",j, " ", distrib_text))+
        labs(x=paste0(distrib_text, " Percentile"), y="Weighted Average Product Entry Rate") +
        theme_minimal()
      ggsave(paste0(output_dir, "share_entry_", x, ".png"), width = 8, height = 5, dpi = 300)
      
      if(cpa_or_pf=="prodfra_plus" & j=="cpa"){
        gen_latex(path = c(paste0(folder_name, "/", x, "/", k, "/", "cpa", "/", "share_entry_", x, ".png"),
                           paste0("cpa", "/", x, "/", k, "/", "NACE", "/", "share_entry_", x, ".png")), 
                  types=c("image", "image"), 
                  captions = c("Using Prodfra", "Using CPA"), file_conn=paste0(output_dir_og, folder_name, '.tex'))
      }
      
      
    }
    
  }
  
}



# product_data[, new:=rowSums(.SD, na.rm=T), .SDcols = (grep("^new_", names(all_results_wide)))]
product_data[, new:=new_0+new_1+new_2+new_4+new_6+new_8+new_10]
product_data[, exit:=exit_0+exit_1+exit_2+exit_4+exit_6+exit_8+exit_10]

test<-product_data %>% select(firmid, year, cpa, first_introduction, grep("^new", names(product_data)), 
                              discontinued, grep("^exit", names(product_data))) %>% 
  mutate(first_introduction=as.numeric(first_introduction, na.rm=T),
         discontinued=as.numeric(discontinued, na.rm=T),
         check_in=first_introduction==new,
         check_ex=discontinued==exit)
test<-test %>% filter(is.na(exit) | is.na(new))
if(nrow(test)>0){
  stop("New and exit categories do not comprise the number of first_introduction or discontinued. Check this")
}
rm(test); gc()



setDT(product_summary)
test_patent<-product_summary[, .(new_products=sum(new_products, na.rm=T),
                                 patents=sum(n_publications, na.rm=T),
                                 rev=sum(rev, na.rm=T)), by=.(firmid)]
test_patent<-test_patent[, `:=`(total_rev=sum(rev, na.rm=T))]
test_patent<-test_patent[, `:=`(rev_share=rev/total_rev,
                                pat_per_prod=ifelse(new_products==0, NA_real_, patents/new_products))]

test_patent<-test_patent[, `:=`(bin=frank(rev, ties.method = "first") %/% (max(frank(rev, ties.method = "first"))/n_bins) +1)]
test_patent<-test_patent[, .(weighted_avg_pat_per_prod=sum(pat_per_prod*rev_share, na.rm=T)/sum(rev_share, na.rm = T)), by=.(bin)]

ggplot(test_patent, aes(x=bin, y=weighted_avg_pat_per_prod)) + 
  geom_point() +
  labs(x=paste0("Firm Size Percentile"), y="Weighted Average Sahre of Patents Per New Products") +
  theme_minimal() + 
  scale_x_continuous(breaks=seq(0,100, by=25)) + 
  ggtitle(paste0("Product Entry Rates by Firm Size "))









table(substr(product_data[new_0==T,]$DEFind, 1,1), product_data[new_0==T,]$core_pf_1)

abcd<-as.data.table(table(substr(product_data[new_0==T,]$DEFind, 1,1), product_data[new_0==T,]$core_pf_1))








# 6) Growth paths before and after dropping core product ----------------------
# 6.1) Growth paths before and after dropping core product ----------------------

distrib<-c("rev", "age")

product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_new_core_analysis", ext, ".RDS"))
product_data_temp<-product_data[year!=first_year & year!=last_year]
product_data_temp<-product_data_temp[, prodcom_rev:=sum(rev, na.rm = T), by=.(firmid, year)]
product_data_temp<-unique(product_data_temp[get(exit_digit)==T, c("firmid", "year", "prodcom_rev")])
product_data_temp<-product_data_temp[, .(yr_core_drop=min(year, na.rm = T)), by=.(firmid)]
# product_data_temp<-product_data_temp[, .(n=.N), by=.(firmid)]

firm_data_select_temp<-firm_data_select[firmid %in% unique(product_data_temp$firmid)]
firm_data_select_temp<-firm_data_select_temp[!is.na(size) & size!="micro"]
firm_data_select_temp<-merge(firm_data_select_temp, product_data_temp, by="firmid", all.x = T)
firm_data_select_temp[, yr_core_drop:=year-yr_core_drop]


data<-product_data[year!=first_year & year!=last_year]
data<-data[, .(rev=sum(rev, na.rm = T)), by=.(firmid, year)]
normal_cols = "rev"
data<-growth_creator(data, normal_cols) %>% select(firmid, year, rev_growth, rev_bar)
firm_data_select_temp<-merge(firm_data_select_temp, data, by=c("firmid", "year"), all.x = T)

folder_name<-"growth pre post core drop"
output_dir<-paste0(output_dir,cpa_or_pf, "/", folder_name , "/")
output_dir_creator(output_dir)
output_dir_og<-output_dir

product_data<-product_data %>% select(firmid, year, cpa, core_pf_6, rev, first_introduction, grep("^new", names(product_data)), 
                                      discontinued, grep("^exit", names(product_data)), everything())

file_conn = file(paste0(output_dir_og, folder_name, '.tex'), 'w')
write( paste0("\\section{Product Reallocation by Firm Size and Age Percentiles} \n"), paste0(output_dir_og, folder_name, '.tex'), append=T)


for(x in distrib){
  
  
  temp<-firm_data_select_temp[, .(nq_growth_uw=mean(nq_growth, na.rm=T),
                                  nq_growth_w=weighted.mean(nq_growth, nq_bar, na.rm=T),
                                  empl_growth_uw=mean(empl_growth, na.rm=T),
                                  empl_growth_w=weighted.mean(empl_growth, empl_bar, na.rm=T),
                                  capital_growth_uw=mean(capital_growth, na.rm=T),
                                  capital_growth_w=weighted.mean(capital_growth, capital_bar, na.rm=T),
                                  rev_growth_uw=mean(rev_growth, na.rm=T),
                                  rev_growth_w=weighted.mean(rev_growth, rev_bar, na.rm=T)),
                              by=.(yr_core_drop, get(x))]
  
  if(x=="young"){
    temp<-temp[, get:=ifelse(get==1, "Young", "Established")]
  }
  
  unique_values<-unique(temp$get)
  base_colors<-setNames(rainbow(length(unique_values)), unique_values)
  color_palette<-c(setNames(sapply(base_colors, function(color) muted(color, l=60)), 
                            paste0(unique_values, " (UW)")),
                   setNames(base_colors, paste0(unique_values, " (W)")))
  
  
  # Adjust the distrib variables from get to their actual name
  setnames(temp, "get", x)
  
  
  temp<-temp[!is.na(get(x)) & yr_core_drop %in% -10:10]
  
  for(y in growth_vars){
    
    # y<-"rev"
    growth_uw<-paste0(y, "_growth_uw")
    growth_w<-paste0(y, "_growth_w")
    
    
    
    
    ggplot(temp, aes(x=yr_core_drop))+
      geom_line(aes(y=get(growth_uw), color=factor(paste0(get(x), " (UW)"))))+
      geom_line(aes(y=get(growth_w), color=factor(paste0(get(x), " (W)"))))+
      scale_color_manual(values=color_palette, name=x)+
      scale_x_continuous(breaks=seq(-10,10, by=1)) + 
      ylim(min(temp %>% select(-x, -yr_core_drop), na.rm = T), 
           max(temp %>% select(-x, -yr_core_drop), na.rm = T))+
      geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
      geom_vline(xintercept = 0, color="black", linetype="dashed") + 
      # ggtitle(paste0("Product Exit Rate by Firm-",j, " Size "))+
      labs(title="Firm Growth Pre and Post Dropping Core Product",
           subtitle = paste0("Firm Growth Variable: ", y),
           x=paste0("Years Relative to Core Product Drop"), y="Growth Rate") +
      theme_minimal()
    ggsave(paste0(output_dir, x, "_", y, "_", "growth pre post core drop.png"), width = 8, height = 5, dpi = 300)
    
  }
  
}





# 6.2) Core and runner up product per size category ----------------------

product_data<-readRDS(paste0("product_creation_destruction", ext, ".RDS"))
setorder(product_data, firmid, year)
product_data<-product_data[, lag_number_of_products:=ifelse( is.na(dplyr::lag(number_of_products, 1)), NA, dplyr::lag(number_of_products, 1)), by=.(firmid)]
product_data<-product_data %>% select(firmid, year, lag_number_of_products, everything())

product_data_temp<-product_data[year!=first_year & year!=last_year]
product_data_temp<-product_data_temp[exit_6==T]
product_data_temp<-merge(product_data_temp, firm_data_select[,  c("firmid", "year", "size", "young")], by=c("firmid", "year"), all.x = T)

make_summary_stats(product_data_temp, c("lag_share_core_pf_6", "lag_share_runup_pf_6", "lag_number_of_products", "number_of_products"), "size", "core_runup_size")

# 6.3) Core and runner up product per size category ----------------------

product_data<-readRDS(paste0("product_creation_destruction", ext, ".RDS"))
product_data<-product_data[, lag_number_of_products:=ifelse( is.na(dplyr::lag(number_of_products, 1)), NA, dplyr::lag(number_of_products, 1)), by=.(firmid)]

product_data_temp<-product_data_temp[year!=first_year & year!=last_year]
product_data_temp<-product_data_temp[, prodcom_rev:=sum(rev, na.rm = T), by=.(firmid, year)]
product_data_temp<-unique(product_data_temp[get(exit_digit)==T, c("firmid", "year", "prodcom_rev")])
product_data_temp<-product_data_temp[, .(yr_core_drop=min(year, na.rm = T)), by=.(firmid)]
# product_data_temp<-product_data_temp[, .(n=.N), by=.(firmid)]

firm_data_select_temp<-firm_data_select[firmid %in% unique(product_data_temp$firmid)]
firm_data_select_temp<-firm_data_select_temp[!is.na(size) & size!="micro"]
firm_data_select_temp<-merge(firm_data_select_temp, product_data_temp, by="firmid", all.x = T)
firm_data_select_temp[, yr_core_drop:=year-yr_core_drop]


data<-product_data[year!=first_year & year!=last_year]
data<-data[, .(rev=sum(rev, na.rm = T)), by=.(firmid, year)]
normal_cols = "rev"
data<-growth_creator(data, normal_cols) %>% select(firmid, year, rev_growth, rev_bar)
firm_data_select_temp<-merge(firm_data_select_temp, data, by=c("firmid", "year"), all.x = T)

folder_name<-"growth pre post core drop"
output_dir<-paste0(output_dir,cpa_or_pf, "/", folder_name , "/")
output_dir_creator(output_dir)
output_dir_og<-output_dir

product_data<-product_data %>% select(firmid, year, cpa, core_pf_6, rev, first_introduction, grep("^new", names(product_data)), 
                                      discontinued, grep("^exit", names(product_data)), everything())

file_conn = file(paste0(output_dir_og, folder_name, '.tex'), 'w')
write( paste0("\\section{Product Reallocation by Firm Size and Age Percentiles} \n"), paste0(output_dir_og, folder_name, '.tex'), append=T)


distrib<-c("rev", "age")

filters<-c("single_product", "multiproduct")

for(filter in filters){
  
  if(filter=="single_product"){
    product_data_temp<-product_data[lag_number_of_products==1]
  }else{
    if(filter=="multiproduct"){
      product_data_temp<-product_data[lag_number_of_products>1]
    }
  }
  
  
  
}

product_data_temp<-product_data_temp[, prodcom_rev:=sum(rev, na.rm = T), by=.(firmid, year)]
product_data_temp<-unique(product_data_temp[exit_6==T, c("firmid", "year", "prodcom_rev")])
product_data_temp<-product_data_temp[, .(yr_core_drop=min(year, na.rm = T)), by=.(firmid)]
# product_data_temp<-product_data_temp[, .(n=.N), by=.(firmid)]

firm_data_select_temp<-firm_data_select[firmid %in% unique(product_data_temp$firmid)]
firm_data_select_temp<-firm_data_select_temp[!is.na(size) & size!="micro"]
firm_data_select_temp<-merge(firm_data_select_temp, product_data_temp, by="firmid", all.x = T)
firm_data_select_temp[, yr_core_drop:=year-yr_core_drop]


data<-product_data[year!=first_year & year!=last_year]
data<-data[, .(rev=sum(rev, na.rm = T)), by=.(firmid, year)]
normal_cols = "rev"
data<-growth_creator(data, normal_cols) %>% select(firmid, year, rev_growth, rev_bar)
firm_data_select_temp<-merge(firm_data_select_temp, data, by=c("firmid", "year"), all.x = T)

folder_name<-"growth pre post core drop"
output_dir<-paste0(output_dir,cpa_or_pf, "/", folder_name , "/")
output_dir_creator(output_dir)
output_dir_og<-output_dir

product_data<-product_data %>% select(firmid, year, cpa, core_pf_6, rev, first_introduction, grep("^new", names(product_data)), 
                                      discontinued, grep("^exit", names(product_data)), everything())

file_conn = file(paste0(output_dir_og, folder_name, '.tex'), 'w')
write( paste0("\\section{Product Reallocation by Firm Size and Age Percentiles} \n"), paste0(output_dir_og, folder_name, '.tex'), append=T)


for(x in distrib){
  
  
  temp<-firm_data_select_temp[, .(nq_growth_uw=mean(nq_growth, na.rm=T),
                                  nq_growth_w=weighted.mean(nq_growth, nq_bar, na.rm=T),
                                  empl_growth_uw=mean(empl_growth, na.rm=T),
                                  empl_growth_w=weighted.mean(empl_growth, empl_bar, na.rm=T),
                                  capital_growth_uw=mean(capital_growth, na.rm=T),
                                  capital_growth_w=weighted.mean(capital_growth, capital_bar, na.rm=T),
                                  rev_growth_uw=mean(rev_growth, na.rm=T),
                                  rev_growth_w=weighted.mean(rev_growth, rev_bar, na.rm=T)),
                              by=.(yr_core_drop, get(x))]
  
  if(x=="young"){
    temp<-temp[, get:=ifelse(get==1, "Young", "Established")]
  }
  
  unique_values<-unique(temp$get)
  base_colors<-setNames(rainbow(length(unique_values)), unique_values)
  color_palette<-c(setNames(sapply(base_colors, function(color) muted(color, l=60)), 
                            paste0(unique_values, " (UW)")),
                   setNames(base_colors, paste0(unique_values, " (W)")))
  
  
  # Adjust the distrib variables from get to their actual name
  setnames(temp, "get", x)
  
  
  temp<-temp[!is.na(get(x)) & yr_core_drop %in% -10:10]
  
  for(y in growth_vars){
    
    # y<-"rev"
    growth_uw<-paste0(y, "_growth_uw")
    growth_w<-paste0(y, "_growth_w")
    
    
    
    
    ggplot(temp, aes(x=yr_core_drop))+
      geom_line(aes(y=get(growth_uw), color=factor(paste0(get(x), " (UW)"))))+
      geom_line(aes(y=get(growth_w), color=factor(paste0(get(x), " (W)"))))+
      scale_color_manual(values=color_palette, name=x)+
      scale_x_continuous(breaks=seq(-10,10, by=1)) + 
      ylim(min(temp %>% select(-x, -yr_core_drop), na.rm = T), 
           max(temp %>% select(-x, -yr_core_drop), na.rm = T))+
      geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
      geom_vline(xintercept = 0, color="black", linetype="dashed") + 
      # ggtitle(paste0("Product Exit Rate by Firm-",j, " Size "))+
      labs(title="Firm Growth Pre and Post Dropping Core Product",
           subtitle = paste0("Firm Growth Variable: ", y),
           x=paste0("Years Relative to Core Product Drop"), y="Growth Rate") +
      theme_minimal()
    ggsave(paste0(output_dir, x, "_", y, "_", "growth pre post core drop.png"), width = 8, height = 5, dpi = 300)
    
  }
  
}






# 7) Merge measures of products introduced and products destroyed in a year to br data ------------------------

product_summary<-readRDS("product_creation_destruction_cpa.RDS")
HHI_firm<-readRDS("HHI_firm_year.RDS")
patent_apps_published<-readRDS("patent_apps_published.RDS")
cis_data<-readRDS("cis_data.RDS") %>% select(-NACE_BR)

firmids_in_firm_data_select_og<-unique(firm_data_select$firmid)


firm_data_select <- merge(firm_data_select, product_summary, by=c("firmid", "year"), all.x = T)
firm_data_select <- merge(firm_data_select, HHI_firm, by=c("firmid", "year"), all.x = T)
firm_data_select <- merge(firm_data_select, patent_apps_published, by=c("firmid", "year"), all = T)
firm_data_select <- merge(firm_data_select, cis_data, by=c("firmid", "year"), all.x = T)

setDT(firm_data_select)

# Additional measures of patenting
firm_data_select[, year_d_pat:=ifelse(is.na(n_publications), NA, year)]
firm_data_select[, year_d_pat:=min(year_d_pat, na.rm = T), by=firmid]
firm_data_select[, d_pat:=ifelse(!is.na(year_d_pat) & year>=year_d_pat, 1,0)]

setorder(firm_data_select, firmid, year)

firm_data_select <- firm_data_select %>% group_by(firmid) %>%   
  mutate(patent_window=case_when(!is.na(n_publications) & n_publications!=0 ~ 1,
                                 !is.na(dplyr::lag(n_publications, 1)) & (year-1)==(dplyr::lag(year, 1)) & dplyr::lag(n_publications, 1)!=0 ~ 1,
                                 !is.na(dplyr::lag(n_publications, 2)) & (year-2)==(dplyr::lag(year, 2)) & dplyr::lag(n_publications, 2)!=0 ~ 1,
                                 !is.na(dplyr::lead(n_publications, 1)) & (year+1)==(dplyr::lead(year, 1)) & dplyr::lead(n_publications, 1)!=0 ~ 1,
                                 !is.na(dplyr::lead(n_publications, 2)) & (year+2)==(dplyr::lead(year, 2)) & dplyr::lead(n_publications, 2)!=0 ~ 1,
                                 TRUE ~ 0))

firm_data_select<-firm_data_select %>% filter(year>2009)
setDT(firm_data_select)
firm_data_select<-firm_data_select[firmid %in% unique(product_data$firmid)]



firm_data_select[, IHI:=(1-HHI)]

firm_data_select_og<-firm_data_select


firm_data_select_og<- firm_data_select_og %>% filter(size!="micro" & size!="extra large" & !is.na(size))

firm_data_select_og$size<-factor(firm_data_select_og$size, levels=c("small", "medium", "large"))

# Measures and outliers in labor productivity
firm_data_select_og[, labor_prod:=ifelse(empl>0, nq/empl, NA)]
firm_data_select_og[, k_to_l:=ifelse(empl>0, capital/empl, NA)]



summary_labor_productivity<-firm_data_select_og %>% arrange(desc(labor_prod)) %>%
  group_by(size) %>% summarise(mean=mean(labor_prod, na.rm=T), 
                               median=median(labor_prod, na.rm=T),
                               p75=quantile(labor_prod, 1-0.25, na.rm = T),
                               p90=quantile(labor_prod, 1-0.1, na.rm = T),
                               p99=quantile(labor_prod, 1-0.01, na.rm = T),
                               p99_9=quantile(labor_prod, 1-0.001, na.rm = T),
                               p99_99=quantile(labor_prod, 1-0.0001, na.rm = T)
  )


summary_labor_productivity<-xtable(summary_labor_productivity)
print.xtable(summary_labor_productivity, file=paste0(output_dir, "summary_stats_labor_productivity", type="latex", include.rownames=F))

table(firm_data_select_og$year)

make_summary_stats(firm_data_select_og, c("empl", "nq", "capital", "firm_age",
                                          "empl_growth", "nq_growth", "capital_growth", 
                                          "t_l", "t_k", "t_m",
                                          "labor_prod", 
                                          "superstar", "young"),
                   "size", paste0("firm_data_select_summary_stats_full_sample"))

make_summary_stats(firm_data_select_og, c("number_of_products", "new_products", "destroyed_products", "paused_products", "reintroduced_products",
                                          "entry_post_exit", "entry_pre_exit", "entry_with_exit",
                                          "exit_post_entry", "exit_pre_entry", "exit_with_entry",
                                          "n_applications", "n_publications",
                                          "HHI"),
                   "size", paste0("product_creation_destruction_summary_stats_full_sample"))

write_rds(firm_data_select_og, paste0("firm_data_select_og_cpa.rds"))

# test<-firm_data_select_og[, c("firmid", "year", "nq_bar", "empl_bar")]
## 7.1) Diversification regressions (ARCHIVED) ----------------------------

firm_data_select_og[, `:=`(log_n_products=log(number_of_products),
                           log_new_products=log(new_products))]

diversification_vars<-c("IHI", "log_n_products", "log_new_products")

# table(firm_data_select_og$number_of_products)
# table(firm_data_select_og$new_products)
# 
# 
# test<-firm_data_select_og %>% filter(number_of_products>=23) %>% select(firmid, year, number_of_products, everything())
# 
# View(product_data[firmid=="302305529"] %>% select(firmid, year, NACE_BR, cpa, rev, rev_l, first_introduction, reintroduced, discontinued, incumbent, everything()))

for(divf_var in diversification_vars){
  
  f_baseline_size<-as.formula(paste0("nq_growth ~ size | NACE_BR^year"))
  f_baseline_divf_var<-as.formula(paste0("nq_growth ~ ", divf_var, " | NACE_BR^year"))
  f_baseline_divf_var_sq<-as.formula(paste0("nq_growth ~ ", divf_var, " + ", divf_var, "^2 | NACE_BR^year"))
  f_size_divf_var<-as.formula(paste0("nq_growth ~ size + ", divf_var, " | NACE_BR^year"))
  f_size_divf_var_sq<-as.formula(paste0("nq_growth ~ size + ", divf_var, " + ", divf_var, "^2 | NACE_BR^year"))
  f_size_divf_var_interaction<-as.formula(paste0("nq_growth ~ size*", divf_var, " | NACE_BR^year"))
  f_size_divf_var_sq_interaction<-as.formula(paste0("nq_growth ~ size*", divf_var, " + size*", divf_var, "^2 | NACE_BR^year"))
  
  formulas<-c("f_baseline_size", "f_baseline_divf_var", "f_baseline_divf_var_sq", 
              "f_size_divf_var", "f_size_divf_var_sq", 
              "f_size_divf_var_interaction", "f_size_divf_var_sq_interaction")
  
  for(i in 1:2){
    
    if(i==1){
      data<-firm_data_select_og
      weight<-data$nq_bar
      label<-"all_firms"
    }else{
      data<-firm_data_select_og[number_of_products>1]
      weight<-data$nq_bar
      label<-"multiproduct_obs"
    }
    
    for (formula in formulas){
      diversification_reg_unweighted<-feols(get(formula), data)
      diversification_reg_weighted<-feols(get(formula), data, weights=weight)
      
      assign(paste0("reg_", formula, "_unweighted"), diversification_reg_unweighted)
      assign(paste0("reg_", formula, "_weighted"), diversification_reg_weighted)
      
    }
    
    models_weighted<-list(reg_f_baseline_size_weighted,
                          reg_f_baseline_divf_var_weighted,
                          reg_f_baseline_divf_var_sq_weighted,
                          reg_f_size_divf_var_weighted,
                          reg_f_size_divf_var_sq_weighted,
                          reg_f_size_divf_var_interaction_weighted,
                          reg_f_size_divf_var_sq_interaction_weighted)
    
    modelsummary(models_weighted, output=paste0(output_dir, "diversification_", divf_var, "_weighted_", label, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                 title=paste0("Diversification and Revenue Growth. Weighted Regressions. Diversification Measure: ", divf_var))
    
    
    models_unweighted<-list(reg_f_baseline_size_unweighted,
                            reg_f_baseline_divf_var_unweighted,
                            reg_f_baseline_divf_var_sq_unweighted,
                            reg_f_size_divf_var_unweighted,
                            reg_f_size_divf_var_sq_unweighted,
                            reg_f_size_divf_var_interaction_unweighted,
                            reg_f_size_divf_var_sq_interaction_unweighted)
    
    modelsummary(models_weighted, output=paste0(output_dir, "diversification_", divf_var, "_unweighted_", label, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                 title=paste0("Diversification and Revenue Growth. Unweighted Regressions. Diversification Measure: ", divf_var))
    
    models<-list(reg_f_baseline_size_weighted,
                 reg_f_baseline_divf_var_weighted,
                 reg_f_baseline_divf_var_sq_weighted,
                 reg_f_size_divf_var_weighted,
                 reg_f_size_divf_var_sq_weighted,
                 reg_f_size_divf_var_interaction_weighted,
                 reg_f_size_divf_var_sq_interaction_weighted,
                 reg_f_baseline_size_unweighted,
                 reg_f_baseline_divf_var_unweighted,
                 reg_f_baseline_divf_var_sq_unweighted,
                 reg_f_size_divf_var_unweighted,
                 reg_f_size_divf_var_sq_unweighted,
                 reg_f_size_divf_var_interaction_unweighted,
                 reg_f_size_divf_var_sq_interaction_unweighted)
    
    modelsummary(models, output=paste0(output_dir, "diversification_", divf_var, "_", label, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                 title=paste0("Diversification and Revenue Growth. Diversification Measure: ", divf_var))
    
    
  }
}




## 7.2) Effect of product introduction/destruction on firm growth ----------------------

firm_data_select_og<-readRDS(paste0("firm_data_select_og_cpa.rds"))

# Filter out entry and exit and this particular firm that is messing up the results of product destruction in t-1
firm_data_select_og[abs(empl_growth)!=2 & firmid!="562094425"]

table(firm_data_select_og$size)

# Define categorical variables of age and size
firm_data_select_og[, size_alt:=fifelse(size=="medium" | size=="large", "large", 
                                        fifelse(empl_bar<50, "small", NA_character_))]
firm_data_select_og[, size_age:=fifelse(is.na(size) | is.na(young), NA_character_,
                                        paste0(size_alt, "_", fifelse(young==1, "young", "established")))]
firm_data_select_og[, young_alt:=ifelse(firm_age>=10, 0, 1)]
firm_data_select_og[, size_age_alt:=fifelse(is.na(size_alt) | is.na(young_alt), NA_character_,
                                        paste0(size_alt, "_", fifelse(young_alt==1, "young", "established")))]

# 
# 
# 
# firm_data_select_og[, size_age_alt:=ifelse(young_alt, paste0(size_alt, "_young"), paste0(size_alt, "_established"))]
# 
# firm_data_select_og[, size_young:=ifelse(young, paste0(size, "_young"), NA)]
# firm_data_select_og[, size_established:=ifelse(young, NA, paste0(size, "_established"))]
# 
# firm_data_select_og[, size_young_alt:=ifelse(young_alt, paste0(size, "_young"), NA)]
# firm_data_select_og[, size_established_alt:=ifelse(young_alt, NA, paste0(size, "_established"))]

baseline_vars<-c("empl", "capital", "nq")
cost_share_vars<-c("t_l", "t_m", "t_k")

# vars<-c(baseline_vars, cost_share_vars)
vars<-c(baseline_vars)


# variables<-list("young", "size", "size_age",  "size_young", "size_established",
#                 "young_alt", "size_age_alt", "size_young_alt", "size_established_alt")

variables<-list("size", "young", "young_alt", "size_age", "size_age_alt")



# Define regression function
regression_reallocation_growth<-function(data, weights, category, filter, creat_var, destr_var, interaction=NULL, firm_fe=T){
  
  # data<-firm_data_select_og
  # weights<-weights
  # category<-"all"
  # filter<-"all"
  # creat_var<-"new_0"
  # destr_var<-"exit_0"
  # interaction<-NULL
  # firm_fe<-T
  # 
  creat_measure<-paste0("factor(", creat_var, lead_or_lag, ">=1)")
  destr_measure<-paste0("factor(", destr_var, lead_or_lag, ">=1)")
  
  if(!is.null(interaction)){
    formula_growth_p_reall<-paste0(var_growth, " ~", creat_measure," *", interaction, " + ", destr_measure,"*", interaction)
  }else{
    if(digit==max(digits)){
      formula_growth_p_reall<-paste0(var_growth, " ~ ",  destr_measure)
    }else{
      formula_growth_p_reall<-paste0(var_growth, " ~ ", creat_measure, " + ", destr_measure)
    }
  }
  
  if(firm_fe){
    f_var_growth_p_reall<-as.formula(paste0(formula_growth_p_reall,  "| firmid+ NACE_BR^year"))
  }else{
    f_var_growth_p_reall<-as.formula(paste0(formula_growth_p_reall,  "| NACE_BR^year"))
  }
  
  print(f_var_growth_p_reall)
  
  regression<-tryCatch(feols(f_var_growth_p_reall, data=data, weights = weights), error=function(e) NA)
  assign(paste0("regression_", var, "_growth_p_reall"), regression)
  print(get(paste0("regression_", var, "_growth_p_reall")))
  
  creat_measure<-paste0("factor(", creat_var, lead_or_lag, " >= 1)TRUE")
  destr_measure<-paste0("factor(", destr_var, lead_or_lag, " >= 1)TRUE")
  
  
  conf1<-tryCatch(confint(regression, parm=creat_measure), error=function(e) data.table(NA, NA))
  point_estimate1<-tryCatch(coef(regression)[creat_measure], error=function(e) NA)
  conf2<-tryCatch(confint(regression, parm=destr_measure), error=function(e) data.table(NA, NA))
  point_estimate2<-tryCatch(coef(regression)[destr_measure], error=function(e) NA)
  
  if(!is.null(interaction)){
    
    conf3<-tryCatch(confint(regression, parm=paste0(creat_measure, ":", interaction)), error=function(e) data.table(NA, NA))
    point_estimate3<-tryCatch(coef(regression)[paste0(creat_measure, ":", interaction)], error=function(e) NA)
    
    conf4<-tryCatch(confint(regression, parm=paste0(destr_measure, ":", interaction)), error=function(e) data.table(NA, NA))
    point_estimate4<-tryCatch(coef(regression)[paste0(destr_measure, ":", interaction)], error=function(e) NA)
    
    conf5<-tryCatch(confint(regression, parm=interaction), error=function(e) data.table(NA, NA))
    point_estimate5<-tryCatch(coef(regression)[interaction], error=function(e) NA)
    
  }
  
  results_temp<-data.table(
    k=k,
    point_estimate_creat=point_estimate1,
    conf_low_creat=conf1[1,1],
    conf_high_creat=conf1[1,2],
    point_estimate_destr=point_estimate2,
    conf_low_destr=conf2[1,1],
    conf_high_destr=conf2[1,2],
    factor=var,
    variable=category,
    filter=filter)
  
  
  if(!is.null(interaction)){
    interactions_table<-data.table(
      point_estimate_creat_interact=point_estimate3,
      conf_low_creat_interact=as.numeric(conf3[1,1]),
      conf_high_creat_interact=as.numeric(conf3[1,2]),
      point_estimate_destr_interact=point_estimate4,
      conf_low_destr_interact=conf4[1,1],
      conf_high_destr_interact=conf4[1,2],
      point_estimate_interact=point_estimate5,
      conf_low_interact=conf5[1,1],
      conf_high_interact=conf5[1,1]
    )
    
    results_temp<-cbind(results_temp, interactions_table)
  }
  
  results<-rbind(results, results_temp, fill=T)
  
  
  
  return(results)
}

output_dir_og<-output_dir

for(digit in c("all", digits)){
  
  # digit<-"6"
  
  results<-data.frame(
    k=numeric(),
    point_estimate_creat=numeric(),
    conf_low_creat=numeric(),
    conf_high_creat=numeric(),
    point_estimate_destr=numeric(),
    conf_low_destr=numeric(),
    conf_high_destr=numeric(),
    
    point_estimate_creat_interact=numeric(),
    conf_low_creat_interact=numeric(),
    conf_high_creat_interact=numeric(),
    point_estimate_destr_interact=numeric(),
    conf_low_destr_interact=numeric(),
    conf_high_destr_interact=numeric(),
    
    point_estimate_interact=numeric(),
    conf_low_interact=numeric(),
    conf_high_interact=numeric(),
    
    
    factor=character(),
    n_obs=numeric()
  )
  
  if(!dir.exists(paste0(output_dir_og, digit, "/"))){
    output_dir<-paste0(output_dir_og, digit, "/")
    output_dir_creator(output_dir)
  }else{
    output_dir<-paste0(output_dir_og, digit, "/")
  }
  
  if(digit=="all"){
    creat_var_temp<-paste0("prod_creat")
    destr_var_temp<-paste0("prod_destr")
    
  }else{
    creat_var_temp<-paste0("new_", digit)
    destr_var_temp<-paste0("exit_", digit)
  }
  

  for(var in vars){
    # var<-"empl"
    print(var)
    
    if(var %in% baseline_vars){
      var_growth<-paste0(var, "_growth")
      print(var_growth)
    }else{
      var_growth<-var
    }
    
    
    for(k in -4:4){
      # k<-0
      if(k>0){
        lead_or_lag<-paste0("_lag", abs(k))
      }else{
        if(k==0){
          lead_or_lag<-paste0("")
        }else{
          if(k<0){
            lead_or_lag<-paste0("_lead", abs(k))
          }
        }
      }
      
      weights<-firm_data_select_og[[paste0(var, "_bar")]]
      results<-regression_reallocation_growth(firm_data_select_og, weights = weights, "all", "all",
                                              creat_var=creat_var_temp,
                                              destr_var=destr_var_temp)
      
      for(variable in variables){

        filters=unique(firm_data_select_og[[variable]])
        filters<-filters[!is.na(filters)]

        if(!is.logical(filter)){
          filters<-filters[filters!="extra large" & filters!="micro"]
        }

        for(filter in filters){
          firm_data_select<-firm_data_select_og[get(variable)==filter]

          weights<-firm_data_select[[paste0(var, "_bar")]]
          # stop("Here")
          results<-regression_reallocation_growth(data=firm_data_select,
                                                  weights = weights,
                                                  category=variable,
                                                  filter=filter,
                                                  creat_var=creat_var_temp,
                                                  destr_var=destr_var_temp,
                                                  firm_fe=T)

        }

      }
      
    }
    
  }
  
  results<-results %>% filter(factor!=TRUE)
  
  for (z in variables) {
    
    results_temp<- results %>% filter(variable==z) %>% filter(factor %in% baseline_vars) 
    
    
    
    y_lim_baseline=c(min(0, results_temp$conf_low_creat, results_temp$conf_low_destr, na.rm = T),
                     max(results_temp$conf_high_creat, results_temp$conf_high_destr, na.rm = T))
    
    results_temp<- results %>% filter(variable==z) %>% filter(factor %in% cost_share_vars)
    
    
    
    
    y_lim_cost_share=c(min(0, results_temp$conf_low_creat, results_temp$conf_low_destr, na.rm = T),
                       max(results_temp$conf_high_creat, results_temp$conf_high_destr, na.rm = T))
    
    
    for(var in vars){
      
      if(var %in% baseline_vars){
        y_lim<-y_lim_baseline
      }else{
        y_lim<-y_lim_cost_share
      }
      
      results_temp<- results %>% filter(factor==var)
      results_temp<- results_temp %>% filter(variable==z | variable=="all")
      
      if(!("conf_low_creat" %in% names(results_temp))){
        results_temp$conf_low_creat<-numeric()
      }
      
      if(!("conf_high_creat" %in% names(results_temp))){
        results_temp$conf_high_creat<-numeric()
      }
      
      
      
      temp_creation<-ggplot(results_temp, aes(x=k, y=point_estimate_creat, color=filter, fill=filter)) +
        geom_point() +
        geom_ribbon(aes(ymin=conf_low_creat, ymax=conf_high_creat), colour=NA, alpha=0.2) +
        geom_line()+
        geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
        geom_vline(xintercept = 0, color="black", linetype="dashed") + 
        scale_x_continuous(breaks=seq(-4, 4, by=1))+
        coord_cartesian(ylim=y_lim)+
        theme_minimal() +
        theme(plot.title = element_text(hjust=0.5))+
        labs(title=paste0(if (var=="nq") "revenue" else var),
             fill=z,
             color=z,
             x="k",
             y="Relation to Product Introduction")
      # ggsave(paste0(output_dir, "prod_creation_", var, "_growth_effects_", z, ".png"), height = 4, width = 6)
      
      
      
      
      
      temp_destruction<-ggplot(results_temp , aes(x=k, y=point_estimate_destr, color=filter, fill=filter)) +
        geom_point() +
        geom_ribbon(aes(ymin=conf_low_destr, ymax=conf_high_destr), colour=NA, alpha=0.2) +
        geom_line()+
        geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
        geom_vline(xintercept = 0, color="black", linetype="dashed") + 
        scale_x_continuous(breaks=seq(-4, 4, by=1))+
        coord_cartesian(ylim=y_lim)+
        theme_minimal() +
        theme(plot.title = element_text(hjust=0.5))+
        labs(x="k",
             fill=z,
             color=z,
             y="Relation to Product Exit")
      # ggsave(paste0(output_dir, "prod_destruction_", var, "_growth_effects_", z, ".png"), height = 4, width = 6)
      
      
      assign(paste0(var, "_", z, "_creation"), temp_creation)
      assign(paste0(var, "_", z, "_destruction"), temp_destruction)
    }
    
    final_plot<-((get(paste0("empl_", z, "_creation"))  +  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()))+
                   (get(paste0("nq_", z, "_creation")) + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())) + 
                   (get(paste0("capital_", z, "_creation"))  + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()))) / 
      (get(paste0("empl_", z, "_destruction")) +  
         (get(paste0("nq_", z, "_destruction")) + theme(axis.title.y=element_blank())) + 
         (get(paste0("capital_", z, "_destruction")) + theme(axis.title.y=element_blank())))+
      plot_layout(guides="collect")+
      theme(legend.position = "right")
    ggsave(paste0(output_dir, "effect of creation and destruction by ", z, " empl nq capital.png"), height = 8, width=11)
    
    if(sum(c("t_l", "t_k", "t_m") %in% vars)==3){
      final_plot<-((get(paste0("t_l_", z, "_creation"))  +  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()))+
                     (get(paste0("t_m_", z, "_creation")) + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())) + 
                     (get(paste0("t_k_", z, "_creation"))  + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()))) / 
        (get(paste0("t_l_", z, "_destruction")) +  
           (get(paste0("t_m_", z, "_destruction")) + theme(axis.title.y=element_blank())) + 
           (get(paste0("t_k_", z, "_destruction")) + theme(axis.title.y=element_blank())))+
        plot_layout(guides="collect")+
        theme(legend.position = "right")
      ggsave(paste0(output_dir, "effect of creation and destruction by ", z, " t_l t_m t_k.png"), height = 8, width=11)
      
    }
    
    
    
  }
  
}




# Summary stats
for(variable in variables){
  # variable<-"young"
  
  filters=unique(firm_data_select_og[[variable]])
  filters<-filters[!is.na(filters)]
  
  if(!is.logical(filter)){
    filters<-filters[filters!="extra large" & filters!="micro"]
  }
  
  firm_data_select_og$share<-NA
  
  for(filter in filters){
    firm_data_select<-firm_data_select_og[get(variable)==filter]
    n_filter=nrow(firm_data_select)
    firm_data_select_og[, share:=ifelse(get(variable)==filter, n_filter/.N, share)]
    # stop("Here")
  }
  
  
  
  make_summary_stats(firm_data_select_og, c(#"empl", "nq", "capital",
    "share",
    "firm_age",
    "nq", "empl",
    "empl_growth", "nq_growth", "capital_growth", 
    "t_l", "t_k", "t_m"),
    #"labor_prod", 
    #"superstar", "young"),
    variable, paste0(variable, "_firm_data_select_summary_stats_full_sample"))
  
}


## 8) Product determinants of high growth young firms ----------------------

firm_data_select_og<-readRDS(paste0("firm_data_select_og_cpa.rds"))

# Leave only needed variables
og_names<-names(firm_data_select_og)
names_to_keep_firm<-c("empl", "capital", "nq", 
                      "empl_bar", "capital_bar", "nq_bar",
                      "empl_growth", "nq_growth", "capital_growth", 
                      "firm_age")
names_to_keep_product<-c("number_of_products",
                         "new_products", "destroyed_products", "paused_products", "reintroduced_products",
                         "patent_window", "n_publications", 
                         names(firm_data_select_og)[grepl("new_[0-9]+$|exit_[0-9]+$", names(firm_data_select_og))])
 
firm_data_select_og<- firm_data_select_og %>% select("firmid", "year", "NACE_BR", names_to_keep_firm, names_to_keep_product)
og_names[grepl("^patent", og_names)]

firm_data_select_og[, patenting:=ifelse(n_publications<=0 | is.na(n_publications), 0, 1)]

# High growth young firms thresholds
threshold_young<-5
threshold_growth<-0.3

# Define high growth young firms
setDT(firm_data_select_og)
firm_data_select_og<-firm_data_select_og[firm_age<=threshold_young & abs(empl_growth)!=2 & abs(nq_growth)]

reall_vars<- names(firm_data_select_og)[grepl("new_[0-9]+$|exit_[0-9]+$", names(firm_data_select_og))]

levels<-c("firm", "firm_year")

for (level in levels) {
  level<-"firm"
  firm_data<-firm_data_select_og
  
  if(level=="firm"){
    firm_data<-firm_data[, c(.(empl_growth=weighted.mean(empl_growth, empl_bar),
                               nq_growth=weighted.mean(nq_growth, nq_bar),
                               capital_growth=weighted.mean(capital_growth, capital_bar),
                               NACE_BR=unique(NACE_BR),
                               new_products=ifelse(sum(new_products, na.rm=T)>0, 1,0),
                               destroyed_products=ifelse(sum(destroyed_products, na.rm = T)>0, 1,0),
                               patenting=ifelse(sum(patenting, na.rm = T)>0, 1, 0)) ,
                             lapply(.SD, sum, na.rm=T)), by=.(firmid), .SDcols = reall_vars]
    
  }

  table(firm_data$new_products)
  table(firm_data$patenting)
  
  firm_data[, top_growth:=fifelse(frank(empl_growth, ties.method="min")/.N>(1-threshold_growth) & 
                                              frank(nq_growth, ties.method="min")/.N>(1-threshold_growth),1,0)]
  
  # Make summary stats
  make_summary_stats(firm_data, intersect(names_to_keep_firm, names(firm_data)) , "top_growth", paste0("high_growth_young_firm_", gsub("[.]", "_", as.character(threshold_growth) ), "_", level ))
  make_summary_stats(firm_data, intersect(c(names_to_keep_product,"patenting"), names(firm_data)), "top_growth", paste0("high_growth_young_product_", gsub("[.]", "_", as.character(threshold_growth) ), "_", level))
  
  formula_agg_reallocation<-"top_growth ~  patenting + new_products + destroyed_products"
  formula_disag_reallocation<-"top_growth ~ factor(new_0>=1) +  factor(new_1>=1) + factor(new_2>=1) + factor(new_4>=1) + factor(exit_0>=1) + factor(exit_1>=1) + factor(exit_2>=1) + factor(exit_4>=1) + factor(exit_6>=1) + patenting"
  
  formulas<-c("formula_agg_reallocation", "formula_disag_reallocation")
  
  for (formula in formulas) {
    formula_baseline<-as.formula(get(formula))
    
    if(level=="firm"){
      formula_NACE_fe<-as.formula(paste0(get(formula), "| NACE_BR"))
      formula_firmid_fe<-NULL
      formula_firmid_NACE_fe<-NULL
      
      model_baseline<-feols(formula_baseline, data=firm_data)
      model_NACE_fe<- feols(formula_NACE_fe, data=firm_data)
      
      models<-list(model_baseline, 
                   model_NACE_fe)

      modelsummary(models, output=paste0(output_dir, "/high_growth_young_reall_innovation_", gsub("[.]", "_", as.character(threshold_growth) ), level, "_", formula, ".tex"), 
                   stars = T, 
                   gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                   title=paste0("Regression on Being a Young Firm in the Top ", threshold_growth, " of Growth on Product Reallocation and Patenting - Level: ", level)
      )
      
      
    }else{
      formula_NACE_fe<-as.formula(paste0(get(formula), "| NACE_BR^year"))
      formula_firmid_fe<-as.formula(paste0(get(formula), "| firmid"))
      formula_firmid_NACE_fe<-as.formula(paste0(get(formula), "| firmid + NACE_BR^year"))
      
      model_baseline<-feols(formula_baseline, data=firm_data, weights = firm_data$empl_bar)
      model_NACE_fe<- feols(formula_NACE_fe, data=firm_data, weights = firm_data$empl_bar)
      model_firmid_fe<-feols(formula_firmid_fe, data=firm_data, weights = firm_data$empl_bar)
      model_firmid_NACE_fe<-feols(formula_firmid_NACE_fe, data=firm_data, weights = firm_data$empl_bar)
      
      models<-list(model_baseline, 
                   model_NACE_fe,
                   model_firmid_fe,
                   model_firmid_NACE_fe)
      
      modelsummary(models, output=paste0(output_dir, "/high_growth_young_reall_innovation_", gsub("[.]", "_", as.character(threshold_growth) ), level, "_", formula, ".tex"), 
                   stars = T, 
                   gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                   title=paste0("Regression on Being a Young Firm in the Top ", threshold_growth, " of Growth on Product Reallocation and Patenting - Level: ", level))

    }

  }
  
  

  
  # Compile the models we ran
  
  
}





## Effect of product introduction/destruction on firm growth ----------------------

firms_patenting <- firm_data_select[, .(d_pat=sum(d_pat, na.rm = T)), by=firmid]
firms_patenting <- firms_patenting[d_pat>=1,]
firms_patenting <- unique(firms_patenting$firmid)

firms_patenting <- firm_data_select[firmid %in% firms_patenting, c("new_products", "firmid", "year", "d_pat", "year_d_pat")]
firms_patenting <- firms_patenting %>% group_by(firmid) %>% mutate(unique_d_pat=paste(unique(d_pat), collapse= ","),
                                                                   d_pat=ifelse(unique_d_pat=="0,1", d_pat, 0)) %>% select(-unique_d_pat)


feols(asin(new_products) ~ d_pat | firmid + year , data=firms_patenting)
