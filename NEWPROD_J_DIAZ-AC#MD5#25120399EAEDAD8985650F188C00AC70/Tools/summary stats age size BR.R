source(paste0("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code", "/Main.R"))

# Define the directory path for exporting results
output_dir<-paste0(output_dir, "Product reallocation and firm dynamics/Export 05.12/")
folder_name<-"BR_sum_stats"
output_dir<-paste0(output_dir, folder_name, "/")

# Create the necessary output directory structure
output_dir_creator(output_dir)   

###########

product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_cpa_.RDS"))
firm_data<-readRDS('sbs_br_combined_cleaned.RDS') #Coming from "a. Data preparation.R" part 2
young<-fread("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/firm_birth_year.csv")
young<-firm_data %>% select(firmid, firm_birth_year)
young<-unique(young)
young<-young[!is.na(firm_birth_year)]


# Check that all firms have a single birth year
if(n_distinct(young$firmid)!=nrow(young)){
  stop("Some firms have diferent birth years. Check this")
}


# Define the start and end years for the business registry (BR) data
BR_start_year<-1994
BR_last_year<-2021
PC_start_year<-2009
PC_last_year<-2021

# Identify firms that appear in the product data
firms_in_prodcom<-unique(product_data$firmid)

# Create summary statistics for birth and death information from BR and PC data
birth_death_BR<-firm_data[, .(first_year_BR=as.numeric(min(year[empl>=1], na.rm = T)),
                              last_year_BR=as.double(max(year[empl>=1], na.rm=T)),
                              legal_birth_year=as.numeric(min(firm_birth_year, na.rm = T))), by=.(firmid)]

birth_death_PC<-product_data[, .(first_year_PC=min(year, na.rm = T),
                                 last_year_PC=max(year[died==F], na.rm=T)), by=.(firmid)]

# Merge birth/death statistics from both datasets (BR and PC)
birth_death<-merge(birth_death_BR, birth_death_PC, by="firmid", all.x=T)

# Add computed variables to track firm birth and death information
birth_death[, `:=`(legal_birth_BR=fifelse(legal_birth_year<BR_start_year, 0,1),
                   distance_legal_stat_birth=legal_birth_year-first_year_BR,
                   distance_BR_PC_birth=(first_year_PC-first_year_BR),
                   distance_BR_PC_death=(last_year_PC-last_year_BR),
                   stat_birth=fifelse(first_year_BR==BR_start_year, fifelse(legal_birth_year<BR_start_year, 0, 1), 1),
                   stat_death=fifelse(last_year_BR<BR_last_year, 1,0)
                   )]


# Flag firms that have a valid observed birth year in the product data (PC)
# birth_death[, `:=`(birth_in_prodcom=ifelse(stat_birth==1 & first_year_BR==first_year_PC, 1, 
#                                            fifelse(stat_birth==0, NA_real_, 0)),
#                    death_in_prodcom=ifelse(stat_death==1 & last_year_BR==last_year_PC, 1, 
#                                            fifelse(stat_death==0, NA_real_, 0)))]
birth_death[, `:=`(birth_in_prodcom=ifelse(first_year_BR==first_year_PC, 1, 0),
                   death_in_prodcom=ifelse(stat_death==1 & last_year_BR==last_year_PC, 1, 
                                           fifelse(stat_death==0, NA_real_, 0)))]

# table(birth_death$birth_in_prodcom, useNA = "always")
# table(birth_death$death_in_prodcom, useNA = "always")
# 
# # test<-birth_death[firmid %in% firms_in_prodcom]
#  
# 
# 
# product_data<-merge(product_data, young, by=c("firmid"), all.x=T)
# sum(is.na(product_data$young))
# 
# 
# 
# # test<-product_data %>% mutate(young=ifelse(young==1, "young", "established")) %>% group_by(firmid, young) %>% summarise()
# 
# test<-merge(product_data, birth_death, by="firmid", all.x = T)
# # test<-merge(test, young, by="firmid", all.x = T)
# 
# test[, firm_age:=ifelse(first_year_BR!=1994, year-first_year_BR, 
#                         ifelse(is.na(distance_legal_stat_birth), year-first_year_BR,
#                                ifelse(distance_legal_stat_birth<=0, year-firm_birth_year, year-first_year_BR)))]
# 
# test[, young:=ifelse(is.na(firm_age), NA, ifelse(firm_age<=5, 1, 0))]
# test<-test %>% mutate(young=ifelse(young==1, "young", "established")) %>% group_by(firmid, young) %>% summarise()
# test<-merge(test, birth_death, by="firmid", all.x = T)
# sum(is.na(test$young))
# 
# 
# table(test$birth_in_prodcom, test$young, useNA = "always")
# table(test$death_in_prodcom, test$young, useNA = "always")
# 
# test$young<-NULL
# test<-unique(test)
# table(test$birth_in_prodcom,  useNA = "always")
# table(test$death_in_prodcom,  useNA = "always")


for(dataset in c("product_data", "firm_data")){
  dataset<-"product_data"
  data<-get(dataset)
  
  if(dataset=="firm_data"){
    data<-data[year>=PC_start_year & empl>=1]
  }
  
  test<-merge(data, birth_death, by="firmid", all.x = T)
  # test<-merge(test, young, by="firmid", all.x = T)
  
  test[, firm_age:=ifelse(first_year_BR!=1994, year-first_year_BR, 
                          ifelse(is.na(distance_legal_stat_birth), year-first_year_BR,
                                 ifelse(distance_legal_stat_birth<=0, year-legal_birth_year, year-first_year_BR)))]
  
  test[, young:=ifelse(is.na(firm_age), NA, ifelse(firm_age<=5, 1, 0))]
  table(test$young)
  table(test$year)
  
  shares_by_year<-test[, .(share_young=mean(young),
                           n_young=sum(young==1),
                           n_established=sum(young==0),
                           n=.N), by=year]
  mean(shares_by_year$share_young)
  
  test<-test %>% mutate(young=ifelse(young==1, "young", "established")) %>% group_by(firmid, young) %>% summarise()
  test<-merge(test, birth_death, by="firmid", all.x = T)
  sum(is.na(test$young))
  table(test$young)
  n_distinct(test$firmid)
  
  print(paste0(dataset, ": birth in prodcom x young"))
  table(test$birth_in_prodcom, test$young, useNA = "always")
  print(paste0(dataset, ": death in prodcom x young"))
  table(test$death_in_prodcom, test$young, useNA = "always")
  
  test$young<-NULL
  test<-unique(test)
  print(paste0(dataset, ": birth in prodcom x all"))
  table(test$birth_in_prodcom,  useNA = "always")
  print(paste0(dataset, ": birth in prodcom x young"))
  table(test$death_in_prodcom,  useNA = "always")
  
  setDT(test)
  n_distinct(test$firmid)
  n_distinct(test[young=="young"]$firmid)
  
}



# Merge the birth/death statistics back into the firm data
firm_data<-merge(firm_data, birth_death, by=c("firmid"), all.x = T)





# subsets<-c("all_firms", "prodcom_firms")
subsets<-c("prodcom_firms")


# Initialize LaTeX file for storing results
output_dir_og<-output_dir
file_conn = file(paste0(output_dir_og, folder_name, '.tex'), 'w')
write( paste0("\\section{Summary Statistics on Firm Age, Size, Entry and Exit in BR} \n"), paste0(output_dir_og, folder_name, '.tex'), append=T)

firm_data_og<-firm_data

# Loop through each subset to generate summary statistics
for(subset in subsets){
  
  # Write LaTeX section header for the current subset of firms
  write( paste0("\\subsection{", tools::toTitleCase(gsub("_", " ", subset)), " } \n"), paste0(output_dir_og, folder_name, '.tex'), append=T)
  
  
  # Create output directory for the current subset  
  output_dir_creator(output_dir = output_dir_og, temp = T, sub_folder = subset)
  
  # Filter data for prodcom firms
  if(subset=="prodcom_firms"){
    firm_data<-firm_data_og[firmid %in% firms_in_prodcom]
  }else{
    firm_data<-firm_data_og
  }
  

  # Create a density plot showing birth and death years of firms
  ggplot() +
    geom_density(data = firm_data, aes(x = first_year_BR, fill = "Stat. First Year"), alpha = 0.3) +
    geom_density(data = firm_data, aes(x = last_year_BR, fill = "Stat. Last Year"), alpha = 0.3) +
    geom_density(data = firm_data, aes(x = legal_birth_year, fill = "Legal Birth Year"), alpha = 0.3) +
    labs(title = "Birth and Death Years of Firms",
         subtitle = paste0("Sample: ", subset),
         x = "Year", y = "Density") +
    scale_fill_discrete(name = "Year Type") +
    scale_x_continuous(breaks = seq(min(firm_data$legal_birth_year, na.rm = TRUE), 
                                    max(firm_data$last_year_BR, na.rm = TRUE), 
                                    by = 10)) +
    theme_minimal()
  ggsave(paste0(output_dir, "birth_death_years.png"), width = 11, height = 8)
  gen_latex(paste0(folder_name, "/", subset, "/", "birth_death_years.png"), "image", captions = "", file_conn=paste0(output_dir_og, folder_name, '.tex'))
  
  table(firm_data$size, useNA = "always")
  
  firm_data[, size_alt:=ifelse(size=="medium" | size=="large", "large", size)]
  table(firm_data$size_alt, useNA = "always")
  
  firm_data[, size_age := fifelse(is.na(size) | is.na(young), NA_character_, 
                                  paste0(size_alt, "_", fifelse(young==1, "young", "established")))]
  table(firm_data$size_age, useNA = "always")
  
  firm_data[, young_alt:=ifelse(firm_age>=10, 0, 1)]
  table(firm_data$young_alt, useNA = "always")
  
  firm_data[, size_age_alt := fifelse(is.na(size) | is.na(young), NA_character_, 
                                      paste0(size_alt, "_", fifelse(young_alt==1, "young_alt", "established_alt")))]
  table(firm_data$size_age_alt, useNA = "always")
  
  # firm_data[, size_young:=ifelse(young, paste0(size, "_young"), NA)]
  # firm_data[, size_established:=ifelse(young, NA, paste0(size, "_established"))]
  # 
  # 
  # firm_data[, size_young_alt:=ifelse(young_alt, paste0(size, "_young"), NA)]
  # firm_data[, size_established_alt:=ifelse(young_alt, NA, paste0(size, "_established"))]
  
  baseline_vars<-c("empl", "capital", "nq")
  cost_share_vars<-c("t_l", "t_m", "t_k")
  
  vars<-c(baseline_vars, cost_share_vars)
  
  
  # variables<-list("young", "size", "size_age",  "size_young", "size_established",
  #                 "young_alt", "size_age_alt", "size_young_alt", "size_established_alt")
  
  # Select variables to compute summary statistics on
  variables<-list("size_age",  "size_age_alt", "young", "young_alt")
  
  
  # Loop through the selected variables and compute summary statistics
  for(variable in variables){
    # variable<-"young"
    
    filters=unique(firm_data[[variable]])
    filters<-filters[!is.na(filters)]
    
    if(!is.logical(filter)){
      filters<-filters[filters!="extra large" & filters!="micro"]
    }
    
    firm_data$share<-NA
    
    # Compute share of firms in each category
    for(filter in filters){
      firm_data_select<-firm_data[get(variable)==filter]
      n_filter=nrow(firm_data_select)
      firm_data[, share:=ifelse(get(variable)==filter, n_filter/.N, share)]
      # stop("Here")
    }

    # Generate summary statistics and save to LaTeX
    make_summary_stats(firm_data, c(#"empl", "nq", "capital",
      "share",
      "stat_birth", "stat_death", "legal_birth_BR", "distance_BR_PC_birth", "distance_BR_PC_death", "birth_in_prodcom", "death_in_prodcom"),
      #"labor_prod", 
      #"superstar", "young"),
      variable, paste0(subset, "_", variable, "_firm_data_select_summary_stats_full_sample"))
    gen_latex(paste0(folder_name, "/", subset, "/", subset, "_", variable, "_firm_data_select_summary_stats_full_sample_horizontal"), "table", captions = "", file_conn=paste0(output_dir_og, folder_name, '.tex'))

  }
  
  write("\\clearpage \n", paste0(output_dir_og, folder_name, '.tex'), append=T)
  
}




