source("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Main.R")
output_dir<-paste0(output_dir, "Product reallocation and firm dynamics/Export 09.11/")
output_dir<-paste0(output_dir, "BR_sum_stats/")

output_dir_creator(output_dir)   


firm_data<-readRDS('sbs_br_combined_cleaned.rds') #Coming from "a. Data preparation.R" part 2


firm_data[, size_alt:=ifelse(size=="medium" | size=="large", "large", "small")]
firm_data[, size_age:=ifelse(young, paste0(size_alt, "_young"), paste0(size_alt, "_established"))]
firm_data[, young_alt:=ifelse(firm_age>=10, F, T)]
firm_data[, size_age_alt:=ifelse(young_alt, paste0(size_alt, "_young"), paste0(size_alt, "_established"))]

firm_data[, size_young:=ifelse(young, paste0(size, "_young"), NA)]
firm_data[, size_established:=ifelse(young, NA, paste0(size, "_established"))]


firm_data[, size_young_alt:=ifelse(young_alt, paste0(size, "_young"), NA)]
firm_data[, size_established_alt:=ifelse(young_alt, NA, paste0(size, "_established"))]

baseline_vars<-c("empl", "capital", "nq")
cost_share_vars<-c("t_l", "t_m", "t_k")

vars<-c(baseline_vars, cost_share_vars)


variables<-list("young", "size", "size_age",  "size_young", "size_established",
                "young_alt", "size_age_alt", "size_young_alt", "size_established_alt")


for(variable in variables){
  # variable<-"young"
  
  filters=unique(firm_data[[variable]])
  filters<-filters[!is.na(filters)]
  
  if(!is.logical(filter)){
    filters<-filters[filters!="extra large" & filters!="micro"]
  }
  
  firm_data$share<-NA
  
  for(filter in filters){
    firm_data_select<-firm_data[get(variable)==filter]
    n_filter=nrow(firm_data_select)
    firm_data[, share:=ifelse(get(variable)==filter, n_filter/.N, share)]
    # stop("Here")
  }
  
  
  
  make_summary_stats(firm_data, c(#"empl", "nq", "capital",
    "share",
    "firm_age",
    "nq", "empl",
    "empl_growth", "nq_growth", "capital_growth", 
    "t_l", "t_k", "t_m"),
    #"labor_prod", 
    #"superstar", "young"),
    variable, paste0(variable, "_firm_data_select_summary_stats_full_sample"))
  
}
