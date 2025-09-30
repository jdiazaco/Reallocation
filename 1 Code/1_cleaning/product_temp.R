# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
parameters(prodfra_or_pcc8, only_prodfra_in_prodcom)

##import supplementary data
harmonized_prodfra = fread(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/product_harmonization_output/harmonized codes/prodfra_harmonized_2009to2023_', prodfra_or_pcc8, '.csv'))
active_firm_list = readRDS('active_firm_list.rds')
birth_death = readRDS('firm_birth_death.rds') 
#Juli?n: Pc8_entry_year
PC8_entry_year <- fread('PC8_years_entry/PC8_prodfra_years_entry.csv', select=c("codes", "code_entry_year"))
unit_collapsed <- fread("~/Reallocation/6 Publish/1 Code/Ancillary datasets/unit_collapsed.csv")
unit_collapsed_M2<-unit_collapsed %>% mutate(unit_obv="M2")
unit_collapsed<-unique(rbind(unit_collapsed, unit_collapsed_M2)); rm(unit_collapsed_M2);gc()

## import the prodcom data + harmonize product codes 
start = 2009
end = 2023

product_data = rbindlist(lapply(c(start:end),function(yr){
  # yr<-2022
  print(yr)
  # import product data 
  # filepath = paste0(raw_dir,'prodcom/prodcom',yr,'.csv')
  filepath = paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Raw_data/Data/prodcom/new_2025/eap',yr, '_pp.csv')
  dta_temp = fread(filepath)
  dta_temp[, firmid := as.character(firmid)]
  dta_temp[, pcc8 := as.character(pcc8)]
  dta_temp$firmid<-str_pad(dta_temp$firmid, width = 9, side="left", pad="0")
  
  #Juli?n: Replace NAs with 0s
  vars<-c("rev", "prod_q", "sold_q")
  for(var in vars){
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
    dta_temp[[var]][is.na(dta_temp[[var]])]<-0
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
  }
  
  #keep product values that aren't dropped in harmonization
  prodfra_var = paste0('prodfra_',yr)
  prodfra_codes =  unique(harmonized_prodfra %>% select(prodfra_var,prodfra_plus) %>% filter(!is.na(prodfra_var)))
  dta_temp = merge(dta_temp, prodfra_codes,by.x = prodfra_or_pcc8, by.y = prodfra_var)
  
  #keep firms that employ labor
  dta_temp = merge(dta_temp, active_firm_list, by = c('year','firmid'))
  
  # define active as positive rev or quantities, keep only active products
  dta_temp = dta_temp[(rev + sold_q + prod_q)>0]
  
  #Juli?n: Merge information about the year of codes' first appearance in harmonized PC8 tables
  dta_temp = merge(dta_temp, PC8_entry_year, by.x = 'pcc8', by.y = "codes", all.x = T)
  
  # collapse data to firm-prodfra_plus level
  dta_temp = dta_temp[, .(rev = sum(rev, na.rm = T), 
                          code_entry_year=min(code_entry_year), 
                          sold_q=sum(sold_q, na.rm = T)),
                      by= .(firmid, prodfra_plus, year, unit)]#, code_entry_year)]
  
  #generate product-HHI variable
  dta_temp[, total_rev := sum(rev, na.rm = T), by = firmid]
  dta_temp[,HHI := ifelse(total_rev > 0, sum((rev/total_rev)^2),NA), by = firmid]
  dta_temp[, total_rev:=NULL]
  
  ## generate active status marker
  dta_temp[,active := 1]
}))

# Adjust units 
product_data[, unit_collapsed:=paste(unique(unit[!is.na(unit) & unit!="M2"]), collapse=","), by=.(firmid,prodfra_plus)]
product_data<-merge(product_data, unit_collapsed, by.x=c("unit_collapsed", "unit"), by.y=c("unit_collapsed", "unit_obv"), all.x = T)
product_data[, sold_q:=sold_q*factor]

saveRDS(product_data, paste0("product_data_", filter_indicator,  "_.RDS"))
