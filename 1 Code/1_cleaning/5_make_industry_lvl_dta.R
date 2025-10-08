division_levels = as.data.table(read_excel("C:/Users/Public/1. Microprod/Reallocation_work/2 Data/product_harmonization_output/division_to_tech_level.xlsx", 
                                           sheet = "division_to_tech_level"))

firm_yr_lvl_br_dta[, `:=`(
  rank_within_industry = frank(nq_bar, ties.method = "average", na.last = "keep"),
  n_firms_in_industry  = .N
), by = .(year, NACE_BR)] 

#Calculate industry HHI
total_revenue<-firm_data_select[, .(total_revenue=sum(nq, na.rm=T)), by=.(year, NACE_BR)]
total_revenue<-merge(firm_data_select, total_revenue, by=c("year", "NACE_BR"))
total_revenue[, market_share:=nq/total_revenue]
total_revenue[, market_share_squared:=market_share^2]
hhi<-total_revenue[, .(HHI_industry=sum(market_share_squared, na.rm=T),
                       av_firm_size_empl=mean(empl, na.rm=T),
                       median_firm_size_empl=median(empl, na.rm=T),
                       av_firm_size_nq=mean(nq, na.rm=T),
                       median_firm_size_nq=median(nq, na.rm=T)), by=.(year, NACE_BR)]
total_revenue<-NULL; gc() #Delete total_revenue and clean revenue
# firm_data_select<-merge(firm_data_select, hhi, by=c("year", "NACE_BR"))

