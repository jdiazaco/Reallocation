growth_creator<-function(data, normal_cols, n_lag, by_vars=c('firmid','year')){
  setDT(data)
  lag_cols = paste0(normal_cols,'_l')
  data<-data %>% select(by_vars, normal_cols)
  data_l = data %>% mutate(year = year + n_lag) 
  colnames(data_l)[names(data_l) %in% normal_cols] = lag_cols
  data = merge(data, data_l,by=by_vars, all = T) 
  
  ## Define 2-year averages,quantiles, growth_rates, shares
  bar_cols = paste0(normal_cols, '_bar')
  growth_cols = paste0(normal_cols, '_growth')
  reallocation_cols = paste0(normal_cols, '_reallocation')
  growth_weighted_cols = paste0(growth_cols, '_weighted')
  reallocation_weighted_cols = paste0(reallocation_cols, '_weighted')
  
  share_cols = paste0(normal_cols, '_share')
  
  for (i in seq_along(normal_cols)){
    col = normal_cols[i]; lag = lag_cols[i]
    
    data[, bar_cols[i] := .5*(get(col)+get(lag))]
    data[year == start,bar_cols[i] := NA]
    
    # if(normal_cols[i]!='nq'){
    data[, share_cols[i] := get(bar_cols[i])/ sum(get(bar_cols[i]), na.rm = T), by = year] 
    data[, reallocation_cols[i] :=  abs(ifelse(get(bar_cols[i]) != 0,
                                               (get(col)-get(lag))/get(bar_cols[i]), 0))]
    data[, growth_cols[i] :=  ifelse(get(bar_cols[i]) != 0,
                                     (get(col)-get(lag))/get(bar_cols[i]), 0)]
    data[, growth_weighted_cols[i] := get(share_cols[i])* get(growth_cols[i])]
    data[, reallocation_weighted_cols[i] := get(share_cols[i])* get(reallocation_cols[i])]
    
    # }
  }
  return(data)
}
