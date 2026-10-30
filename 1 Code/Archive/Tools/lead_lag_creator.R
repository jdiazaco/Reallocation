lead_lag_creator<-function(data, var, n_lags, to_dummy=T){
  # data<-product_summary
  # creat_var<-"exit_1"
  # destr_var<-"exit_1"
  # n_lags<-4
  setorder(data, firmid, year)
  setDT(data)
  
  if(to_dummy){
    for(i in 1:n_lags){
      x_lag<-paste0(var, "_lag", i)
      x_lead<-paste0(var, "_lead", i)
      
      vars<-c(x_lag, x_lead)
      
      data[, (x_lag):=ifelse( is.na(dplyr::lag(get(var), i)), NA, ifelse(dplyr::lag(get(var), i)>0, 1,0)), by=.(firmid)]
      data[, (x_lead):=ifelse( is.na(dplyr::lead(get(var), i)), NA, ifelse(dplyr::lead(get(var), i)>0, 1,0)), by=.(firmid)]
      
    }
  }else{
    for(i in 1:n_lags){
      x_lag<-paste0(var, "_lag", i)
      x_lead<-paste0(var, "_lead", i)
      vars<-c(x_lag, x_lead)
      data[, (x_lag):=dplyr::lag(get(var), i), by=.(firmid)]
      data[, (x_lead):=dplyr::lead(get(var), i),by=.(firmid)]
    }
  }
  
  return(data)
}
