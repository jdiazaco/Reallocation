outliers_remove<-function(data, outliers_threshold){
  data <- data[
    nq_capital >= quantile(nq_capital, outliers_threshold, na.rm = TRUE) & nq_capital <= quantile(nq_capital, 1-outliers_threshold, na.rm = TRUE) &
      nq_empl >= quantile(nq_empl, outliers_threshold, na.rm = TRUE) & nq_empl <= quantile(nq_empl, 1-outliers_threshold, na.rm = TRUE) &
      nq_raw_materials >= quantile(nq_raw_materials, outliers_threshold, na.rm = TRUE) & nq_raw_materials <= quantile(nq_raw_materials, 1-outliers_threshold, na.rm = TRUE)
  ]
  
}