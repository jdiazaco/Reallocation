growth_creator<-function(data, normal_cols, n_lag, by_vars=c('firmid','year'), create_born_died=F, data_type="census"){
  setDT(data)
  lag_cols = paste0(normal_cols, "_l")

  og_names <- names(data)
  
  # if(all(c("born", "died", "status", "birth_year", "death_year") %in% colnames(data))){
  #   vars_to_keep <- c(by_vars, normal_cols, "born", "died", "status")
  # }else {
  #   vars_to_keep <- c(by_vars, normal_cols)
  # }

  if (!(all(c("consolidated_birth_year", "economic_birth_year", "legal_birth_year", "economic_death_year") %in% colnames(data)))) {
    birth_death <- read_parquet("firm_lists/firm_birth_death.parquet")
    data <- merge(data, birth_death %>% select(firmid, setdiff(names(birth_death), names(data))), by = "firmid", all.x = T)
  }

  vars_to_keep <- c(by_vars, normal_cols, "consolidated_birth_year", "economic_birth_year", "legal_birth_year", "economic_death_year")

  # data <- data %>% select(vars_to_keep)

  # data[, `:=`(
  #   first_year = min(year, na.rm = T),
  #   last_year = max(year, na.rm = T)
  # ),
  # by = eval(setdiff(by_vars, "year"))
  # ]

  data_l = data[year<end] %>% mutate(year = year + n_lag) %>% select(by_vars, normal_cols, "consolidated_birth_year", "economic_birth_year", "legal_birth_year", "economic_death_year") 
  # Due to year to year changes in NACE_BR, we should take out this variables form data_l. Otherwise, the rows with the change in years will be duplicated
  
  NACE_vars<-c("NACE_BR", "DEFind_BR", "NACE_2d_BR")
  if(all(NACE_vars %in% names(data_l))){
    data_l <- data_l %>% select(-all_of(NACE_vars))
  }
  
  colnames(data_l)[names(data_l) %in% normal_cols] = lag_cols

  data = merge(data, data_l, by = c(by_vars, "consolidated_birth_year", "economic_birth_year", "legal_birth_year", "economic_death_year"), all = T)
  # data = merge(data, data_l, all = T)
  
  
  for (i in seq_along(normal_cols)){
    data[is.na(get(normal_cols[i])), normal_cols[i] := 0]
    data[is.na(get(lag_cols[i])), lag_cols[i] := 0]
  }
  
  if(create_born_died){

    data[, `:=`(
      born = !is.na(consolidated_birth_year) & consolidated_birth_year == year,
      died = !is.na(economic_death_year) & economic_death_year < year
    )]
  }
  
  if (all(c("born", "died") %in% colnames(data))) {
    for (i in seq_along(normal_cols)) {
      data[born == T, lag_cols[i] := 0]
      data[died == T, normal_cols[i] := 0]
    }
  }

  ## Define 2-year averages,quantiles, growth_rates, shares
  bar_cols = paste0(normal_cols, '_bar')
  growth_cols = paste0(normal_cols, '_growth')
  reallocation_cols = paste0(normal_cols, '_reallocation')
  growth_weighted_cols = paste0(growth_cols, '_weighted')
  reallocation_weighted_cols = paste0(reallocation_cols, '_weighted')
  
  share_cols = paste0(normal_cols, '_share')
  
  for (i in seq_along(normal_cols)) {
    col = normal_cols[i]
    lag = lag_cols[i]

    data[, bar_cols[i] := .5 * (get(col) + get(lag))]
    data[year == start, bar_cols[i] := NA]


    # if(normal_cols[i]!='nq'){
    data[, share_cols[i] := get(bar_cols[i]) / sum(get(bar_cols[i]), na.rm = T), by = year]
    data[, reallocation_cols[i] := abs(ifelse(get(bar_cols[i]) != 0,
      (get(col) - get(lag)) / get(bar_cols[i]), 0
    ))]
    data[, growth_cols[i] := ifelse(get(bar_cols[i]) != 0,
      (get(col) - get(lag)) / get(bar_cols[i]), 0
    )]
    data[, growth_weighted_cols[i] := get(share_cols[i]) * get(growth_cols[i])]
    data[, reallocation_weighted_cols[i] := get(share_cols[i]) * get(reallocation_cols[i])]

    # }
    
    # If we are dealing with a survey, not only first year of data may not be actual entry, but also first year of observing the firm in the data is not really entry
    # Only if the first year in data==consolidated_birth do we set growth and reallocation measures to actual value. If it is first year in data we assume it is not entry and set vars to NA
    # We have to do the same for exit
    if(data_type=="survey"){
      
      if(by_vars[1] != "firmid" & by_vars[1]!="firmid_char"){
        stop("For survey data, by_vars[1] must be 'firmid'")
      }
      
      data[, first_year_temp := min(year, na.rm = T), by = eval(by_vars[1])]
      data[year == first_year_temp & year!=consolidated_birth_year, c(reallocation_cols[i], growth_cols[i], growth_weighted_cols[i], reallocation_weighted_cols[i]) := NA]
      
      data[, max_year_temp := max(year, na.rm = T), by = eval(by_vars[1])]
      data[year == max_year_temp & !died, c(reallocation_cols[i], growth_cols[i], growth_weighted_cols[i], reallocation_weighted_cols[i]) := NA]
      
      data[, first_year_temp := NULL]
      data[, max_year_temp := NULL]
    }
    
  }

  print(paste0("Created the following variables: ", paste(setdiff(names(data), og_names), collapse = ", ")))

  # data <- data %>% select(-normal_cols)
  
  return(data)
}
