#' @description
#' This function runs a fixed effect regression and returns a list object with results.
#' It only runs a one-time regression (not the set of regressions from -t_k to t_k).
#'
#' @param data: character to indicate the dataset name;
#' @param y: dependent variable in the fixed effect regression;
#' @param x: vector of explanatory variables in the fixed effect regression;
#' @param fix_eff: vector with set of fixed effect;
#' @param weight_var: character with the name of the weight variable;
#' @param k: numeric with the lag/lead information for the regression;
#' @param reg_name: character with user reader name of the regression;
#'
#' @return List object with the set of 
#'
#' @author Julián Díaz-Acosta, 06/03/2025.
#' @export

static_reg_reallocation<-function(data, y, x, fix_eff, weight_var, disag_var, k, reg_name){
  
  # Create data frame where to store results
  results<-data.frame(  )
  
  # k<--0
  if(k<0){
    lead_or_lag<-paste0("_lag", abs(k))
  }else{
    if(k==0){
      lead_or_lag<-paste0("")
    }else{
      if(k>0){
        lead_or_lag<-paste0("_lead", abs(k))
      }
    }
  }
  
  # Assemble variables including lead or lag, accounting for possible interactions
  adjust_x<-function(x, lead_or_lag){
    ifelse(grepl("\\*", x),
           gsub("(.*)\\*(.*)", paste0("\\1", lead_or_lag, "*\\2", lead_or_lag), x),
           paste0(x, lead_or_lag))
  }
  x_adj<-sapply(x, adjust_x, lead_or_lag)
  
  # Create leads and lags if they are not available yet in the dataset, accounting for possible interactions
  for(i in 1:length(x_adj)){
    if(!(x_adj[i] %in% names(data))){ # Detect interaction
      if(grepl("\\*", x_adj[i])){
        temp_vect<-str_split(x_adj[i], "\\*") # Check if each of the terms of the interaction exist and create if not
        if(!(temp_vect[[1]][1] %in% names(data))){
          product_summary<-lead_lag_creator(product_summary, temp_vect[[1]][1], k)
        } 
        if(!(temp_vect[[1]][3] %in% names(data))){
          product_summary<-lead_lag_creator(product_summary, temp_vect[[1]][2], k)
        } 
      }else{
        product_summary<-lead_lag_creator(product_summary, x[i], k)
      }
    }
  }
  
  # Initialize models result data table
  models<-data.table()
  
  for (fe in fix_eff) {

    # Create formulas for regressions considering interactions and fixed effects 
    formula<-paste0(y, " ~ ", paste(x, collapse = " + "))
    if(fe!="") {
      formula<-paste0(formula, " | ", fe)
    }
    formula<-as.formula(formula)
    
    # Store weights in a vector
    weight_data<-data[[paste0(weight_var)]]
    
    
    # Now get the results disaggregating by other variables (normally size and age and their interactions, but could be others)
    for(variable in disag_var){
      
      # Run regression for the all sample
      regression_all<-tryCatch(feols(formula, data=data, weights = weight_data), error=function(e) NA)
      
      # Store the results in the results for export
      models_temp<-data.table(variable=variable,
                              filter="all",
                              fe=fe,
                              reg_name=reg_name,
                              regression=list(regression_all))
      models<-rbind(models_temp, models)
      # models[[paste0(variable, "_all_", fe, "_", reg_name)]]<-regression_all

      # Find the unique realizations of the disaggregating variables to loop over them
      filters=unique(data[[variable]])
      filters<-filters[!is.na(filters)]
      
      for(filter in filters){
        
        # Keep only obs with the specified filters
        data_temp<-data[get(variable)==filter]
        
        # Store weights in a vector
        weight_data<-data_temp[[paste0(weight_var)]]
        
        # Run regressions for the filter 
        regression<-tryCatch(feols(formula, data=data_temp, weights = weight_data), error=function(e) NA)
        
        # Store the results in the results for export
        models_temp<-data.table(variable=variable,
                                filter=filter,
                                fe=fe,
                                reg_name=reg_name,
                                regression=list(regression))
        models<-rbind(models_temp, models)
        # models[[paste0(variable, "_", filter, "_", fe, "_", reg_name)]]<-regression
        
      }
    }
  }
  
  return(models)
  
}


