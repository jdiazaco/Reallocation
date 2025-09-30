#' NACE classification harmonization
#'
#' @description
#' This function runs a set of fixed effect regression and returns a table with results
#' It uses regression_reallocation_growth-
#' Given a specified dependent variable, independent variables, set of fixed effects,
#' variables to disaggregate (e.g. "size" for firm size) and number of lead/lags,
#' it returns a table with point estimates and confidence intervals for 
#' each of the explanatory variables in the fe regressions. It also includes columns
#' k=each of the lags specified, factor=y, variable=category, filter=filter, 
#' fix_eff=fix_eff 
#' 
#' @param data: character to indicate the dataset name;
#' @param y: dependent variable in the fixed effect regression;
#' @param x: vector of explanatory variables in the fixed effect regression;
#' @param fix_eff: vector with fixed effect components;
#' @param weight_var: character with the name of the weighting variable;
#' @param disag_var: character with name of the subsample variable (e.g. "size" for firm size);
#' @param t_k: numeric with the lead/lag number indicator (t=-x,...,0,...x);
#' @param interaction: vector with data for weights in the regression;
#'
#' @return results dataset with point estimates and confidence intervals for 
#' each of the explanatory variables in the fe regressions. It also includes columns
#' k=t_k, factor=y, variable=category, filter=filter, fix_eff=fix_eff
#'
#' @author Julián Díaz-Acosta., 22/10/2024.
#' @export


dynamic_reg_reallocation<-function(data, y, x, fix_eff, weight_var=NULL, disag_var, n_lags_bw, n_lags_fw){
  
  
  # digit<-"6"
  
  # Create data frame where to store results
  results<-data.frame(  )
  
  
  # Loop for capturing the effects for t=-4, ..., 0, ...4
  for(k in -n_lags_bw:n_lags_fw){
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
          temp_vect_non_adj<-str_split(x[i], "\\*") # Check if each of the terms of the interaction exist and create if not
          
          if(!(temp_vect[[1]][1] %in% names(data))){
            data<-lead_lag_creator(data, temp_vect_non_adj[[1]][1], max(n_lags_bw, n_lags_fw))
          } 
          if(!(temp_vect[[1]][3] %in% names(data))){
            data<-lead_lag_creator(data, temp_vect_non_adj[[1]][2], max(n_lags_bw, n_lags_fw))
          } 
        }else{
          data<-lead_lag_creator(data, x[i], max(n_lags_bw, n_lags_fw))
        }
      }
    }

    for (fe in fix_eff) {
      # Store weights in a vector
      if(is.null(weight_var)){
        weight_data<-NULL
      }else{
        weight_data<-data[[paste0(weight_var)]]
      }

      # First get the results for all firms, later you can disaggregate by other variables
      results_all<-regression_reallocation_growth(data, y, x_adj, fe, weight_data = weight_data, "all", "all", k)
      results_all[,`:=`(x=paste0(y, " = ", paste(x, collapse ="+")))]
      results<-rbind(results, results_all, fill=T)
      
      # Now get the results disaggregating by other variables (normally size and age and their interactions, but could be others)
      for(variable in disag_var){
        
        # Find the unique realizations of the disaggregating variables to loop over them
        filters=unique(data[[variable]])
        filters<-filters[!is.na(filters)]
        
        for(filter in filters){
          
          data_temp<-data[get(variable)==filter]
          
          # Store weights in a vector
          if(is.null(weight_var)){
            weight_data<-NULL
          }else{
            weight_data<-data_temp[[paste0(weight_var)]]
          }
          
          # Run regressions
          results_temp<-regression_reallocation_growth(data=data_temp, 
                                                       y=y, 
                                                       x=x_adj,
                                                       fix_eff = fe,
                                                       weight_data = weight_data, 
                                                       category=variable,
                                                       filter=filter,
                                                       t_k=k)
          
          
          results_temp[,`:=`(x=paste0(y, " = ", paste(x, collapse ="+")))]
          
          results<-rbind(results, results_temp, fill=T)

        }
      }
    }
  }
  return(results)
}
