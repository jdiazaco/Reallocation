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
    formula<-paste0(y, " ~ (", paste(x, collapse = " + "), ")")
    
    # if(contrast_var!=""){
    #   formula<-paste0(formula, "*", contrast_var)
    # }
    
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
      
      mfx<-list()
      # # ## START MESS
      # contrast_baseline<-levels(data[[contrast_var]])[1]
      # contrast_levels<-levels(data[[contrast_var]])[-1]
      # contrasts<-c(contrast_baseline, contrast_levels)
      # coef_table<-broom::tidy(regression_all)
      # 
      # # Results for baseline level
      # coefs<-coef_table%>% filter(!grepl(paste(contrast_levels, collapse ="|"), term)) %>% mutate(term=gsub(":+?", "", term))
      # setDT(coefs)
      # coefs[, (contrast_baseline):=paste0(add_stars(estimate, p.value))]
      # coefs[, (paste0(contrast_baseline, "_se")):=paste0("(", round(std.error, 3), ")")]
      # coefs<-coefs %>% select(c("term", contrast_baseline, paste0(contrast_baseline, "_se")))
      # 
      # newdata_list<-list()
      # newdata_list[[contrast_var]] <- contrasts
      # test3<-marginaleffects(regression_all, 
      #                        variables = unique(unlist(strsplit(x, "\\*"))), 
      #                        by=c(contrast_var, "patent_window", "tm_window"))
      #                        # newdata = datagrid(!!rlang::sym(contrast_var):=contrasts, patent_window=c(1, 0), tm_window=c(1,0)))
      # test3<-test3[, c("term", "estimate", "std.error", "p.value", contrast_var, "patent_window", "tm_window")]
      # test3<-pivot_wider(test3, names_from=contrast_var, values_from = c(estimate, std.error, p.value))
      # setDT(test3)
      # test3[,term:=paste0(term, ifelse(tm_window==1, "tm_window", ""))]
      # test3[,term:=paste0(term, ifelse(patent_window==1, "patent_window", ""))]
      # test3<-test3[!grep(contrast_var, term)]
      # 
      # for(i in 1:length(contrast_levels)){
      #   # i<-1
      #   # Create the contrast level character
      #   contrast_level<-paste0(contrast_var, contrast_levels[i])
      #   
      #   # Adjust the names for merging to go through, by taking semicolons and the names of the contrasts
      #   coef_temp<-coef_table%>% filter(grepl(contrast_level, term)) %>% 
      #     mutate(term=ifelse(term==contrast_level, "(Intercept)", term), 
      #            term=gsub(paste0("(^|:?)", contrast_level, "(:?|$)"), "\\1", term)) %>% 
      #     mutate(term=gsub(":+?", "", term))
      #   setDT(coef_temp)
      #   
      #   # Create difference variable
      #   coef_temp[, (paste0("difference_", contrast_levels[i])):=paste0(add_stars(estimate, p.value))]
      #   coef_temp[, (paste0("difference_", contrast_levels[i], "_se")):=paste0("(", round(std.error, 3), ")")]
      #   coef_temp<-coef_temp %>% select(c("term", paste0("difference_", contrast_levels[i]), paste0("difference_", contrast_levels[i], "_se")))
      #   if(i==1){
      #     suffix_baseline<-paste0("_", contrast_baseline)
      #   }else{
      #     # names(coef_temp)<-c("term", paste0(names(coef_temp)[!grepl("term", names(coef_temp))], "_", contrast_levels[i]))
      #   }
      #   suffix_temp<-paste0("_", contrast_levels[i])
      #   
      #   # Update the coefficients table
      #   coefs<-left_join(coefs, coef_temp,
      #                    by=c("term"="term"), suffix=c(suffix_baseline, suffix_temp))
      # }
      # 
      # 
      # # Prepare variables with mfx for outputting
      # for(i in 1:length(contrasts)){
      #   test3[, (paste0("mfx_", contrasts[i])):=(add_stars(unlist(get(paste0("estimate_", contrasts[i]))), unlist(get(paste0("p.value_", contrasts[i])))))]
      #   test3[, (paste0("mfx_", contrasts[i], "_se")):=paste0("(", round(unlist(get(paste0("std.error_", contrasts[i])), 3)), ")")]
      # }
      # 
      # test3<-test3 %>% select(names(test3)[c(1, grep("mfx", names(test3)))])
      # 
      # # Merge regression coefficients with mfx estimates
      # coefs<-merge(coefs, test3, by="term", all.x=T)
      # 
      # # Identify all columns ending in "se"
      # se_cols<-grep("_se$", names(coefs), value=T)
      # 
      # # Extract the base column names
      # base_col <- gsub("^(.*)_se$", "\\1", se_cols)
      # 
      # # Create new rows for each _se column
      # df_se_rows <- coefs %>% select(term, all_of(se_cols)) %>% 
      #   pivot_longer(-term, names_to="s", values_to="value") %>%
      #   mutate(s=gsub("_se$", "", s)) %>%
      #   pivot_wider(names_from=s, values_from=value, values_fill=NA) 
      # 
      # coefs<-rbind(coefs, df_se_rows, fill=T) %>% arrange(term)
      # coefs<-coefs %>% select(names(df_se_rows))
      # 
      # mfx<-list()
      # mfx[[1]]<-coefs
      # 
      # 
      # 
      # ## END MESS
      
      # Store the results in the results for export
      models_temp<-data.table(variable=variable,
                              filter="all",
                              fe=fe,
                              reg_name=reg_name,
                              regression=list(regression_all),
                              mfx=mfx)
      models<-rbind(models_temp, models)
      # models[[paste0(variable, "_all_", fe, "_", reg_name)]]<-regression_all

      # Find the unique realizations of the disaggregating variables to loop over them
      filters=unique(data[[variable]])
      filters<-filters[!is.na(filters)]
      
      ### HERE STARTS THE MESS
      # interact<-as.vector(outer(x, variable, FUN=function(x,y) paste0(x, "*", y)))
      # formula<-paste0(y, " ~ ", paste(interact, collapse = " + "))
      # if(fe!="") {
      #   formula<-paste0(formula, " | ", fe)
      # }
      # formula<-as.formula(formula)
      # 
      # for(filter in filters){
      #   data<-data[, (filter):=ifelse(get(variable)==filter, 1, 0)]
      # }
      # 
      # test<-feols(nq_growth ~ net_product_creat_window * size, data=data, weights = weight_data)
      # test2<-marginaleffects(test, by=c("size"))
      # print(test)
      # data$predicted_nq_growth<-predict(test, newdata = data)
      # test<-data[, .(nq_growth=mean(predicted_nq_growth, na.rm=T)), by=.(size)]
      # 
      # 
      # regression_all<-tryCatch(feols(formula, data=data, weights = weight_data), error=function(e) NA)
      # 
      # modelsummary(test, output=paste0(output_dir, "test.xlsx"), fmt=6,
      #              stars = F, 
      #              gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
      #              title=title)
      # 
      # test<-emmeans(regression_all)
      # test<-predictions(regression_all, new)
      # kable(test, format="latex", booktabs=T) %>% kable_styling(latex_options=c("striped", "hold_position"))
      # xtable(test)
      # 
      # models_by_size<-split(regression_all, f=~size)
      # 
      # emmeans(regression_all, ~patent_window |size)
      ### HERE ENDS THE MESS
      
      
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
        models<-rbind(models_temp, models, fill=T)
        # models[[paste0(variable, "_", filter, "_", fe, "_", reg_name)]]<-regression
        
      }
    }
  }
  
  return(models)
  
}


