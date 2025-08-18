#' @description
#' This function runs a fixed effect regression and returns a table with results
#' It is supposed to go along with dynamic_reg_reallocation.
#' It only runs and one-time regression (not the set of regressions from -t_k to t_k).
#' 
#'
#' @param data: character to indicate the dataset name;
#' @param y: dependent variable in the fixed effect regression;
#' @param x: vector of explanatory variables in the fixed effect regression;
#' @param fix_eff: character with fixed effect;
#' @param weight_data: vector with data for weights in the regression;
#' @param category: character with name of the subsample variable (e.g. "size" for firm size);
#' @param filter: character with the subsample variable (e.g. "small", "medium" or "large" for firm size);
#' @param t_k: numeric with the lead/lag number indicator (t=-x,...,0,...x);
#' @param interaction: vector with data for weights in the regression;

#' @param weight_emp: logical to decide whether or not to weight code matches
#'        by employment (instead of number of firms).
#' @param cumsh_del: number indicating the cumulative residual share of firms
#'        deleted for the construction of year by year concordances.
#' @param sh_del: number indicating the share of firms deleted for the
#'        construction of year by year concordances.
#'
#' @return results dataset with point estimates and confidence intervals for 
#' each of the explanatory variables in the fe regressions. It also includes columns
#' k=t_k, factor=y, variable=category, filter=filter, fix_eff=fix_eff
#'
#' @author Julián Díaz-Acosta., 22/10/2024.
#' @export

regression_reallocation_growth<-function(data, y, x, fix_eff,  weight_data, category, filter, t_k){
  results<-data.table()

  # Create formulas for regressions considering interactions and fixed effects 
  formula<-paste0(y, " ~ ", paste(x, collapse = " + "))
  if(fix_eff!="") {
    formula<-paste0(formula, " | ", fix_eff)
  }
  formula<-as.formula(formula)
  
  
  # Run regressions 
  regression<-tryCatch(feols(formula, data=data, weights = weight_data), error=function(e) NA)
  print(regression)
  
  # Regression may not run (e.g. because of multicollinarity with the fixed effects).
  # If this happens, fill the results table with NAs
  
  if(is.list(regression)){
    # Adjust the variables names after running the regression
    x<-setdiff(names(coef(regression)), "(Intercept)")
    
    # Start the results data table with the general variables
    results_temp<-data.table(
      k=t_k,
      factor=y,
      variable=category,
      filter=filter,
      fix_eff=fix_eff)
    
    for(i in 1:length(x)){
      
      # Define var name
      var<-gsub(":", "_", x[i])
      var<-gsub(paste0("(_lag\\d+|_lead\\d+)"), "", var)
      var<-gsub(paste0("TRUE"), "", var)
      
      
      # Capture and store point estimate information
      point_estimate<- tryCatch(coef(regression)[x[i]], error=function(e) NA)
      results_temp[, (var):=point_estimate]
      
      # Capture and store confidence interval information
      conf<- tryCatch(confint(regression, parm=x[i]), error=function(e) data.table(NA, NA))
      conf_high_var<-paste0("chigh_", var)
      results_temp[, (conf_high_var):=conf[1,2]]
      conf_low_var<-paste0("clow_", var)
      results_temp[, (conf_low_var):=conf[1,1]]
      results_temp[, nobs:=regression$nobs]
      
    }
  }else{
    results_temp<-data.table(
      k=t_k,
      factor=y,
      variable=category,
      filter=filter,
      fix_eff=fix_eff)
  }
  
  
  # Gather all results
  results<-rbind(results, results_temp)
  
  return(results)
  

  
}
