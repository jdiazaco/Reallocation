#' Creator of dummy variable for whether a variable is greater than 0 within a time window
#'
#' @description
#' This function takes a dataset, a variable and a time window and creates 
#' a dummy variable that takes the value of 1 if the variable was greater than
#' 0 in that time window and 0 otherwise. It considers possible gaps in the dataset,
#' i.e., it evaluates the time window not by just taking the lagged value of the variable
#' in the data, but making sure that that the lagged value is the actual consecutive
#' previous year or falls within the specified time window.
#'
#' 
#' @param data: dataset on which to perform the operation;
#' @param unit_var: character with the economic unit, usually firmid;
#' @param time_var: character with the time unit, usually year;
#' @param target_var: character with the variable for which the window will be created;
#' @param window_length: number with the length of time (from -window length to 0)
#' @param result_var_name: name of the resulting variable;
#'
#' @return dataset a new variable named as result_var_name, that will contain the
#' window dummy indicator for target_var
#'
#' @author Julián Díaz-Acosta., 25/02/2025.
#' @export


window_var_cretor<-function(data, unit_var, time_var, target_var, window_length_bw, window_length_fw=0, result_var_name, na_rm){
  do.call(setorder, c(list(data), as.list(unit_var, time_var)))
  
  # data[, (result_var_name):=as.integer(Reduce(`|`, shift(get(target_var)>0, 0:window_length, type="lag", fill=0))), by=get(unit_var)]
  data[, (result_var_name):=as.integer(
    # sapply(year, function(y) any(get(target_var)[year >= (y-window_length) & year <= y]>0, na.rm = T))
    sapply(year, function(y) 
      if(na_rm){
        any(get(target_var)[year >= (y-window_length_bw) & year <= y+window_length_fw]>0, na.rm = T)
      }else{
        any(get(target_var)[year >= (y-window_length_bw) & year <= y+window_length_fw]>0, na.rm = F)
      }
    )
  ), by=get(unit_var)]
  
  return(data)
}
