create_lags <- function(data, vars_to_lag, id_vars, time_var, all=T) {
  data <- as.data.table(data)
  lag_dt <- data[, .SD, .SDcols = c(id_vars, time_var, vars_to_lag)]
  lag_names <- paste("lag_", vars_to_lag, sep="")
  colnames(lag_dt)[names(lag_dt) %in% vars_to_lag] = lag_names
  lag_dt[[time_var]] <- lag_dt[[time_var]] + 1
  data <- merge(data, lag_dt, by = c(id_vars, time_var), all.x=T, all.y=all)
  return(data)
}
