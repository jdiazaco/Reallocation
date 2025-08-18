#' Calculate Weighted and Unweighted Means
#'
#' This function calculates both weighted and unweighted means for specified variables in a dataset, 
#' grouped by a specified grouping variable. The function can compute weighted means using a provided 
#' weight variable, and optionally includes the calculation of unweighted means.
#'
#' @param data A data.table or data.frame containing the dataset from which to calculate means.
#' @param weight_var A string representing the name of the variable to be used for weighting.
#' @param variables_to_weight A vector of strings, each representing the names of the variables 
#' to be weighted and averaged.
#' @param group_var A string representing the name of the grouping variable by which to aggregate results.
#' @param weighted A logical value indicating whether to calculate weighted means. Defaults to TRUE.
#' 
#' @return A data.table containing the grouped variable and the calculated means (both weighted and unweighted) 
#' for each specified variable.
#'
#' @examples
#' # Sample data
#' library(data.table)
#' data <- data.table(firmid = c(1, 1, 2, 2, 3, 3),
#'                    year = c(2020, 2020, 2021, 2021, 2020, 2020),
#'                    sales = c(100, 200, 150, 250, 300, 400),
#'                    weight = c(1, 1, 1, 1, 2, 2))
#'
#' # Calculate weighted and unweighted means
#' results <- calculate_weighted_means(data, 
#'                                      weight_var = "weight", 
#'                                      variables_to_weight = c("sales"), 
#'                                      group_var = "firmid", 
#'                                      weighted = TRUE)
#'
#' print(results)
#'
#' @import data.table
#' @export
#' 
#' 
calculate_weighted_means <- function(data, weight_var, variables_to_weight, group_var, weighted = TRUE) {
  
  # Initialize an empty list to store each variable's calculation
  calculations <- lapply(variables_to_weight, function(var) {
    
    # Initialize a result data.table for the current variable
    result <- data.table(get = unique(data[[group_var]]))
    
    # Compute weighted average if requested
    if (weighted) {
      weighted_mean <- data[, .(weighted_mean = sum(get(var) * get(weight_var), na.rm = TRUE) / 
                                  sum(get(weight_var)[!is.na(get(var))], na.rm = TRUE)), 
                            by = get(group_var)]
      setnames(weighted_mean, "weighted_mean", paste0("weighted_", var))
      result <- merge(result, weighted_mean, by = "get", all.x = TRUE)
    }
    
    # Compute unweighted average
    unweighted_mean <- data[, .(unweighted_mean = mean(get(var), na.rm = TRUE)), 
                            by = get(group_var)]
    setnames(unweighted_mean, "unweighted_mean", paste0("unweighted_", var))
    result <- merge(result, unweighted_mean, by = "get", all.x = TRUE)
    
    return(result)
  })
  
  # Combine the calculations into a single data.table by binding columns
  result <- Reduce(function(x, y) merge(x, y, all = TRUE), calculations)
  
  # Fix names (optional)
  setnames(result, "get", get("group_var"))
  
  return(result)
}



# Define a function that takes data, weight, and variables to weight as arguments
calculate_weighted_means <- function(data, weight_var, variables_to_weight, group_var, weighted = TRUE) {
  
  # Initialize an empty list to store each variable's calculation
  calculations <- lapply(variables_to_weight, function(var) {
    
    # Initialize a result data.table for the current variable
    # result <- data[, .(group = get(group_var))]  # Start with the group variable
    result<-data.table(get=unique(data[[group_var]]))
    
    # Compute weighted average if requested
    if (weighted) {
      weighted_mean <- data[, .(weighted_mean = sum(get(var) * get(weight_var), na.rm = TRUE) / 
                                  sum(get(weight_var)[!is.na(get(var))], na.rm = TRUE)), 
                            by = get(group_var)]
      setnames(weighted_mean, "weighted_mean", paste0("weighted_", var))
      result <- merge(result, weighted_mean, by = "get", all.x = TRUE)
    }
    
    # Compute unweighted average
    unweighted_mean <- data[, .(unweighted_mean = mean(get(var), na.rm = TRUE)), 
                            by = get(group_var)]
    setnames(unweighted_mean, "unweighted_mean", paste0("unweighted_", var))
    result <- merge(result, unweighted_mean, by = "get", all.x = TRUE)
    
    return(result)
  })
  
  # Combine the calculations into a single data.table by binding columns
  result <- Reduce(function(x, y) merge(x, y, all = TRUE), calculations)
  
  # Fix names (optional)
  setnames(result, "get", get("group_var"))
  
  return(result)
}
