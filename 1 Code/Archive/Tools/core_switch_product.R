# Define a function to identify the core products within each firm, by category aggregation level
core_switch_product <- function(data, n_digits) {
  pf <- paste0("pf_", n_digits)
  core <- paste0("core_", pf)
  switch <- paste0("switch_", pf)
  n_core <- paste0("n_core_", pf)
  share_core <- paste0("share_core_", pf)
  share_runup <- paste0("share_runup_", pf)
  
  setDT(data)
  
  # Define category based on number of digits
  data[[pf]] <- if (n_digits == 1) substr(data$DEFind_pf, 1, 1) else substr(data[[cpa_or_pf]], 1, n_digits)
  
  # Aggregate revenue by firm-year-category
  data <- data[, .(rev = sum(rev, na.rm = TRUE)), by = .(firmid, year, cat = get(pf))]
  
  # Compute revenue share within each firm-year
  data[, share := rev / sum(rev, na.rm = TRUE), by = .(firmid, year)]
  
  # Rank products by revenue and extract top 2 categories
  data <- data[order(firmid, year, -rev)]
  data[, rank := frank(-rev, ties.method = "first"), by = .(firmid, year)]
  data[, `:=`(
    share_core = ifelse(rank == 1, share, NA_real_),
    share_runup = ifelse(rank == 2, share, NA_real_)
  ), by = .(firmid, year)]
  
  data[, `:=`(
    share_core = max(share_core, na.rm = TRUE),
    share_runup = max(share_runup, na.rm = TRUE)
  ), by = .(firmid, year)]
  
  # Keep only top ranked categories
  data <- data[ , .SD[rev == max(rev, na.rm = TRUE) & rev > 0], by = .(firmid, year)]
  
  # Summarize core categories
  data <- data[, .(
    n_core = uniqueN(cat),
    cat = paste(unique(cat), collapse = ", "),
    share_core = unique(share_core),
    share_runup = unique(share_runup)
  ), by = .(firmid, year)]
  
  # Replace infinite values
  data[, share_runup := ifelse(is.infinite(share_runup), NA, share_runup)]
  
  setorder(data, firmid, year)
  
  # Flag switching core category
  data<-data[, switch:=!(ifelse(is.na(dplyr::lag(cat, 1)), NA,  str_detect(dplyr::lag(cat), cat))), by=.(firmid)] # Flag if there has been a switch in category  
  
  # Rename output columns  
  setnames(data, c("cat", "switch", "n_core", "share_core", "share_runup"), c(core, switch, n_core, share_core, share_runup)) # Adjust names
  
  return(data)
}
