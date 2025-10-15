winsorize <- function(data, outliers_threshold) {
    cols <- c("nq_capital", "nq_empl", "nq_raw_materials")
    for (col in cols) {
        lower <- quantile(data[[col]], outliers_threshold, na.rm = TRUE)
        upper <- quantile(data[[col]], 1 - outliers_threshold, na.rm = TRUE)
        data[[col]] <- pmin(pmax(data[[col]], lower), upper)
    }
    data
}