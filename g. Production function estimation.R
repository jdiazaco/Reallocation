library(data.table)

# Step 0: Convert to data.table and order
setDT(product_data)
setkey(product_data, firmid, prodfra_plus, year)

# Step 1: Calculate log price and log price growth
product_data[, price := rev / sold_q]
product_data[, ln_price := log(price)]
product_data[, ln_growth_p := ln_price - shift(ln_price), by = .(firmid, prodfra_plus)]

# Step 2: Calculate revenue shares and Laspeyres weights
product_data[, s_unr := rev / sum(rev, na.rm = TRUE), by = .(firmid, year)]
product_data[, s_unr_lag := shift(s_unr), by = .(firmid, prodfra_plus)]
product_data[, s_weight_unr := (s_unr + s_unr_lag)/2]
product_data[is.na(s_weight_unr), s_weight_unr := 0]

# Step 3: Aggregate to firm-year level to compute delta_p_unr
firm_index <- product_data[!is.na(ln_growth_p), .(
  delta_p_unr = sum(s_weight_unr * ln_growth_p, na.rm = TRUE),
  total_weight = sum(s_unr, na.rm = TRUE),
  valid = sum(s_unr, na.rm = TRUE) >= 0.95
), by = .(firmid, year)]

setorder(firm_index, firmid, year)

# Step 4: Initialize log price index
firm_index[, P_index_ln := NA_real_]
firm_index[, P_index_ln := ifelse(year == min(year), 0, NA_real_), by = firmid]

# Step 5: Chain the price index recursively
firm_ids <- unique(firm_index$firmid)
for (f in firm_ids) {
  years <- firm_index[firmid == f, year]
  for (i in 2:length(years)) {
    current <- years[i]
    prev <- years[i - 1]
    if (firm_index[firmid == f & year == current, valid]) {
      firm_index[firmid == f & year == current, P_index_ln := 
                   firm_index[firmid == f & year == prev, P_index_ln] + 
                   firm_index[firmid == f & year == current, delta_p_unr]]
    } else {
      firm_index[firmid == f & year == current, P_index_ln := 
                   firm_index[firmid == f & year == prev, P_index_ln]]
    }
  }
}

# Step 6: Exponentiate to get final price index
firm_index[, P_index_unr := exp(P_index_ln)]

# Final Output
firm_price_index <- firm_index[, .(firmid, year, P_index_unr, delta_p_unr, total_weight, valid)]

# Optional: Merge back to product-level
product_data <- merge(product_data, firm_price_index, by = c("firmid", "year"), all.x = TRUE)
