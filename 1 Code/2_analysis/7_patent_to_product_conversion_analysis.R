# =============================================================================
# Module 7: From Patents to Products – Has French Innovation Become Less Disruptive?
# =============================================================================
# Objective: Analyze the conversion of patents to products, its evolution, and its real effects.
# Data: Uses patenting_products and variables constructed in previous modules.
# =============================================================================


# --- Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# --- Data Loading ------------------------------------------------------------
# Always load the main dataset explicitly (standalone script)
patenting_products <- read_rds("temp/patenting_products_firm_level.RDS")
patenting_products <- as.data.table(patenting_products)
setorder(patenting_products, firmid, year)

# --- Parameters --------------------------------------------------------------
conversion_windows <- c(2, 3, 5) # years: baseline=2, robustness=3,5
burst_threshold <- 5
burst_thresholds <- c(2, 5, 10)

# --- Variable Construction ---------------------------------------------------
# Product burst indicator
patenting_products[, burst := as.integer(prod_added > burst_threshold)]
for (i in burst_thresholds) {
  patenting_products[, (paste0("burst_", i)) := as.integer(prod_added >= i)]
}

# Patent burst indicator (90th percentile within firm)
patenting_products[, p90_patents := quantile(num_pat_families, 0.90, na.rm = TRUE), by = firmid]
patenting_products[, p75_patents := quantile(num_pat_families, 0.75, na.rm = TRUE), by = firmid]
patenting_products[, patent_burst := as.integer(num_pat_families > p90_patents)]
patenting_products[, patent_burst_p75 := as.integer(num_pat_families > p75_patents)]

# Firm size bin (quartiles)
if ("firm_size" %in% names(patenting_products)) {
  patenting_products[, size_bin := cut(firm_size, quantile(firm_size, probs=0:4/4, na.rm=TRUE), include.lowest=TRUE)]
}

# Firm age bin
if ("firm_age" %in% names(patenting_products)) {
  patenting_products[, age_bin := cut(firm_age, breaks = c(0,5,10,20,100), labels = c("0-5","6-10","11-20","21+"), include.lowest=TRUE)]
}

# NACE4 (industry 4-digit)
if ("NACE_BR" %in% names(patenting_products)) {
  patenting_products[, NACE4 := substr(NACE_BR, 1, 4)]
}

# --- 1. Identify Patent→Product Conversion -----------------------------------
# For each firm-year, count patents filed in year t, and products added in t:(t+window)

for (w in conversion_windows) {
  # For each firm-year, sum products added in t:(t+w)
  patenting_products[, paste0("future_products_", w) := 
    shift(frollsum(prod_added, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]
  # Number of converted patents: min(patents, future products)
  patenting_products[, paste0("converted_patents_", w) := 
    pmin(num_pat_families, get(paste0("future_products_", w)))
  ]
  # Non-converted patents: remainder
  patenting_products[, paste0("nonconverted_patents_", w) := 
    num_pat_families - get(paste0("converted_patents_", w))
  ]
}


# --- 2. Plot Conversion Rate by Year and Firm Size/Age -----------------------
# Example for baseline window (2 years)
patenting_products[, conversion_rate_2 := fifelse(num_pat_families > 0, converted_patents_2 / num_pat_families, NA_real_)]

# Plot: mean conversion rate by year
library(ggplot2)
conv_by_year <- patenting_products[, .(mean_conv = mean(conversion_rate_2, na.rm=TRUE)), by=year]
print(ggplot(conv_by_year, aes(x=year, y=mean_conv)) +
  geom_line() +
  labs(title="Patent→Product Conversion Rate by Year", y="Conversion Rate", x="Year"))

# Plot: by firm size/age bins (example: size quartiles)
if ("size_bin" %in% names(patenting_products)) {
  conv_by_year_size <- patenting_products[, .(mean_conv = mean(conversion_rate_2, na.rm=TRUE)), by=.(year, size_bin)]
  print(ggplot(conv_by_year_size, aes(x=year, y=mean_conv, color=size_bin)) +
    geom_line() +
    labs(title="Conversion Rate by Year and Firm Size", y="Conversion Rate", x="Year"))
}

# --- 3. Regression: Conversion Rate Trends -----------------------------------
# Pooled regression (add FEs as needed)
lm_conv <- lm(conversion_rate_2 ~ year, data=patenting_products)
summary(lm_conv)


# --- 4. Event Study: Real Effects of Converted vs Non-Converted --------------
# For each product launch tied to a patent, align outcomes (sales, employment, etc) around event year
# Compare to patent bursts with no product launch

# Create event time for each firm-year relative to converted patent event
event_window <- 4  # years before/after
patenting_products[, event_year := NA_integer_]
patenting_products[converted_patents_2 > 0, event_year := 0]
patenting_products <- lead_lag_creator(patenting_products, "event_year", n_lags = event_window, to_dummy = FALSE)

# Stack event time for converted patent events
event_cols <- grep("^event_year_(lag|lead)\\d+$", names(patenting_products), value = TRUE)

event_panel <- rbindlist(lapply(event_cols, function(col) {
    dt <- patenting_products[, .(
        firmid, year, sales, empl, prod_added, converted_patents_2, nonconverted_patents_2,
        event_time = get(col)
    )]
    dt[, event_time_type := col]
    dt
}), use.names = TRUE, fill = TRUE)

event_panel <- event_panel[!is.na(event_time)]
event_panel[, rel_year := as.integer(sub(".*?(\\d+)$", "\\1", event_time_type))]
event_panel[, rel_year := fifelse(grepl("lag", event_time_type), -rel_year, rel_year)]

# Calculate mean outcomes by event time

# --- Standard Event Study using fixest::sunab() ---
# Prepare event study panel: one row per firm-year, with event time relative to conversion
# Define treatment: 1 if firm has a converted patent in year t (event year), 0 otherwise
patenting_products[, treat := as.integer(converted_patents_2 > 0)]

# Create relative year (event time) variable for all firm-years

# Correct event_time coding for sunab():
# For each firm, set event_time to first treatment year if ever treated, else NA
patenting_products[, event_time := NA_integer_]
first_treat_year <- patenting_products[converted_patents_2 > 0, .(first_treat_year = min(year)), by = firmid]
patenting_products <- merge(patenting_products, first_treat_year, by = "firmid", all.x = TRUE, sort = FALSE)
patenting_products[!is.na(first_treat_year), event_time := first_treat_year]
patenting_products[, first_treat_year := NULL]

# Estimate event study using fixest::sunab()
# Outcome: sales (can repeat for empl, prod_added, etc.)
library(fixest)
es_model <- feols(sales ~ sunab(event_time, year, ref.p = -1) | firmid + year, data = patenting_products)
summary(es_model)

# Plot event study coefficients

# --- Classic Event Study by Event-Year Size Bin ---
if ("firm_size" %in% names(event_panel)) {
  # Assign event-year size bin to each event (if not already present)
  if (!"event_size_bin" %in% names(event_panel)) {
    event_panel[, event_size := NA_real_]
    event_panel[rel_year == 0, event_size := firm_size]
    event_panel[, event_size := zoo::na.locf(event_size, na.rm=FALSE), by=firmid]
    qs <- quantile(event_panel$event_size, probs=0:4/4, na.rm=TRUE)
    event_panel[, event_size_bin := cut(event_size, qs, include.lowest=TRUE)]
  }
  event_panel_group <- event_panel[!is.na(event_size_bin),
    .(
      mean_sales = mean(sales, na.rm=TRUE),
      se_sales = sd(sales, na.rm=TRUE)/sqrt(.N)
    ),
    by = .(rel_year, event_size_bin)
  ]
  event_panel_group[, ci_low := mean_sales - 1.96*se_sales]
  event_panel_group[, ci_high := mean_sales + 1.96*se_sales]
  print(
    ggplot(event_panel_group, aes(x=rel_year, y=mean_sales, color=event_size_bin, group=event_size_bin)) +
      geom_line() +
      geom_ribbon(aes(ymin=ci_low, ymax=ci_high, fill=event_size_bin), alpha=0.2, color=NA) +
      labs(title="Event Study: Sales by Size Bin", x="Years from Event", y="Mean Sales") +
      theme_minimal()
  )
}

# --- Classic Event Study by Event-Year Age Bin ---
if ("firm_age" %in% names(event_panel)) {
  if (!"event_age_bin" %in% names(event_panel)) {
    event_panel[, event_age := NA_real_]
    event_panel[rel_year == 0, event_age := firm_age]
    event_panel[, event_age := zoo::na.locf(event_age, na.rm=FALSE), by=firmid]
    event_panel[, event_age_bin := cut(event_age, breaks = c(0,5,10,20,100), labels = c("0-5","6-10","11-20","21+"), include.lowest=TRUE)]
  }
  event_panel_group_age <- event_panel[!is.na(event_age_bin),
    .(
      mean_sales = mean(sales, na.rm=TRUE),
      se_sales = sd(sales, na.rm=TRUE)/sqrt(.N)
    ),
    by = .(rel_year, event_age_bin)
  ]
  event_panel_group_age[, ci_low := mean_sales - 1.96*se_sales]
  event_panel_group_age[, ci_high := mean_sales + 1.96*se_sales]
  print(
    ggplot(event_panel_group_age, aes(x=rel_year, y=mean_sales, color=event_age_bin, group=event_age_bin)) +
      geom_line() +
      geom_ribbon(aes(ymin=ci_low, ymax=ci_high, fill=event_age_bin), alpha=0.2, color=NA) +
      labs(title="Event Study: Sales by Age Bin", x="Years from Event", y="Mean Sales") +
      theme_minimal()
  )
}

# --- Event Study by Initial Size and Age Category ---
# For each event, assign the event-year size and age category to all event window observations ("frozen" at event year)

# 1. By Size

# --- 5. Deterrence Test: Competitor Product Entry ----------------------------
# For each firm-year, define competitor set (same 4-digit industry)
# Regress competitor product entry on leader's converted/non-converted patents

# Assume NACE_BR is 4-digit industry code
patenting_products[, NACE4 := substr(NACE_BR, 1, 4)]

# For each firm-year, sum product entry of competitors (excluding focal firm)
competitor_entry <- patenting_products[, .(
  competitor_prod_entry = sum(prod_added[firmid != .BY$firmid]),
  converted_patents_2 = unique(converted_patents_2),
  nonconverted_patents_2 = unique(nonconverted_patents_2)
), by = .(NACE4, year, firmid)]

# Merge back leader's patent variables
competitor_entry <- merge(competitor_entry, 
                         patenting_products[, .(firmid, year, converted_patents_2, nonconverted_patents_2)],
                         by = c("firmid", "year"), suffixes = c("_comp", "_leader"))

# Regression: competitor product entry on leader's converted/non-converted patents
library(fixest)
deterrence_model <- feols(competitor_prod_entry ~ converted_patents_2 + nonconverted_patents_2 | NACE4 + year, data = competitor_entry)
summary(deterrence_model)

# --- 6. Aggregate Dynamism Link ----------------------------------------------
# Construct industry-year conversion rate and relate to reallocation, entry/exit, etc.

industry_year <- patenting_products[, .(
  total_patents = sum(num_pat_families, na.rm=TRUE),
  total_converted = sum(converted_patents_2, na.rm=TRUE),
  total_products = sum(prod_added, na.rm=TRUE),
  n_firms = .N
), by = .(NACE4, year)]
industry_year[, conversion_rate := total_converted / total_patents]

# Example: plot conversion rate vs product reallocation rate (proxy: std dev of prod_added)
industry_year[, prod_reallocation := sd(total_products, na.rm=TRUE), by = NACE4]
ggplot(industry_year, aes(x=conversion_rate, y=prod_reallocation)) +
  geom_point() +
  labs(title="Industry-Year: Conversion Rate vs Product Reallocation", x="Conversion Rate", y="Product Reallocation (SD)")

# --- 7. Robustness: Alternative Windows --------------------------------------
# Repeat above for 3-year and 5-year windows
for (w in c(3,5)) {
  patenting_products[, paste0("conversion_rate_", w) := get(paste0("converted_patents_", w)) / num_pat_families]
  conv_by_year <- patenting_products[, .(mean_conv = mean(get(paste0("conversion_rate_", w)), na.rm=TRUE)), by=year]
  print(conv_by_year)
}


# --- 8. Burst Linkage --------------------------------------------------------
# Are product bursts and patent bursts related at the firm level?
# Cross-tabulate by firm size/age
patenting_products[, burst_link := fifelse(burst == 1 & patent_burst == 1, "Both", 
                                    fifelse(burst == 1, "Product Only", 
                                    fifelse(patent_burst == 1, "Patent Only", "Neither")))]
if ("size_bin" %in% names(patenting_products)) {
  burst_table <- patenting_products[, .N, by = .(burst_link, size_bin)]
  print(burst_table)
}
if ("age_bin" %in% names(patenting_products)) {
  burst_age_table <- patenting_products[, .N, by = .(burst_link, age_bin)]
  print(burst_age_table)
}

# Document each step and adjust code to match actual variable names and data structure as needed.
