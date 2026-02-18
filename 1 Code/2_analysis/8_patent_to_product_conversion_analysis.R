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

# Load patent to product links
nace_pat_prod <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2e_firm_lvl_patent_product_nace2d_dta.parquet")) %>% setDT()
# remove any variable that has "h1, h3, h4, h5" in its name
nace_pat_prod <- nace_pat_prod[, !grepl("h[1345]", names(nace_pat_prod)), with = FALSE]

# --- Parameters --------------------------------------------------------------
conversion_windows <- c(0, 2, 3, 5) # years: baseline=2, robustness=3,5
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

# Firm age bin
if ("firm_age" %in% names(patenting_products)) {
  patenting_products[, age_bin := cut(firm_age, breaks = c(0,5,10,20,100), labels = c("0-5","6-10","11-20","21+"), include.lowest=TRUE)]
}

# Leave patenting_products only with necessary columns
patenting_products <- patenting_products[
  ,
  .(firmid, year, num_pat_families, number_of_products, prod_added, NACE_BR, burst, patent_burst, size_quartile, age_bin)
]
patenting_products <- merge(patenting_products, 
                            nace_pat_prod , 
                            by = c("firmid", "year"), all.x = TRUE, sort = TRUE)

# --- 1. Identify Patent→Product Conversion -----------------------------------
# For each firm-year, count patents filed in year t, and products added in t:(t+window)

for (w in conversion_windows) {
  # For each firm-year, sum products added in t:(t+w)
  patenting_products[, paste0("future_products_", w) := 
    shift(frollsum(prod_added, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]
  # Number of converted patents: min(patents, future products)
  patenting_products[, paste0("converted_patents_", w) := 
                       fifelse(num_pat_families>0, pmin(1, num_pat_families, get(paste0("future_products_", w))), NA_real_)]
  # Non-converted patents: remainder
  patenting_products[, paste0("nonconverted_patents_", w) := 
    1 - get(paste0("converted_patents_", w))
  ]
}

View(patenting_products[, .(firmid, year, num_pat_families, 
                            number_of_products, prod_added, 
                            future_products_2, converted_patents_2, nonconverted_patents_2,
                            new_patents,
                            share_new_patent_in_new_products_h2, share_new_patent_in_new_products_h0
                            )])

# --- 2. Plot Conversion Rate by Year and Firm Size/Age -----------------------
# Example for baseline window (2 years)
# patenting_products[, conversion_rate_2 := fifelse(num_pat_families > 0, converted_patents_2 / num_pat_families, NA_real_)]
# patenting_products[, nonconversion_rate_2 := fifelse(num_pat_families > 0, nonconverted_patents_2 / num_pat_families, NA_real_)]
# View(patenting_products[, .(firmid, year, num_pat_families, 
#                             number_of_products, prod_added, 
#                             future_products_2, converted_patents_2, nonconverted_patents_2, conversion_rate_2, nonconversion_rate_2)])



# Plot: mean conversion rate by year
conv_by_year <- patenting_products[, .(
  mean_conv_specific_2 = mean(share_new_patent_in_new_products_h2, na.rm = TRUE),
  mean_conv_specific_0 = mean(share_new_patent_in_new_products_h0, na.rm = TRUE),
  mean_conv_general_2 = mean(converted_patents_2, na.rm = TRUE),
  mean_conv_general_0 = mean(converted_patents_0, na.rm = TRUE)
  # mean_nonconverted = mean(nonconversion_rate_2, na.rm = TRUE)
), by = year]

print(
  ggplot(conv_by_year, aes(x = year)) +
    geom_line(aes(y = mean_conv_specific_2, color = "Specific (2yr)")) +
    geom_line(aes(y = mean_conv_specific_0, color = "Specific (0yr)")) +
    geom_line(aes(y = mean_conv_general_2, color = "General (2yr)")) +
    geom_line(aes(y = mean_conv_general_0, color = "General (0yr)")) +
    labs(
      title = "Patent→Product Conversion Rates by Year",
      y = "Conversion Rate",
      x = "Year",
      color = "Conversion Type"
    ) +
    theme_minimal()
)


# Plot: by firm size/age bins (example: size quartiles)
if ("size_quartile" %in% names(patenting_products)) {
  conv_by_year_size <- patenting_products[, .(
    mean_conv_specific_2 = mean(share_new_patent_in_new_products_h2, na.rm = TRUE),
    mean_conv_specific_0 = mean(share_new_patent_in_new_products_h0, na.rm = TRUE),
    mean_conv_general_2 = mean(converted_patents_2, na.rm = TRUE),
    mean_conv_general_0 = mean(converted_patents_0, na.rm = TRUE)
  ), by = .(year, size_quartile)]

  print(
    ggplot(conv_by_year_size[!is.na(size_quartile) & size_quartile %in% c(1, 4)], aes(x = year, color = as.factor(size_quartile), group = as.factor(size_quartile))) +
      geom_line(aes(y = mean_conv_specific_2, linetype = "Specific (2yr)")) +
      geom_line(aes(y = mean_conv_specific_0, linetype = "Specific (0yr)")) +
      labs(
        title = "Patent→Product Conversion Rates by Year and Firm Size",
        y = "Conversion Rate",
        x = "Year",
        color = "Size Quartile"
      ) +
      theme_minimal() +
      facet_wrap(~size_quartile)
  )
}





# --- Parameters ---------------------------------------------------------------
# Conversion windows in YEARS (baseline=2; robustness=0,3,5)
conversion_windows <- c(0, 2, 3, 5)

# Burst thresholds
# Product burst: number of new products added in year t
product_burst_threshold_main <- 5
product_burst_thresholds <- c(2, 5, 10)

# Patent burst: within-firm percentile threshold
patent_burst_quantile_main <- 0.90
patent_burst_quantiles <- c(0.75, 0.90)

# Event-study window
event_window <- 4

# Baseline window
w0 <- 2

# --- 2) Define bursts (CENTRAL TO STORY) -------------------------------------
# Product bursts
patenting_products[, product_burst := as.integer(prod_added >= product_burst_threshold_main)]
for (k in product_burst_thresholds) {
  patenting_products[, (paste0("product_burst_", k)) := as.integer(prod_added >= k)]
}

# Patent bursts (within-firm percentile)
for (q in patent_burst_quantiles) {
  qname <- paste0("p", as.integer(100 * q), "_patents")
  bname <- paste0("patent_burst_p", as.integer(100 * q))
  patenting_products[, (qname) := quantile(num_pat_families, q, na.rm = TRUE), by = firmid]
  patenting_products[, (bname) := as.integer(num_pat_families > get(qname))]
}

patenting_products[, patent_burst := get(paste0("patent_burst_p", as.integer(100 * patent_burst_quantile_main)))]

# Combined burst year: either product burst or patent burst
patenting_products[, burst_year := as.integer(product_burst == 1 | patent_burst == 1)]

# --- 3) FACT A: Bursts increasingly consist of nonconverted patents -----------
# (i) Among burst-year patenting observations, compute nonconversion share over time
burst_patents_ts <- patenting_products[burst_year == 1 & num_pat_families > 0,
                                       .(
                                         mean_nonconversion = mean(get(paste0("nonconverted_patents_", w0)), na.rm = TRUE),
                                         mean_conversion = mean(get(paste0("converted_patents_", w0)), na.rm = TRUE)
                                       ),
                                       by = year
]
burst_patents_ts[, share_nonconverted := mean_nonconversion / (mean_nonconversion + mean_conversion)]

p_burst_nonconv <- ggplot(burst_patents_ts, aes(x = year)) +
  # geom_line(aes(y = share_nonconverted, color = "Share nonconverted (burst-year patents)")) +
  geom_line(aes(y = mean_nonconversion, color = "Mean nonconversion rate (firm-year)"), linetype = "dashed") +
  labs(
    title = "Bursts increasingly consist of nonconverted patents",
    subtitle = paste0("Baseline conversion window = ", w0, " years"),
    x = "Year",
    y = "Nonconversion",
    color = NULL
  ) +
  theme_minimal()
print(p_burst_nonconv)

ggsave("output/module8/fig_burst_nonconversion_trend.png", p_burst_nonconv, width = 9, height = 5)

# Regression version (trend)
# Interpretation: positive coef means nonconversion within bursts rises over time
m_burst_nonconv_trend <- feols(
  as.formula(paste0("nonconverted_patents_", w0, " ~ year | NACE_BR")),
  data = patenting_products[burst_year == 1 & num_pat_families > 0]
)

# --- 4) FACT B: Bursts by large firms are less product-generating -------------
# Compare conversion in burst years by size quartile and over time
burst_by_size <- patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile),
                                    .(
                                      mean_conv = mean(get(paste0("converted_patents_", w0)), na.rm = TRUE),
                                      mean_nonconv = mean(get(paste0("nonconverted_patents_", w0)), na.rm = TRUE),
                                      n = .N
                                    ),
                                    by = .(year, size_quartile)
]

p_burst_size <- ggplot(burst_by_size, aes(x = year, y = mean_conv, color = as.factor(size_quartile))) +
  geom_line() +
  facet_wrap(~ size_quartile) +
  labs(
    title = "Burst-years: Conversion is lower for larger firms",
    subtitle = paste0("Baseline conversion window = ", w0, " years"),
    x = "Year",
    y = "Mean conversion rate",
    color = "Size quartile"
  ) +
  theme_minimal()

print(p_burst_size)

ggsave("output/module8/fig_burst_conversion_by_size.png", p_burst_size, width = 10, height = 6)

# Regression: conversion in burst years ~ size + trend (industry FE)
m_burst_size <- feols(
  as.formula(paste0("converted_patents_", w0, " ~ i(size_quartile, ref = 1) + year | NACE_BR")),
  data = patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile)]
)

# Simple regression: is conversion higher for larger vs smaller firms?
m_burst_size_simple <- feols(
  as.formula(paste0("converted_patents_", w0, " ~ i(size_quartile, ref = 1) | NACE_BR")),
  data = patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile)]
)
m_burst_size_simple <- feols(
  as.formula(paste0("share_new_patent_in_new_products_h", w0, " ~ i(size_quartile, ref = 1) | NACE_BR")),
  data = patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile)]
)


# Interaction: does the size gradient steepen over time?
patenting_products[, year_c := year - min(year, na.rm = TRUE)]
m_burst_size_trend <- feols(
  as.formula(paste0("converted_patents_", w0, " ~ i(size_quartile, year_c, ref = 1) | NACE_BR")),
  data = patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile)]
)




# --- 3. Regression: Conversion Rate Trends -----------------------------------
# Pooled regression (add FEs as needed)
lm_conv <- feols(conversion_rate_2 ~ year | NACE_BR, data=patenting_products)
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

# For each firm-year, sum product entry of competitors (excluding focal firm)
competitor_entry <- patenting_products[, .(
  competitor_prod_entry = sum(prod_added[firmid != .BY$firmid]),
  converted_patents_2 = unique(converted_patents_2),
  nonconverted_patents_2 = unique(nonconverted_patents_2)
), by = .(NACE_BR, year, firmid)]

# Merge back leader's patent variables
competitor_entry <- merge(competitor_entry, 
                         patenting_products[, .(firmid, year, converted_patents_2, nonconverted_patents_2)],
                         by = c("firmid", "year"), suffixes = c("_comp", "_leader"))

# Regression: competitor product entry on leader's converted/non-converted patents
library(fixest)
deterrence_model <- feols(competitor_prod_entry ~ converted_patents_2 + nonconverted_patents_2 | NACE_BR + year, data = competitor_entry)
summary(deterrence_model)

# --- 6. Aggregate Dynamism Link ----------------------------------------------
# Construct industry-year conversion rate and relate to reallocation, entry/exit, etc.

industry_year <- patenting_products[, .(
  total_patents = sum(num_pat_families, na.rm=TRUE),
  total_converted = sum(converted_patents_2, na.rm=TRUE),
  total_products = sum(prod_added, na.rm=TRUE),
  n_firms = .N
), by = .(NACE_BR, year)]
industry_year[, conversion_rate := total_converted / total_patents]

# Example: plot conversion rate vs product reallocation rate (proxy: std dev of prod_added)
industry_year[, prod_reallocation := sd(total_products, na.rm=TRUE), by = NACE_BR]
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
if ("size_quartile" %in% names(patenting_products)) {
  burst_table <- patenting_products[, .N, by = .(burst_link, size_quartile)]
  print(burst_table)
}
if ("age_bin" %in% names(patenting_products)) {
  burst_age_table <- patenting_products[, .N, by = .(burst_link, age_bin)]
  print(burst_age_table)
}

# Document each step and adjust code to match actual variable names and data structure as needed.
