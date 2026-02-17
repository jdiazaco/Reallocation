# =============================================================================
# Module 8: Bursts, Conversion, and Disruptiveness
# Paper spine: Innovation bursts have become less product-generating over time.
# Key claims to test:
#   (i) Bursts increasingly consist of nonconverted patents.
#   (ii) Bursts by large firms are less product-generating.
#   (iii) Nonconverted patents in burst years are associated with weaker creative destruction
#        (deterrence of competitor product entry).
# Data: FARE/FICUS (firm outcomes), EAP (products, 2009+), patents (1994+), patent↔product links.
# =============================================================================

# --- Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(fixest)
  library(arrow)
})

# --- Output directory ---------------------------------------------------------
if (!dir.exists("output/module8")) dir.create("output/module8", recursive = TRUE)

# --- Data Loading -------------------------------------------------------------
# Main dataset
patenting_products <- read_rds("temp/patenting_products_firm_level.RDS")
patenting_products <- as.data.table(patenting_products)
setorder(patenting_products, firmid, year)

# Patent↔product links by firm-year (if available)
# NOTE: this file is assumed to already include variables like share_new_patent_in_new_products_h0/h2
nace_pat_prod <- read_parquet(paste0("2_product_data/", cpa_or_pf, "/2e_firm_lvl_patent_product_nace2d_dta.parquet")) %>%
  setDT()
# remove any variable that has "h1, h3, h4, h5" in its name
nace_pat_prod <- nace_pat_prod[, !grepl("h[1345]", names(nace_pat_prod)), with = FALSE]

# Merge links
patenting_products <- merge(
  patenting_products,
  nace_pat_prod,
  by = c("firmid", "year"),
  all.x = TRUE,
  sort = TRUE
)

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

# --- Required variables sanity checks ----------------------------------------
needed_vars <- c("firmid", "year", "num_pat_families", "prod_added", "number_of_products", "NACE_BR")
missing_needed <- setdiff(needed_vars, names(patenting_products))
if (length(missing_needed) > 0) {
  stop(paste0("Missing required variables in patenting_products: ", paste(missing_needed, collapse = ", ")))
}

# Optional outcomes (we will use if present)
outcome_vars <- c("nq", "empl", "value_added", "tfp", "markup")

# --- Construct size and age bins ---------------------------------------------
# Size proxy preference order: sales -> empl -> value_added -> number_of_products
if (!"size_proxy" %in% names(patenting_products)) {
  if ("nq" %in% names(patenting_products)) {
    patenting_products[, size_proxy := nq]
  } else if ("empl" %in% names(patenting_products)) {
    patenting_products[, size_proxy := empl]
  } else if ("value_added" %in% names(patenting_products)) {
    patenting_products[, size_proxy := value_added]
  } else {
    patenting_products[, size_proxy := number_of_products]
  }
}

# Size quartiles by year (keeps cross-year comparability)
# patenting_products[, size_quartile := NA_integer_]
# patenting_products[!is.na(size_proxy), size_quartile := as.integer(cut(
#   size_proxy,
#   breaks = quantile(size_proxy, probs = seq(0, 1, 0.25), na.rm = TRUE),
#   include.lowest = TRUE
# )))]

# Age bins if available
if ("firm_age" %in% names(patenting_products)) {
  patenting_products[, age_bin := cut(
    firm_age,
    breaks = c(0, 5, 10, 20, 1000),
    labels = c("0-5", "6-10", "11-20", "21+"),
    include.lowest = TRUE
  )]
}

# --- 1) Patent→Product conversion (CORE MEASURE) -----------------------------
# General conversion: patents in year t are considered "converted" if the firm adds products
# in t..t+w. This produces an intensity measure (NOT capped at 1).

# Ensure prod_added is non-negative
patenting_products[is.na(prod_added), prod_added := 0]
patenting_products[prod_added < 0, prod_added := 0]

# Ensure patent counts are non-negative
patenting_products[is.na(num_pat_families), num_pat_families := 0]
patenting_products[num_pat_families < 0, num_pat_families := 0]

for (w in conversion_windows) {
  # products added in t..t+w (within firm)
  patenting_products[, paste0("future_products_", w) :=
    frollsum(prod_added, n = w + 1, align = "left", fill = NA_real_),
    by = firmid
  ]

  # Converted patents: can’t exceed patents filed or products added in window
  patenting_products[, paste0("converted_patents_", w) :=
    pmin(1, num_pat_families, get(paste0("future_products_", w)))
  ]

  # Nonconverted patents: remainder
  patenting_products[, paste0("nonconverted_patents_", w) :=
    pmax(0, get(paste0("converted_patents_", w)))
  ]

  # # Conversion rate
  # patenting_products[, paste0("conversion_rate_", w) := fifelse(
  #   num_pat_families > 0,
  #   get(paste0("converted_patents_", w)) / num_pat_families,
  #   NA_real_
  # )]
  # 
  # # Nonconversion rate
  # patenting_products[, paste0("nonconversion_rate_", w) := fifelse(
  #   num_pat_families > 0,
  #   get(paste0("nonconverted_patents_", w)) / num_pat_families,
  #   NA_real_
  # )]
}

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
    total_patents = sum(num_pat_families, na.rm = TRUE),
    total_nonconverted = sum(get(paste0("nonconverted_patents_", w0)), na.rm = TRUE),
    mean_nonconversion = mean(get(paste0("nonconversion_rate_", w0)), na.rm = TRUE),
    mean_conversion = mean(get(paste0("conversion_rate_", w0)), na.rm = TRUE)
  ),
  by = year
]
burst_patents_ts[, share_nonconverted := total_nonconverted / total_patents]

p_burst_nonconv <- ggplot(burst_patents_ts, aes(x = year)) +
  geom_line(aes(y = share_nonconverted, color = "Share nonconverted (burst-year patents)")) +
  geom_line(aes(y = mean_nonconversion, color = "Mean nonconversion rate (firm-year)"), linetype = "dashed") +
  labs(
    title = "Bursts increasingly consist of nonconverted patents",
    subtitle = paste0("Baseline conversion window = ", w0, " years"),
    x = "Year",
    y = "Nonconversion",
    color = NULL
  ) +
  theme_minimal()

ggsave("output/module8/fig_burst_nonconversion_trend.png", p_burst_nonconv, width = 9, height = 5)

# Regression version (trend)
# Interpretation: positive coef means nonconversion within bursts rises over time
m_burst_nonconv_trend <- feols(
  get(paste0("nonconversion_rate_", w0)) ~ year | NACE_BR,
  data = patenting_products[burst_year == 1 & num_pat_families > 0]
)

# --- 4) FACT B: Bursts by large firms are less product-generating -------------
# Compare conversion in burst years by size quartile and over time
burst_by_size <- patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile),
  .(
    mean_conv = mean(get(paste0("conversion_rate_", w0)), na.rm = TRUE),
    mean_nonconv = mean(get(paste0("nonconversion_rate_", w0)), na.rm = TRUE),
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

ggsave("output/module8/fig_burst_conversion_by_size.png", p_burst_size, width = 10, height = 6)

# Regression: conversion in burst years ~ size + trend (industry FE)
m_burst_size <- feols(
  get(paste0("conversion_rate_", w0)) ~ i(size_quartile, ref = 1) + year | NACE_BR,
  data = patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile)]
)

# Interaction: does the size gradient steepen over time?
patenting_products[, year_c := year - min(year, na.rm = TRUE)]
m_burst_size_trend <- feols(
  get(paste0("conversion_rate_", w0)) ~ i(size_quartile, year_c, ref = 1) | NACE_BR,
  data = patenting_products[burst_year == 1 & num_pat_families > 0 & !is.na(size_quartile)]
)

# --- 5) FACT C: Conversion overall vs within bursts --------------------------
conv_ts <- patenting_products[num_pat_families > 0,
  .(
    mean_conv_all = mean(get(paste0("conversion_rate_", w0)), na.rm = TRUE),
    mean_conv_burst = mean(get(paste0("conversion_rate_", w0))[burst_year == 1], na.rm = TRUE),
    mean_conv_nonburst = mean(get(paste0("conversion_rate_", w0))[burst_year == 0], na.rm = TRUE)
  ),
  by = year
]

p_conv_compare <- ggplot(conv_ts, aes(x = year)) +
  geom_line(aes(y = mean_conv_all, color = "All")) +
  geom_line(aes(y = mean_conv_burst, color = "Burst years")) +
  geom_line(aes(y = mean_conv_nonburst, color = "Non-burst years"), linetype = "dashed") +
  labs(
    title = "Patent→product conversion: bursts vs non-bursts",
    subtitle = paste0("Baseline conversion window = ", w0, " years"),
    x = "Year",
    y = "Mean conversion rate",
    color = NULL
  ) +
  theme_minimal()

ggsave("output/module8/fig_conversion_burst_vs_nonburst.png", p_conv_compare, width = 9, height = 5)

# --- 6) Event studies: bursts are the treatment ------------------------------
# We run event studies around the FIRST burst-year per firm.

# First burst year per firm
first_burst <- patenting_products[burst_year == 1, .(first_burst_year = min(year)), by = firmid]
patenting_products <- merge(patenting_products, first_burst, by = "firmid", all.x = TRUE, sort = FALSE)

# Two types of burst (central to story):
# (A) "Product-generating burst": burst_year==1 and conversion_rate_2 >= median among burst-years
# (B) "Non-product burst": burst_year==1 and conversion_rate_2 below median (or 0)
median_conv_burst <- patenting_products[burst_year == 1 & num_pat_families > 0,
  median(get(paste0("conversion_rate_", w0)), na.rm = TRUE)
]

patenting_products[, burst_type := NA_character_]
patenting_products[burst_year == 1 & num_pat_families == 0 & product_burst == 1, burst_type := "Product-only burst (no patents)"]
patenting_products[burst_year == 1 & num_pat_families > 0 & get(paste0("conversion_rate_", w0)) >= median_conv_burst, burst_type := "Product-generating burst"]
patenting_products[burst_year == 1 & num_pat_families > 0 & get(paste0("conversion_rate_", w0)) < median_conv_burst, burst_type := "Non-product patent burst"]

# Collapse burst type to firm-level using first burst year
burst_firm_type <- patenting_products[burst_year == 1 & year == first_burst_year,
  .(burst_type = burst_type[1], burst_sizeq = size_quartile[1]),
  by = firmid
]

patenting_products <- merge(patenting_products, burst_firm_type, by = "firmid", all.x = TRUE, sort = FALSE)

# Outcomes: use what exists
available_outcomes <- intersect(outcome_vars, names(patenting_products))
if (length(available_outcomes) == 0) {
  message("No standard outcomes (sales/empl/value_added/tfp/markup) found. Event studies will be skipped.")
} else {
  for (yvar in available_outcomes) {
    # overall burst event study
    m_es <- feols(
      as.formula(paste0(yvar, " ~ sunab(first_burst_year, year, ref.p = -1) | firmid + year")),
      data = patenting_products[!is.na(first_burst_year)]
    )

    png(paste0("output/module8/es_", yvar, "_all_bursts.png"), width = 900, height = 550)
    print(iplot(m_es, main = paste0("Event study around first burst-year: ", yvar)))
    dev.off()

    # by burst type (focus: non-product patent bursts vs product-generating bursts)
    m_es_type <- feols(
      as.formula(paste0(
        yvar,
        " ~ sunab(first_burst_year, year, ref.p = -1) * i(burst_type, ref = 'Product-generating burst') | firmid + year"
      )),
      data = patenting_products[!is.na(first_burst_year) & !is.na(burst_type)]
    )

    png(paste0("output/module8/es_", yvar, "_by_burst_type.png"), width = 1000, height = 600)
    print(iplot(m_es_type, main = paste0("Event study by burst type: ", yvar)))
    dev.off()

    # by size quartile within bursts
    m_es_size <- feols(
      as.formula(paste0(
        yvar,
        " ~ sunab(first_burst_year, year, ref.p = -1) * i(burst_sizeq, ref = 1) | firmid + year"
      )),
      data = patenting_products[!is.na(first_burst_year) & !is.na(burst_sizeq)]
    )

    png(paste0("output/module8/es_", yvar, "_by_size_quartile.png"), width = 1000, height = 600)
    print(iplot(m_es_size, main = paste0("Event study by size quartile: ", yvar)))
    dev.off()
  }
}

# --- 7) Deterrence: do nonconverted burst patents depress competitor entry? ---
# Clean construction:
# 1) Identify industry-year leader (highest size_proxy).
# 2) Compute competitor product entry excluding leader.
# 3) Regress competitor entry on leader converted/nonconverted patents, focusing on burst years.

# Leader per industry-year
leaders <- patenting_products[!is.na(size_proxy),
  .SD[which.max(size_proxy)],
  by = .(NACE_BR, year)
]
leaders <- leaders[, .(
  NACE_BR, year,
  leader_firmid = firmid,
  leader_patents = num_pat_families,
  leader_converted = get(paste0("converted_patents_", w0)),
  leader_nonconverted = get(paste0("nonconverted_patents_", w0)),
  leader_burst_year = burst_year
)]

# Competitor product entry (excluding leader)
comp_entry <- merge(patenting_products, leaders, by = c("NACE_BR", "year"), all.x = TRUE, sort = FALSE)
comp_entry <- comp_entry[firmid != leader_firmid]

comp_entry_indyr <- comp_entry[, .(
  competitor_prod_entry = sum(prod_added, na.rm = TRUE),
  competitor_n_firms = uniqueN(firmid)
), by = .(NACE_BR, year)]

comp_entry_indyr <- merge(comp_entry_indyr, leaders, by = c("NACE_BR", "year"), all.x = TRUE, sort = FALSE)

# Regression focuses on years where the leader is in a burst year (central story)
m_deterrence <- feols(
  competitor_prod_entry ~ leader_converted + leader_nonconverted | NACE_BR + year,
  data = comp_entry_indyr[leader_burst_year == 1]
)

# Optional: allow differential effects by leader size quartile (computed within leaders)
leaders[, leader_sizeq := NA_integer_]
leaders[!is.na(leader_patents), leader_sizeq := as.integer(cut(
  leader_patents,
  breaks = quantile(leader_patents, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE
))]
comp_entry_indyr <- merge(comp_entry_indyr, leaders[, .(NACE_BR, year, leader_sizeq)], by = c("NACE_BR", "year"), all.x = TRUE)

m_deterrence_size <- feols(
  competitor_prod_entry ~ leader_nonconverted * i(leader_sizeq, ref = 1) + leader_converted | NACE_BR + year,
  data = comp_entry_indyr[leader_burst_year == 1 & !is.na(leader_sizeq)]
)

# --- 8) Save regression outputs ----------------------------------------------
# Store model summaries for paper tables
sink("output/module8/model_summaries.txt")
cat("\n--- Burst nonconversion trend (industry FE) ---\n")
print(summary(m_burst_nonconv_trend))

cat("\n--- Burst conversion by size (industry FE) ---\n")
print(summary(m_burst_size))

cat("\n--- Burst conversion size×trend (industry FE) ---\n")
print(summary(m_burst_size_trend))

cat("\n--- Deterrence (leader burst years): competitor entry on leader converted/nonconverted ---\n")
print(summary(m_deterrence))

cat("\n--- Deterrence with leader size interactions ---\n")
print(summary(m_deterrence_size))

cat("\n--- Notes ---\n")
cat("Baseline conversion window:", w0, "years\n")
cat("Product burst threshold:", product_burst_threshold_main, "new products/year\n")
cat("Patent burst quantile:", patent_burst_quantile_main, "within-firm\n")
cat("Median conversion among burst-year patenting obs:", median_conv_burst, "\n")

sink()

# --- 9) Robustness: alternative conversion windows ---------------------------
robust_list <- list()
for (w in c(0, 3, 5)) {
  tmp <- patenting_products[burst_year == 1 & num_pat_families > 0,
    .(
      year,
      mean_nonconv = mean(get(paste0("nonconversion_rate_", w)), na.rm = TRUE),
      mean_conv = mean(get(paste0("conversion_rate_", w)), na.rm = TRUE)
    ),
    by = year
  ]
  tmp[, window := w]
  robust_list[[as.character(w)]] <- tmp
}
robust_ts <- rbindlist(robust_list, use.names = TRUE, fill = TRUE)

p_robust <- ggplot(robust_ts, aes(x = year, y = mean_nonconv, color = as.factor(window))) +
  geom_line() +
  labs(
    title = "Robustness: Nonconversion in burst years by conversion window",
    x = "Year",
    y = "Mean nonconversion rate",
    color = "Window (years)"
  ) +
  theme_minimal()

ggsave("output/module8/fig_robust_nonconversion_windows.png", p_robust, width = 9, height = 5)

message("Module 8 complete. Outputs saved to output/module8/")
