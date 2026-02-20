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
patenting_products[, product_burst := as.integer(prod_added >= burst_threshold)]
patenting_products[, product_nonburst := as.integer(prod_added >0 & prod_added < burst_threshold)]

for (i in burst_thresholds) {
  patenting_products[, (paste0("burst_", i)) := as.integer(prod_added >= i)]
}

# Patent burst indicator (90th percentile within firm)
burst_thresholds <- read_parquet("4b_patenting_products_firm_level.parquet") %>%
  setDT() %>%
  .[num_pat_families > 0, .(
    p90_patents = quantile(num_pat_families, 0.90, na.rm = TRUE),
    p75_patents = quantile(num_pat_families, 0.75, na.rm = TRUE)
  ), by = year]

get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
burst_90_threshold <- get_mode(burst_thresholds$p90_patents)
burst_75_threshold <- get_mode(burst_thresholds$p75_patents)
patenting_products[, patent_burst := as.integer(num_pat_families >= burst_90_threshold)]
patenting_products[, patent_nonburst := as.integer(num_pat_families >0 & num_pat_families < burst_90_threshold)]
patenting_products[, patent_burst_p75 := as.integer(num_pat_families > burst_75_threshold)]

# Firm age bin
if ("firm_age" %in% names(patenting_products)) {
  patenting_products[, age_bin := cut(
    firm_age,
    breaks = c(0, 1, 2, 5, 10, 20, 100),
    labels = c("0", "1-2", "3-5", "6-10", "11-20", "21+"),
    include.lowest = TRUE
  )]
}

# Firm size bin
if ("empl_bar" %in% names(patenting_products)) {
  patenting_products[, size_bin := cut(
    empl_bar,
    breaks = c(1, 5, 10, 20, 50, 100, 250, 500, 1000, Inf),
    labels = c("1-4", "5-9", "10-19", "20-49", "50-99", "100-249", "250-499", "500-999", "1000+"),
    include.lowest = TRUE
  )]
}

# Leave patenting_products only with necessary columns
patenting_products <- patenting_products[
  ,
  .(firmid, year, NACE_BR, 
  num_pat_families, number_of_products, prod_added, age_bin, 
  tm_dummy,
  product_burst, product_nonburst, patent_burst, patent_nonburst, 
    nq_growth, empl_growth, capital_growth, size_bin,
    size_quartile, size_decile, size_percentile, size_1000tile, top_4_leaders, top_10_leaders)
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
  
  # For each firm-year, sum burst products added in t:(t+w)
  patenting_products[, paste0("future_prod_burst", w) :=
    shift(frollsum(product_burst, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]
  
  #  For each firm-year, sum non-burst products added in t:(t+w)
  patenting_products[, paste0("future_prod_nonburst", w) :=
    shift(frollsum(product_nonburst, n = w + 1, align = "left"), type = "lead", n = 0), by = firmid]

  # Number of converted patents: min(patents, future products)
  patenting_products[, paste0("converted_patents_", w) :=
    fifelse(num_pat_families > 0, pmin(1, num_pat_families, get(paste0("future_products_", w))), NA_real_)]
  
  # For burst/non-burst breakdowns, assign converted patents to burst vs non-burst products (if both exist, assign to burst first)
  patenting_products[, paste0("non_burst_patent_to_non_burst_products_", w) :=
                       fifelse(patent_nonburst>0, pmin(1, get(paste0("future_prod_nonburst", w))), NA_real_)]
  patenting_products[, paste0("burst_patent_to_non_burst_products_", w) :=
                       fifelse(patent_burst>0, pmin(1, get(paste0("future_prod_nonburst", w))), NA_real_)]
  patenting_products[, paste0("non_burst_patent_to_burst_products_", w) :=
                       fifelse(patent_nonburst>0, pmin(1, get(paste0("future_prod_burst", w))), NA_real_)]
  patenting_products[, paste0("burst_patent_to_burst_products_", w) :=
                       fifelse(patent_burst>0, pmin(1, get(paste0("future_prod_burst", w))), NA_real_)]

  # Non-converted patents: remainder
  patenting_products[, paste0("nonconverted_patents_", w) :=
    1 - get(paste0("converted_patents_", w))
  ]
}

 # --- 1 Conversion rates by size and age bin ---
 baseline_window <- 2
conversion_rates_vars <- list(
  "Converted patents (General)" = paste0("converted_patents_", baseline_window),
  "Converted patents (Specific)" = paste0("share_new_patent_in_new_products_h", baseline_window),
  "Non-burst patents to non-burst products" = paste0("non_burst_patent_to_non_burst_products_", baseline_window),
  "Burst patents to non-burst products" = paste0("burst_patent_to_non_burst_products_", baseline_window),
  "Non-burst patents to burst products" = paste0("non_burst_patent_to_burst_products_", baseline_window),
  "Burst patents to burst products" = paste0("burst_patent_to_burst_products_", baseline_window)
)

  for (i in seq_along(conversion_rates_vars)) {
    var <- conversion_rates_vars[[i]]

    conv_by_size <- patenting_products[, .(mean_conv = mean(get(var), na.rm = TRUE)),
      by = c("size_bin", "age_bin")
    ]
    setorder(conv_by_size, size_bin, age_bin)

    conv_by_size <- conv_by_size %>%
      pivot_wider(names_from = all_of("size_bin"), values_from = mean_conv) %>%
      select(-`NA`) %>%
      filter(!is.na(.data[["age_bin"]]))
    
    create_latex_table(
      data = conv_by_size,
      var_name = names(conversion_rates_vars)[i],
      caption = paste0("Conversion Rates by Size Bin and Age Bin - ", names(conversion_rates_vars)[i]),
      output_dir = output_dir,
      include_preamble = FALSE,
      digits = 4,
      filename = paste0("test_", names(conversion_rates_vars)[i])
    )
  }

# --- 2. Effects of conversion by age and size --------------------------------

y_vars <- c("nq_growth", "empl_growth", "capital_growth")
y_labels <- c(
  nq_growth = "Output Growth",
  empl_growth = "Employment Growth",
  capital_growth = "Capital Growth"
)
x_vars <- conversion_rates_vars


# --- 4. Event Study: Real Effects of Converted vs Non-Converted --------------
# For each product launch tied to a patent, align outcomes (sales, employment, etc) around event year
# Compare to patent bursts with no product launch

# Create event time for each firm-year relative to converted patent event
event_window <- 4  # years before/after

for (x_name in names(x_vars)) {
  x_var <- x_vars[[x_name]]
  stop()

  patenting_products[, event_year := NA_integer_]
  patenting_products[!is.na(get(x_var)), event_year := 0]
  patenting_products <- lead_lag_creator(patenting_products, "event_year", n_lags = event_window, to_dummy = FALSE)
  patenting_products[, event_year_lag0 := event_year]

  event_cols <- grep("^event_year_(lag|lead)\\d+$", names(patenting_products), value = TRUE)

  event_panel <- rbindlist(lapply(event_cols, function(col) {
    dt <- patenting_products[, .(
      firmid, year, nq_growth, capital_growth, empl_growth, prod_added, converted_patents_2, nonconverted_patents_2,
      size_bin, age_bin,
      event_time = get(col)
    )]
    dt[, event_time_type := col]
    dt
  }), use.names = TRUE, fill = TRUE)

  event_panel <- event_panel[!is.na(event_time)]
  event_panel[, rel_year := as.integer(sub(".*?(\\d+)$", "\\1", event_time_type))]
  event_panel[, rel_year := fifelse(grepl("lag", event_time_type), rel_year, -rel_year)]

  # --- Standard Event Study using fixest::sunab() ---
  patenting_products[, treat := as.integer(get(x_var) > 0)]

  patenting_products[, event_time := NA_integer_]
  first_treat_year <- patenting_products[get(x_var) > 0, .(first_treat_year = min(year)), by = firmid]
  patenting_products <- merge(patenting_products, first_treat_year, by = "firmid", all.x = TRUE, sort = FALSE)
  patenting_products[!is.na(first_treat_year), event_time := first_treat_year]
  View(patenting_products %>% select(firmid, year, x_var, first_treat_year, event_time))
  patenting_products[, event_time := fifelse(is.na(event_time), 10000, event_time)]

  patenting_products[, first_treat_year := NULL]
  patenting_products <- patenting_products[!is.na(get(x_var))]

  for (y_var in y_vars) {
    stop()
    es_model <- feols(as.formula(paste0(y_var, " ~ sunab(event_time, year, ref.p = -1) | firmid + year")),
      data = patenting_products
    )
    png(
      filename = paste0(output_dir, "/event_study_", x_name, "_", y_var, ".png"),
      width = 800,
      height = 600
    )
    iplot(
      es_model,
      type = "dynamic",
      xlab = "Years from Event",
      ylab = y_labels[y_var],
      main = paste0("Event Study: ", y_labels[y_var], " by ", x_name)
    )
    dev.off()

  }

  # --- Classic Event Study by Event-Year Size/Age Bin ---
  for (group_var in c("size_bin", "age_bin")) {
    group_label <- ifelse(group_var == "size_bin", "Size Bin", "Age Bin")
    event_group_var <- paste0("event_", group_var)

    if (!event_group_var %in% names(event_panel)) {
      event_panel[, year_to_add_size_age := year - rel_year]
      group_dt <- patenting_products[, .(firmid, year, group_value = get(group_var))]
      setnames(group_dt, "group_value", event_group_var)
      event_panel <- merge(event_panel,
        group_dt,
        by.x = c("firmid", "year_to_add_size_age"),
        by.y = c("firmid", "year"),
        all.x = TRUE
      )
    }

    for (y_var in y_vars) {
      y_label <- unname(y_labels[y_var])
      event_panel_group <- event_panel[!is.na(get(event_group_var)),
        .(
          mean_outcome = mean(get(y_var), na.rm = TRUE),
          se_outcome = sd(get(y_var), na.rm = TRUE) / sqrt(.N)
        ),
        by = .(rel_year, group = get(event_group_var))
      ]
      event_panel_group[, ci_low := mean_outcome - 1.96 * se_outcome]
      event_panel_group[, ci_high := mean_outcome + 1.96 * se_outcome]
        ggplot(event_panel_group, aes(x = rel_year, y = mean_outcome, color = as.factor(group), group = as.factor(group))) +
          geom_line() +
          geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = as.factor(group)), alpha = 0.2, color = NA) +
          labs(title = paste0("Event Study: ", y_label, " by ", group_label, " (", x_name, ")"), x = "Years from Event", y = y_label) +
          theme_minimal()
        ggsave(paste0(output_dir, "/event_study_", x_name, "_", y_var, "_", group_var, ".png"), width = 8, height = 6)

      for (k in unique(event_panel_group$rel_year)) {
          ggplot(event_panel_group[rel_year == k], aes(x = group, y = mean_outcome, fill = as.factor(group))) +
            geom_bar(stat = "identity") +
            geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
            labs(title = paste0("Event Study: ", y_label, " by ", group_label, " (", x_name, ") - Year ", k), x = group_label, y = y_label) +
            theme_minimal() +
            theme(legend.position = "none")
          ggsave(paste0(output_dir, "/event_study_", x_name, "_", y_var, "_", group_var, "_year_", k, ".png"), width = 8, height = 6)
      }
    }
  }


}

# --- Event Study by Initial Size and Age Category ---
# For each event, assign the event-year size and age category to all event window observations ("frozen" at event year)

# 1. By Size

# --- 5. Deterrence Test: Competitor Product Entry ----------------------------
# For each firm-year, define competitor set (same 4-digit industry)
# Regress competitor product entry on leader's converted/non-converted patents

# For each firm-year, sum product entry of competitors (excluding focal firm)
competitor_entry <- patenting_products[, .(
  competitor_prod_entry = sum(prod_added[firmid != .BY$firmid], na.rm = T),
  converted_patents_2 = unique(converted_patents_2),
  nonconverted_patents_2 = unique(nonconverted_patents_2)
), by = .(NACE_BR, year, firmid)]

competitor_entry <- patenting_products[, `:=`(
  competitor_prod_entry = sum(prod_added, na.rm = T) - prod_added
  # converted_patents_2 = unique(converted_patents_2),
  # nonconverted_patents_2 = unique(nonconverted_patents_2)
), by = .(NACE_BR, year)] %>% select(firmid, NACE_BR, year, prod_added, competitor_prod_entry, 
                                     empl_growth, nq_growth, capital_growth,
                                     converted_patents_2, nonconverted_patents_2, 
                                     share_new_patent_in_new_products_h2, 
                                     matches("burst"),
                                     starts_with("size_"), starts_with("top_"))

# Merge back leader's patent variables
competitor_entry <- merge(competitor_entry, 
                         patenting_products[, .(firmid, year, converted_patents_2, nonconverted_patents_2)],
                         by = c("firmid", "year"), suffixes = c("_comp", "_leader"))
vars_to_lag <- c("converted_patents_2", "share_new_patent_in_new_products_h2", "patent_burst", "product_burst_5")
for(var in vars_to_lag){
  competitor_entry <- lead_lag_creator(competitor_entry, var, 2)
  
}

# Regression: competitor product entry on leader's converted/non-converted patents
library(fixest)
deterrence_model <- feols(empl_growth ~  converted_patents_2  | NACE_BR^year, data = competitor_entry)
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
patenting_products[, burst_link := fifelse(product_burst == 1 & patent_burst == 1, "Both", 
                                    fifelse(product_burst == 1, "Product Only", 
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
