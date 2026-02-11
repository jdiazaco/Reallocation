# ============================================================================
# Module 6: Patent-Burst Analysis
# ============================================================================
# Objective: Link patents to the burst framework using French firm data
# 
# This module replicates and extends the product burst analysis (Module 5)
# by incorporating patent data to validate innovation bursts and test
# the theoretical burst model predictions.
#
# Core Research Questions:
# 1. Do patents exhibit burst-like patterns similar to products?
# 2. How many products does one patent generate?
# 3. Can we validate that product bursts reflect real innovation (not just reclassification)?
# 4. Do firm types (high vs low quality) differ in patent→product conversion?
# 5. Can we instrument product bursts with patents?
#
# Author: Patent-Burst Analysis Team
# Date: 2026-02-09
# ============================================================================

# Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# ============================================================================
# SECTION 1: DATA PREPARATION
# ============================================================================

# Load cleaned datasets from prior modules
message("Loading firm-level product and patent data...")

start <- 2009
end <- 2022

# Main firm-level dataset with product information
patenting_products <- read_rds("temp/patenting_products_firm_level.RDS")

# Patent-specific variables
patent_data <- read_rds("4b_patenting_products_firm_level.rds")

# Trademark data (optional)
tm_data <- read_rds("4c_tm_firm_level.rds")

message("✓ Data loaded successfully")

# ============================================================================
# SECTION 2: DEFINE BURST THRESHOLDS AND CREATE BURST INDICATORS
# ============================================================================

# Define burst thresholds - using prod_added (product creation) variable
burst_threshold <- 5  # Main threshold
burst_thresholds <- c(2, 5, 10)  # Alternative thresholds for robustness

# Create burst indicators based on product creation
patenting_products <- patenting_products[, burst := ifelse(prod_added > burst_threshold, 1, 0)] %>%
  group_by(firmid) %>%
  setDT() %>%
  mutate(ever_burst = any(prod_added >= burst_threshold))

# Create alternative burst thresholds
for (i in burst_thresholds) {
  patenting_products[, (paste0("burst_", i)) := ifelse(prod_added >= i, 1, 0)]
}

# Create patent burst indicators (using num_pat_families variable)
patenting_products <- patenting_products %>%
  group_by(firmid) %>%
  mutate(
    # Define patent burst based on patent families
    p90_patents = quantile(num_pat_families, 0.90, na.rm = TRUE),
    p75_patents = quantile(num_pat_families, 0.75, na.rm = TRUE),
    patent_burst = num_pat_families > p90_patents,
    patent_burst_p75 = num_pat_families > p75_patents,
    ever_patent_burst = any(num_pat_families > p90_patents, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  setDT()

# Joint burst indicator
patenting_products[, joint_burst := burst == 1 & patent_burst == TRUE]

# Burst type classification
patenting_products[, burst_type := case_when(
  burst == 1 & patent_burst == TRUE ~ "Joint Burst",
  burst == 1 & patent_burst != TRUE ~ "Product Only",
  burst != 1 & patent_burst == TRUE ~ "Patent Only",
  TRUE ~ "No Burst"
)]

message("✓ Burst indicators created")

# ============================================================================
# SECTION 3: SUMMARY STATISTICS BY BURST TYPE (TABLE 1)
# ============================================================================

create_table1_summary_stats <- function(data) {
  
  # Filter out rows with missing burst indicators
  data_clean <- data[!is.na(burst) & !is.na(num_pat_families)]
  
  summary_table <- data_clean[, .(
    n_observations = .N,
    n_firms = n_distinct(firmid),
    pct_total = 100 * .N / nrow(data_clean),
    
    # Product metrics
    avg_products_added = mean(prod_added, na.rm = TRUE),
    median_products_added = median(prod_added, na.rm = TRUE),
    sd_products_added = sd(prod_added, na.rm = TRUE),
    
    # Patent metrics
    avg_patents = mean(num_pat_families, na.rm = TRUE),
    median_patents = median(num_pat_families, na.rm = TRUE),
    sd_patents = sd(num_pat_families, na.rm = TRUE),
    
    # Growth metrics
    avg_empl_growth = mean(empl_growth, na.rm = TRUE),
    avg_rev_growth = mean(rev_growth, na.rm = TRUE),
    
    # Firm characteristics
    avg_firm_age = mean(firm_age, na.rm = TRUE),
    avg_empl = mean(empl, na.rm = TRUE)
    
  ), by = burst_type] %>%
    arrange(factor(burst_type, levels = c("No Burst", "Patent Only", "Product Only", "Joint Burst")))
  
  return(summary_table)
}

# Generate Table 1
table1 <- create_table1_summary_stats(patenting_products)
print(table1)

# ============================================================================
# SECTION 4: REGRESSION ANALYSIS - PATENT-PRODUCT RELATIONSHIP
# ============================================================================

# Table 2: Do Products per Patent Vary by Burst Status?
run_table2_patent_product_relationship <- function(data) {
  
  # Create ratio variable (products per patent)
  data_reg <- data %>%
    filter(num_pat_families > 0 & prod_added >= 0) %>%
    mutate(
      products_per_patent = prod_added / num_pat_families,
      log_products = log(prod_added + 1),
      log_patents = log(num_pat_families)
    ) %>%
    setDT()
  
  # Model 1: Simple - products vs patents
  model1 <- feols(
    log_products ~ log_patents,
    data = data_reg,
    na.action = "na.omit"
  )
  
  # Model 2: Add burst indicator
  model2 <- feols(
    log_products ~ log_patents + patent_burst,
    data = data_reg,
    na.action = "na.omit"
  )
  
  # Model 3: With interaction
  model3 <- feols(
    log_products ~ log_patents * patent_burst + 
      log(empl + 1) + log(firm_age + 1),
    data = data_reg,
    na.action = "na.omit"
  )
  
  # Model 4: With firm fixed effects
  model4 <- feols(
    log_products ~ log_patents * patent_burst | firmid,
    data = data_reg,
    na.action = "na.omit"
  )
  
  return(list(m1 = model1, m2 = model2, m3 = model3, m4 = model4))
}

# Table 3: Do Product Bursts Co-occur with Patent Bursts?
run_table3_burst_correlation <- function(data) {
  
  data_reg <- data[!is.na(burst) & !is.na(num_pat_families)] %>% setDT()
  
  # Overall
  model_overall <- feols(
    burst ~ patent_burst + log(empl + 1) + log(firm_age + 1),
    data = data_reg
  )
  
  # By size quartile
  model_by_size <- feols(
    burst ~ patent_burst | size_quartile,
    data = data_reg
  )
  
  # Including NACE
  model_with_industry <- feols(
    burst ~ patent_burst + log(empl + 1) | NACE_BR,
    data = data_reg
  )
  
  return(list(overall = model_overall, by_size = model_by_size, 
              with_industry = model_with_industry))
}

# Table 4: Burst Amplification Effect
run_table4_amplification <- function(data) {
  
  data_reg <- data[prod_added > 0 & num_pat_families > 0] %>%
    mutate(
      log_products = log(prod_added),
      log_patents = log(num_pat_families)
    ) %>%
    setDT()
  
  # Test if bursts amplify product creation from patents
  model1 <- feols(
    log_products ~ log_patents,
    data = data_reg
  )
  
  model2 <- feols(
    log_products ~ log_patents * burst,
    data = data_reg
  )
  
  model3 <- feols(
    log_products ~ log_patents * burst + 
      log(empl + 1) + firm_age | NACE_BR,
    data = data_reg
  )
  
  return(list(m1 = model1, m2 = model2, m3 = model3))
}

message("✓ Regression functions defined")

# ============================================================================
# SECTION 5: DISTRIBUTION ANALYSIS - VALIDATE TAIL PARAMETER
# ============================================================================

analyze_burst_distribution <- function(data) {
  
  # Create rank-frequency data for products
  products_ranked <- data[prod_added > 0, .(
    rank = rank(-prod_added),
    prod_added = prod_added
  ), by = year] %>%
    .[, .(
      log_rank = log(rank),
      log_prod = log(prod_added)
    )]
  
  # Create rank-frequency data for patents
  patents_ranked <- data[num_pat_families > 0, .(
    rank = rank(-num_pat_families),
    num_pat = num_pat_families
  ), by = year] %>%
    .[, .(
      log_rank = log(rank),
      log_pat = log(num_pat)
    )]
  
  # Fit power-law to each
  fit_products <- lm(log_prod ~ log_rank, data = products_ranked)
  fit_patents <- lm(log_pat ~ log_rank, data = patents_ranked)
  
  # Estimate theta (tail exponent) from negative slope
  theta_products <- -coef(fit_products)[2]
  theta_patents <- -coef(fit_patents)[2]
  
  results <- list(
    theta_products = theta_products,
    theta_patents = theta_patents,
    fit_products = fit_products,
    fit_patents = fit_patents,
    products_ranked = products_ranked,
    patents_ranked = patents_ranked
  )
  
  return(results)
}

message("✓ Distribution analysis functions defined")

# ============================================================================
# SECTION 6: REGRESSION SPECIFICATIONS (LEGACY - kept for reference)
# ============================================================================

# These functions are now replaced by the data-driven versions above,
# but kept for potential reference or extended analysis

# ============================================================================
# SECTION 7: EVENT STUDY AND DYNAMICS
# ============================================================================

# Simplified event study using existing variables
analyze_burst_dynamics <- function(data) {
  
  # Lead-lag analysis: how do firms behave around burst years?
  data_for_dynamics <- data %>%
    arrange(firmid, year) %>%
    group_by(firmid) %>%
    mutate(
      burst_lag1 = lag(burst),
      burst_lead1 = lead(burst),
      patents_lag1 = lag(num_pat_families),
      patents_lead1 = lead(num_pat_families),
      empl_growth_lag1 = lag(empl_growth),
      empl_growth_lead1 = lead(empl_growth)
    ) %>%
    ungroup() %>%
    setDT()
  
  # Test: Do product bursts predict patent bursts (lead)?
  # Or do patent bursts predict product bursts (lead)?
  model_lead <- feols(
    burst_lead1 ~ patent_burst,
    data = data_for_dynamics
  )
  
  model_lag <- feols(
    burst ~ lag(patent_burst),
    data = data_for_dynamics
  )
  
  return(list(lead = model_lead, lag = model_lag))
}

message("✓ Event study functions defined")

# ============================================================================
# SECTION 8: VISUALIZATION FUNCTIONS
# ============================================================================

plot_patent_product_burst <- function(data) {
  
  # Scatter: Patents vs Products colored by burst status
  plot_data <- data[prod_added > 0 & num_pat_families > 0] %>%
    mutate(
      burst_status = case_when(
        burst == 1 & patent_burst == TRUE ~ "Joint Burst",
        burst == 1 ~ "Product Burst",
        patent_burst == TRUE ~ "Patent Burst",
        TRUE ~ "No Burst"
      ),
      size_cat = cut(empl, breaks = quantile(empl, c(0, 0.33, 0.67, 1), na.rm = TRUE),
                     labels = c("Small", "Medium", "Large"), include.lowest = TRUE)
    ) %>%
    setDT()
  
  fig_scatter <- ggplot(plot_data, aes(x = log(num_pat_families), y = log(prod_added))) +
    geom_point(aes(color = burst_status, size = empl), alpha = 0.4) +
    geom_smooth(method = "lm", color = "black", se = TRUE, alpha = 0.1) +
    scale_color_manual(
      values = c("Joint Burst" = "darkred", "Product Burst" = "blue",
                 "Patent Burst" = "green", "No Burst" = "gray")
    ) +
    labs(
      title = "Patent-Product Correlation by Burst Status",
      x = "Log(Patent Families)",
      y = "Log(Products Added)",
      color = "Burst Type",
      size = "Employment"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(fig_scatter)
}

plot_rank_frequency <- function(data, year_filter = NULL) {
  
  # Prepare data by year if specified
  if (!is.null(year_filter)) {
    data <- data[year == year_filter]
  }
  
  # Rank-frequency plot for products
  products_ranked <- data[prod_added > 0, .(
    rank = rank(-prod_added),
    prod_added = prod_added
  )] %>%
    mutate(type = "Products") %>%
    filter(rank <= 1000)
  
  # Rank-frequency plot for patents
  patents_ranked <- data[num_pat_families > 0, .(
    rank = rank(-num_pat_families),
    num_pat = num_pat_families
  )] %>%
    mutate(type = "Patents") %>%
    filter(rank <= 1000)
  
  # Combine for plotting
  plot_data <- bind_rows(
    products_ranked %>% rename(value = prod_added) %>% select(rank, value, type),
    patents_ranked %>% rename(value = num_pat) %>% select(rank, value, type)
  )
  
  fig_rank <- ggplot(plot_data, aes(x = log(rank), y = log(value), color = type)) +
    geom_point(alpha = 0.4, size = 2) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
    scale_color_manual(values = c("Products" = "blue", "Patents" = "red")) +
    labs(
      title = "Rank-Frequency Distribution: Products vs Patents",
      subtitle = "Testing for power-law (thick-tailed) distribution",
      x = "Log(Rank)",
      y = "Log(Quantity)",
      color = "Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(fig_rank)
}

plot_burst_composition <- function(data) {
  
  # Summary by burst type and year
  composition <- data[, .(
    count = .N,
    avg_products = mean(prod_added, na.rm = TRUE),
    avg_patents = mean(num_pat_families, na.rm = TRUE)
  ), by = .(year, burst_type)]
  
  fig_composition <- ggplot(composition, aes(x = year, y = count, fill = burst_type)) +
    geom_col(position = "fill") +
    scale_fill_manual(
      values = c("Joint Burst" = "darkred", "Product Only" = "blue",
                 "Patent Only" = "green", "No Burst" = "lightgray")
    ) +
    labs(
      title = "Evolution of Burst Types Over Time",
      x = "Year",
      y = "Share of Firms",
      fill = "Burst Type"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(fig_composition)
}

message("✓ Visualization functions defined")

# ============================================================================
# SECTION 9: HETEROGENEITY ANALYSIS
# ============================================================================

analyze_heterogeneity_by_size <- function(data) {
  
  # Test: Does patent-product relationship vary by firm size?
  data_reg <- data[prod_added > 0 & num_pat_families > 0] %>%
    mutate(
      log_products = log(prod_added),
      log_patents = log(num_pat_families)
    ) %>%
    setDT()
  
  # By size quartile
  results_by_size <- list()
  
  for (q in 1:4) {
    data_subset <- data_reg[size_quartile == q]
    
    model <- feols(
      log_products ~ log_patents * burst,
      data = data_subset
    #   se = "robust"
    )
    
    results_by_size[[paste0("Q", q)]] <- model
  }
  
  return(results_by_size)
}

analyze_heterogeneity_by_age <- function(data) {
  
  # Test: Does innovation timing differ for young vs old firms?
  data_reg <- data[!is.na(young) & !is.na(burst)] %>%
    setDT()
  
  model_young <- feols(
    burst ~ patent_burst + log(empl + 1),
    data = data_reg[young == 1]
  )
  
  model_old <- feols(
    burst ~ patent_burst + log(empl + 1),
    data = data_reg[young == 0]
  )
  
  return(list(young = model_young, old = model_old))
}

analyze_heterogeneity_by_industry <- function(data) {
  
  # Test: Do some industries have tighter patent-product links?
  data_reg <- data[prod_added > 0 & num_pat_families > 0] %>%
    mutate(
      log_products = log(prod_added),
      log_patents = log(num_pat_families)
    ) %>%
    setDT()
  
  # Estimate correlation by industry
  industry_results <- data_reg[, .(
    n_obs = .N,
    n_firms = n_distinct(firmid),
    correlation = cor(log_patents, log_products, use = "complete.obs"),
    avg_products = mean(prod_added),
    avg_patents = mean(num_pat_families),
    pct_bursts = 100 * mean(burst, na.rm = TRUE)
  ), by = NACE_BR] %>%
    arrange(-n_obs)
  
  return(industry_results)
}

message("✓ Heterogeneity functions defined")

# ============================================================================
# SECTION 10: ROBUSTNESS CHECKS
# ============================================================================

run_robustness_checks <- function(data) {
  
  checks <- list()
  
  # Check 1: Alternative burst threshold (p75 instead of p90)
  checks$p75_threshold <- feols(
    prod_added ~ num_pat_families * patent_burst_p75,
    data = data
  )
  
  # Check 2: Different burst threshold (5 products instead of 2)
  data_rob <- data[!is.na(burst_5)] %>% setDT()
  checks$threshold_5 <- feols(
    prod_added ~ num_pat_families * burst_5,
    data = data_rob
  )
  
  # Check 3: Include controls
  checks$with_controls <- feols(
    prod_added ~ num_pat_families * burst + 
      log(empl + 1) + firm_age + young,
    data = data
  )
  
  # Check 4: By NACE sector
  checks$by_industry <- data[!is.na(NACE_BR)] %>%
    .[, .(
      coef = coef(feols(prod_added ~ num_pat_families, se = "robust")),
      n = .N
    ), by = NACE_BR] %>%
    setorder(-n)
  
  return(checks)
}

message("✓ Robustness functions defined")

# ============================================================================
# SECTION 11: VISUALIZATION FUNCTIONS (LEGACY - kept for reference)
# ============================================================================

# ============================================================================
# SECTION 12: MAIN EXECUTION FLOW
# ============================================================================

main_patent_burst_analysis <- function() {
  

# =========================
# STEP-BY-STEP ANALYSIS FLOW
# =========================

message("PATENT BURST ANALYSIS - MODULE 6")
message("Starting analysis with firm-level data...")

# 1. Summary Statistics by burst type
message("\n[1/5] Creating summary statistics by burst type...")
data_clean <- patenting_products[!is.na(burst) & !is.na(num_pat_families)]
table1 <- data_clean[, .(
  n_observations = .N,
  n_firms = n_distinct(firmid),
  pct_total = 100 * .N / nrow(data_clean),
  avg_products_added = mean(prod_added, na.rm = TRUE),
  median_products_added = median(prod_added, na.rm = TRUE),
  sd_products_added = sd(prod_added, na.rm = TRUE),
  avg_patents = mean(num_pat_families, na.rm = TRUE),
  median_patents = median(num_pat_families, na.rm = TRUE),
  sd_patents = sd(num_pat_families, na.rm = TRUE),
  avg_empl_growth = mean(empl_growth, na.rm = TRUE),
  avg_rev_growth = mean(rev_growth, na.rm = TRUE),
  avg_firm_age = mean(firm_age, na.rm = TRUE),
  avg_empl = mean(empl, na.rm = TRUE)
), by = burst_type] %>%
  arrange(factor(burst_type, levels = c("No Burst", "Patent Only", "Product Only", "Joint Burst")))
print(table1)

# 2. Regression Analyses
message("\n[2/5] Running regression analyses...")

# Table 2: Patent-Product Relationship
message("  - Table 2: Patent-Product Relationship")
data_reg2 <- patenting_products %>%
  filter(num_pat_families > 0 & prod_added >= 0) %>%
  mutate(
    products_per_patent = prod_added / num_pat_families,
    log_products = log(prod_added + 1),
    log_patents = log(num_pat_families)
  ) %>%
  setDT()
model2_1 <- feols(log_products ~ log_patents, data = data_reg2, na.action = "na.omit")
model2_2 <- feols(log_products ~ log_patents + patent_burst, data = data_reg2, na.action = "na.omit")
model2_3 <- feols(log_products ~ log_patents * patent_burst + log(empl + 1) + log(firm_age + 1), data = data_reg2, na.action = "na.omit")
model2_4 <- feols(log_products ~ log_patents * patent_burst | firmid, data = data_reg2, na.action = "na.omit")

# Table 3: Burst Correlation
message("  - Table 3: Burst Correlation")
data_reg3 <- patenting_products[!is.na(burst) & !is.na(num_pat_families)] %>% setDT()
model3_overall <- feols(burst ~ patent_burst + log(empl + 1) + log(firm_age + 1), data = data_reg3)
model3_by_size <- feols(burst ~ patent_burst | size_quartile, data = data_reg3)
model3_with_industry <- feols(burst ~ patent_burst + log(empl + 1) | NACE_BR, data = data_reg3)

# Table 4: Amplification Effects
message("  - Table 4: Amplification Effects")
data_reg4 <- patenting_products[prod_added > 0 & num_pat_families > 0] %>%
  mutate(
    log_products = log(prod_added),
    log_patents = log(num_pat_families)
  ) %>%
  setDT()
model4_1 <- feols(log_products ~ log_patents, data = data_reg4)
model4_2 <- feols(log_products ~ log_patents * burst, data = data_reg4)
model4_3 <- feols(log_products ~ log_patents * burst + log(empl + 1) + firm_age | NACE_BR, data = data_reg4)

# 3. Distribution Analysis
message("\n[3/5] Analyzing burst distributions...")
products_ranked <- patenting_products[prod_added > 0, .(
  rank = rank(-prod_added),
  prod_added = prod_added
), by = year] %>%
  .[, .(
    log_rank = log(rank),
    log_prod = log(prod_added)
  )]
patents_ranked <- patenting_products[num_pat_families > 0, .(
  rank = rank(-num_pat_families),
  num_pat = num_pat_families
), by = year] %>%
  .[, .(
    log_rank = log(rank),
    log_pat = log(num_pat)
  )]
fit_products <- lm(log_prod ~ log_rank, data = products_ranked)
fit_patents <- lm(log_pat ~ log_rank, data = patents_ranked)
theta_products <- -coef(fit_products)[2]
theta_patents <- -coef(fit_patents)[2]
message(sprintf("  - Estimated θ (products): %.2f", theta_products))
message(sprintf("  - Estimated θ (patents): %.2f", theta_patents))
message(sprintf("  - Paper reference: θ = 3.10"))

# 4. Heterogeneity Analysis
message("\n[4/5] Analyzing heterogeneous effects...")

# By firm size
message("  - By firm size")
data_reg_size <- patenting_products[prod_added > 0 & num_pat_families > 0] %>%
  mutate(
    log_products = log(prod_added),
    log_patents = log(num_pat_families)
  ) %>%
  setDT()
het_size <- list()
for (q in 1:4) {
  data_subset <- data_reg_size[size_quartile == q]
  het_size[[paste0("Q", q)]] <- feols(log_products ~ log_patents * burst, data = data_subset)
}

# By firm age
message("  - By firm age")
data_reg_age <- patenting_products[!is.na(young) & !is.na(burst)] %>% setDT()
het_age_young <- feols(burst ~ patent_burst + log(empl + 1), data = data_reg_age[young == 1])
het_age_old <- feols(burst ~ patent_burst + log(empl + 1), data = data_reg_age[young == 0])

# By industry
message("  - By industry")
data_reg_ind <- patenting_products[prod_added > 0 & num_pat_families > 0] %>%
  mutate(
    log_products = log(prod_added),
    log_patents = log(num_pat_families)
  ) %>%
  setDT()
industry_results <- data_reg_ind[, .(
  n_obs = .N,
  n_firms = n_distinct(firmid),
  correlation = cor(log_patents, log_products, use = "complete.obs"),
  avg_products = mean(prod_added),
  avg_patents = mean(num_pat_families),
  pct_bursts = 100 * mean(burst, na.rm = TRUE)
), by = NACE_BR] %>%
  arrange(-n_obs)

# 5. Visualizations
message("\n[5/5] Creating visualizations...")

# Scatter: Patents vs Products colored by burst status
plot_data <- patenting_products[prod_added > 0 & num_pat_families > 0] %>%
  mutate(
    burst_status = case_when(
      burst == 1 & patent_burst == TRUE ~ "Joint Burst",
      burst == 1 ~ "Product Burst",
      patent_burst == TRUE ~ "Patent Burst",
      TRUE ~ "No Burst"
    ),
    size_cat = cut(empl, breaks = quantile(empl, c(0, 0.33, 0.67, 1), na.rm = TRUE),
                   labels = c("Small", "Medium", "Large"), include.lowest = TRUE)
  ) %>%
  setDT()
fig_scatter <- ggplot(plot_data, aes(x = log(num_pat_families), y = log(prod_added))) +
  geom_point(aes(color = burst_status, size = empl), alpha = 0.4) +
  geom_smooth(method = "lm", color = "black", se = TRUE, alpha = 0.1) +
  scale_color_manual(
    values = c("Joint Burst" = "darkred", "Product Burst" = "blue",
               "Patent Burst" = "green", "No Burst" = "gray")
  ) +
  labs(
    title = "Patent-Product Correlation by Burst Status",
    x = "Log(Patent Families)",
    y = "Log(Products Added)",
    color = "Burst Type",
    size = "Employment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
message("  ✓ Patent-product scatter plot")

# Rank-frequency distribution
products_ranked_plot <- patenting_products[prod_added > 0, .(
  rank = rank(-prod_added),
  prod_added = prod_added
)] %>%
  mutate(type = "Products") %>%
  filter(rank <= 1000)
patents_ranked_plot <- patenting_products[num_pat_families > 0, .(
  rank = rank(-num_pat_families),
  num_pat = num_pat_families
)] %>%
  mutate(type = "Patents") %>%
  filter(rank <= 1000)
plot_data_rank <- bind_rows(
  products_ranked_plot %>% rename(value = prod_added) %>% select(rank, value, type),
  patents_ranked_plot %>% rename(value = num_pat) %>% select(rank, value, type)
)
fig_rank <- ggplot(plot_data_rank, aes(x = log(rank), y = log(value), color = type)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1) +
  scale_color_manual(values = c("Products" = "blue", "Patents" = "red")) +
  labs(
    title = "Rank-Frequency Distribution: Products vs Patents",
    subtitle = "Testing for power-law (thick-tailed) distribution",
    x = "Log(Rank)",
    y = "Log(Quantity)",
    color = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
message("  ✓ Rank-frequency distribution")

# Burst composition over time
composition <- patenting_products[, .(
  count = .N,
  avg_products = mean(prod_added, na.rm = TRUE),
  avg_patents = mean(num_pat_families, na.rm = TRUE)
), by = .(year, burst_type)]
fig_composition <- ggplot(composition, aes(x = year, y = count, fill = burst_type)) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c("Joint Burst" = "darkred", "Product Only" = "blue",
               "Patent Only" = "green", "No Burst" = "lightgray")
  ) +
  labs(
    title = "Evolution of Burst Types Over Time",
    x = "Year",
    y = "Share of Firms",
    fill = "Burst Type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom")
message("  ✓ Burst composition over time")

message("✓ ANALYSIS COMPLETE")

# Execute main analysis
analysis_results <- main_patent_burst_analysis()

# ============================================================================
# SECTION 13: EXPORT RESULTS TO TABLES AND FIGURES
# ============================================================================

# Export regression tables
export_regression_tables <- function(results, output_dir = output_dir) {
  
  message("Exporting regression tables...")
  
  # Table 2: Patent-Product Relationship
  etable(
    results$table2_models$m1,
    results$table2_models$m2,
    results$table2_models$m3,
    results$table2_models$m4,
    tex = TRUE,
    file = paste0(output_dir, "table2_patent_product.tex"),
    title = "Patent-Product Relationship Across Model Specifications",
    dict = c(log_products = "Log(Products Added)", log_patents = "Log(Patents)",
             patent_burst = "Patent Burst", empl = "Employment", firm_age = "Firm Age")
  )
  message("  ✓ Table 2 exported")
  
  # Table 3: Burst Correlation
  etable(
    results$table3_models$overall,
    results$table3_models$by_size,
    results$table3_models$with_industry,
    tex = TRUE,
    file = paste0(output_dir, "table3_burst_correlation.tex"),
    title = "Product-Patent Burst Correlation",
    dict = c(burst = "Product Burst", patent_burst = "Patent Burst")
  )
  message("  ✓ Table 3 exported")
  
  # Table 4: Amplification Effects
  etable(
    results$table4_models$m1,
    results$table4_models$m2,
    results$table4_models$m3,
    tex = TRUE,
    file = paste0(output_dir, "table4_amplification.tex"),
    title = "Burst Amplification: How Patents Amplify Product Creation"
  )
  message("  ✓ Table 4 exported")
}

# Export summary statistics table
export_summary_table <- function(results, output_dir = output_dir) {
  
  message("Exporting summary statistics...")
  
  table_formatted <- results$summary_table %>%
    as.data.frame()
  
  # Save as CSV
  write.csv(table_formatted, 
            paste0(output_dir, "table1_summary_stats.csv"), 
            row.names = FALSE)
  
  # Save as LaTeX
  print(xtable(table_formatted),
        file = paste0(output_dir, "table1_summary_stats.tex"),
        include.rownames = FALSE)
  
  message("  ✓ Table 1 exported")
}

# Export distribution analysis results
export_distribution_results <- function(results, output_dir = output_dir) {
  
  message("Exporting distribution analysis...")
  
  dist_summary <- data.frame(
    Distribution = c("Products", "Patents"),
    Theta_Estimate = c(results$distribution$theta_products, 
                       results$distribution$theta_patents),
    Theory_Reference = c(3.10, NA),
    Interpretation = c("Rank-frequency slope", "Rank-frequency slope")
  )
  
  write.csv(dist_summary, 
            paste0(output_dir, "distribution_analysis.csv"),
            row.names = FALSE)
  
  message("  ✓ Distribution results exported")
}

# Save all figures
save_all_figures <- function(results, output_dir = output_dir) {
  
  message("Saving figures...")
  
  ggsave(paste0(output_dir, "fig_patent_product_burst.png"),
         results$figures$scatter, width = 10, height = 6, dpi = 300)
  message("  ✓ Figure 1: Patent-Product Burst Scatter")
  
  ggsave(paste0(output_dir, "fig_rank_frequency.png"),
         results$figures$rank_freq, width = 10, height = 6, dpi = 300)
  message("  ✓ Figure 2: Rank-Frequency Distribution")
  
  ggsave(paste0(output_dir, "fig_burst_composition.png"),
         results$figures$composition, width = 10, height = 6, dpi = 300)
  message("  ✓ Figure 3: Burst Composition Over Time")
}

# Main export function
export_all_results <- function(results, output_dir = output_dir) {
  
  message("\n" %+% "="*70)
  message("EXPORTING RESULTS")
  message("="*70)
  
  export_summary_table(results, output_dir)
  export_regression_tables(results, output_dir)
  export_distribution_results(results, output_dir)
  save_all_figures(results, output_dir)
  
  message("\n✓ ALL RESULTS EXPORTED")
  message("="*70 %+% "\n")
}

# Execute export if analysis completed
if (exists("analysis_results")) {
  export_all_results(analysis_results)
}

# ============================================================================
# SECTION 14: OPTIONAL - EXTENDED ANALYSES
# ============================================================================

# These extensions can be run for additional robustness and insight

# Extension 1: Test timing dynamics
analyze_timing_dynamics <- function(data) {
  
  # Does patent activity predict future product bursts?
  data_dynamics <- data %>%
    arrange(firmid, year) %>%
    group_by(firmid) %>%
    mutate(
      burst_next = lead(burst),
      patents_current = num_pat_families,
      patents_lag1 = lag(num_pat_families)
    ) %>%
    ungroup() %>%
    setDT()
  
  # Forward-looking model
  model_forward <- feols(
    burst_next ~ patents_current,
    data = data_dynamics,
    se = "robust"
  )
  
  return(model_forward)
}

# Extension 2: Robustness check - alternative burst measures
run_robustness_alternative_measures <- function(data) {
  
  checks <- list()
  
  # Using different product thresholds
  for (threshold in c(3, 4, 5)) {
    data_temp <- data %>%
      mutate(burst_alt = prod_added >= threshold) %>%
      setDT()
    
    checks[[paste0("threshold_", threshold)]] <- feols(
      burst_alt ~ patent_burst,
      data = data_temp,
      se = "robust"
    )
  }
  
  return(checks)
}

# ============================================================================
# END OF MODULE 6 - PATENT BURST ANALYSIS
# ============================================================================
#
# SUMMARY OF KEY OUTPUTS:
# 1. Summary statistics by burst type (Table 1)
# 2. Patent-product relationship regressions (Table 2)
# 3. Burst correlation analysis (Table 3)
# 4. Amplification effect models (Table 4)
# 5. Distribution analysis with theta estimates
# 6. Heterogeneity effects by size, age, and industry
# 7. Visualization figures
# 8. Robustness checks
#
# Key datasets created:
# - patenting_products (with burst indicators and patent merges)
# - Burst classifications: product_burst, patent_burst, joint_burst
#
# Next steps:
# 1. Modify output_dir as needed
# 2. Run main_patent_burst_analysis()
# 3. Export results with export_all_results()
# 4. Modify specifications as needed for publication
#
# ============================================================================

