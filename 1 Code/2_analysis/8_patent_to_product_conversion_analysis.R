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

test <- feols(nq_growth ~ young*size, data=patenting_products)
vcov_size_age   <- vcov(test)


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
  nq_growth, empl_growth, capital_growth, 
  nq_bar, empl_bar, capital_bar,
  size_bin,
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

View(patenting_products[, .(firmid, year, num_pat_families, prod_added, future_products_2, converted_patents_2, nonconverted_patents_2,
                             future_prod_burst2, future_prod_nonburst2, burst_patent_to_burst_products_2, burst_patent_to_non_burst_products_2,
                             non_burst_patent_to_burst_products_2, non_burst_patent_to_non_burst_products_2)])

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

y_weights <- c(
  nq_growth = "nq_bar",
  empl_growth = "empl_bar",
  capital_growth = "capital_bar"
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

    w_name <- y_weights[y_var]
    weight_vec <- patenting_products[[w_name]]

    # Start formulas
    formula_size <- as.formula(paste0(y_var, " ~ "))
    formula_age <- as.formula(paste0(y_var, " ~ "))
    formula_size_age <- as.formula(paste0(y_var, " ~ "))

    # Create dummies for years before and after event (relative to event year)
    patenting_products <- lead_lag_creator(patenting_products, y_var, n_lags = event_window, to_dummy = FALSE)
    View(patenting_products %>% select(firmid, year, starts_with(y_var)))
    
    #
    results_age <- list()
    results_size <- list()

    for(i in -event_window:(event_window)) {
      
      y_var_adj <- if (i == 0) {
        y_var
      } else if (i < 0) {
        paste0(y_var, "_lag", abs(i))
      } else {
        paste0(y_var, "_lead", i)
      }

    model_size <- lm(
      as.formula(paste0(y_var_adj, " ~ ", x_var, " * size_bin")),
      data = patenting_products,
      weights = weight_vec
    )
    model_age <- lm(
      as.formula(paste0(y_var_adj, " ~ ", x_var, " * age_bin")),
      data = patenting_products,
      weights = weight_vec
    )
    model_size_age <- lm(
      as.formula(paste0(y_var_adj, " ~ ", x_var, " * size_bin * age_bin")),
      data = patenting_products,
      weights = weight_vec
    )
    
    slopes_size <- as.data.frame(
      emtrends(model_size, specs = ~size_bin, var = x_var, infer = c(TRUE, TRUE))
    )
    slopes_age <- as.data.frame(
      emtrends(model_age, specs = ~age_bin, var = x_var, infer = c(TRUE, TRUE))
    )
    emtrends_size_age_obj <- emtrends(model_size_age, specs = ~size_bin * age_bin, var = x_var, infer = c(TRUE, TRUE))
    slopes_size_age <- as.data.frame(emtrends_size_age_obj)
    vcov_size_age   <- vcov(emtrends_size_age_obj)
    
    slope_col <- paste0(x_var, ".trend")
    
    age_weights <- patenting_products[
      ,
      .(w_age = sum(get(w_name), na.rm = TRUE)),
      by = age_bin
    ] %>%
      mutate(w_age = w_age / sum(w_age))
    
    size_weights <- patenting_products[
      ,
      .(w_size = sum(get(w_name), na.rm = TRUE)),
      by = size_bin
    ] %>%
      mutate(w_size = w_size / sum(w_size))
    
    # Tag row indices before joins so we can slice vcov_size_age by position
    slopes_size_age_idx <- slopes_size_age %>% mutate(.row_idx = row_number())
    
    # Correct SE via sqrt(w' Σ w) using the full covariance matrix of cell slopes
    vcov_se_size <- slopes_size_age_idx %>%
      left_join(age_weights, by = "age_bin") %>%
      group_by(size_bin) %>%
      group_modify(~ {
        keep <- !is.na(.x[[slope_col]]) & !is.na(.x$w_age)
        sub  <- .x[keep, ]
        idx  <- sub$.row_idx
        w    <- sub$w_age
        data.frame(se_size_age_control_vcov = sqrt(as.numeric(w %*% vcov_size_age[idx, idx, drop = FALSE] %*% w)))
      }) %>%
      ungroup()
    
    vcov_se_age <- slopes_size_age_idx %>%
      left_join(size_weights, by = "size_bin") %>%
      group_by(age_bin) %>%
      group_modify(~ {
        keep <- !is.na(.x[[slope_col]]) & !is.na(.x$w_size)
        sub <- .x[keep, ]
        idx <- sub$.row_idx
        w <- sub$w_size
        data.frame(se_age_size_control_vcov = sqrt(as.numeric(w %*% vcov_size_age[idx, idx, drop = FALSE] %*% w)))
      }) %>%
      ungroup()
    
    # setDT(size_effects)
    # size_effects[, test := se_size_age_control_vcov - se_size_age_control]
    
    size_effects <- slopes_size_age %>%
      left_join(age_weights, by = "age_bin") %>%
      group_by(size_bin) %>%
      summarise(
        beta_size_age_control = sum(.data[[slope_col]] * w_age, na.rm = TRUE),
        se_size_age_control   = sqrt(sum((w_age ^ 2) * (.data[["SE"]] ^ 2), na.rm = TRUE)),  # naive: assumes independence
        .groups = "drop"
      ) %>%
      left_join(vcov_se_size, by = "size_bin") %>%  # correct: sqrt(w' Σ w)
      left_join(
        slopes_size %>%
          transmute(
            size_bin,
            beta_size = .data[[slope_col]],
            se_size = .data[["SE"]]
          ),
        by = "size_bin"
      )
    
    age_effects <- slopes_size_age %>%
      left_join(size_weights, by = "size_bin") %>%
      group_by(age_bin) %>%
      summarise(
        beta_age_size_control = sum(.data[[slope_col]] * w_size, na.rm = TRUE),
        se_age_size_control   = sqrt(sum((w_size ^ 2) * (.data[["SE"]] ^ 2), na.rm = TRUE)),  # naive: assumes independence
        .groups = "drop"
      ) %>%
      left_join(vcov_se_age, by = "age_bin") %>%  # correct: sqrt(w' Σ w)
      left_join(
        slopes_age %>%
          transmute(
            age_bin,
            beta_age = .data[[slope_col]],
            se_age = .data[["SE"]]
          ),
        by = "age_bin"
      )
    
    # Plot size effects
    plot_data_size <- size_effects %>%
      select(size_bin, beta_size, se_size, beta_size_age_control, se_size_age_control) %>%
      pivot_longer(
      cols = starts_with("beta_"),
      names_to = "type",
      values_to = "beta",
      names_prefix = "beta_"
      ) %>%
      left_join(
      size_effects %>%
        select(size_bin, beta_size, se_size, beta_size_age_control, se_size_age_control) %>%
        pivot_longer(
        cols = starts_with("se_"),
        names_to = "type",
        values_to = "se",
        names_prefix = "se_"
        ),
      by = c("size_bin", "type")
      ) %>%
      mutate(
      ci_low = beta - 1.96 * se,
      ci_high = beta + 1.96 * se,
      type = factor(type, levels = c("size", "size_age_control"),
              labels = c("Direct (size-only)", "Age-weighted"))
      )

      # Add to results list
      setDT(plot_data_size)
      results_size[[as.character(i)]] <- plot_data_size[, time := i]

    p_size <- ggplot(plot_data_size, aes(x = size_bin, y = beta, color = type, fill = type, group = type)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 2) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
      title = paste0(x_name, " \u2192 ", y_labels[y_var], " (Size)"),
      subtitle = paste0("Time: ", i, " years from event"),
      x = "Size bin",
      y = "Estimated effect",
      color = "Effect",
      fill = "Effect"
      )

    print(p_size)
    ggsave(
      paste0(output_dir, "/effect_", x_name, "_", y_var, "_size_bin_", i, ".png"),
      plot = p_size,
      width = 8,
      height = 6
    )

    # Plot age effects
    plot_data_age <- age_effects %>%
      select(age_bin, beta_age, se_age, beta_age_size_control, se_age_size_control) %>%
      pivot_longer(
      cols = starts_with("beta_"),
      names_to = "type",
      values_to = "beta",
      names_prefix = "beta_"
      ) %>%
      left_join(
      age_effects %>%
        select(age_bin, beta_age, se_age, beta_age_size_control, se_age_size_control) %>%
        pivot_longer(
        cols = starts_with("se_"),
        names_to = "type",
        values_to = "se",
        names_prefix = "se_"
        ),
      by = c("age_bin", "type")
      ) %>%
      mutate(
      ci_low = beta - 1.96 * se,
      ci_high = beta + 1.96 * se,
      type = factor(type, levels = c("age", "age_size_control"),
              labels = c("Direct (age-only)", "Size-weighted"))
      )

      # Add to results list
      setDT(plot_data_age)
      results_age[[as.character(i)]] <- plot_data_age[, time := i]


    p_age <- ggplot(plot_data_age, aes(x = age_bin, y = beta, color = type, fill = type, group = type)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 2) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
      title = paste0(x_name, " \u2192 ", y_labels[y_var], " (Age)"),
      subtitle = paste0("Time: ", i, " years from event"),
      x = "Age bin",
      y = "Estimated effect",
      color = "Effect",
      fill = "Effect"
      )

    print(p_age)
    ggsave(
      paste0(output_dir, "/effect_", x_name, "_", y_var, "_age_bin_", i, ".png"),
      plot = p_age,
      width = 8,
      height = 6
    )

    }

    results_age <- rbindlist(results_age)
    results_age[, age_bin := factor(age_bin, levels = c("0", "1-2", "3-5", "6-10", "11-20", "21+"))]
    results_age <- results_age %>% setorder(age_bin)
        p_age_over_time <- ggplot(results_age, aes(x = time, y = beta, color = type, fill = type, group = type)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 2) +
      facet_wrap(~age_bin) +
      theme_minimal() +
      labs(
        title = paste0(x_name, " \u2192 ", y_labels[y_var], " (Age: ", age_bin_particular, ")"),
        x = "Time from event (years)",
        y = "Estimated effect",
        color = "Effect",
        fill = "Effect"
      )
      print(p_age_over_time)
      ggsave(
        paste0(output_dir, "/effect_over_time_", x_name, "_", y_var, "_age_bin_", age_bin_particular, ".png"),
        plot = p_age_over_time,
        width = 8,
        height = 6




    results_size <- rbindlist(results_size)
    results_size[, size_bin := factor(size_bin, levels = c("1-4", "5-9", "10-19", "20-49", "50-99", "100-249", "250-499", "500-999", "1000+"))]
      p_size_over_time <- ggplot(results_size[!(size_bin %in% c("1-4", "5-9"))], aes(x = time, y = beta, color = type, fill = type, group = type)) +
        geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 2) +
        facet_wrap(~size_bin) +
        theme_minimal() +
        labs(
          title = paste0(x_name, " \u2192 ", y_labels[y_var], " (Size: ", size_bin_particular, ")"),
          x = "Time from event (years)",
          y = "Estimated effect",
          color = "Effect",
          fill = "Effect"
        )
      print(p_size_over_time)
      ggsave(
        paste0(output_dir, "/effect_over_time_", x_name, "_", y_var, "_size_bin_", size_bin_particular, ".png"),
        plot = p_size_over_time,
        width = 8,
        height = 6
      )

  }
}


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
