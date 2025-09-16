#' ------------------------------------------------------------------------------
#' Script: Patent Data Cleaning and Cumulative IPCR/NACE Generation
#' Author: Juli?n D?az-Acosta
#' Last update: 2025-02-27 (optimized 2025-04-03)
#' ------------------------------------------------------------------------------

# 0) Setup ----------------------------------------------------------------------
source(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Main.R"))
folder_name <- ""
output_dir <- paste0(output_dir, "2025/Export 04.09/")
output_dir_creator(output_dir)

# 1) Load and clean patent data ------------------------------------------------
patent_data_upd <- read_parquet("tm_patent/patent_record_level_final.parquet")
setDT(patent_data_upd)
patent_data_upd <- patent_data_upd[!is.na(siren)]

# 2) Expand multiple firm IDs per patent ---------------------------------------
patent_data_upd[, siren_list := strsplit(siren, ",")]
patent_data_upd <- patent_data_upd[
  , .(firmid = unlist(siren_list)),
  by = setdiff(names(patent_data_upd), "siren_list")
]

# 3) Process IPCR codes --------------------------------------------------------
patent_data_upd[, `:=`(
  ipcr_upd = gsub(" ", "", ipcr),
  patent_id = .I
)]

patent_data_upd[, ipcr_list := strsplit(ipcr_upd, ",")]
test <- patent_data_upd[, .(ipcr = unlist(ipcr_list)), by = .(firmid, application_year, patent_id)]

# Normalize IPCR to 4 digits + handle special codes
special_codes <- c("A61K8", "B65F1", "B65F3", "B65F5", "B65F7", "B65F9")
test[, ipcr4 := substr(toupper(ipcr), 1, 5)]
test[, ipcr4 := ifelse(ipcr4 %in% special_codes, ipcr4, substr(ipcr4, 1, 4))]

test <- unique(test[, .(firmid, application_year, patent_id, ipcr4)])

# 4) Add NACE codes via concordance --------------------------------------------
nace_ipc_concord <- unique(fread("IPC V8_NACE Rev 2.txt"))
test <- merge(test, nace_ipc_concord[, .(IPCV2015, NACE2)],
  by.x = "ipcr4", by.y = "IPCV2015", all.x = TRUE
)

# 5) Create full firm-year panel ------------------------------------------------
start <- 1990
end <- 2024
years_per_firm <- test[, .(min_year = start, max_year = end), by = firmid]
all_years <- years_per_firm[, .(application_year = min_year:max_year), by = firmid]

test_expanded <- merge(all_years, test, by = c("firmid", "application_year"), all = TRUE)
rm(test, years_per_firm, patent_data_upd, nace_ipc_concord)
gc()
setorder(test_expanded, firmid, application_year)

# 6) Efficient cumulative IPCR4 and NACE2 sets ----------------------------------
cum_data <- test_expanded[
  order(firmid, application_year),
  .(ipcr4 = list(ipcr4), NACE2 = list(NACE2)),
  by = .(firmid, application_year)
][
  , `:=`(
    ipcr_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), ipcr4, accumulate = TRUE),
    NACE_cum = Reduce(function(x, y) unique(c(x[!is.na(x)], if (all(is.na(y))) character(0) else y)), NACE2, accumulate = TRUE)
  ),
  by = firmid
][
  , .(firmid, application_year, ipcr_cum, NACE_cum)
][
  , `:=`(
    ipcr_cum = lapply(ipcr_cum, unique),
    NACE_cum = lapply(NACE_cum, unique),
    year = application_year
  )
][, `:=`(
  n_ipcr = sapply(ipcr_cum, function(x) sum(!is.na(x))),
  n_NACE = sapply(NACE_cum, function(x) sum(!is.na(x)))
)]
growth <- growth_creator(cum_data, c("n_NACE", "n_ipcr"), 1)[, c("firmid", "year", "n_NACE_bar", "n_ipcr_bar", "n_NACE_growth", "n_ipcr_growth")]
cum_data_og <- copy(cum_data)
cum_data <- merge(cum_data, growth, by = c("firmid", "year"), all.x = T)

# Shift forward for lag comparison
cum_data_fwd <- copy(cum_data)[, application_year := application_year + 1][, c("firmid", "application_year", "ipcr_cum", "NACE_cum")]
setnames(cum_data_fwd, c("ipcr_cum", "NACE_cum"), c("ipcr_cum_l", "NACE_cum_l"))

# Merge current and lagged cumulative data
ipcr_cumulative <- merge(cum_data, cum_data_fwd, by = c("firmid", "application_year"), all.x = TRUE)

# 7) Identify new IPCR/NACE codes per year -------------------------------------
ipcr_cumulative[, `:=`(
  new_ipcr = Map(setdiff, ipcr_cum, ipcr_cum_l),
  new_NACE = Map(setdiff, NACE_cum, NACE_cum_l)
)]
ipcr_cumulative[, ipcr_creat := sapply(new_ipcr, function(x) ifelse(length(x) > 0, 1, 0))]
ipcr_cumulative[, ipcr_creat := ifelse(ipcr_creat == 1 & is.na(n_NACE_growth), 0, ipcr_creat)]

saveRDS(ipcr_cumulative, "ipcr_cumulative.RDS")

# 8) IPC analysis -----------------

product_data <- readRDS(paste0("product_level_growth_", filter_indicator, "_.RDS")) # Product level data (from a.)
ipcr_cumulative <- readRDS("ipcr_cumulative.RDS") # From above
br_industry_HHI <- readRDS("br_industry_HHI.RDS") # Industry data (from c.)
patenting_products <- readRDS("product_firm_data_pre_high_growth.RDS") %>% # Product information aggregated at the firm level (from e.)
  .[abs(empl_growth) != 2 & abs(nq_growth) != 2 & abs(rev_growth) != 2] %>%
  select(
    firmid, year, NACE_BR, young,
    patent, patent_window, ever_patent,
    size, young, firm_age, number_of_products,
    # superstar, superstar_cr4, superstar_tfp_99, superstar_tfp_90,
    prod_creat, prod_destr,
    within_economy_rev_share_BR, within_industry_rev_share,
    tm, tm_window, ever_tm,
    new_products, first_introduction,
    net_product_creat, net_product_creat_window,
    net_product_destr, net_product_destr_window,
    empl_bar, empl_l, empl_growth, nq_growth, nq_bar,
    capital, capital_growth,
    tfp_growth, tfp_bar, rev_growth, rev_bar,
    size_quartile, size_decile, size_percentile,
    age_size_bucket, age_size_quartile, age_size_decile, age_size_percentile,
    ipcr_cum, NACE_cum, n_ipcr, n_NACE, n_NACE_bar, n_ipcr_bar, n_NACE_growth,
    n_ipcr_growth, ipcr_cum_l, NACE_cum_l, new_ipcr, new_NACE, ipcr_creat
  )

# Identify firms that have patented at some point
ipcr_firmids <- unique(ipcr_cumulative$firmid) # length(ipcr_firmids) 118129
prod_firmids <- unique(patenting_products$firmid) # length(prod_firmids) 70354
patenting_prod_firmids <- intersect(ipcr_firmids, prod_firmids) # length(patenting_prod_firmids) 9109

# Create important variables and clean data
patenting_products[, `:=`(ipcr_creat = fifelse(is.na(ipcr_creat), 0, ipcr_creat))] %>% # Treat NA as 0 for ipcr_creat
  window_var_cretor("firmid", "year", "ipcr_creat", 2, 0, "ipcr_creat_window", na_rm = F) %>% # Create 2-year window for ipcr_creat
  .[, `:=`(
    log_firm_age = log(firm_age),
    log_empl_bar = log(empl_bar)
  )] %>%
  .[, product_innovative_patent_window := net_product_creat_window * patent_window] %>% # Interaction between product creation and patenting
  .[, product_strategic_patent_window := (1 - net_product_creat_window) * patent_window] # Interaction between lack of product creation and patenting

# Create revenue share growth variables
rev_share_growth <- growth_creator(patenting_products, c("within_economy_rev_share_BR", "within_industry_rev_share"), 1) %>%
  select(
    firmid, year,
    within_economy_rev_share_BR_growth, within_economy_rev_share_BR_l,
    within_industry_rev_share_growth, within_industry_rev_share_l, withindustry_rev_share_bar
  )

# Merge all data
patenting_products <- merge(patenting_products, rev_share_growth, by = c("firmid", "year"), all.x = T)
patenting_products <- merge(patenting_products, br_industry_HHI, by = c("NACE_BR", "year"), all.x = T)
rm(rev_share_growth, br_industry_HHI, ipcr_cumulative)
gc()

# patenting_products[, `:=`(young_q1=fifelse(young==1 & size_quartile==1, 1, 0))]
# patenting_products[, `:=`(young_q2=fifelse(young==1 & size_quartile==2, 1, 0))]
# patenting_products[, `:=`(young_q3=fifelse(young==1 & size_quartile==3, 1, 0))]
# patenting_products[, `:=`(young_q4=fifelse(young==1 & size_quartile==4, 1, 0))]
# patenting_products[, `:=`(young_q10=fifelse(young==1 & size_decile==10, 1, 0))]
# patenting_products[, `:=`(young_q100=fifelse(young==1 & size_percentile==100, 1, 0))]
#
# patenting_products[, `:=`(mature_q1=fifelse(young==0 & size_quartile==1, 1, 0))]
# patenting_products[, `:=`(mature_q2=fifelse(young==0 & size_quartile==2, 1, 0))]
# patenting_products[, `:=`(mature_q3=fifelse(young==0 & size_quartile==3, 1, 0))]
# patenting_products[, `:=`(mature_q4=fifelse(young==0 & size_quartile==4, 1, 0))]
# patenting_products[, `:=`(mature_q10=fifelse(young==0 & size_decile==10, 1, 0))]
# patenting_products[, `:=`(mature_q100=fifelse(young==0 & size_percentile==100, 1, 0))]



write_rds(patenting_products, "patenting_products_firm_level.RDS")


# 8a) Growth, patenting and trademarking -----------------

patenting_products <- read_rds("patenting_products_firm_level.RDS")

ys <- c("nq_growth", "empl_growth", "rev_growth", "within_industry_rev_share_growth") # , "rev_growth")
patent_var_og <- "patent"
ipcr_var_og <- "ipcr_creat"
threshold_young <- 5
output_dir_og <- output_dir
patenting_products_og <- patenting_products
inno_vars_og <- c("tm_window", "tm_window*net_product_creat_window", "patent_window", "patent_window*net_product_creat_window", "net_product_destr_window", "net_product_creat_window", "patent_window + ipcr_creat_window")
graphs <- T


# patenting_products<-patenting_products_og[, `:=`(young_small=fifelse(young==1 & size=="small", 1, 0))]
# patenting_products<-patenting_products_og[, `:=`(young_large=fifelse(young==1 & size!="small", 1, 0))]
# patenting_products<-patenting_products_og[, `:=`(mature_small=fifelse(young==0 & size=="small", 1, 0))]
# patenting_products<-patenting_products_og[, `:=`(mature_medium=fifelse(young==0 & size=="medium", 1, 0))]
# patenting_products<-patenting_products_og[, `:=`(mature_large=fifelse(young==0 & size=="large", 1, 0))]






ys <- c("nq_growth", "empl_growth", "rev_growth", "within_industry_rev_share_growth") # , "rev_growth")
xs <- c("tm_window", "tm_window*net_product_creat_window", "patent_window", "patent_window*net_product_creat_window", "net_product_destr_window", "net_product_creat_window", "patent_window + ipcr_creat_window")
controls <- c("log(empl_l)")
fe <- c("", "firmid + year", "NACE_BR + year", "NACE_BR^year")


formulas <- create_formulas(ys, xs, controls, fe)
titles_and_restrictions <- fread(
  "title, restriction
  M. Small, mature_small==1
  M. Medium, mature_medium==1
  M. Large, mature_large==1
  Y. Small, young_small==1
  Y. NonSmall, young_large==1
  All, NA"
)



# For nq, empl, prod_Creat variables, segmenting the sample
for (type in c("all_firms")) {
  output_dir <- paste0(output_dir_og, type, "/")
  if (!dir.exists(output_dir)) {
    output_dir_creator(output_dir)
  }

  for (subset in c("all")) {
    patenting_products <- age_data_filter(patenting_products_og, threshold_young, subset)

    if (type == "patenting_firms") {
      patenting_products <- patenting_products[firmid %in% patenting_prod_firmids]
    }

    output_dir <- paste0(output_dir_og, type, "/", subset, "/")
    if (!dir.exists(output_dir)) {
      output_dir_creator(output_dir)
    }

    for (y in ys) {
      if (y == "net_product_creat_window") {
        inno_vars <- inno_vars_og[!str_detect(inno_vars_og, y)]
      } else {
        inno_vars <- inno_vars_og
      }

      for (inno_var in inno_vars) {
        print(paste0(y, " ~ ", inno_var))

        models <- setNames(vector("list", nrow(model_table)), model_table$title)
        for (i in seq_len(nrow(model_table))) {
          title <- model_table$title[i]
          restriction <- model_table$restriction[i]
          weight <- model_table$weight[i]

          if (is.na(model_table$restriction[i])) {
            # "All" case
            models[[i]] <- feols(formula, data, weights = if (!is.na(weight)) data[[weight]] else NULL) 
          } else {
            models[[i]] <- feols(formula, data[eval(parse(text = restriction))], weights = if (!is.na(model_table$weight[i])) data[[model_table$weight[i]]] else NULL) # nolint # nolint
          }
        }

        vars_interactions <- c("young_small", "young_large", "mature_small", "mature_medium", "mature_large", "superstar_cr4TRUE", "superstarTRUE", "superstar_tfp_90TRUE", "superstar_tfp_99TRUE")
        cum_coef_maps <- c()

        split_inno_var <- strsplit(inno_var, "\\*")[[1]]
        split_inno_var <- trimws(split_inno_var)

        if (length(split_inno_var) != 1) {
          keys <- paste0(paste(split_inno_var, collapse = ":"), ":", vars_interactions)
          values <- rep(paste0(paste(split_inno_var, collapse = ":"), ":category"), length(keys))
          coef_map <- setNames(values, keys)
          cum_coef_maps <- c(cum_coef_maps, coef_map)
          cum_coef_maps <- c(cum_coef_maps, setNames(gsub("\\*", ":", inno_var), gsub("\\*", ":", inno_var)))
        } else {
          split_inno_var <- strsplit(split_inno_var, "\\+")[[1]]
          split_inno_var <- trimws(split_inno_var)
        }

        cum_coef_maps <- c(cum_coef_maps, setNames(split_inno_var, split_inno_var))

        for (var in split_inno_var) {
          keys <- paste0(var, ":", vars_interactions)
          values <- rep(paste0(var, ":category"), length(keys))
          coef_map <- setNames(values, keys)
          cum_coef_maps <- c(cum_coef_maps, coef_map)
        }
        for (var in vars_interactions) {
          keys <- paste0(var)
          values <- rep(paste0("category"), length(keys))
          coef_map <- setNames(values, keys)
          cum_coef_maps <- c(cum_coef_maps, coef_map)
        }

        modelsummary(
          models,
          coef_map = cum_coef_maps,
          output = paste0(output_dir, "regressions_ipcr_addition_", y, "_", gsub("\\*", "_", inno_var), ".tex"),
          label = paste0("regressions_ipcr_addition_", y, "_", gsub("\\*", "_", inno_var)),
          stars = TRUE,
          # title = tools::toTitleCase(paste0(gsub("_", " ", y), " on ", gsub("_", " ", gsub("\\*", "_", inno_var)), " - Sample: ", subset, " within ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
        )

        if (graphs) {
          tidy_models <- imap(models, function(model, model_name) {
            if (str_detect(inno_var, "\\*")) {
              tidy(model, conf.int = T) %>%
                filter(str_detect(term, ".*:.*") | str_detect(term, paste(strsplit(inno_var, "\\*")[[1]], collapse = "|")) |
                  str_detect(term, paste(trimws(strsplit(inno_var, "\\+")[[1]]), collapse = "|"))) %>%
                mutate(model_label = model_name)
            } else {
              tidy(model, conf.int = T) %>%
                filter(str_detect(term, inno_var) |
                  str_detect(term, paste(trimws(strsplit(inno_var, "\\+")[[1]]), collapse = "|"))) %>%
                mutate(model_label = model_name)
            }
          })

          results_df <- bind_rows(tidy_models)
          setDT(results_df)
          results_df[, group := fifelse(
            grepl("Y.", model_label), "Young",
            fifelse(
              grepl("SS", model_label), "Superstar",
              fifelse(grepl("M.", model_label), "Mature", "All")
            )
          )]

          setDT(results_df)
          results_df[, c("matched_var", "term_clean") := {
            matched <- NA_character_
            for (v in vars_interactions) {
              if (str_ends(term, v)) {
                matched <- v
                break
              }
            }
            cleaned <- if (!is.na(matched)) str_remove(term, paste0(matched)) else "term"
            list(matched, cleaned)
          }, by = seq_len(nrow(results_df))]

          pattern <- paste(paste0(":", vars_interactions), collapse = "|")
          results_df[, term_clean := str_remove_all(term, pattern)]
          # results_df<-results_df[!is.na(matched_var)]

          results_df$model_label <- factor(results_df$model_label, levels = names(models))

          for (coefficient in unique(results_df$term_clean)) {
            results_df_temp <- results_df[term_clean == coefficient]

            group_levels <- unique(results_df_temp$group)
            color_values <- setNames(c(scales::hue_pal()(length(group_levels) - 1), "black"), group_levels)

            ggplot(results_df_temp, aes(x = model_label, y = estimate, ymin = conf.low, ymax = conf.high, color = group)) +
              geom_pointrange() +
              geom_hline(yintercept = 0, linetype = "dashed") +
              scale_color_manual(values = color_values) +
              labs(
                x = tools::toTitleCase(paste0("Subset")),
                y = paste("Estimate (with 95% CI)")
              )
            # title=tools::toTitleCase(paste0(gsub("_", " ", gsub("window", "W", y)),
            #                                 " = ",
            #                                 gsub("_", " ", gsub("_window", " W",  gsub("\\*", "? ", inno_var))))),
            # subtitle = tools::toTitleCase(paste0("Variable: ", gsub(" window", "  W",  gsub(":", " ? ", gsub("_", " ", coefficient) )))))+
            theme(legend.position = "none")
            ggsave(paste0(output_dir, "", y, "_", gsub("\\*", "_", inno_var), "_param_", gsub(":", " x ", coefficient), ".png"), height = 4, width = 7)
          }
        }
        print(paste0(output_dir, "regressions_ipcr_addition_", y, "_", inno_var, ".tex"))
      }
    }
  }
}




# 8b) Patenting Graphs -----------------

setDT(patenting_products)

start_year <- 1991
end_year <- 2022

#' Create firm typology, distinguishing firms that never patent,
#' that have patented at some point in the same ipc cluster,
#' and that have patented at some point in new ipc codes
firm_typology <- patenting_products[, .(
  sum_patenting = sum(patent, na.rm = T),
  sum_ipcr_creat = sum(ipcr_creat, na.rm = T)
),
by = .(firmid)
][
  , pat_firm_type := fifelse(
    sum_patenting == 0, "never patent",
    fifelse(sum_ipcr_creat > 0, "new ipcr", "same ipcr")
  )
][
  , c("firmid", "pat_firm_type")
]
patenting_products <- merge(patenting_products, firm_typology, by = "firmid", all.x = T)

#' Create product introduction rates by
patenting_year <- patenting_products[, .(
  net_product_creat_wt = weighted.mean(net_product_creat, nq_bar, na.rm = T),
  net_product_creat = mean(net_product_creat, na.rm = T)
), by = .(year, pat_firm_type)]

unweighted <- ggplot(patenting_year, aes(x = year, y = net_product_creat, color = pat_firm_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(patenting_products$year), max(patenting_products$year), by = 1)) +
  labs(
    title = "Net Product Creation Over Time by Firm Type",
    subtitle = "Unweigthed Average",
    x = "Year",
    y = "Net Product Creation",
    color = "Firm Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.25))

weighted <- ggplot(patenting_year, aes(x = year, y = net_product_creat_wt, color = pat_firm_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(patenting_products$year), max(patenting_products$year), by = 1)) +
  labs(
    subtitle = "Weighted by Average Revenue",
    x = "Year",
    y = "Net Product Creation",
    color = "Firm Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.25))

plot <- unweighted + theme(legend.position = "none") + weighted
print(unweighted)
ggsave(paste0(output_dir, "net_product_creat_time_pat_firm_type.png"), width = 6, height = 4, dpi = 300)


ipcr_year <- ipcr_cumulative[, .(ipcr_creat = mean(ipcr_creat, na.rm = T)), by = .(year)][year %in% start_year:end_year]

ggplot(ipcr_year, aes(x = year, y = ipcr_creat)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(ipcr_year$year), max(ipcr_year$year), by = 1)) +
  labs(
    title = "Share of Patenting Firms Expanding into New IPC Codes",
    x = "Year",
    y = "Share of Patenting Firms",
    color = "Firm Type"
  ) +
  theme_minimal() +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.3)


ggsave(paste0(output_dir, "share_pat_firms_new_ipc.png"), height = 5, width = 9)

# Graphs for patenting and tm by age and size--------------------------------
patent_tm_data <- readRDS("patent_tm_clean.RDS")
firm_data_select <- readRDS("sbs_br_combined_cleaned.rds")

size_age <- firm_data_select[, c(
  "firmid", "year", "NACE_BR", "size", "young",
  "superstar", "superstar_cr4", "superstar_tfp_99", "superstar_tfp_90"
)]
size_age <- merge(size_age, patent_tm_data[, c("firmid", "year", "num_patent", "num_tm")], by = c("firmid", "year"), all.x = T)

size_age[, patent := ifelse(num_patent <= 0 | is.na(num_patent), 0, 1)]
size_age[, tm := ifelse(num_tm <= 0 | is.na(num_tm), 0, 1)]

# Number and share of patents and trademarks per year
patent_tm_year <- size_age[, .(
  patent = sum(num_patent, na.rm = T),
  tm = sum(num_tm, na.rm = T),
  patent_share = mean(patent, na.rm = T),
  tm_share = mean(tm, na.rm = T),
  n_firms = .N
), by = .(year)]
patent_tm_year <- patent_tm_year[, `:=`(
  patent_per_firm = patent / n_firms,
  tm_per_firm = tm / n_firms
)]
for (var in c("patent", "tm", "patent_share", "tm_share", "patent_per_firm", "tm_per_firm")) {
  ggplot(patent_tm_year, aes(x = year, y = .data[[var]])) +
    geom_line()
  ggsave(paste0(output_dir, var, "_year.png"), height = 4, width = 8)
}

# Number and share of patents and trademarks per year and size

for (diff_var in c("size", "young")) {
  patent_tm_year <- size_age[, .(
    patent = sum(num_patent, na.rm = T),
    tm = sum(num_tm, na.rm = T),
    patent_share = mean(patent, na.rm = T),
    tm_share = mean(tm, na.rm = T),
    n_firms = .N
  ), by = .(year, get(diff_var))]
  # names(patent_tm_year)[names(patent_tm_year)=="get"]<-diff_var

  patent_tm_year <- patent_tm_year[, `:=`(
    patent_per_firm = patent / n_firms,
    tm_per_firm = tm / n_firms
  )]
  patent_tm_year <- patent_tm_year[!is.na(get)]


  for (var in c("patent", "tm", "patent_share", "tm_share", "patent_per_firm", "tm_per_firm")) {
    ggplot(patent_tm_year, aes(x = year, y = .data[[var]], color = as.factor(get))) +
      geom_line() +
      labs(color = diff_var)
    ggsave(paste0(output_dir, var, "_", diff_var, "_year.png"), height = 4, width = 8)
  }

  unique_vals <- unique(patent_tm_year$get)

  for (unique_val in unique_vals) {
    for (var in c("patent", "tm", "patent_share", "tm_share", "patent_per_firm", "tm_per_firm")) {
      ggplot(patent_tm_year[get == unique_val], aes(x = year, y = .data[[var]], color = as.factor(get))) +
        geom_line() +
        labs(color = diff_var)
      ggsave(paste0(output_dir, var, "_", diff_var, "_", unique_val, "_year.png"), height = 4, width = 8)
    }
  }
}










# 9) Patenting trademarking analysis -----------------

patenting_products <- readRDS("patenting_products_firm_level.RDS")
ys <- c("patent_window", "empl_growth")
patent_var_og <- "patent"
threshold_young <- 5
output_dir_og <- output_dir
graphs <- F
output_dir_og <- output_dir

formulas <- fread("formula, description
                .[y]  ~ log_empl_bar*HHI_industry + av_nq_growth + young | firmid + year, emp_interaction_firmfe
                .[y]  ~ log_empl_bar + within_industry_rev_share*HHI_industry + av_nq_growth + young | firmid + year, rev_interaction_firmfe
                .[y]  ~ log_empl_bar*HHI_industry + av_nq_growth + young | NACE_BR + year, emp_interaction_industryfe
                .[y]  ~ log_empl_bar + within_industry_rev_share*HHI_industry + av_nq_growth + young | NACE_BR + year, rev_interaction_industryfe
                .[y]  ~ log_empl_bar*(HHI_quartile==4) + av_nq_growth + young | firmid + year, emp_HHIq_firmfe
                .[y]  ~ log_empl_bar + within_industry_rev_share*(HHI_quartile==4) + av_nq_growth + young | firmid + year, rev_HHIQ_firmfe",
  sep = ","
)

patenting_products_og <- patenting_products

for (type in c("all_firms", "patenting_firms")) {
  output_dir <- paste0(output_dir_og, type, "/")
  if (!dir.exists(output_dir)) {
    output_dir_creator(output_dir)
  }

  for (subset in c("all")) { # Age subset

    patenting_products <- age_data_filter(patenting_products_og, threshold_young, subset)

    if (type == "patenting_firms") {
      patenting_products <- patenting_products[firmid %in% patenting_prod_firmids]
    }

    output_dir <- paste0(output_dir_og, type, "/", subset, "/")
    if (!dir.exists(output_dir)) {
      output_dir_creator(output_dir)
    }

    for (y in ys) {
      if (y != "empl_growth" & type == "patenting_firms") {
        next
      }


      for (i in 1:nrow(formulas)) {
        formula_description <- formulas[i, description]
        formula <- as.formula(formulas[i, formula])


        print(paste0(y))

        models <- list(
          "M. Small" = feols(formula, patenting_products[mature_small == 1], cluster = "firmid"),
          "M. Medium" = feols(formula, patenting_products[mature_medium == 1], cluster = "firmid"),
          "M. Large" = feols(formula, patenting_products[mature_large == 1], cluster = "firmid"),
          "Y. Small" = feols(formula, patenting_products[young_small == 1], cluster = "firmid"),
          "Y. NonSmall" = feols(formula, patenting_products[young_large == 1], cluster = "firmid"),
          "All" = feols(formula, weights = patenting_products[["nq_bar"]], patenting_products, cluster = "firmid")
        )

        modelsummary(
          models,
          # coef_map = cum_coef_maps,
          output = paste0(output_dir, "regressions_", y, "_", formula_description, "_size_cutoffs.tex"),
          label = paste0("regressions_", y),
          stars = TRUE,
          # title = tools::toTitleCase(paste0(gsub("_", " ", y), " on ", gsub("_", " ", gsub("\\*", "_", inno_var)), " - Sample: ", subset, " within ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
        )

        models <- list(
          "M. Q1" = feols(formula, patenting_products[mature_q1 == 1], cluster = "firmid"),
          # "M. Q2" = feols(formula, patenting_products[mature_q2==1], cluster = "firmid"),
          # "M. Q3" = feols(formula, patenting_products[mature_q3==1], cluster = "firmid"),
          "M. Q4" = feols(formula, patenting_products[mature_q4 == 1], cluster = "firmid"),
          "M. Q10" = feols(formula, patenting_products[mature_q10 == 1], cluster = "firmid"),
          "M. Q100" = feols(formula, patenting_products[mature_q100 == 1], cluster = "firmid"),
          "Y. Q1" = feols(formula, patenting_products[young_q1 == 1], cluster = "firmid"),
          # "Y. Q2" = feols(formula, patenting_products[young_q2==1], cluster = "firmid"),
          # "Y. Q3" = feols(formula, patenting_products[young_q3==1], cluster = "firmid"),
          "Y. Q4" = feols(formula, patenting_products[young_q4 == 1], cluster = "firmid"),
          "Y. Q10" = feols(formula, patenting_products[young_q10 == 1], cluster = "firmid"),
          "Y. Q100" = feols(formula, patenting_products[young_q100 == 1], cluster = "firmid"),
          "All" = feols(formula, weights = patenting_products[["nq_bar"]], patenting_products, cluster = "firmid")
        )

        modelsummary(
          models,
          # coef_map = cum_coef_maps,
          output = paste0(output_dir, "regressions_", y, "_", formula_description, "_size_quartiles.tex"),
          label = paste0("regressions_", y),
          stars = TRUE,
          # title = tools::toTitleCase(paste0(gsub("_", " ", y), " on ", gsub("_", " ", gsub("\\*", "_", inno_var)), " - Sample: ", subset, " within ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
        )

        if (formula_description == "emp_interaction_firmfe") {
          model_industry <- dynamic_reg_reallocation(patenting_products[mature_large == 1],
            y = y,
            x = c("log_empl_bar*HHI_industry", "av_nq_growth", "young"),
            fix_eff = c("firmid + year"),
            disag_var = "NACE_BR",
            n_lags_bw = 0,
            n_lags_fw = 0
          )

          model_industry <- merge(model_industry, br_industry_HHI[, .(median_HHI = median(HHI_industry)), by = NACE_BR],
            by.x = "filter",
            by.y = "NACE_BR",
            all.x = T
          ) %>%
            .[, sector := substr(filter, 1, 2)]


          ggplot(model_industry, aes(x = median_HHI, y = log(log_empl_bar_HHI_industry), color = as.factor(substr(filter, 1, 2)))) +
            geom_point(alpha = 0.5) +
            labs(
              x = "Median HHI",
              y = "Coefficient for log_empl_bar*HHI_industry",
              subtitle = "Mature Large Firms",
              color = "Sector"
            )
          ggsave(paste0(output_dir, y, "_", formula_description, ".png"), height = 6, width = 8)

          model_industry <- model_industry[nobs > 5]

          fwrite(model_industry, paste0(output_dir, "data_for_image_", y, "_", formula_description, ".csv"))
        }
      }




      if (graphs) {
        tidy_models <- imap(models, function(model, model_name) {
          if (str_detect(inno_var, "\\*")) {
            tidy(model, conf.int = T) %>%
              filter(str_detect(term, ".*:.*") | str_detect(term, paste(strsplit(inno_var, "\\*")[[1]], collapse = "|")) |
                str_detect(term, paste(trimws(strsplit(inno_var, "\\+")[[1]]), collapse = "|"))) %>%
              mutate(model_label = model_name)
          } else {
            tidy(model, conf.int = T) %>%
              filter(str_detect(term, inno_var) |
                str_detect(term, paste(trimws(strsplit(inno_var, "\\+")[[1]]), collapse = "|"))) %>%
              mutate(model_label = model_name)
          }
        })

        results_df <- bind_rows(tidy_models)
        setDT(results_df)
        results_df[, group := fifelse(
          grepl("Y.", model_label), "Young",
          fifelse(
            grepl("SS", model_label), "Superstar",
            fifelse(grepl("M.", model_label), "Mature", "All")
          )
        )]

        setDT(results_df)
        results_df[, c("matched_var", "term_clean") := {
          matched <- NA_character_
          for (v in vars_interactions) {
            if (str_ends(term, v)) {
              matched <- v
              break
            }
          }
          cleaned <- if (!is.na(matched)) str_remove(term, paste0(matched)) else "term"
          list(matched, cleaned)
        }, by = seq_len(nrow(results_df))]

        pattern <- paste(paste0(":", vars_interactions), collapse = "|")
        results_df[, term_clean := str_remove_all(term, pattern)]
        # results_df<-results_df[!is.na(matched_var)]

        results_df$model_label <- factor(results_df$model_label, levels = names(models))

        for (coefficient in unique(results_df$term_clean)) {
          results_df_temp <- results_df[term_clean == coefficient]

          group_levels <- unique(results_df_temp$group)
          color_values <- setNames(c(scales::hue_pal()(length(group_levels) - 1), "black"), group_levels)

          ggplot(results_df_temp, aes(x = model_label, y = estimate, ymin = conf.low, ymax = conf.high, color = group)) +
            geom_pointrange() +
            geom_hline(yintercept = 0, linetype = "dashed") +
            scale_color_manual(values = color_values) +
            labs(
              x = tools::toTitleCase(paste0("Subset")),
              y = paste("Estimate (with 95% CI)")
            )
          # title=tools::toTitleCase(paste0(gsub("_", " ", gsub("window", "W", y)),
          #                                 " = ",
          #                                 gsub("_", " ", gsub("_window", " W",  gsub("\\*", "? ", inno_var))))),
          # subtitle = tools::toTitleCase(paste0("Variable: ", gsub(" window", "  W",  gsub(":", " ? ", gsub("_", " ", coefficient) )))))+
          theme(legend.position = "none")
          ggsave(paste0(output_dir, "", y, "_", gsub("\\*", "_", inno_var), "_param_", gsub(":", " x ", coefficient), ".png"), height = 4, width = 7)
        }
      }
      # print(paste0(output_dir, "regressions_ipcr_addition_", y, "_", inno_var, ".tex"))
    }
  }
}
