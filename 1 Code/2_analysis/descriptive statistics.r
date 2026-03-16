
# =============================================================================
# Objective: Descriptive statistics of the number and share of firms in different
#            samples (full sample, prodcom firms, patenting firms), by size, age and industry.
# =============================================================================

# --- Setup -------------------------------------------------------------------
source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))

# Ancillary datasets
nace_labels_dt <- read_csv("Ancillary datasets/nace_labels.csv", col_types = cols(nace_2d = col_character()))
nace_labels    <- setNames(nace_labels_dt$description, nace_labels_dt$nace_2d)

# --- Data Loading ------------------------------------------------------------
full_sample <- as.data.table(read_parquet("1_firm_yr_lvl_br_dta.parquet"))
prodcom_sample <- as.data.table(read_parquet(paste0("2_product_data/", cpa_or_pf, "/2c_firm_lvl_product_dta.parquet"))) %>%
  merge(full_sample[, .(firmid, year, NACE_2d_BR, size_bin, age_bin)], by = c("firmid", "year"), all.x = TRUE)
patenting_sample <- as.data.table(read_rds("4b_patenting_products_firm_level.rds")) %>%
  merge(full_sample[, .(firmid, year, NACE_2d_BR, size_bin, age_bin)], by = c("firmid", "year"), all.x = TRUE)

samples <- list(
  "Full BR Sample" = full_sample,
  "BR Sample - Prodcom Sectors" = full_sample[NACE_2d_BR %in% prodcom_sectors],
  "Prodcom Sample" = prodcom_sample,
  "Full Patenting Sample" = patenting_sample,
  "Patenting Sample - Prodcom Sectors" = patenting_sample[NACE_2d_BR %in% prodcom_sectors]
)

# --- Helpers -----------------------------------------------------------------
make_table <- function(data, names_from, values_from, filename_prefix,
                       caption_prefix, sample_name, percent = FALSE) {
  out <- data %>%
    filter(!is.na(.data[[names_from]])) %>%
    pivot_wider(names_from = all_of(names_from), values_from = all_of(values_from)) %>%
    select(-any_of("NA")) %>%
    arrange(across(1))
  create_latex_table(
    data = out,
    var_name = sample_name,
    caption = paste0(caption_prefix, " - ", sample_name),
    output_dir = output_dir,
    include_preamble = FALSE,
    digits = 2,
    filename = paste0(filename_prefix, gsub("\\s+", "_", tolower(sample_name))),
    color_cells = TRUE,
    percent = percent
  )
}

make_industry_table <- function(industry_long, values_from, filename, caption,
                                percent = FALSE) {
  out <- industry_long %>%
    filter(!is.na(NACE_2d_BR)) %>%
    pivot_wider(id_cols = "NACE_2d_BR", names_from = "sample", values_from = all_of(values_from)) %>%
    arrange(NACE_2d_BR) %>%
    mutate(Description = nace_labels[as.character(NACE_2d_BR)]) %>%
    select(NACE_2d_BR, Description, everything())
  create_latex_table(
    data = out,
    caption = caption,
    output_dir = output_dir,
    include_preamble = FALSE,
    digits = 2,
    filename = filename,
    color_cells = TRUE,
    percent = percent
  )
}

# --- Tables ------------------------------------------------------------------
ref_year <- 2016

# By age and size (one table per sample)
for (sample in names(samples)) {
  sample_name <- sample
  sample_data <- samples[[sample]][year == ref_year]

  sample_data[, size_bin := factor(size_bin, levels = c("1-4", "5-9", "10-19", "20-49", "50-99", "100-249", "250-499", "500-999", "1000+"), ordered = TRUE)]
  sample_data[, age_bin := factor(age_bin, levels = c("0", "1-2", "3-5", "6-10", "11-20", "21+"), ordered = TRUE)]

  setorder(sample_data, age_bin, size_bin)
  n_firms_age_size <- sample_data[, .(n_firms = n_distinct(firmid)), by = c("size_bin", "age_bin")]
  setorder(n_firms_age_size, size_bin, age_bin)
  make_table(n_firms_age_size, "size_bin", "n_firms", "n_firms_", "Number of Firms by Age and Size", sample_name)

  share_firms_age_size <- n_firms_age_size[, .(age_bin, size_bin, share_firms = n_firms / sum(n_firms))]
  make_table(share_firms_age_size, "size_bin", "share_firms", "share_firms_", "Share of Firms by Age and Size", sample_name, percent = TRUE)
}

# By industry (one combined table per metric, columns = samples)
industry_long <- rbindlist(lapply(names(samples), function(sample) {
  n <- samples[[sample]][year == ref_year, .(n_firms = n_distinct(firmid)), by = "NACE_2d_BR"]
  n[, `:=`(share_firms = n_firms / sum(n_firms), sample = sample)]
  n
}))

make_industry_table(industry_long, "n_firms",    "n_firms_industry",    "Number of Firms by Industry")
make_industry_table(industry_long, "share_firms", "share_firms_industry", "Share of Firms by Industry", percent = TRUE)
