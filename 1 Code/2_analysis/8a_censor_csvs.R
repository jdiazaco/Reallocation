# =============================================================================
# Standalone confidentiality censoring — no regressions required.
# Reads existing CSVs, joins observation counts from the raw RDS, censors
# cells with N < MIN_OBS, and adds min_numobs per row.
# =============================================================================

source(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Main.R"))
library(data.table); library(tidyr)

MIN_OBS         <- 5
results_csv_dir <- file.path(output_dir, "csv_results")

# --- Load only the minimal dataset (no regressions) --------------------------
pp <- readRDS("temp/patenting_products_firm_level.RDS") |> as.data.table()

# --- N lookup tables ---------------------------------------------------------
n_sz_ag <- pp[, .N, by = .(size_bin, age_bin)]

n_bins <- rbind(
  pp[, .N, by = size_bin][, .(bin = as.character(size_bin), bin_var = "size_bin", N)],
  pp[, .N, by = age_bin ][, .(bin = as.character(age_bin),  bin_var = "age_bin",  N)]
)

# --- 1. desc_*.csv  (wide: age_bin rows × size_bin cols) --------------------
for (f in list.files(results_csv_dir, pattern = "^desc_(?!slopes).+\\.csv$", full.names = TRUE, perl = TRUE)) {
  dt   <- fread(f)
  long <- melt(dt, id.vars = "age_bin", variable.name = "size_bin",
               value.name = "value", variable.factor = FALSE)
  long <- merge(long, n_sz_ag, by = c("age_bin", "size_bin"), all.x = TRUE)
  long[N < MIN_OBS, value := NA]
  long[, min_numobs := min(N, na.rm = TRUE), by = age_bin]
  mins <- unique(long[, .(age_bin, min_numobs)])
  wide <- dcast(long, age_bin ~ size_bin, value.var = "value")
  fwrite(merge(wide, mins, by = "age_bin"), f)
}

# --- 2. desc_slopes.csv & main_results.csv  (long: one row per bin × model) -
# N = firm-years per bin level in the full dataset (conservative; actual N may
# be slightly lower if x_var has NAs, but negligible for disclosure purposes).
for (fname in c("desc_slopes.csv", "main_results.csv")) {
  path <- file.path(results_csv_dir, fname)
  if (!file.exists(path)) next
  dt <- fread(path)
  dt <- merge(dt, n_bins, by = c("bin", "bin_var"), all.x = TRUE)

  # group_cols = all id columns; min_numobs = min N across bins in same "table"
  grp <- setdiff(names(dt), c("bin", "beta", "se", "ci_low", "ci_high", "N"))
  dt[, min_numobs := min(N, na.rm = TRUE), by = grp]
  dt[N < MIN_OBS, c("beta", "se", "ci_low", "ci_high") := NA]
  dt[, N := NULL]
  fwrite(dt, path)
}

# --- 3. burst_size.csv & burst_age.csv  (long: marginal effects by bin) ------
burst_horizons <- c(0, 2, 3, 5)

for (cfg in list(list(f = "burst_size.csv", bv = "size_bin"),
                 list(f = "burst_age.csv",  bv = "age_bin"))) {
  path <- file.path(results_csv_dir, cfg$f)
  if (!file.exists(path)) next
  bv <- cfg$bv

  # N per bin level × horizon (future_burst_cat is NA for some firm-years)
  burst_n <- rbindlist(lapply(burst_horizons, \(h) {
    var <- paste0("future_burst_cat_h", h)
    if (!var %in% names(pp)) return(NULL)
    pp[!is.na(get(var)), .N, by = bv][, horizon := h]
  }))

  if (!nrow(burst_n)) { message("Skipping ", cfg$f, ": no future_burst_cat variables found"); next }
  dt  <- fread(path)
  dt  <- merge(dt, burst_n, by = c(bv, "horizon"), all.x = TRUE)
  val <- intersect(c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), names(dt))
  dt[, min_numobs := min(N, na.rm = TRUE), by = c("innov_var", "contrast", "horizon")]
  dt[N < MIN_OBS, (val) := lapply(.SD, \(x) NA_real_), .SDcols = val]
  dt[, N := NULL]
  fwrite(dt, path)
}
