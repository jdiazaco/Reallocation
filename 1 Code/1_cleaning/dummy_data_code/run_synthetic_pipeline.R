# =============================================================================
# Run the pipeline on the synthetic inputs, outside RStudio
# =============================================================================
# Each script runs in its own R session, with REALLOCATION_SYNTHETIC = TRUE and
# rstudioapi::getActiveDocumentContext() pointing at the script, so the usual
# `source(.../Main.R)` header works unchanged.
#
# Usage (from any folder):
#   Rscript run_synthetic_pipeline.R                                   # cleaning 1 -> 9 (3 is skipped:
#                                                                      # the patent file is an input; see 8 below)
#   Rscript run_synthetic_pipeline.R 2_analysis/11_patents_predict_bursts.R
# Env vars: REALLOCATION_SYNTHETIC_DIR (data root, as in Main.R) and
# REALLOCATION_MAIN (Main.R to use if the scripts' "1 Code/Main.R" is missing;
# defaults to <data root>/harness/Main.R when present).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
this_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)), winslash = "/")
code_root <- dirname(dirname(dirname(this_file)))  # .../1 Code
rscript <- file.path(R.home("bin"), "Rscript")
syn_dir <- Sys.getenv("REALLOCATION_SYNTHETIC_DIR", "C:/Users/nb/Documents/Reallocation_synthetic/")
# harness/Main.R: copy of Tools/Main.R whose tools_dir merges Tools and Archive/Tools
harness_main <- file.path(syn_dir, "harness/Main.R")
main_fallback <- Sys.getenv("REALLOCATION_MAIN", if (file.exists(harness_main)) harness_main else file.path(code_root, "Tools/Main.R"))

# 2bc uses the firm-level patent panel from 4; 8 is skipped (reads the old
# 6_final_firm_lvl_dta.rds and writes nothing)
cleaning <- c("1_make_firm_yr_lvl_br_dta.R", "1a_make_firm_lvl_br_dta.R", "2a_make_product_yr_lvl_dta.R",
              "4_make_firm_lvl_patent_dta.R", "2bc_make_firm_lvl_product_dta.R", "5_make_industry_lvl_dta.R",
              "6_make_firm_ent_lvl_dta.R", "7_make_final_firm_lvl_dta.R", "9_make_product_market_lvl_dta.R")
scripts <- if (length(args)) file.path(code_root, args) else file.path(code_root, "1_cleaning", cleaning)

log_dir <- file.path(syn_dir, "logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

for (s in normalizePath(scripts, winslash = "/", mustWork = TRUE)) {
  wrapper <- tempfile(fileext = ".R")
  writeLines(c(
    'Sys.setenv(REALLOCATION_SYNTHETIC = "TRUE")',
    # debugging View() calls become no-ops
    'local({e <- as.environment("package:utils"); unlockBinding("View", e); assign("View", function(...) invisible(NULL), e)})',
    sprintf(paste0('utils::assignInNamespace("getActiveDocumentContext", local({p <- "%s"; ',
                   'function() list(id = "", path = p, contents = "", selection = NULL)}), "rstudioapi")'), s),
    sprintf('source <- function(file, ...) { if (basename(file) == "Main.R" && !file.exists(file)) file <- "%s"; base::source(file, ...) }',
            main_fallback),
    sprintf('base::source("%s")', s)
  ), wrapper)

  log_file <- file.path(log_dir, sub("\\.R$", ".log", basename(s), ignore.case = TRUE))
  cat(format(Sys.time(), "%H:%M:%S"), "running", basename(s), "\n")
  t0 <- Sys.time()
  status <- system2(rscript, shQuote(wrapper), stdout = log_file, stderr = log_file)
  cat(sprintf("   %s after %.1f min (log: %s)\n", if (status == 0) "done" else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "mins")), log_file))
  if (status != 0) {
    cat(tail(readLines(log_file, warn = FALSE), 25), sep = "\n")
    stop("Pipeline stopped at ", basename(s))
  }
}
