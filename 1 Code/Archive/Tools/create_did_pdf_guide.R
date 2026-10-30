# Build a PDF guide for did_imputation and event_plot without external dependencies.
out_file <- "Tools/did_imputation_event_plot_guide.pdf"

wrap_text <- function(text, width = 95) {
  if (length(text) == 0) return(character(0))
  unlist(strwrap(text, width = width, simplify = FALSE))
}

add_line <- function(lines, text = "", gap_after = 0, width = 95) {
  w <- if (nzchar(text)) wrap_text(text, width = width) else ""
  c(lines, w, rep("", gap_after))
}

lines <- character(0)

lines <- add_line(lines, "did_imputation and event_plot: User Guide", gap_after = 1)
lines <- add_line(lines, "This document explains how to use the two R commands implemented in Tools/did_imputation.R.", gap_after = 2)

lines <- add_line(lines, "1. Setup", gap_after = 1)
lines <- add_line(lines, "source('Tools/did_imputation.R')")
lines <- add_line(lines, "library(data.table)")
lines <- add_line(lines, "library(fixest)")
lines <- add_line(lines, "library(ggplot2)", gap_after = 2)

lines <- add_line(lines, "2. did_imputation()", gap_after = 1)
lines <- add_line(lines, "Purpose: Estimate treatment effects in staggered adoption DiD using imputation (Borusyak, Jaravel, Spiess style workflow).", gap_after = 1)
lines <- add_line(lines, "Core call:")
lines <- add_line(lines, "fit <- did_imputation(data = df, y = 'outcome', i = 'unit_id', t = 'year', ei = 'treat_year')", gap_after = 1)
lines <- add_line(lines, "Required arguments:")
lines <- add_line(lines, "- data: data.frame/data.table")
lines <- add_line(lines, "- y: outcome variable name")
lines <- add_line(lines, "- i: unit id variable name")
lines <- add_line(lines, "- t: time variable name")
lines <- add_line(lines, "- ei: treatment timing variable name (NA for never-treated)", gap_after = 1)

lines <- add_line(lines, "Important options:")
lines <- add_line(lines, "- horizons = c(0,1,2,...): estimate tau by horizon")
lines <- add_line(lines, "- all_horizons = TRUE: use all non-negative horizons present in data")
lines <- add_line(lines, "- hbalance = TRUE: enforce balanced horizon sample")
lines <- add_line(lines, "- pretrends = K: add K lead coefficients pre1...preK and run joint pretrend test")
lines <- add_line(lines, "- controls = c('x1','x2'): time-varying controls")
lines <- add_line(lines, "- fe = c('unit','year','state#year'): fixed effects (use # for interactions)")
lines <- add_line(lines, "- cluster = 'cluster_id': cluster-robust SE target")
lines <- add_line(lines, "- autosample = TRUE: auto-drop treated observations that cannot be imputed")
lines <- add_line(lines, "- shift = 0: anticipation shift applied in relative-time K construction")
lines <- add_line(lines, "- weights = 'w' or numeric vector: estimation weights")
lines <- add_line(lines, "- wtr = NULL or c('w1','w2'): custom treated-observation weights")
lines <- add_line(lines, "- sum = TRUE: interpret weighted estimand as sum instead of average")
lines <- add_line(lines, "- hetby = 'group': split treatment effects by subgroup")
lines <- add_line(lines, "- project = c('z1','z2'): project effects on covariates")
lines <- add_line(lines, "- minn = 30: minimum effective sample size threshold", gap_after = 1)

lines <- add_line(lines, "Example: dynamic effects with pretrends")
lines <- add_line(lines, "fit <- did_imputation(")
lines <- add_line(lines, "  data = df,")
lines <- add_line(lines, "  y = 'y', i = 'id', t = 'year', ei = 'first_treat_year',")
lines <- add_line(lines, "  horizons = 0:8,")
lines <- add_line(lines, "  pretrends = 5,")
lines <- add_line(lines, "  controls = c('x1','x2'),")
lines <- add_line(lines, "  cluster = 'id',")
lines <- add_line(lines, "  autosample = TRUE")
lines <- add_line(lines, ")", gap_after = 1)

lines <- add_line(lines, "Outputs (fit object):")
lines <- add_line(lines, "- fit$coefficients: named vector (tau*, pre*, controls)")
lines <- add_line(lines, "- fit$vcov: variance-covariance matrix")
lines <- add_line(lines, "- fit$table: estimate/SE/CI table")
lines <- add_line(lines, "- fit$Nt: treated observation counts by effect")
lines <- add_line(lines, "- fit$Nc: number of control observations")
lines <- add_line(lines, "- fit$pre_F, fit$pre_p, fit$pre_df: joint pretrend test")
lines <- add_line(lines, "- fit$cannot_impute: logical flag per row")
lines <- add_line(lines, "- fit$sample: estimation sample flag per row", gap_after = 2)

lines <- add_line(lines, "3. event_plot()", gap_after = 1)
lines <- add_line(lines, "Purpose: Plot leads/lags with confidence intervals from did_imputation and other supported models.", gap_after = 1)
lines <- add_line(lines, "Core call after did_imputation:")
lines <- add_line(lines, "pobj <- event_plot(fit)")
lines <- add_line(lines, "print(pobj$plot)", gap_after = 1)

lines <- add_line(lines, "Model inputs accepted:")
lines <- add_line(lines, "- did_imputation object")
lines <- add_line(lines, "- fixest or lm model")
lines <- add_line(lines, "- list(b = named_coef_vector, V = vcov_matrix_optional)", gap_after = 1)

lines <- add_line(lines, "How coefficients are found:")
lines <- add_line(lines, "- Uses stub_lag and stub_lead patterns, where # marks the numeric horizon")
lines <- add_line(lines, "- did_imputation default stubs: stub_lag = 'tau#', stub_lead = 'pre#'", gap_after = 1)

lines <- add_line(lines, "Important options:")
lines <- add_line(lines, "- stub_lag, stub_lead: coefficient-name patterns")
lines <- add_line(lines, "- trimlag, trimlead: max lag and lead depth to display")
lines <- add_line(lines, "- plottype: 'connected', 'line', or 'scatter'")
lines <- add_line(lines, "- ciplottype: 'auto', 'rarea', 'rcap', or 'none'")
lines <- add_line(lines, "- together = TRUE: draw leads/lags as one series per model")
lines <- add_line(lines, "- shift: integer horizontal shift in event time")
lines <- add_line(lines, "- perturb: offsets multiple models to avoid overlap")
lines <- add_line(lines, "- savecoef = TRUE: writes __event_* vectors into caller environment")
lines <- add_line(lines, "- noplot = TRUE: return plot data without drawing", gap_after = 1)

lines <- add_line(lines, "Examples:")
lines <- add_line(lines, "# Single model")
lines <- add_line(lines, "p1 <- event_plot(fit, default_look = TRUE)")
lines <- add_line(lines, "print(p1$plot)", gap_after = 1)

lines <- add_line(lines, "# Compare two estimators")
lines <- add_line(lines, "p2 <- event_plot(")
lines <- add_line(lines, "  fit_bjs, fit_ols,")
lines <- add_line(lines, "  stub_lag = c('tau#', 'L#event'),")
lines <- add_line(lines, "  stub_lead = c('pre#', 'F#event'),")
lines <- add_line(lines, "  plottype = 'scatter',")
lines <- add_line(lines, "  together = TRUE")
lines <- add_line(lines, ")")
lines <- add_line(lines, "print(p2$plot)", gap_after = 2)

lines <- add_line(lines, "4. Practical notes", gap_after = 1)
lines <- add_line(lines, "- ei must be treatment timing, not treatment dummy.")
lines <- add_line(lines, "- Never-treated units should have ei = NA.")
lines <- add_line(lines, "- If imputation fails for relevant treated observations, use autosample=TRUE or adjust sample/weights.")
lines <- add_line(lines, "- For fixest/lm in event_plot, always provide stubs unless coefficient names exactly match defaults.")
lines <- add_line(lines, "- In this implementation, treatment-effect SEs use a clustered influence-style approximation; results may differ from Stata's iterative SE routine in edge cases.", gap_after = 1)


`%+%` <- function(a, b) paste0(a, b)

# Regenerate final line now that operator exists
lines[length(lines)] <- paste0("Generated on: ", as.character(Sys.time()))

# Render to PDF
# Use text plot pages so no LaTeX/Pandoc dependencies are required.
grDevices::pdf(out_file, width = 8.5, height = 11)
on.exit(grDevices::dev.off(), add = TRUE)

par(mar = c(0.8, 0.8, 0.8, 0.8))
line_h <- 0.028
usable_top <- 0.97
usable_bottom <- 0.04
lines_per_page <- floor((usable_top - usable_bottom) / line_h)

idx <- 1
while (idx <= length(lines)) {
  plot.new()
  page_lines <- lines[idx:min(idx + lines_per_page - 1, length(lines))]
  y <- usable_top
  for (txt in page_lines) {
    if (grepl("^[0-9]+\\.", txt)) {
      text(0.03, y, labels = txt, adj = c(0, 1), cex = 1.0, font = 2, xpd = NA)
    } else if (grepl("^did_imputation and event_plot", txt)) {
      text(0.03, y, labels = txt, adj = c(0, 1), cex = 1.25, font = 2, xpd = NA)
    } else if (grepl("^Core call:|^Required arguments:|^Important options:|^Example:|^Outputs|^Model inputs accepted:|^How coefficients are found:|^Examples:|^Purpose:", txt)) {
      text(0.03, y, labels = txt, adj = c(0, 1), cex = 0.95, font = 2, xpd = NA)
    } else {
      text(0.03, y, labels = txt, adj = c(0, 1), cex = 0.9, family = "mono", xpd = NA)
    }
    y <- y - line_h
  }
  idx <- idx + lines_per_page
}

dev.off()
cat(normalizePath(out_file), "\n")
