create_latex_table <- function(data,
                                          output_dir,
                                          var_name,
                                          include_preamble = TRUE,
                                          caption,
                                          digits = 4,
                                          filename,
                                          escape_latex = TRUE,
                                          color_cells = FALSE,
                                          percent = FALSE) {
  if (missing(data)) stop("`data` is required")
  if (missing(output_dir)) stop("`output_dir` is required")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (color_cells) {
    # Helper to manually escape LaTeX special characters in non-numeric columns.
    # This is needed because cell_spec() generates raw LaTeX, forcing escape=FALSE
    # globally, so we must protect text columns ourselves.
    escape_latex_str <- function(x) {
      x <- gsub("\\\\", "\\\\textbackslash{}", x)       # \ first
      x <- gsub("([&%$#_{}])", "\\\\\\1", x)            # & % $ # _ { }
      x <- gsub("~",  "\\\\textasciitilde{}",  x, fixed = TRUE)
      x <- gsub("\\^", "\\\\textasciicircum{}", x)
      x
    }

    # Coerce to plain data frame so column subsetting works regardless of
    # whether the caller passed a data.table (where dt[, var] has different semantics).
    data <- as.data.frame(data)

    num_cols <- sapply(data, is.numeric)
    num_vals <- unlist(data[, num_cols, drop = FALSE])
    val_min  <- min(num_vals, na.rm = TRUE)
    val_max  <- max(num_vals, na.rm = TRUE)
    rng      <- val_max - val_min

    palette  <- colorRampPalette(c("#FF4444", "#FFDD00", "#44BB44"))(100)

    # Escape non-numeric columns and column names manually (only if the caller
    # wanted escaping), since we must set escape=FALSE globally for cell_spec.
    if (escape_latex) {
      data <- data %>%
        mutate(across(!where(is.numeric), ~ escape_latex_str(as.character(.))))
      colnames(data) <- escape_latex_str(colnames(data))
    }

    data <- data %>%
      mutate(across(where(is.numeric), ~ {
        is_na      <- is.na(.)
        normalized <- if (rng == 0) rep(0.5, length(.)) else (. - val_min) / rng
        normalized[is_na] <- 0
        idx <- pmax(1L, pmin(100L, as.integer(round(normalized * 99)) + 1L))
        bg  <- ifelse(is_na, "#FFFFFF", palette[idx])
        display <- if (percent) {
          ifelse(is_na, "NA", paste0(formatC(. * 100, digits = digits, format = "f"), "%"))
        } else {
          as.character(round(., digits))
        }
        cell_spec(
          display,
          format     = "latex",
          background = bg
        )
      }))

    escape_latex <- FALSE
    digits       <- NULL
  }

  if (percent && !color_cells) {
    data <- as.data.frame(data) %>%
      mutate(across(where(is.numeric), ~ . * 100))
  }

  xcolor_pkg <- if (color_cells) "\\usepackage[table]{xcolor}\n" else ""
  preamble <- paste0(
    "\\documentclass{article}\n\\usepackage{booktabs}\n\\usepackage{float}\n",
    "\\usepackage{graphicx}\n\\usepackage[margin=1in]{geometry}\n\\usepackage{caption}\n",
    xcolor_pkg,
    "\n\\begin{document}\n\n"
  )
  ending <- "\n\\end{document}\n"

  conv_table <- data %>%
      kable(
        format = "latex",
        booktabs = TRUE,
        escape = escape_latex,
        caption = caption,
        digits = digits
      ) %>%
      kable_styling(latex_options = c("hold_position", "scale_down"))

  latex_document <- if (include_preamble) {
    paste0(preamble, conv_table, ending)
  } else {
    paste0(conv_table)
  }

  tex_file <- file.path(
    output_dir,
    paste0(filename, ".tex")
  )

  writeLines(latex_document, tex_file)

  invisible(list(table = conv_table, path = tex_file))
}
