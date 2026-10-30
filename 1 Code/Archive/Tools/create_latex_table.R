create_latex_table <- function(data,
                                          output_dir,
                                          var_name,
                                          include_preamble = TRUE,
                                          caption,
                                          digits = 4,
                                          filename,
                                          escape_latex = TRUE,
                                          color_cells = FALSE,
                                          percent = FALSE,
                                          add_summary_row = FALSE,
                                          summary_row_fun = c("mean", "sum"),
                                          summary_row_label = NULL,
                                          add_summary_col = FALSE,
                                          summary_col_fun = c("mean", "sum"),
                                          summary_col_label = NULL,
                                          top_label = NULL,
                                          left_label = NULL) {
  if (missing(data)) stop("`data` is required")
  if (missing(output_dir)) stop("`output_dir` is required")

  summary_row_fun <- match.arg(summary_row_fun)
  summary_col_fun <- match.arg(summary_col_fun)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  escape_latex_str <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([&%$#_{}])", "\\\\\\1", x)
    x <- gsub("~",  "\\\\textasciitilde{}",  x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x
  }

  format_numeric <- function(x) {
    ifelse(
      is.na(x),
      "NA",
      if (percent) {
        paste0(formatC(x * 100, digits = digits, format = "f"), "%")
      } else {
        formatC(x, digits = digits, format = "f")
      }
    )
  }

  escape_latex_value <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("%", "\\%", x, fixed = TRUE)
    x
  }

  latex_cell <- function(value, background = NULL, already_escaped = FALSE) {
    rendered_value <- if (already_escaped) escape_latex_value(value) else if (escape_latex) escape_latex_str(value) else value
    if (!is.null(background) && !is.na(background)) {
      paste0("\\cellcolor[HTML]{", sub("^#", "", background), "}", rendered_value)
    } else {
      rendered_value
    }
  }

  apply_summary <- function(x, fun_name) {
    if (!length(x) || all(is.na(x))) {
      return(NA_real_)
    }
    switch(
      fun_name,
      mean = mean(x, na.rm = TRUE),
      sum = sum(x, na.rm = TRUE)
    )
  }

  colorize_numeric <- function(x, palette_colors) {
    if (!length(x)) {
      return(character())
    }
    palette <- colorRampPalette(palette_colors)(100)
    out <- rep(NA_character_, length(x))
    valid <- !is.na(x)
    if (!any(valid)) {
      return(rep("#FFFFFF", length(x)))
    }
    val_min <- min(x[valid])
    val_max <- max(x[valid])
    rng <- val_max - val_min
    normalized <- rep(0, length(x))
    normalized[valid] <- if (rng == 0) 0.5 else (x[valid] - val_min) / rng
    idx <- pmax(1L, pmin(100L, as.integer(round(normalized * 99)) + 1L))
    out[valid] <- palette[idx[valid]]
    out[!valid] <- "#FFFFFF"
    out
  }

  data <- as.data.frame(data)
  original_names <- colnames(data)
  if (is.null(original_names)) {
    original_names <- paste0("V", seq_len(ncol(data)))
    colnames(data) <- original_names
  }

  first_data_col <- if (ncol(data) >= 1) 1L else 0L
  row_summary_candidates <- if (ncol(data) >= 2L) seq.int(2L, ncol(data)) else integer()
  row_summary_candidates <- row_summary_candidates[sapply(data[row_summary_candidates], is.numeric)]
  col_summary_candidates <- seq_len(ncol(data))[sapply(data, is.numeric)]

  summary_col_values <- NULL
  if (add_summary_col) {
    summary_col_values <- if (length(row_summary_candidates)) {
      apply(data[row_summary_candidates], 1, apply_summary, fun_name = summary_col_fun)
    } else {
      rep(NA_real_, nrow(data))
    }
  }

  summary_row_values <- NULL
  if (add_summary_row) {
    summary_row_values <- rep(NA_real_, ncol(data))
    for (j in col_summary_candidates) {
      summary_row_values[j] <- apply_summary(data[[j]], summary_row_fun)
    }
  }

  body_num_cols <- sapply(data, is.numeric)
  body_colors <- list()
  if (color_cells) {
    body_palette <- c("#FF4444", "#FFDD00", "#44BB44")
    for (j in which(body_num_cols)) {
      body_colors[[j]] <- colorize_numeric(data[[j]], body_palette)
    }
  }

  summary_col_colors <- NULL
  summary_row_colors <- NULL
  if (color_cells && add_summary_col) {
    summary_col_colors <- colorize_numeric(summary_col_values, c("#FFFFFF", "#1F78B4"))
  }
  if (color_cells && add_summary_row) {
    summary_row_colors <- rep(NA_character_, ncol(data))
    if (length(col_summary_candidates)) {
      summary_row_colors[col_summary_candidates] <- colorize_numeric(
        summary_row_values[col_summary_candidates],
        c("#FFFFFF", "#1F78B4")
      )
    }
  }

  display_names <- if (escape_latex) escape_latex_str(original_names) else original_names
  if (add_summary_col) {
    summary_col_label <- summary_col_label %||% tools::toTitleCase(summary_col_fun)
  }
  if (add_summary_row) {
    summary_row_label <- summary_row_label %||% tools::toTitleCase(summary_row_fun)
  }

  header_cells <- display_names
  if (add_summary_col) {
    header_cells <- c(header_cells, latex_cell(summary_col_label))
  }

  table_rows <- vector("list", nrow(data) + if (add_summary_row) 1L else 0L)
  n_body_rows <- nrow(data)

  for (i in seq_len(nrow(data))) {
    row_cells <- character()

    if (!is.null(left_label)) {
      if (n_body_rows <= 1L && i == 1L) {
        row_cells <- c(row_cells, paste0("\\rotatebox[origin=c]{90}{", latex_cell(left_label), "}"))
      } else if (i == 1L) {
        row_cells <- c(row_cells, "")
      } else if (i == 2L) {
        row_cells <- c(
          row_cells,
          paste0(
            "\\multirow{", n_body_rows - 1L, "}{*}{\\rotatebox[origin=c]{90}{",
            latex_cell(left_label),
            "}}"
          )
        )
      } else {
        row_cells <- c(row_cells, "")
      }
    }

    for (j in seq_len(ncol(data))) {
      if (is.numeric(data[[j]])) {
        display <- format_numeric(data[[j]][i])
        bg <- if (color_cells) body_colors[[j]][i] else NULL
        row_cells <- c(row_cells, latex_cell(display, background = bg, already_escaped = TRUE))
      } else {
        value <- as.character(data[[j]][i])
        row_cells <- c(row_cells, latex_cell(value))
      }
    }

    if (add_summary_col) {
      display <- format_numeric(summary_col_values[i])
      bg <- if (color_cells) summary_col_colors[i] else NULL
      row_cells <- c(row_cells, latex_cell(display, background = bg, already_escaped = TRUE))
    }

    table_rows[[i]] <- row_cells
  }

  if (add_summary_row) {
    row_cells <- character()
    if (!is.null(left_label)) {
      row_cells <- c(row_cells, "")
    }

    for (j in seq_len(ncol(data))) {
      if (j == first_data_col) {
        row_cells <- c(row_cells, latex_cell(summary_row_label))
      } else if (is.numeric(data[[j]])) {
        display <- format_numeric(summary_row_values[j])
        bg <- if (color_cells) summary_row_colors[j] else NULL
        row_cells <- c(row_cells, latex_cell(display, background = bg, already_escaped = TRUE))
      } else {
        row_cells <- c(row_cells, "")
      }
    }

    if (add_summary_col) {
      row_cells <- c(row_cells, "")
    }

    table_rows[[length(table_rows)]] <- row_cells
  }

  align_parts <- character()
  if (!is.null(left_label)) {
    align_parts <- c(align_parts, "c")
  }
  for (j in seq_len(ncol(data))) {
    align_parts <- c(align_parts, if (j == 1L || !is.numeric(data[[j]])) "l" else "r")
  }
  if (add_summary_col) {
    align_parts <- c(align_parts, "|r")
  }
  align <- paste0(align_parts, collapse = "")

  top_header <- NULL
  if (!is.null(top_label) && length(header_cells) >= 2L) {
    prefix_blanks <- if (!is.null(left_label)) 2L else 1L
    span_size <- length(header_cells) - 1L
    top_cells <- c(rep("", prefix_blanks), paste0("\\multicolumn{", span_size, "}{c}{", latex_cell(top_label), "}"))
    top_header <- paste(top_cells, collapse = " & ")
  }

  header_prefix <- if (!is.null(left_label)) "" else NULL
  header_line <- paste(c(header_prefix, header_cells), collapse = " & ")
  row_lines <- vapply(table_rows, function(x) paste(x, collapse = " & "), character(1))

  tabular_lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    if (!missing(caption) && !is.null(caption)) paste0("\\caption{", if (escape_latex) escape_latex_str(caption) else caption, "}") else NULL,
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule"
  )

  if (!is.null(top_header)) {
    tabular_lines <- c(tabular_lines, paste0(top_header, " \\\\"), "\\midrule")
  }

  tabular_lines <- c(tabular_lines, paste0(header_line, " \\\\"), "\\midrule")

  if (length(row_lines)) {
    if (add_summary_row && length(row_lines) > 1L) {
      tabular_lines <- c(tabular_lines, paste0(row_lines[-length(row_lines)], " \\\\"))
      tabular_lines <- c(tabular_lines, "\\midrule", paste0(row_lines[length(row_lines)], " \\\\"))
    } else {
      tabular_lines <- c(tabular_lines, paste0(row_lines, " \\\\"))
    }
  }

  tabular_lines <- c(
    tabular_lines,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{table}"
  )

  conv_table <- paste(tabular_lines, collapse = "\n")

  xcolor_pkg <- if (color_cells) "\\usepackage[table]{xcolor}\n" else ""
  multirow_pkg <- if (!is.null(left_label)) "\\usepackage{multirow}\n" else ""
  preamble <- paste0(
    "\\documentclass{article}\n\\usepackage{booktabs}\n\\usepackage{float}\n",
    "\\usepackage{graphicx}\n\\usepackage[margin=1in]{geometry}\n\\usepackage{caption}\n",
    multirow_pkg,
    xcolor_pkg,
    "\n\\begin{document}\n\n"
  )
  ending <- "\n\\end{document}\n"

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
