create_latex_table <- function(data,
                                           output_dir,
                                           var_name,
                                           include_preamble = TRUE,
                                           caption, 
                                           digits = 4,
                                           filename,
                                           escape_latex = TRUE) {
  if (missing(data)) stop("`data` is required")
  if (missing(output_dir)) stop("`output_dir` is required")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  preamble <- "\\documentclass{article}\n\\usepackage{booktabs}\n\\usepackage{float}\n\\usepackage{graphicx}\n\\usepackage[margin=1in]{geometry}\n\\usepackage{caption}\n\n\\begin{document}\n\n"
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
