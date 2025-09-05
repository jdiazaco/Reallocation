create_latex_from_folder <- function(folder_path, relative_root, output_tex = "Main.R") {
  # Get list of files in the folder
  files <- list.files(folder_path, full.names = FALSE)
  
  # Filter by file types you want to include
  # You can modify this depending on your LaTeX needs
  tex_files <- files[grepl("\\.tex$", files)]
  pdf_files <- files[grepl("\\.pdf$", files)]
  img_files <- files[grepl("\\.(png|jpg|jpeg|bmp)$", files, ignore.case = TRUE)]
  
  # Start building the LaTeX document
  latex_content <- c(
    "\\documentclass{article}",
    "\\usepackage{graphicx}",
    "\\usepackage{pdfpages}",
    "\\usepackage[margin=1in]{geometry}",
    "\\begin{document}"
  )
  
  # Include .tex files using \input{}
  if (length(tex_files) > 0) {
    tex_files<-tex_files[grepl("MU_lab_cs", tex_files)] # Exclude files that don't have to do with labor markups,
    for (f in tex_files) {
      latex_content <- c(latex_content, paste0("\\input{", paste0(relative_root, f), "}"))
    }
  }
  
  # Include .pdf files using \includepdf[]
  if (length(pdf_files) > 0) {
    for (f in pdf_files) {
      latex_content <- c(latex_content, paste0("\\includepdf[pages=-]{", paste0(relative_root, f), "}"))
    }
  }
  
  # Include image files using \includegraphics[]
  if (length(img_files) > 0) {
    for (f in img_files) {
      latex_content <- c(
        latex_content,
        "\\begin{figure}[ht]",
        paste0("  \\centering\\includegraphics[width=\\linewidth]{", paste0(relative_root, f), "}"),
        paste0("  \\caption{", paste0(relative_root, f), "}"),
        "\\end{figure}"
      )
    }
  }
  
  # End document
  latex_content <- c(latex_content, "\\end{document}")
  
  # Write to .tex file
  cat(latex_content, sep="\n")
  writeLines(latex_content, file.path(folder_path, output_tex))
  message("LaTeX file created at: ", file.path(folder_path, output_tex))
}

folder_path<-"C:/Users/nb/Dropbox/Reallocation - shared folder/Exports/Zip files/04.09/markups/all_firms/all/"
relative_root<-"markups/all_firms/all/"
output_tex = "Main.R"
