gen_latex <- function(path, types, captions=NULL, text_width=0.8, file_conn) {
  if (length(path) != length(types)) {
    stop("path and types vectors must have the same length.")
  }
  if (!is.null(captions) & length(path) != length(captions)) {
    stop("path and caption vectors must have the same length.")
  }
  
  
  # Function to generate code for a single image or table
  single_entry <- function(path, type, caption) {
    if (type == "image") {
      return(paste0("\\begin{figure}[H]\n",
                    "\\centering\n",
                    "\\includegraphics[width=", text_width,"\\textwidth]{", path, "}\n",
                    "\\caption{", caption, "}\n",
                    "\\label{fig:", gsub("/", "_", path), "}\n",
                    "\\end{figure}\n"))
    } else if (type == "table") {
      return(paste0("\\input{", path, "}\n"))
    } else {
      stop("Type must be 'image' or 'table'.")
    }
  }
  
  # Write code to file_conn based on the number of path
  if (length(path) == 1) {
    # If there's only one path, generate code for a single image or table
    write(single_entry(path[1], types[1], caption = captions[1]), file_conn, append=T)
    
  } else {
    # If there are multiple path, handle images side by side
    for (i in seq_along(path)) {
      
      # path<- path[i]
      if (types[i] == "image") {
        # Side-by-side images
        if (i == 1) {
          write("\\begin{figure}[H]\n\\centering\n", file_conn, append=T)
        }
        write(paste0("\\begin{subfigure}{0.45\\textwidth}\n",
                     "\\centering\n",
                     "\\includegraphics[width=\\linewidth]{", path[i], "}\\hspace{0.5cm}\n",
                     "\\caption{", captions[i], "}\n",
                     "\\label{fig:", gsub("/", "_", path[i]) , "}\n",
                     "\\end{subfigure}\\hfill"), file_conn, append=T)
        if (i == length(path)) {
          write("\\caption{}\n\\end{figure}\n", file_conn, append=T)
        }
      } else if (types[i] == "table") {
        # Write code for a standalone table
        write(single_entry(path[i], types[i], caption = captions[i]), file_conn, append=T)
      } else {
        stop("Type must be 'image' or 'table'.")
      }
    }
  }
  # close(get(file_conn))
}
