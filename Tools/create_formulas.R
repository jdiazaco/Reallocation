create_formulas <- function(ys, xs, controls, fe) {
  #' Create Model Formulas and Descriptions
  #'
  #' Generates all combinations of dependent variables (`ys`), independent variables (`xs`), control variables (`controls`), and fixed effects (`fe`),
  #' and constructs corresponding formula strings and descriptive labels for each combination.
  #'
  #' @param ys A character vector of dependent variable names.
  #' @param xs A character vector of independent variable names.
  #' @param controls A character vector of control variable names.
  #' @param fe A character vector of fixed effect specifications.
  #'
  #' @return A `data.table` containing all combinations with columns:
  #'   \itemize{
  #'     \item \code{y}: Dependent variable name.
  #'     \item \code{x}: Independent variable name.
  #'     \item \code{control}: Control variable name.
  #'     \item \code{fixed_effect}: Fixed effect specification.
  #'     \item \code{weight}: Weight variable based on dependent variable.
  #'     \item \code{formula}: Model formula as a string.
  #'     \item \code{description}: Unique description for each combination.
  #'   }
  #'
  #' @examples
  #' ys <- c("y1", "y2")
  #' xs <- c("x1", "x2")
  #' controls <- c("ctrl1", "ctrl2")
  #' fe <- c("fe1", "fe2")
  #' create_formulas(ys, xs, controls, fe)
  #'
  #' @import data.table
  #' @export

  # Create all combinations of ys, xs, controls, and fe
  combinations <- expand.grid(
    y = ys,
    x = xs,
    control = controls,
    fixed_effect = fe,
    stringsAsFactors = FALSE
  )

  # Add the weight column
  combinations$weight <- sapply(combinations$y, function(y) {
    if (grepl("growth", y)) {
      sub("growth", "bar", y)
    } else {
      "nq_bar"
    }
  })

  # Add the formula column
    combinations$formula <- apply(combinations, 1, function(row) {
        ifelse(is.na(row["fixed_effect"]) | row["fixed_effect"] == "",
            paste0(row["y"], " ~ ", row["x"], " + ", row["control"]),
            paste0(
                row["y"], " ~ ", row["x"], " + ", row["control"],
                " | ", row["fixed_effect"]
            )
        )

    })

  
  # Add the description column
  combinations$description <- apply(combinations, 1, function(row) {
    paste0(
      row["y"], "_",
      "x", "_", which(xs == row["x"]), "_",
      "ctrl", "_", which(controls == row["control"]), 
      ifelse(is.na(row["fixed_effect"]) | row["fixed_effect"]=="", "_nofe", "_fe_"),
      gsub("[^A-Za-z0-9_]", "", row["fixed_effect"])
    )
  })

  # Convert to data.table for easier manipulation
  model_table <- data.table::as.data.table(combinations)

  return(model_table)
}
