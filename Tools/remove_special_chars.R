remove_special_chars <- function(x) {
  # Function to remove special characters from a string
  gsub("[^a-zA-Z0-9_]", "_", x)
}