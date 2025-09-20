remove_special_chars <- function(x, character="_") {
  # Function to remove special characters from a string
  gsub("[^a-zA-Z0-9_]", character , x)
}