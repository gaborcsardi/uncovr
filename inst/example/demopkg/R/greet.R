greet <- function(name, polite = FALSE) {
  if (polite) {
    paste0("Good day, ", name, ".")
  } else {
    paste0("Hi ", name, "!")
  }
}
