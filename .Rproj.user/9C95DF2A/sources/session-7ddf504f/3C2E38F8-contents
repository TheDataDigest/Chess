# streak function ----

longest_sequence <- function(a) {
  max_length <- 0
  current_length <- 0
  
  for (i in seq_along(a)) {
    if (a[i]) {
      current_length <- current_length + 1
      if (current_length > max_length) {
        max_length <- current_length
      }
    } else {
      current_length <- 0
    }
  }
  return(c(length = max_length))
}  
