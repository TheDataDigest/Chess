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



# Function to track sequences of TRUEs
winning_streak_distribution <- function(logical_vector) {
  # Initialize result vector
  result <- c()
  
  # Initialize a counter for the current streak
  current_streak <- 0
  
  # Iterate through the logical vector
  for (value in logical_vector) {
    if (value) {
      # If the value is TRUE, increment the streak
      current_streak <- current_streak + 1
    } else {
      # If the value is FALSE, check if there was a streak
      if (current_streak > 0) {
        # Append the streak length to the result
        result <- c(result, current_streak)
        # Reset the streak counter
        current_streak <- 0
      }
    }
  }
  
  # If the vector ends with a streak, add it to the result
  if (current_streak > 0) {
    result <- c(result, current_streak)
  }
  
  return(result)
}

# Example usage
# a <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
# result <- winning_streak_distribution(a)
# print(result)
