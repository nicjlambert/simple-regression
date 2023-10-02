# Load Required Packages
packages <- c("dplyr", "markovchain")
new_packages <- setdiff(packages, rownames(installed.packages()))
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

sapply(packages, require, character.only = TRUE)

# Simulated Weather Data
set.seed(123)
weather <- c("sunny", "sunny", "rain", "cloudy")
weather_data <- sample(weather, 100, replace = TRUE)

# Function to Calculate Transition Probabilities
calculate_transitions <- function(data, states) {
  trans_matrix <- matrix(0, nrow = length(states), ncol = length(states), dimnames = list(states, states))
  
  for (i in seq_len(length(data) - 1)) {
    trans_matrix[data[i], data[i + 1]] <- trans_matrix[data[i], data[i + 1]] + 1
  }
  
  trans_matrix <- sweep(trans_matrix, 1, rowSums(trans_matrix), FUN = "/")
  return(trans_matrix)
}

# Build Transition Matrix
trans_matrix <- calculate_transitions(weather_data, unique(weather))

# Create Markov Chain Model
markov_model <- new("markovchain", transitionMatrix = trans_matrix, name = "Weather")

# Plot Model
set.seed(1)
plot(markov_model)
