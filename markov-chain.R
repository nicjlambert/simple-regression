# Load Required Packages
packages <- c("dplyr", "markovchain")
new_packages <- setdiff(packages, rownames(installed.packages()))
if(length(new_packages)) install.packages(new_packages)

lapply(packages, require, character.only = TRUE)

# Simulated Weather Data
set.seed(123)
weather <- c("sunny", "sunny", "rain", "cloudy")
weather_data <- sample(weather, 100, replace = TRUE)

# Function to Calculate Transition Probabilities
calculate_transitions <- function(data, states) {
  trans_matrix <- matrix(0, nrow = length(states), ncol = length(states))
  rownames(trans_matrix) <- states
  colnames(trans_matrix) <- states
  
  for (i in 1:(length(data) - 1)) {
    current <- data[i]
    next <- data[i + 1]
    trans_matrix[current, next] <- trans_matrix[current, next] + 1
  }
  
  trans_matrix <- trans_matrix / rowSums(trans_matrix)
  return(trans_matrix)
}

# Build Transition Matrix
trans_matrix <- calculate_transitions(weather_data, unique(weather))

# Create Markov Chain Model
markov_model <- new("markovchain", 
                    transitionMatrix = trans_matrix,
                    name = "Weather")

# Plot Model
set.seed(1)
plot(markov_model)
