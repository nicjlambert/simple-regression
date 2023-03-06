# Markov Chain

# Package Installation and Loading ---------------------------------------------------------
packages <- c("dplyr", "markovchain")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, require, character.only = TRUE)

weather <- c("sunny", "sunny", "rain", "cloudy")

set.seed(123)

# This code generates a simulated dataset of 100 weather conditions 
# using a Markov chain model
weather_data <- sample(weather, 100, replace = TRUE)
head(weather_data, 10)

# If today is sunny, what is the probability that tomorrow will be sunny as well? 
# We simply just need to calculate how many times that today is sunny and the next 
# day is sunny as well from the data.

<<<<<<< HEAD
embed(weather_data, dimension = 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(current == "sunny") %>% 
   arrange(next_day) %>% 
   count(current, next_day)

=======
>>>>>>> 4ae3ce8db6108e8dac69f9e18fe5d8b7e36aa652
# Probability if today is cloudy
prob_weather <- function(df, filter) {
embed(df, 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(eval(rlang::parse_expr(filter))) %>%
   pull(next_day) %>% 
   table("Probability of each weather tomorrow if today is cloudy" = .) %>% prop.table()
}

# Probability if today is cloudy
weather_cloudy <- prob_weather(weather_data, "current %in% c('cloudy')")

# Probability if today is rainy
weather_rain <- prob_weather(weather_data, "current %in% c('rain')")

# Probability if today is sunny
weather_sunny <- prob_weather(weather_data, "current %in% c('sunny')")

# transition probability matrix
trans_matrix <- rbind(weather_cloudy, weather_rain, weather_sunny) %>% 
   `rownames<-`(c("cloudy", "rain", "sunny"))

trans_matrix

# convert the matrix as a markov chain object
markov_model <- new("markovchain", 
                    transitionMatrix = trans_matrix, # Input Transition Matrix
                    name = "Weather") # Name of the Markov Chains
markov_model
set.seed(1)
plot(markov_model)

