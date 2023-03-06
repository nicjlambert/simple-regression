# Markov Chain

library(dplyr)
library(markovchain)

weather <- c("sunny", "sunny", "rain", "cloudy")

set.seed(123)

# This code generates a simulated dataset of 100 weather conditions using a Markov chain model
weather_data <- sample(weather, 100, replace = TRUE)
head(weather_data, 10)

#If today is sunny, what is the probability that tomorrow will be sunny as well? 
#We simply just need to calculate how many times that today is sunny and the next day is sunny as well from the data.

embed(weather_data, 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(current == "sunny") %>% 
   arrange(next_day) %>% 
   count(current, next_day)

# Probability if today is cloudy

weather_type <- function(data, weather) {
embed(data, 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(current == weather) %>% 
   pull(next_day) %>% 
   table(paste0("Probability of each weather tomorrow if today is ", cloudy) %>% prop.table()
}

weather_cloudy <- weather_type(weather_data, "cloudy")
weather_cloudy

# Probability if today is rainy
weather_rain <- weather_type(weather_data, "rain")
weather_rain

# Probability if today is sunny
weather_rain <- weather_type(weather_data, "sunny")
weather_rain

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

