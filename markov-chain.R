# Markov Chain

library(dplyr)
library(markovchain)

weather <- c("sunny", "sunny", "rain", "cloudy")

set.seed(123)
weather_data <- sample(weather, 100, replace = T)
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
weather_cloudy <- embed(weather_data, 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(current == "cloudy") %>% 
   pull(next_day) %>% 
   table("Probability of each weather tomorrow if today is cloudy" = .) %>% prop.table()


# Probability if today is rainy
weather_rain <- embed(weather_data, 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(current == "rain") %>% 
   pull(next_day) %>% 
   table("Probability of each weather tomorrow if today is rain" = .) %>% prop.table()

weather_cloudy

# Probability if today is rainy
weather_sunny <- embed(weather_data, 2)[, 2:1] %>% 
   as.data.frame() %>% 
   rename(current = V1, next_day = V2) %>% 
   filter(current == "sunny") %>% 
   pull(next_day) %>% 
   table("Probability of each weather tomorrow if today is sunny" = .) %>% prop.table()

weather_sunny

trans_matrix <- rbind(weather_cloudy, weather_rain, weather_sunny) %>% 
   `rownames<-`(c("cloudy", "rain", "sunny"))

trans_matrix


# convert the matrix as a markov chain object
markov_model <- new("markovchain", 
                    transitionMatrix = trans_matrix, # Input Transition Matrix
                    name = "Weather") # Name of the Markov Chains

markov_model
