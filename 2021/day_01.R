library(tidyverse)
source("data_helper.R")

raw_data <- get_data(1, 2021) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()


data <- tibble(measurement = raw_data) %>% 
  mutate(measurement = as.numeric(measurement))

# Part 1
sum(data$measurement > lag(data$measurement), na.rm = TRUE)

# Part 2
data %>% 
  mutate(measurement2 = lead(measurement),
         measurement3 = lead(measurement2),
         three_measurement_sum = measurement + measurement2 + measurement3,
         bigger_sum = three_measurement_sum > lag(three_measurement_sum)) %>% 
  pull(bigger_sum) %>% 
  sum(na.rm = TRUE)
