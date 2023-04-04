library(tidyverse)
source("data_helper.R")


raw_data <- get_data(20, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(values = raw_data) %>% 
  grid_tidy(values, sep = " ") %>% 
  mutate(value = as.numeric(value))

# Idea
# 
# Loop through every row number 1-5k
# Pull the current location and then the intended location
# create new df that slices from 1:cur and then cur+1:5k with 
# the movement that took place
# 
# Will need to control for looping spots