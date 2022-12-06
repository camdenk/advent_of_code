library(tidyverse)
source("data_helper.R")

raw_data <- get_data(1, 2020) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines() %>% 
  as.numeric()

# Part one
data <- crossing(value1 = raw_data, value2 = raw_data)

data %>% 
  filter(value1 + value2 == 2020) %>% 
  mutate(prod = value1*value2) %>% 
  slice(1) %>% 
  pull(prod)


# Part two
data <- crossing(value1 = raw_data, value2 = raw_data, value3 = raw_data)

data %>% 
  filter(value1 + value2 + value3 == 2020) %>% 
  mutate(prod = value1*value2*value3) %>% 
  slice(1) %>% 
  pull(prod)
