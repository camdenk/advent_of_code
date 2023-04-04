library(tidyverse)
source("data_helper.R")


# Idea
# 
# Wrap all numbers in brackets,
# For each step, step into the next bracket and then split on possible ","
# 
# Second idea
# 
# Convert to nested lists



raw_data <- get_data(13, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(left_side = raw_data[seq(1, 448, 3)],
               right_side = raw_data[seq(2, 449, 3)])


first_comparison <- data %>% slice(1)
