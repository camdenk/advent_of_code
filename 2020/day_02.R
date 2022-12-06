library(tidyverse)
source("data_helper.R")

raw_data <- get_data(2, 2020) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- raw_data %>% as_tibble() %>% 
  separate(value, c("rule", "password"), sep = ": ") %>% 
  separate(rule, c("number", "character"), sep = " ") %>% 
  separate(number, c("min", "max"), sep = "-", convert = TRUE)

# Part one

data %>% 
  mutate(char_count = str_count(password, character)) %>% 
  rowwise() %>% 
  filter(between(char_count, min, max)) %>% 
  ungroup() %>% 
  nrow()


# Part two

data %>% 
  mutate(min_pos = substr(password, min, min),
         max_pos = substr(password, max, max),
         min_match = min_pos == character,
         max_match = max_pos == character,
         passes = min_match + max_match == 1) %>% 
  filter(passes) %>% 
  nrow()
