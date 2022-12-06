library(tidyverse)
source("data_helper.R")

raw_data <- get_data(6, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

# Part one
no_matches <- TRUE
counter <- 0

while (no_matches) {
  counter <- counter + 1
  substring <- substring(raw_data, counter, counter+3)
  
  split_string <- str_split(substring, "")
  
  max_char_usage <- split_string[[1]] %>% 
    as_tibble() %>% 
    count(value) %>% 
    pull(n) %>% 
    max()
  
  no_matches <- max_char_usage > 1
}

answer <- counter +3


# Part two
no_matches <- TRUE
counter <- 0

while (no_matches) {
  counter <- counter + 1
  substring <- substring(raw_data, counter, counter+13)
  
  split_string <- str_split(substring, "")
  
  max_char_usage <- split_string[[1]] %>% 
    as_tibble() %>% 
    count(value) %>% 
    pull(n) %>% 
    max()
  
  no_matches <- max_char_usage > 1
}

answer <- counter +13