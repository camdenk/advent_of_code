library(tidyverse)
source("data_helper.R")


raw_data <- get_data(4, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(data = raw_data) %>% 
  separate(data, into = c("first", "second"), sep = ",") %>% 
  separate(first, into = c("bottom_1", "top_1")) %>% 
  separate(second, into = c("bottom_2", "top_2")) %>% 
  mutate(across(everything(), as.numeric))

# Part one
data %>% 
  mutate(subset = ((bottom_1 <= bottom_2) & (top_1 >= top_2)) | ((bottom_1 >= bottom_2) & (top_1 <= top_2))) %>% 
  pull(subset) %>% 
  sum()

# Part two
data %>% 
  rowwise() %>% 
  mutate(overlap = between(bottom_1, bottom_2, top_2) | between(top_1, bottom_2, top_2) | between(bottom_2, bottom_1, top_1)| between(top_2, bottom_1, top_1)) %>% 
  ungroup() %>% 
  pull(overlap) %>% 
  sum()



