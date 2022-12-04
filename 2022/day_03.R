library(tidyverse)
source("data_helper.R")


raw_data <- get_data(3, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(rucksacks = raw_data) %>% 
  mutate(rucksack_id = row_number())

data <- data.frame(str_split_fixed(raw_data, "", max(nchar(raw_data)))) %>% 
  as_tibble() %>% 
  mutate(rucksack_id = row_number() - 1) %>% 
  pivot_longer(!rucksack_id,
               names_to = "pos",
               values_to = "character") %>% 
  filter(character != "")


values_df <- tibble(letter = c(letters, LETTERS)) %>% 
  mutate(values = row_number())


# Part one

data %>% 
  group_by(rucksack_id) %>% 
  mutate(front_half = row_number() <= n()/2) %>% 
  group_by(rucksack_id, character) %>% 
  summarize(halves = n_distinct(front_half)) %>% 
  filter(halves > 1) %>% 
  left_join(values_df, by = c("character" = "letter")) %>% 
  pull(values) %>% sum()


# Part two

data %>% 
  mutate(elf_group = rucksack_id %/% 3) %>% 
  group_by(elf_group, character) %>% 
  summarize(distinct_rucks = n_distinct(rucksack_id)) %>% 
  filter(distinct_rucks == 3) %>% 
  left_join(values_df, by = c("character" = "letter")) %>% 
  pull(values) %>% sum()
