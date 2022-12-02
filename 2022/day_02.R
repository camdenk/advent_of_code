library(tidyverse)
source("data_helper.R")


raw_data <- get_data(2, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(data = raw_data) %>% 
  separate(data, c("Elf", "Mine"))

# Part one

data %>% 
  mutate(result = case_when(paste0(Elf, Mine) %in% c("AX", "BY", "CZ") ~  3,
                            paste0(Elf, Mine) %in% c("AY", "BZ", "CX") ~ 6,
                            TRUE ~ 0),
         value = case_when(Mine == "X" ~ 1,
                           Mine == "Y" ~ 2,
                           Mine == "Z" ~ 3),
         round_points = result+value
         ) %>% 
  pull(round_points) %>% 
  sum()



# Part two - Janky but effective
data %>% 
  rename(result = Mine) %>% 
  mutate(result_points = case_when(result == "X" ~ 0,
                                   result == "Y" ~ 3,
                                   result == "Z" ~ 6),
         Mine = case_when(result == "Y" ~ Elf,
                          result == "X" & Elf == "A" ~ "C",
                          result == "X" & Elf == "B" ~ "A",
                          result == "X" & Elf == "C" ~ "B",
                          result == "Z" & Elf == "A" ~ "B",
                          result == "Z" & Elf == "B" ~ "C",
                          result == "Z" & Elf == "C" ~ "A"),
         value = case_when(Mine == "A" ~ 1, 
                           Mine == "B" ~ 2,
                           Mine == "C" ~ 3),
         round_points = result_points+value
  ) %>% 
  pull(round_points) %>% 
  sum()
  