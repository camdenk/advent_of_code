library(tidyverse)
source("data_helper.R")


raw_data <- get_data(1, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()


data <- tibble(calorie_count = raw_data) %>% 
  mutate(calorie_count = as.numeric(calorie_count))


elf_calorie_counts <- data %>% 
  mutate(new_elf = if_else(row_number() == 1, 
                           TRUE, 
                           is.na(lag(calorie_count)))) %>% 
  filter(!is.na(calorie_count)) %>% 
  mutate(elf_id = cumsum(new_elf)) %>% 
  group_by(elf_id) %>% 
  summarize(n_snacks = n(),
            total_calories = sum(calorie_count),
            .groups = "drop") %>% 
  arrange(-total_calories)



# Part 1
elf_calorie_counts %>% 
  pull(total_calories) %>% 
  max()

# Part 2
elf_calorie_counts %>% 
  slice_max(total_calories, n = 3) %>% 
  pull(total_calories) %>% 
  sum()
