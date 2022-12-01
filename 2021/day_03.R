library(tidyverse)
source("data_helper.R")

raw_data <- get_data(3, 2021) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(binary = raw_data)

# Part one
split_data <- data.frame(str_split_fixed(data$binary, "", max(nchar(data$binary))))

column_names <- 1:max(nchar(data$binary))

colnames(split_data) <- column_names

split_data <- split_data %>% 
  as_tibble() 

long_data <- split_data %>% 
  pivot_longer(everything(),
               names_to = "character",
               values_to = "value") %>% 
  mutate(across(everything(), as.numeric)) 



averages <- long_data %>% 
  group_by(character) %>% 
  summarize(value = round(mean(value)))

gamma <- averages %>% 
  ungroup() %>% 
  mutate(binary_value = value*2^(12-character)) %>% 
  pull(binary_value) %>% 
  sum()

epsilon <- averages %>% 
  ungroup() %>% 
  mutate(value = 1-value,
         binary_value = value*2^(12-character)) %>% 
  pull(binary_value) %>% 
  sum()

gamma*epsilon



# Part two
oxygen_number <- split_data %>% 
  filter(`1` == floor(0.5+mean(`1`))) %>% 
  filter(`2` == floor(0.5+mean(`2`))) %>% 
  filter(`3` == floor(0.5+mean(`3`))) %>% 
  filter(`4` == floor(0.5+mean(`4`))) %>% 
  filter(`5` == floor(0.5+mean(`5`))) %>% 
  filter(`6` == floor(0.5+mean(`6`))) %>% 
  filter(`7` == floor(0.5+mean(`7`))) %>% 
  filter(`8` == floor(0.5+mean(`8`))) %>% 
  filter(`9` == floor(0.5+mean(`9`))) %>% 
  filter(`10` == floor(0.5+mean(`10`))) %>% 
  filter(`11` == floor(0.5+mean(`11`))) %>% 
  filter(`12` == floor(0.5+mean(`12`))) %>% 
  pivot_longer(everything(),
               names_to = "character",
               values_to = "value") %>% 
  mutate(across(everything(), as.numeric)) %>%
  mutate(binary_value = value*2^(12-character)) %>% 
  pull(binary_value) %>% 
  sum()

c02_number <- split_data %>% 
  filter(`1` != floor(0.5+mean(`1`))) %>% 
  filter(`2` != floor(0.5+mean(`2`))) %>% 
  filter(`3` != floor(0.5+mean(`3`))) %>% 
  filter(`4` != floor(0.5+mean(`4`))) %>% 
  filter(`5` != floor(0.5+mean(`5`))) %>% 
  filter(`6` != floor(0.5+mean(`6`))) %>% 
  filter(`7` != floor(0.5+mean(`7`))) %>% 
  filter(`8` != floor(0.5+mean(`8`))) %>% 
  pivot_longer(everything(),
               names_to = "character",
               values_to = "value") %>% 
  mutate(across(everything(), as.numeric)) %>%
  mutate(binary_value = value*2^(12-character)) %>% 
  pull(binary_value) %>% 
  sum()

oxygen_number*c02_number

