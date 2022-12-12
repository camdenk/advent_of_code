library(tidyverse)
source("data_helper.R")

raw_data <- get_data(12, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()


data <- tibble(values = raw_data)

initial_data <- grid_graph(data, values, mutual = TRUE, directed = TRUE)

# Help from David Robinson's solution prompted me to set up the functions
# in data_helper
# https://twitter.com/drob/status/1602180707316600832

nodes <- initial_data %>% 
  as_tibble() %>% 
  mutate(elevation = case_when(value == "S" ~ 1L,
                               value == "E" ~ 26L,
                               TRUE ~ match(value, letters)))


distance_data <- initial_data %>% 
  tidygraph::activate("edges") %>% 
  mutate(from_value = nodes$elevation[from],
         to_value = nodes$elevation[to]) %>% 
  filter(to_value <= from_value + 1) %>% 
  tidygraph::activate("nodes") %>% 
  mutate(distance = tidygraph::node_distance_to(which(nodes$value == "E"), mode = "out"))

# Part one
distance_data %>% 
  filter(value == "S")


# Part two
distance_data %>% 
  filter(value == "a") %>% 
  arrange(distance)
