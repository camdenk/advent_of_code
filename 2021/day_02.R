library(tidyverse)
source("data_helper.R")

raw_data <- get_data(2, 2021) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()


# Part 1
data <- tibble(movement = raw_data) %>% 
  separate(movement, into = c("direction", "units"), convert = TRUE) %>% 
  mutate(x_mov = if_else(direction == "forward", units, 0L),
         y_mov = case_when(direction == "up" ~ -units,
                           direction == "down" ~ units,
                           TRUE ~ 0L))

sum(data$x_mov) * sum(data$y_mov)


# Part 2
data <- tibble(movement = raw_data) %>% 
  separate(movement, into = c("direction", "units"), convert = TRUE) %>% 
  mutate(x_mov = if_else(direction == "forward", units, 0L),
         aim_adj = case_when(direction == "up" ~ -units,
                             direction == "down" ~ units,
                             TRUE ~ 0L),
         aim = cumsum(aim_adj),
         depth_adj = aim * x_mov)

sum(data$x_mov) * sum(data$depth_adj)

