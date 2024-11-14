library(tidyverse)
source("data_helper.R")


raw_data <- get_data(3, 2023) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()

data <- raw_data |> 
  tibble(input = _) |> 
  grid_tidy(input) |> 
  mutate(is_digit = str_detect(value, "\\d")) |> 
  group_by(row) |> 
  # Could have used number_id = dplyr::consecutive_id(row, is_digit)
  mutate(number_id = paste0(row, ".", cumsum(is_digit != lag(is_digit, default = FALSE)))) |> 
  group_by(number_id) |> 
  mutate(n_val = as.numeric(paste0(value, collapse = "")))


# Part one
data |> 
  filter(!is.na(n_val)) |> 
  adjacent_join(data, diagonal = TRUE) |> 
  filter(value2 != ".",
         !is_digit2) |> 
  arrange(row, col) |> 
  distinct(number_id, .keep_all = TRUE) |> 
  pull(n_val) |> 
  sum()


# Part two
data |> 
  filter(value == "*") |> 
  adjacent_join(data, diagonal = TRUE) |> 
  filter(!is.na(n_val2)) |> 
  distinct(row, col, number_id2, .keep_all = TRUE) |> 
  group_by(row, col) |> 
  summarize(n_gears = n(),
            gear_ratio = prod(n_val2)) |> 
  filter(n_gears == 2) |> 
  pull(gear_ratio) |> 
  sum()
