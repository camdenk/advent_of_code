library(tidyverse)
source("data_helper.R")


raw_data <- get_data(2, 2023) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()

test_data <-
c(
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
)

# Part 1
df <- tibble(input = raw_data)

max <- c("red" = 12, "blue" = 14, "green" = 13)         

df |>
  mutate(game_no = row_number(), 
         info = str_extract_all(input, "\\d+ [a-z]+")) |> 
  unnest(info) |> 
  mutate(input = NULL) |> 
  separate_wider_delim(info, delim = " ", names = c("num", "color")) |>
  mutate(max = max[color],
         num = as.numeric(num)) |> #View()
  group_by(game_no) |> 
  summarize(playable = !any(num > max)) |> 
  filter(playable) |> 
  summarize(sum(game_no))


# Part 2
df2 <- tibble(input = raw_data)

df2 |>
  mutate(game_no = row_number(), 
         info = str_extract_all(input, "\\d+ [a-z]+")) |> 
  unnest(info) |> 
  separate_wider_delim(info, delim = " ", names = c("num", "color")) |> 
  mutate(input = NULL,
         num = as.numeric(num)) |> 
  group_by(game_no, color) |> 
  summarize(min = max(num)) |>
  pivot_wider(names_from = color, values_from = min) |> 
  mutate(across(blue:red, as.numeric),
         power = blue*green*red) |> 
  pull(power) |> 
  sum()
  
