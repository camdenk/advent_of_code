library(tidyverse)
source("data_helper.R")


raw_data <- get_data(4, 2023) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()


test_data <- c("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
               "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
               "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
               "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
               "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
               "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

# tibble(input = test_data) |> 
#   mutate(card = row_number(),
#          cleaned_input = substr(input, 8, nchar(input)),
#          input = NULL) |> 
#   separate_wider_delim(cols = cleaned_input, delim = " | ",
#                        names = c("winning_nums", "card_nums")) |> 
#   mutate(across(winning_nums:card_nums, trimws),
#          across(winning_nums:card_nums, \(x) str_replace_all(x, "  ", " "))) |> 
#   separate_wider_delim(cols = winning_nums, delim = " ",
#                        names = paste0("win_", 1:5)) |> 
#   separate_wider_delim(cols = card_nums, delim = " ",
#                        names = paste0("n_", 1:8)) |> 
#   pivot_longer(starts_with("n_"),
#                names_to = "num_on_card",
#                values_to = "value") |> 
#   pivot_longer(starts_with("win"),
#                names_to = c("win_val"),
#                values_to = "win_value") |> 
#   # rowwise() |> 
#   mutate(winner = value == win_value) |> 
#   group_by(card) |> 
#   summarize(winners = sum(winner)) |> 
#   mutate(card_val = if_else(winners >= 1, 2**(winners-1), 0)) |> 
#   pull(card_val) |> 
#   sum()



# Part one
setup <- tibble(input = raw_data) |> 
  mutate(card = row_number(),
         cleaned_input = substr(input, 11, nchar(input)),
         input = NULL) |> 
  separate_wider_delim(cols = cleaned_input, delim = " | ",
                       names = c("winning_nums", "card_nums")) |> 
  mutate(across(winning_nums:card_nums, trimws),
         across(winning_nums:card_nums, \(x) str_replace_all(x, "  ", " "))) |> 
  separate_wider_delim(cols = winning_nums, delim = " ",
                       names = paste0("win_", 1:10)) |> 
  separate_wider_delim(cols = card_nums, delim = " ",
                       names = paste0("n_", 1:25)) |> 
  pivot_longer(starts_with("n_"),
               names_to = "num_on_card",
               values_to = "value") |> 
  pivot_longer(starts_with("win"),
               names_to = c("win_val"),
               values_to = "win_value") |> 
  mutate(winner = value == win_value) |> 
  group_by(card) |> 
  summarize(winners = sum(winner))
  
setup |> 
  filter(winners > 0) |> 
  mutate(card_val = 2**(winners-1)) |> 
  pull(card_val) |> 
  sum()
  


# Part two
n_games <- nrow(setup)
m <- setup$winners
copies <- rep(1, n_games)


for ( i in seq_len(n_games)) {
  if (m[i] > 0) {
    range <- seq(i+1, min(i + m[i], n_games))
    copies[range] <- copies[range] + copies[i]
  }
}

sum(copies)
