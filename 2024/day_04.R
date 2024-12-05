library(tidyverse)
source("data_helper.R")

raw_data <- get_data(4, 2024) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()



test_data <- c(
  "MMMSXXMASM",
  "MSAMXMSMSA",
  "AMXSXMAAMM",
  "MSAMASMSMX",
  "XMASAMXAMM",
  "XXAMMXXAMA",
  "SMSMSASXSS",
  "SAXAMASAAA",
  "MAMMMXMMMM",
  "MXMXAXMASX"
)


# Part one
custom_adjacent_join_p1 <- function(x, y = x, suffix = c("", "2")) {
  adj <- tibble(row_delta = c(-3, -2, -1, 1, 2, 3,  0,  0,  0, 0, 0, 0),
                col_delta = c( 0,  0,  0, 0, 0, 0, -3, -2, -1, 1, 2, 3))
  
  if (TRUE) { # always true in this custom function
    adj <- bind_rows(adj,
                     tibble(row_delta = c(-3, -2, -1, -3, -2, -1,  1,  2,  3, 1, 2, 3),
                            col_delta = c(-3, -2, -1,  3,  2,  1, -1, -2, -3, 1, 2, 3)))
  }
  
  x %>%
    tidyr::crossing(adj) %>%
    mutate(row2 = row + row_delta,
           col2 = col + col_delta) %>%
    inner_join(y, by = c(row2 = "row", col2 = "col"), suffix = suffix) %>%
    filter(row != row2 | col != col2) %>%
    mutate(cell_delta = pmax(abs(row_delta), abs(col_delta)),
           direction = case_when(
             row_delta == 0 & col_delta > 0 ~ "Right",
             row_delta == 0 & col_delta < 0 ~ "Left",
             row_delta < 0 & col_delta == 0 ~ "Up",
             row_delta > 0 & col_delta == 0 ~ "Down",
             row_delta > 0 & col_delta > 0 ~ "Down and Right",
             row_delta > 0 & col_delta < 0 ~ "Down and Left",
             row_delta < 0 & col_delta > 0 ~ "Up and Right",
             row_delta < 0 & col_delta < 0 ~ "Up and Left"
           )) |> 
    select(-row_delta, -col_delta)
}


convert_to_tibble_p1 <- function(string_vec) {
  string_vec |> 
    tibble(string = _) |> 
    grid_tidy(string) |> 
    custom_adjacent_join_p1()
}


raw_data |> 
  convert_to_tibble_p1() |> 
  filter(value == "X") |> 
  group_by(row, col, direction) |> 
  arrange(row, col, direction, cell_delta) |> 
  filter(value2 == "M", lead(value2) == "A", lead(value2, 2L) == "S") |> 
  nrow()



# Part two
custom_adjacent_join_p2 <- function(x, y = x, suffix = c("", "2")) {
  adj <- tibble(row_delta = c(-1, -1, 1, 1),
                col_delta = c(-1, 1, -1, 1))
  
  x %>%
    tidyr::crossing(adj) %>%
    mutate(row2 = row + row_delta,
           col2 = col + col_delta) %>%
    inner_join(y, by = c(row2 = "row", col2 = "col"), suffix = suffix) %>%
    filter(row != row2 | col != col2) %>%
    mutate(is_left = col_delta < 0,
           is_up = row_delta < 0) |> 
    select(-row_delta, -col_delta)
}


convert_to_tibble_p2 <- function(string_vec) {
  string_vec |> 
    tibble(string = _) |> 
    grid_tidy(string) |> 
    custom_adjacent_join_p2()
}



raw_data |> 
  convert_to_tibble_p2() |> 
  filter(value == "A", value2 %in% c("M", "S")) |> 
  summarize(.by = c(row, col, value2),
            n_letters = n(),
            dir_sum = sum(is_left+is_up)) |> 
  filter(n_letters == 2, dir_sum %in% c(1, 3)) |> 
  count(row, col) |> 
  filter(n == 2) |> 
  nrow()
  
