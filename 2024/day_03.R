library(tidyverse)
source("data_helper.R")

test_data <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

raw_data <- get_data(3, 2024) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()

# Part 1
raw_data |> 
  paste(collapse = "") |> 
  str_extract_all("mul\\(\\d{0,3},\\d{0,3}\\)") |> 
  pluck(1) |> 
  gsub("mul", "", x = _) |> 
  gsub(",", "*", x = _) |> 
  paste(collapse = "+") |> 
  parse(text = _) |> 
  eval()




# Part 2

test_data2 <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

# First pass, too low because it was removing one large chunk, then too high
# after "better" capturing when the directions turn on/off
# 
# Had to include the second step since my string ends with a "don't" call
raw_data |> 
  paste(collapse = "") |> 
  # Remove everything in between don't and do calls
  gsub("don't\\(\\)(.*?)do\\(\\)", "", x = _) |> 
  # Remove everything after the "final" switch to "don't"
  gsub("don't\\(\\).+", "", x = _) |> 
  str_extract_all("mul\\(\\d{0,3},\\d{0,3}\\)") |> 
  pluck(1) |> 
  gsub("mul", "", x = _) |> 
  gsub(",", "*", x = _) |> 
  paste(collapse = "+") |> 
  parse(text = _) |> 
  eval()
