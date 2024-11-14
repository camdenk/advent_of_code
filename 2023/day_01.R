library(tidyverse)
source("data_helper.R")


raw_data <- get_data(1, 2023) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()

# part one

df <- str_extract_all(raw_data, "[0-9]+") |> 
  lapply(\(x) paste0(x, collapse = "")) |> 
  unlist() |> 
  tibble(digits = _) |> 
  mutate(num = paste0(substr(digits, 1,1), substr(digits, nchar(digits), nchar(digits))),
         num = as.numeric(num))



df |> 
  pull(num) |> 
  sum()



# part two

raw_data_test <- c("two1nine",
                   "eightwothree",
                   "abcone2threexyz",
                   "xtwone3four",
                   "4nineeightseven2",
                   "zoneight234",
                   "7pqrstsixteen")

# test_replace <- str_replace_all(raw_data, "eightwo", "eight")
# test_replace <- str_replace_all(test_replace, "eighthree", "eight")
# test_replace <- str_replace_all(test_replace, "sevenine", "seven")
# test_replace <- str_replace_all(test_replace, "nineight", "nine")
# test_replace <- str_replace_all(test_replace, "twone", "two")
# test_replace <- str_replace_all(test_replace, "threeight", "three")
# test_replace <- str_replace_all(test_replace, "fiveight", "five")

# Cheeky way around eightwo or something similar
str_data <- str_replace_all(raw_data, "one", "one1one")
str_data <- str_replace_all(str_data, "two", "two2two")
str_data <- str_replace_all(str_data, "three", "three3three")
str_data <- str_replace_all(str_data, "four", "four4four")
str_data <- str_replace_all(str_data, "five", "five5five")
str_data <- str_replace_all(str_data, "six", "six6six")
str_data <- str_replace_all(str_data, "seven", "seven7seven")
str_data <- str_replace_all(str_data, "eight", "eight8eight")
str_data <- str_replace_all(str_data, "nine", "nine9nine")


df2 <- str_extract_all(str_data, "[1-9]+") |> 
  lapply(\(x) paste0(x, collapse = "")) |> 
  unlist() |> 
  tibble(digits = _) |> 
  mutate(num = paste0(substr(digits, 1,1), substr(digits, nchar(digits), nchar(digits))),
         num = as.numeric(num))



df2 |> 
  pull(num) |> 
  sum()



# str_extract_all("eightwo", "(?=(\\d|one|two|three|four|five|six|seven|eight|nine))")



