library(tidyverse)
source("data_helper.R")

test_data <- c(
  "7 6 4 2 1",
  "1 2 7 8 9",
  "9 7 6 2 1",
  "1 3 2 4 5",
  "8 6 4 4 1",
  "1 3 6 7 9"
)

raw_data <- get_data(2, 2024) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()



# Check Function

check_function <- function(prepped_data) {
  
  prepped_data |> 
    group_by(row) |> 
    mutate(value = as.numeric(value), 
           diff = lead(value) - value) |>
    summarize(num_increasing = sum(diff > 0, na.rm = TRUE),
              num_decreasing = sum(diff < 0, na.rm = TRUE),
              num_zero = sum(diff == 0, na.rm = TRUE),
              biggest_jump = max(abs(diff), na.rm = TRUE),
              smallest_jump = min(abs(diff), na.rm = TRUE)) |> 
    filter(num_zero == 0, biggest_jump <= 3, smallest_jump >= 1,
           num_increasing == 0 | num_decreasing == 0)
  
}


# Part one
raw_data |> 
  tibble(input = _) |> 
  grid_tidy(input, sep = " ") |> 
  check_function() |>
  nrow()



# Part two
# 
# Manually remove a 
data_prep <- raw_data |> 
  tibble(input = _) |> 
  grid_tidy(input, sep = " ") 


remove_one_and_check <- function(data_prepped, col_removed) {
  data_prepped |> 
    filter(col != col_removed) |> 
    check_function()
}


part_two <- map(1:max(data_prep$col), \(x) remove_one_and_check(data_prep, x)) |> 
  data.table::rbindlist() |> 
  pull(row) |> 
  unique() |> 
  length()




# Found this code online when checking day 2 solutions
# Really clever way of using head() and tail()
data_test<-raw_data|>str_extract_all("\\d+")|>lapply(as.numeric)

check <- function(x){
  all((head(x,-1)-tail(x,-1)) %in% -3:-1)|
    all((head(x,-1)-tail(x,-1))%in% 1:3)
}

#Part1 
map(data_test, check)|>unlist()|>sum()

remove_1 <- function(x){
  map(1:length(x),  ~x[-.x]|>check())|>unlist()|>any()
}
#part2 
map(data_test,remove_1)|>unlist()|>sum()


