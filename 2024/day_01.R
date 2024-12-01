library(tidyverse)
source("data_helper.R")


raw_data <- get_data(1, 2024) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()


split_raw_data <- str_split(raw_data, "   ")

first_values <- as.numeric(sapply(split_raw_data, `[`, 1))
second_values <- as.numeric(sapply(split_raw_data, `[`, 2))


# Part one
total_diff <- abs(sort(first_values) - sort(second_values)) |> 
  sum()



# Part two
index_test <- function(ind) {
  
  val_tested <- first_values[ind]
  
  matches <- sum(second_values == val_tested) |> 
    sum()
  
  return(val_tested*matches)
}


manual_search <- map(1:length(first_values), index_test) |> 
  unlist() |> 
  sum()
