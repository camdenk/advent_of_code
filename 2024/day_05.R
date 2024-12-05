library(tidyverse)
source("data_helper.R")

raw_data <- get_data(5, 2024) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()


test_data <- c(
  "47|53",
  "97|13",
  "97|61",
  "97|47",
  "75|29",
  "61|13",
  "75|53",
  "29|13",
  "97|29",
  "53|29",
  "61|53",
  "97|53",
  "61|29",
  "47|13",
  "75|47",
  "97|75",
  "47|61",
  "75|61",
  "47|29",
  "75|13",
  "53|13",
  "",
  "75,47,61,53,29",
  "97,61,53,29,13",
  "75,29,13",
  "75,97,47,61,53",
  "61,13,29",
  "97,13,75,29,47"
)

test_blank_index <- which(test_data == "")
test_rules <- test_data[1:(test_blank_index-1)]
test_updates <- test_data[(test_blank_index+1):length(test_data)]

test_rules_df <- test_rules |> 
  tibble(rules = _) |> 
  separate(rules, c("before", "after"), sep = "\\|")
test_updates_list <- test_updates |> 
  str_split(",")



blank_index <- which(raw_data == "")

raw_rules <- raw_data[1:(blank_index-1)]
raw_updates <- raw_data[(blank_index+1):length(raw_data)]


rules_df <- raw_rules |> 
  tibble(rules = _) |> 
  separate(rules, c("before", "after"), sep = "\\|")
updates_list <- raw_updates |> 
  str_split(",")


# Part one ---------------------------------------------------------------------


pages_check <- function(pages, df_rules) {
  
  length_pages <- length(pages)
  
  return_val <- TRUE
  
  for (i in 1:(length_pages-1)) { # Don't need to check the last page
    
    numbers_need_to_be_before <- df_rules |> 
      filter(after == pages[i]) |> 
      pull(before)
    
    other_pages <- pages[i:length_pages]
    
    page_check <- other_pages %in% numbers_need_to_be_before
    
    
    broken_rule <- any(page_check)
    
    if (broken_rule) {
      return_val <- FALSE
      break
    }
  }
  
  return(return_val)
}


run_through_string <- function(ordered_pages, df_of_rules) {
  
  length_pages <- length(ordered_pages)
  
  return_val <- as.numeric(ordered_pages[ceiling(length_pages/2)])
  
  
  check_results <- pages_check(ordered_pages, df_of_rules)
  
  if (isFALSE(check_results)) {
    return_val <- 0
  }
  
  return(return_val)
}



p1_results <- map(updates_list, \(x) run_through_string(x, rules_df)) |> 
  unlist()

p1_results |> 
  sum()


# Part two ---------------------------------------------------------------------



p2_update_list <- updates_list[which(p1_results == 0)]




current_index_check <- function(pages, df_rules, index) {
  
  length_pages <- length(pages)
  
  numbers_need_to_be_before <- df_rules |> 
    filter(after == pages[index]) |> 
    pull(before)
  
  other_pages <- pages[index:length_pages]
  
  page_check <- other_pages %in% numbers_need_to_be_before
  
  
  broken_rule <- any(page_check)
  
  return(!broken_rule)
}




run_through_string_p2 <- function(pages, df_of_rules) {
  
  length_pages <- length(pages)
  middle_index <- (length_pages+1)/2
  
  keep_going <- TRUE
  
  current_pages_order <- pages
  
  index_checking <- 1
  
  while (TRUE) {
    
    is_current_index_fine <- current_index_check(current_pages_order, 
                                                 df_of_rules,
                                                 index_checking)
    
    if (is_current_index_fine && index_checking == middle_index) {
      break
    } else if (is_current_index_fine) {
      
      index_checking <- index_checking + 1
      
    } else {
      current_pages_order <- c(current_pages_order[-index_checking], current_pages_order[index_checking])
    }
    
  }


  return(as.numeric(current_pages_order[middle_index]))
}



p2_results <- map(p2_update_list, \(x) run_through_string_p2(x, rules_df)) |> 
  unlist()

p2_results |> 
  sum()
