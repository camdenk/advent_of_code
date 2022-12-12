library(tidyverse)
source("data_helper.R")

raw_data <- get_data(11, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

# Idea, create a list that contains a vector for each monkey
# Run through that list and compare to their rules and then update the positions
# of the items

starting_items <- raw_data[seq(2, 51, 7)]

starting_items_formatted <- starting_items %>% 
  substr(19, nchar(starting_items)) %>% 
  str_split(", ")

for (i in 1:length(starting_items_formatted)) {
  starting_items_formatted[[i]] <- starting_items_formatted[[i]] %>% as.numeric()
}




# Create dataframe of rules
monkey_vector <- raw_data[seq(1, 50, 7)] %>% parse_number()
operation_vector <- raw_data[seq(3, 52, 7)] %>% substr(24, nchar(.))
test_vector <- raw_data[seq(4, 53, 7)] %>% parse_number()
true_vector <- raw_data[seq(5, 54, 7)] %>% parse_number()
false_vector <- raw_data[seq(6, 55, 7)] %>% parse_number()

rules_df <- tibble(monkey = monkey_vector,
                   operation = operation_vector,
                   divisibility_test = test_vector,
                   true_monkey = true_vector,
                   false_monkey = false_vector) %>% 
  mutate(monkey = monkey + 1,
         true_monkey = true_monkey + 1,
         false_monkey = false_monkey + 1,
         operation = if_else(operation == "* old", "** 2", operation))


# Part one

item_list <- starting_items_formatted

item_counter <- c(0,0,0,0,0,0,0,0)

for (round in 1:20){
  for (monkey_num in 1:8){
    
    rules_slice <- rules_df %>% slice(monkey_num)
    operation <- rules_slice %>% pull(operation)
    divisibility_number <- rules_slice %>% pull(divisibility_test)
    true_monkey <- rules_slice %>% pull(true_monkey)
    false_monkey <- rules_slice %>% pull(false_monkey)
    
    monkey_items <- item_list[[monkey_num]]
    
    num_items_held <- length(monkey_items)
    
    
    if (num_items_held > 0){
      for (item_num in 1:num_items_held) {
        item_inspecting <- monkey_items[item_num]
        
        worry_operation <- paste(item_inspecting, operation) %>% parse(text = .) %>% eval()
        
        calmed_value <- floor(worry_operation/3)
        
        if (calmed_value %% divisibility_number == 0) {
          item_list[[true_monkey]] <- append(item_list[[true_monkey]], calmed_value)
        } else {
          item_list[[false_monkey]] <- append(item_list[[false_monkey]], calmed_value)
        }
        
      }
      
    }
    
    item_counter[monkey_num] <- item_counter[monkey_num] + num_items_held
    item_list[[monkey_num]] <- numeric()
  }
}

top_two <- item_counter %>% sort(decreasing = TRUE) %>% .[1:2]
top_two[1] * top_two[2]


# Part two

lcm <- function(x, y) {
  # choose the greater number
  if(x > y) {
    greater = x
  } else {
    greater = y
  }
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm = greater
      break
    }
    greater = greater + 1
  }
  return(lcm)
}

lcm_vector <- function(x) Reduce(lcm, x)

lcm_value <- lcm_vector(rules_df$divisibility_test)



item_list <- starting_items_formatted

item_counter <- c(0,0,0,0,0,0,0,0)


for (round in 1:10000){
  for (monkey_num in 1:8){
    
    rules_slice <- rules_df %>% slice(monkey_num)
    operation <- rules_slice %>% pull(operation)
    divisibility_number <- rules_slice %>% pull(divisibility_test)
    true_monkey <- rules_slice %>% pull(true_monkey)
    false_monkey <- rules_slice %>% pull(false_monkey)
    
    monkey_items <- item_list[[monkey_num]]
    
    num_items_held <- length(monkey_items)
    
    
    if (num_items_held > 0){
      for (item_num in 1:num_items_held) {
        item_inspecting <- monkey_items[item_num]
        
        worry_operation <- paste(item_inspecting, operation) %>% parse(text = .) %>% eval()
        
        calmed_value <- worry_operation%%lcm_value
        
        if (calmed_value %% divisibility_number == 0) {
          item_list[[true_monkey]] <- append(item_list[[true_monkey]], calmed_value)
        } else {
          item_list[[false_monkey]] <- append(item_list[[false_monkey]], calmed_value)
        }
        
      }
      
    }
    
    item_counter[monkey_num] <- item_counter[monkey_num] + num_items_held
    item_list[[monkey_num]] <- numeric()
  }
}

top_two <- item_counter %>% sort(decreasing = TRUE) %>% .[1:2]
top_two[1] * top_two[2]


