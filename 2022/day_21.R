library(tidyverse)
source("data_helper.R")


raw_data <- get_data(21, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(values = raw_data) %>% 
  separate(values, c("name", "value"), sep = ": ")


known_number_starts <- data %>% 
  mutate(value = as.numeric(value)) %>% 
  drop_na(value)

unknown_number_starts <- data %>% 
  anti_join(known_number_starts, by = "name") %>% 
  extract(value, c("first_name", "math_expression", "second_name"), 
          "(\\S+) (\\S) (\\S+)")



known_numbers <- known_number_starts
unknown_numbers <- unknown_number_starts


root_known <- FALSE



# Part One

tictoc::tic()
while(!root_known){
  finding_numbers <- unknown_numbers %>% 
    left_join(known_numbers, by = c("first_name" = "name")) %>% 
    left_join(known_numbers, by = c("second_name" = "name")) %>% 
    mutate(value = case_when(math_expression == "+" ~ value.x + value.y,
                             math_expression == "-" ~ value.x - value.y,
                             math_expression == "*" ~ value.x * value.y,
                             math_expression == "/" ~ value.x / value.y)) %>% 
    drop_na(value)
  
  
  known_numbers <- bind_rows(known_numbers,
                             finding_numbers %>% select(name, value))
  
  unknown_numbers <- unknown_numbers %>% 
    anti_join(known_numbers, by = "name")
  
  if (unknown_numbers %>% filter(name == "root") %>% nrow() != 1){
    root_known <- TRUE
  }
}

tictoc::toc()

# Part Two

target_value <- 2*26605796414957

tictoc::tic()

continue_loop <- TRUE
loop_counter <- 2000000000000

# while (continue_loop) {
  loop_counter <- loop_counter + 1

testing_function <- function(input_value){
  

  pt_two_data <- tibble(values = raw_data) %>% 
    separate(values, c("name", "value"), sep = ": ") %>% 
    mutate(value = if_else(name == "humn", as.character(input_value), value))
  
  
  
  known_number_starts <- pt_two_data %>% 
    mutate(value = as.numeric(value) %>% suppressWarnings()) %>% 
    drop_na(value)
  
  unknown_number_starts <- pt_two_data %>% 
    anti_join(known_number_starts, by = "name") %>% 
    extract(value, c("first_name", "math_expression", "second_name"), 
            "(\\S+) (\\S) (\\S+)")
  
  known_numbers <- known_number_starts
  unknown_numbers <- unknown_number_starts
  
  
  mini_loop <- TRUE
  
  while (mini_loop){
    
    finding_numbers <- unknown_numbers %>% 
      left_join(known_numbers, by = c("first_name" = "name")) %>% 
      left_join(known_numbers, by = c("second_name" = "name")) %>% 
      mutate(value = case_when(math_expression == "+" ~ value.x + value.y,
                               math_expression == "-" ~ value.x - value.y,
                               math_expression == "*" ~ value.x * value.y,
                               math_expression == "/" ~ value.x / value.y)) %>% 
      drop_na(value)
    
    
    known_numbers <- bind_rows(known_numbers,
                               finding_numbers %>% select(name, value))
    
    unknown_numbers <- unknown_numbers %>% 
      anti_join(known_numbers, by = "name")
    
    
    
    if (unknown_numbers %>% filter(name == "root") %>% nrow() != 1){
      mini_loop <- FALSE
    }
  }
  

  if (known_numbers %>% filter(name == "root") %>% pull(value) == target_value) {
    continue_loop <- FALSE
  }
# }

loop_value <-  known_numbers %>% filter(name == "root") %>% pull(value)

return(target_value - loop_value)
}
tictoc::toc()


# Manually found that ptvl = 26605796414957
# Therefore jsrw = 26605796414957

# Manually found the answer using the above function
# 
# testing_function(3617613952378)









