library(tidyverse)
source("data_helper.R")

raw_data <- get_data(8, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

# raw_data <- c("30373", "25512", "65332", "33549", "35390")


data <- tibble(values = raw_data) %>% 
  separate(values, into = paste0("col_", (1:100)-1), sep = "") %>% 
  select(-col_0) %>% 
  mutate(across(everything(), as.numeric))



# Part one
height_check <- function(tree_grid, tree_row, tree_column){
  # Check top to bottom
  max_height_above <- tree_grid[1:(tree_row-1), tree_column] %>% 
    pull(1) %>%
    max()
  
  # Check bottom to top
  max_height_below <- tree_grid[99:(tree_row+1), tree_column] %>% 
    pull(1) %>%
    max()
  
  # Check left to right
  max_height_left <- tree_grid[tree_row, 1:(tree_column-1)] %>% 
    as.vector() %>% 
    unname() %>% 
    cummax() %>% 
    max()
  
  # Check left to right
  max_height_right <- tree_grid[tree_row, 99:(tree_column+1)] %>% 
    as.vector() %>% 
    unname() %>% 
    cummax() %>% 
    max()
  
  
  tree_height <- tree_grid[tree_row, tree_column] %>% pull(1)
  
  visible_above <- tree_height > max_height_above
  visible_below <- tree_height > max_height_below
  visible_left <- tree_height > max_height_left
  visible_right <- tree_height > max_height_right
  
  
  visible <- visible_above | visible_below | visible_left | visible_right
  
  return(as.numeric(visible))
}


visible_counter <- 0

visible_counter <- visible_counter + 2*nrow(data) + 2*ncol(data) - 4# Don't double count corners

# Don't run for outside trees
for (row in 2:98){
  for (column in 2:98){
    visible_counter <- visible_counter + height_check(data, row, column)
  }
}

visible_counter




# Part two

# Remove all aspects of the vector where the cummax is higher than the tree

visible_trees <- function(tree_grid, tree_row, tree_column){
  
  tree_height <- tree_grid[tree_row, tree_column] %>% pull(1)
  
  
  # Check top to bottom
  max_height_above_vec <- tree_grid[(tree_row-1):1, tree_column] %>% 
    pull(1) %>%
    cummax()
  
  max_height_above <- max(max_height_above_vec)
  
  if (max_height_above >= tree_height){
    trees_above <- length(max_height_above_vec[max_height_above_vec < tree_height]) + 1
  } else {
    trees_above <- length(max_height_above_vec)
  }
  
  
  # Check bottom to top
  max_height_below_vec <- tree_grid[(tree_row+1):99, tree_column] %>% 
    pull(1) %>%
    cummax()
  
  
  max_height_below <- max(max_height_below_vec)
  
  if (max_height_below >= tree_height){
    trees_below <- length(max_height_below_vec[max_height_below_vec < tree_height]) + 1
  } else {
    trees_below <- length(max_height_below_vec)
  }
  
  
  # Check left to right
  max_height_left_vec <- tree_grid[tree_row, (tree_column-1):1] %>% 
    as.vector() %>% 
    unname() %>% 
    cummax()
  
  max_height_left <- max(max_height_left_vec)
  
  if (max_height_left >= tree_height){
    trees_left <- length(max_height_left_vec[max_height_left_vec < tree_height]) + 1
  } else {
    trees_left <- length(max_height_left_vec)
  }
  
  # Check left to right
  max_height_right_vec <- tree_grid[tree_row, (tree_column+1):99] %>% 
    as.vector() %>% 
    unname() %>% 
    cummax()
  
  max_height_right <- max(max_height_right_vec)
  
  if (max_height_right >= tree_height){
    trees_right <- length(max_height_right_vec[max_height_right_vec < tree_height]) + 1
  } else {
    trees_right <- length(max_height_right_vec)
  }
  
  return(list(trees_above, trees_below, trees_left, trees_right))
  
}

visible_df <- tibble(row = numeric(),
                     column = numeric(),
                     trees_above = numeric(),
                     trees_below = numeric(),
                     trees_left = numeric(),
                     trees_right = numeric())

# Don't run for outside trees
for (row in 2:98){
  for (column in 2:98){
    result <- visible_trees(data, row, column)
    
    visible_df <- visible_df %>% 
      add_row(row = row,
              column = column,
              trees_above = result[[1]],
              trees_below = result[[2]],
              trees_left = result[[3]],
              trees_right = result[[4]])
  }
}

visible_df %>%
  mutate(scenic_score = trees_above*trees_below*trees_left*trees_right) %>% 
  arrange(-scenic_score)

