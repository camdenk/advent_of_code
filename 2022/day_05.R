library(tidyverse)
source("data_helper.R")


raw_data <- get_data(5, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

instructions <- raw_data[11:length(raw_data)] %>% as_tibble() %>% 
  separate(value, into = c("1", "num_move", "2", "col_from", "3", "col_to"), convert = TRUE) %>% 
  select(-`1`, -`2`, -`3`)

configuration <- raw_data[1:8] %>% 
  # Definitely a better way to do this, but it worked
  sub("    ", " [0]", .) %>% 
  sub("    ", " [0]", .) %>% 
  sub("    ", " [0]", .) %>% 
  sub("    ", " [0]", .) %>% 
  sub("    ", " [0]", .) %>% 
  sub("    ", " [0]", .) %>% 
  as_tibble() %>% 
  separate(value, into = paste0("col_", 1:9), sep = " ") %>% 
  na_if("[0]") %>% 
  arrange(-row_number())

# Precautionary buffer
na_df <- tibble(col_1 = rep(NA, 100),
                col_2 = rep(NA, 100),
                col_3 = rep(NA, 100),
                col_4 = rep(NA, 100),
                col_5 = rep(NA, 100),
                col_6 = rep(NA, 100),
                col_7 = rep(NA, 100),
                col_8 = rep(NA, 100),
                col_9 = rep(NA, 100)) %>% 
  mutate(across(everything(), as.character))

og_configuration <- configuration %>% 
  bind_rows(na_df)

move_function <- function(configuration_df, move_from, move_to){
  nrow_move_from <- configuration_df[move_from] %>% drop_na() %>% nrow()
  nrow_move_to <- configuration_df[move_to] %>% drop_na() %>% nrow()
  
  moved_df <- configuration_df
  
  # Move item
  moved_df[nrow_move_to+1, move_to] <- moved_df[nrow_move_from, move_from]
  
  # Remove item
  moved_df[nrow_move_from, move_from] <- NA
  
  return(moved_df)
}


# Part one

config_pt1 <- og_configuration

# Loop through movements
for (i in 1:nrow(instructions)) {
  row_used <- instructions %>% slice(i)
  
  items_moved <- row_used %>% pull(num_move)
  col_from <- row_used %>% pull(col_from)
  col_to <- row_used %>% pull(col_to)
  
  # Move for each time it says in the row
  for (moves in 1:items_moved){
    config_pt1 <- move_function(config_pt1, col_from, col_to)
  }
}


# Return top of non NAs
for (i in 1:9){
  num_boxes <- config_pt1[i] %>% drop_na() %>% nrow()
  
  print(config_pt1[num_boxes, i])
}





# Part two

adj_move_function <- function(configuration_df, move_from, move_to, offset){
  nrow_move_from <- configuration_df[move_from] %>% drop_na() %>% nrow()
  nrow_move_to <- configuration_df[move_to] %>% drop_na() %>% nrow()
  
  nrow_move_from  <- nrow_move_from - offset + 1
  
  moved_df <- configuration_df
  
  # Move item
  moved_df[nrow_move_to+1, move_to] <- moved_df[nrow_move_from, move_from]
  
  # Remove item
  moved_df[nrow_move_from, move_from] <- "0"
  
  return(moved_df)
}


config_pt2 <- og_configuration

# Loop through movements
for (i in 1:nrow(instructions)) {
  row_used <- instructions %>% slice(i)
  
  items_moved <- row_used %>% pull(num_move)
  col_from <- row_used %>% pull(col_from)
  col_to <- row_used %>% pull(col_to)
  
  # Move for each time it says in the row
  for (moves in items_moved:1){
    config_pt2 <- adj_move_function(config_pt2, col_from, col_to, moves)
  }
  config_pt2 <- config_pt2 %>% na_if("0")
}


# Return top of non NAs
for (i in 1:9){
  num_boxes <- config_pt2[i] %>% drop_na() %>% nrow()
  
  print(config_pt2[num_boxes, i])
}







