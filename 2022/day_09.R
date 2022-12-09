library(tidyverse)
source("data_helper.R")

# Drew inspiration for follow function from David Robinson's solution
# https://twitter.com/drob/status/1601235202789232648/photo/1


follow <- function(tail, head){
  if (max(abs(head-tail)) <= 1){
    return (tail)
  }
  
  map2_dbl(tail, head, ~ .x + sign(.y - .x))
}

raw_data <- get_data(9, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

data <- tibble(data = raw_data) %>% 
  separate(data, c("direction", "steps"), convert = TRUE) %>% 
  uncount(steps)

movement_df <- tibble(direction = c("R", "L", "U", "D"),
                      x_val = c(1, -1, 0, 0),
                      y_val = c(0, 0, 1, -1))


# Part one
data %>%
  left_join(movement_df) %>% 
  mutate(head_x = cumsum(x_val),
         head_y = cumsum(y_val),
         head = map2(head_x, head_y, c),
         tail = accumulate(head, follow, .init = c(0,0))[-1]) %>% 
  hoist(tail, tail_x = 1, tail_y = 2) %>%
  distinct(tail_x, tail_y) %>% 
  nrow()

# Part two

pt2_data <- data %>%
  left_join(movement_df) %>% 
  mutate(head_x = cumsum(x_val),
         head_y = cumsum(y_val),
         head = map2(head_x, head_y, c),
         tail1 = accumulate(head, follow, .init = c(0,0))[-1])

for (i in 2:9){
  pt2_data[[paste0("tail", i)]] = accumulate(pt2_data[[paste0("tail", i-1)]],
                                           follow, .init = c(0,0))[-1]
}


pt2_data %>% 
  hoist(tail9, tail_x = 1, tail_y = 2) %>%
  distinct(tail_x, tail_y) %>% 
  nrow()