library(tidyverse)
source("data_helper.R")

raw_data <- get_data(10, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

# Try to test out extract() for this as well
data <- tibble(data = raw_data) %>% 
  separate(data, c("direction", "v_change"), convert = TRUE, sep = " ") %>% 
  mutate(v_change = replace_na(v_change, 0),
         v_change = if_else(row_number() == 1, v_change+1L, v_change))

# Part one
data %>% 
  mutate(cycle_value = if_else(direction == "noop", 1, 2),
         total_cycles = cumsum(cycle_value),
         total_cycles = total_cycles +1, # Adding one to all cycles helps line up the "end of cycle value" and "during cycle value"
         v_value = cumsum(v_change)) %>% 
  filter(total_cycles %in% seq(20, 220, by = 40) | 
           (total_cycles %in% (seq(20, 220, by = 40) + 1) & direction == "addx")) %>% 
  mutate(cycle_v_value = if_else(total_cycles %% 4 != 0, v_value - v_change, v_value),
         cur_cycle = if_else(total_cycles %% 4 != 0, total_cycles - 1, total_cycles),
         sum_value = cur_cycle * cycle_v_value)  %>% 
  pull(sum_value) %>% 
  sum()


# Part two
directions_tibble <- tibble(total_cycles = as.double(1:240))

formatted_data <- data %>% 
  mutate(cycle_value = if_else(direction == "noop", 1, 2),
         total_cycles = cumsum(cycle_value),
         v_value = cumsum(v_change),
         intra_cycle_v_value = lag(v_value),
         intra_cycle_v_value = if_else(row_number() == 1, 1L, intra_cycle_v_value))

part_two_df <- directions_tibble %>% 
  left_join(formatted_data, by = "total_cycles") %>% 
  fill(intra_cycle_v_value, .direction = "up") %>% 
  rowwise() %>% 
  mutate(lit_up = if_else(between((total_cycles %% 40)-1, intra_cycle_v_value -1, intra_cycle_v_value+1),
                          "#", "."))

answer <- c(
part_two_df[1:40, "lit_up"] %>% as_vector() %>% unname() %>% paste0(collapse = ""),
part_two_df[41:80, "lit_up"] %>% as_vector() %>% unname() %>% paste0(collapse = ""),
part_two_df[81:120, "lit_up"] %>% as_vector() %>% unname() %>% paste0(collapse = ""),
part_two_df[121:160, "lit_up"] %>% as_vector() %>% unname() %>% paste0(collapse = ""),
part_two_df[161:200, "lit_up"] %>% as_vector() %>% unname() %>% paste0(collapse = ""),
part_two_df[201:240, "lit_up"] %>% as_vector() %>% unname() %>% paste0(collapse = "")
)
