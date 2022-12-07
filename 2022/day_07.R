library(tidyverse)
source("data_helper.R")

# Pulled the cd_func and accumulate functions from David Robinson's solution
# https://twitter.com/drob/status/1600360338498764800?s=20&t=-unjfX807DHNiY0BIImmJQ

raw_data <- get_data(7, 2022) %>% 
  httr::content(encoding = "UTF-8") %>% 
  read_lines()

cd_func <- function(path, dir = NA) {
  if (any(is.na(dir))) return(path)
  if (any(dir == "..")) return(head(path, -1))
  return(c(path, paste0(tail(path, 1), "/", dir)))
}

data <- tibble(values = raw_data) 

# Part one
formatted_data <- data %>% 
  mutate(path = values %>% 
           str_extract("cd (.*)") %>% 
           str_remove("cd "),
         path = accumulate(path, cd_func)
  ) %>% 
  unnest(path)

listed_files <- formatted_data %>% 
  filter(str_detect(values, "^[0-9]")) %>% 
  group_by(path) %>% 
  summarize(size = values %>%
              str_extract("^[0-9]+") %>%
              as.numeric() %>%
              sum()
  ) %>% 
  arrange(-size)

listed_files %>% 
  filter(size < 100000) %>% 
  pull(size) %>% 
  sum()


# Part two
total <- 70000000
required <- 30000000
used <- listed_files %>% slice_max(size, n = 1) %>% pull(size)
available <- total-used


listed_files %>% 
  filter(size >= (required - available)) %>% 
  arrange(size)

