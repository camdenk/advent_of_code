library(tidyverse)
source("data_helper.R")

raw_data <- get_data(5, 2023) |> 
  httr::content(encoding = "UTF-8") |> 
  read_lines()


seeds <- str_extract_all(raw_data[1], "[\\d]+") |> 
  pluck(1)


create_map <- function(input_data) {
  input_data |> 
    tibble(input = _) |> 
    separate_wider_delim(input, " ", names = c("output_val",
                                               "input_val",
                                               "range")
    ) |> 
    mutate(across(output_val:range, as.numeric),
           max_input_val = input_val + range-1)
}

seed_to_soil_map <- raw_data[4:13] |> 
  create_map()

soil_to_fertilizer_map <- raw_data[16:31] |> 
  create_map()

fertilizer_to_water_map <- raw_data[34:48] |> 
  create_map()

water_to_light_map <- raw_data[51:95] |> 
  create_map()

light_to_temperature_map <- raw_data[98:112] |> 
  create_map()

temperature_to_humidity_map <- raw_data[115:137] |> 
  create_map()

humidity_to_location_map <- raw_data[140:150] |> 
  create_map()


find_output_id <- function(input, map) {
  
  condensed_map <- map |> 
    rowwise() |> 
    mutate(is_in_range = between(input, input_val, max_input_val)) |> 
    ungroup() |> 
    filter(is_in_range)
  
  if (nrow(condensed_map) == 1) {
    returned_val <- condensed_map |> 
      mutate(val = (input - input_val) + output_val) |> 
      pull(val)
    
    return(returned_val)
  } else {
    return(input)
  }
  
}


traverse_seed <- function(seed) {
  seed <- as.numeric(seed)
  
  returned_location <- find_output_id(seed, seed_to_soil_map) |> 
    find_output_id(soil_to_fertilizer_map) |> 
    find_output_id(fertilizer_to_water_map) |> 
    find_output_id(water_to_light_map) |> 
    find_output_id(light_to_temperature_map) |> 
    find_output_id(temperature_to_humidity_map) |> 
    find_output_id(humidity_to_location_map)
}


# Part one

results <- purrr::map(seeds, traverse_seed)

results |> 
  unlist() |> 
  min()



# Part two
# 
# 
# # Brute force takes too long, do reverse brute force
# seeds <- as.numeric(seeds)
# 
# seed_help <- function(i) {
#   seeds[i]:(seeds[i]+seeds[i+1]-1)
# }
# 
# # mins <- c()
# 
# min_loc <- Inf
# for (index in seq(1, 19, 2)) {
#   print(index)
#   
#   brute_seed <- seeds[index]
#   brute_seed_max <- brute_seed + seeds[index+1]
#   
#   while (brute_seed < brute_seed_max) {
#     loc <- traverse_seed(brute_seed)
#     if (loc < min_loc) {
#       min_loc <- loc
#     }
#     brute_seed <- brute_seed+1
#   }
#   
# }
# 
# min_loc


find_input_id <- function(input, map) {
  
  condensed_map <- map |> 
    mutate(max_output_val = output_val + range - 1) |> 
    filter(input >= output_val,
           input <= max_output_val)
  
  if (nrow(condensed_map) == 1) {
    returned_val <- condensed_map |> 
      mutate(val = (input - output_val) + input_val) |> 
      pull(val)
    
    return(returned_val)
  } else {
    return(input)
  }
  
}


traverse_seed_backwards <- function(location) {
  location <- as.numeric(location)
  
  returned_seed <- find_input_id(location, humidity_to_location_map) |> 
    find_input_id(temperature_to_humidity_map) |> 
    find_input_id(light_to_temperature_map) |> 
    find_input_id(water_to_light_map) |> 
    find_input_id(fertilizer_to_water_map) |> 
    find_input_id(soil_to_fertilizer_map) |> 
    find_input_id(seed_to_soil_map)
  
  return(returned_seed)
}


locations <- tibble(start = seeds) |> 
  mutate(range = lead(start)) |> 
  filter(row_number()%%2 == 1) |> 
  mutate(max = start + range - 1)



go <- TRUE
loc_num <- 304083216

# max num: 304083216 
# min num:   6092748

while (go) {
  
  backwards_seed <- traverse_seed_backwards(loc_num)
  
  condensed_loc <- locations |> 
    filter(backwards_seed >= start,
           backwards_seed <= max)
  
  if(nrow(condensed_loc) == 0) {
    go <- FALSE
    print(loc_num)
  }
  
  loc_num <- loc_num - 1

}

