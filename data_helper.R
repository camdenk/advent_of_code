
# Pull data
.get_data <- function(day, year,
                      session_cookie = keyring::key_get("RStudio Keyring Secrets", 
                                                        "Advent of Code Session Cookie")) {
  aoc_url <- glue::glue("adventofcode.com/{year}/day/{day}/input")
  cookie <- httr::set_cookies(session = session_cookie)
  response <- httr::GET(aoc_url, cookie)
  return(response)
}

get_data <- memoise::memoise(.get_data)



# Functions inspired by David Robinson's adventdrob package
# (remade here so I can walk through them step by step more easily)

grid_tidy <- function(input, var, sep = "") {
  ret <- input %>%
    mutate(row = row_number()) %>%
    mutate(value = stringr::str_split({{ var }}, sep)) %>%
    select(-{{ var }}) %>%
    tidyr::unnest(value) %>%
    group_by(row) %>%
    mutate(col = row_number()) %>%
    ungroup()

  return(ret)
}





grid_graph <- function(input,
                       ...,
                       directed = FALSE,
                       mutual = FALSE,
                       circular = FALSE) {
  td <- grid_tidy(input, ...)
  dimensions <- c(max(td$col), max(td$row))
  
  tidygraph::create_lattice(dimensions,
                            directed = directed,
                            mutual = mutual,
                            circular = circular) %>%
    mutate(!!!td)
}





adjacent_join <- function(x, y = x, diagonal = FALSE, suffix = c("", "2")) {
  adj <- tibble(row_delta = c(-1, 1, 0, 0),
                col_delta = c(0, 0, -1, 1))
  
  if (diagonal) {
    adj <- bind_rows(adj,
                     tibble(row_delta = c(-1, -1, 1, 1),
                            col_delta = c(-1, 1, -1, 1)))
  }
  
  x %>%
    tidyr::crossing(adj) %>%
    mutate(row2 = row + row_delta,
           col2 = col + col_delta) %>%
    inner_join(y, by = c(row2 = "row", col2 = "col"), suffix = suffix) %>%
    filter(row != row2 | col != col2) %>%
    select(-row_delta, -col_delta)
}
