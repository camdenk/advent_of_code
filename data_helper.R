
.get_data <- function(day, year,
                      session_cookie = keyring::key_get("RStudio Keyring Secrets", 
                                                        "Advent of Code Session Cookie")) {
  aoc_url <- glue::glue("adventofcode.com/{year}/day/{day}/input")
  cookie <- httr::set_cookies(session = session_cookie)
  response <- httr::GET(aoc_url, cookie)
  return(response)
}

get_data <- memoise::memoise(.get_data)