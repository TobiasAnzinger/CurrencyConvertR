library(jsonlite)
library(httr)

# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

.cache_env <- new.env()


.update_conversion_rates <- function(date) {
  url <- paste("https://api.exchangerate.host/", date, sep = "")
  response <- GET(url)
  if (status_code(response) == 200) {
    json_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(json_data)
    return(parsed_data)
  } else {
    print("Failed to get conversion rates")
    return(NULL)
  }
}

.get_conversion_rate <- function(from, to, date) {
  if(from == "EUR") {
    return(.cache_env[[date]][["rates"]][[to]])
  } else {
    return(1 / .cache_env[[date]][["rates"]][[from]] * .cache_env[[date]][["rates"]][[to]])
  }
}


convert_currency <- function(ammount = 1, from = "EUR", to = "USD", date = Sys.Date()) {
  tryCatch({date <- format(date, "%Y-%m-%d")}, error = function(e){})
  if (exists(date, envir = .cache_env)){
    parsed_data <- get(date, envir = .cache_env)
  } else {
    parsed_data <- .update_conversion_rates(date)
    assign(date, parsed_data, envir = .cache_env)
  }
  result <- .get_conversion_rate(
    from = from,
    to = to,
    date = date
  )
  return(result * ammount)
}

convert_currency(ammount = 1, date = "2023-04-04")
convert_currency(ammount = 1, date = "2023-05-06")
convert_currency(ammount = 1, date = "2023-06-06")
convert_currency(ammount = 1, from = "CHF", to = "USD")
convert_currency(ammount = 1, from = "CAD", to = "USD")



