library(jsonlite)
library(httr)


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
