library(jsonlite)
library(httr)


.cache_env <- new.env()


#' Update conversion rates
#'
#' This internal function updates the conversion rates by requesting data from
#' "https://api.exchangerate.host/". The data is stored in an environment for
#' future quick access and to avoid calling the api multible times with the same
#' GET request.
#'
#' @import httr
#' @import jsonlite
#'
#' @param date The date for which conversion rates are required.
#'
#' @return JSON object of conversion rates for the date. If request fails, NULL is returned and message is printed.
#'
#' @examples
#' \dontrun{
#' .update_conversion_rates('2023-07-21')
#' }
.update_conversion_rates <- function(date) {
  url <- paste("https://api.exchangerate.host/", date, sep = "")
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    json_data <- httr::content(response, "text", encoding = "UTF-8")
    parsed_data <- jsonlite::fromJSON(json_data)
    return(parsed_data)
  } else {
    print("Failed to get conversion rates")
    return(NULL)
  }
}

#' Get conversion rate
#'
#' This internal function retrieves the conversion rate from one currency to another for a specific date.
#' If the base currency is not "EUR", the conversion rate is calculated using EUR as an intermediary.
#'
#' @param from The currency to convert from.
#' @param to The currency to convert to.
#' @param date The date for which conversion rates are required.
#'
#' @return Conversion rate from 'from' currency to 'to' currency.
#'
#' @examples
#' \dontrun{
#' .get_conversion_rate('EUR', 'USD', '2023-07-21')
#' .get_conversion_rate('CAD', 'CHF', '2023-05-07')
#' }
.get_conversion_rate <- function(from, to, date) {
  if(from == "EUR") {
    return(.cache_env[[date]][["rates"]][[to]])
  } else {
    return(1 / .cache_env[[date]][["rates"]][[from]] * .cache_env[[date]][["rates"]][[to]])
  }
}


#' Currency is valid
#'
#' This internal function checks if the provided currencies are in a list of provided
#' allowed_currencies. If not an exception is thrown.
#'
#' @param currencies currencys to check
#' @param allowed_currencies allowed currencys
#'
#' @return TRUE if its valid else an error is thrown
#'
#' @examples
#' \dontrun{
#' .currency_is_valid(c("EUR", "USD"), c("EUR", "CAD", "USD"))
#' }
.currency_is_valid <- function(currencies, allowed_currencies) {
  if(all(currencies %in% names(allowed_currencies))){
    return(TRUE)
  } else {
    stop("one of the provided currency formats is not valid. Please use the ISO 4217 codes from the Code colum here https://www.iban.com/currency-codes")
    }
}



#' Convert currency
#'
#' This function converts an amount from one currency to another using a conversion rate from a specific date.
#' It uses an environment to store conversion rates to avoid multiple requests for the same date.
#' If the conversion rates for the date are not available in the environment, they are retrieved using .update_conversion_rates().
#'
#' @param amount The amount to convert. Default is 1.
#' @param from The currency to convert from. Default is "EUR".
#' @param to The currency to convert to. Default is "USD".
#' @param date The date for which conversion rates are required. Default is the current date.
#'
#' @return Converted amount from 'from' currency to 'to' currency (numeric).
#'
#' @examples
#' \dontrun{
#' convert_currency()
#' convert_currency(to = 'CHF')
#' convert_currency(amount = 100, from = 'USD', to = "CAD")
#' convert_currency(amount = 123, from = 'EUR', to = 'USD', date = '2023-07-21')
#' convert_currency(amount = 123, from = 'CHF', to = 'CAD', date = '2023-04-12')
#' }
convert_currency <- function(amount = 1, from = "EUR", to = "USD", date = Sys.Date()) {
  tryCatch({date <- format(date, "%Y-%m-%d")}, error = function(e){})
  if (exists(date, envir = .cache_env)){
    parsed_data <- get(date, envir = .cache_env)
  } else {
    parsed_data <- .update_conversion_rates(date)
    assign(date, parsed_data, envir = .cache_env)
  }
  .currency_is_valid(c(from, to), .cache_env[[date]][["rates"]])

  result <- .get_conversion_rate(
    from = from,
    to = to,
    date = date
  )
  return(amount * result)
}
