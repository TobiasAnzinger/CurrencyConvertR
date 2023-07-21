library(httr)
library(lubridate)
library(ggplot2)
library(tidyr)


#' Plot Historical Currency Data
#'
#' This function retrieves historical exchange rate data from exchangerate.host and plots it over the specified timeframe.
#' The function allows the user to choose a specific currency and the base currency for comparison. If the base currency is not specified, it defaults to "EUR".
#' The user can specify the time frame as "month" for the last 30 days or "year" for the past year.
#'
#' @param time_frame Character string, the timeframe over which to plot data. It must be either "month" or "year".
#' @param currency Character string, the ISO code of the currency to plot (e.g. "USD").
#' @param base_currency Character string, the ISO code of the base currency to which the other currency is compared. Default is "EUR".
#'
#' @return A ggplot2 object, which is a line plot of the currency's value over the specified timeframe. If the API request fails, the function will return NULL and print an error message.
#'
#' @import httr
#' @import jsonlite
#' @importFrom lubridate years
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_longer unnest_wider
#' @importFrom tibble enframe
#' @importFrom ggplot2 ggplot aes geom_line ggtitle xlab ylab theme_minimal
#' @importFrom purrr %>% set_names
#'
#' @examples
#' \dontrun{
#' plot_historical_data(time_frame = "month", currency = "USD")
#' plot_historical_data(time_frame = "year", currency = "USD", base_currency = "GBP")
#' }
plot_historical_data <- function(time_frame, currency, base_currency = "EUR") {
  url <- paste0('https://api.exchangerate.host/timeseries?start_date=', Sys.Date() - lubridate::years(1), '&end_date=', Sys.Date(), '&base=', base_currency)
  req <- httr::GET(url)

  if (req$status_code != 200){
    print("API Request failed")
    return(NULL)
  }

  char <- rawToChar(req$content)
  hist_data <- jsonlite::fromJSON(char)
  hist_data <- hist_data$rates

  df <- hist_data %>%
    enframe(name = "date", value = "currencies") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    tidyr::unnest_wider(currencies) %>%
    tidyr::pivot_longer(cols = -date, names_to = "currencyData", values_to = "value")

  currency_list <- df %>% dplyr::select(currencyData) %>% deplyr::distinct()

  if(any(currency_list == base_currency) && any(currency_list == currency)) {

    if(time_frame == "month") {
      last_30_days <- df %>% filter(date >= Sys.Date() - days(30),currencyData == currency)

      ggplot2::ggplot(last_30_days, aes(x = date, y = value)) +
        geom_line() +
        ggtitle(paste0(currency, " value over last 30 days (compared to ", base_currency, ")")) +
        xlab("Date") +
        ylab("Value")+
        theme_minimal()

    } else if(time_frame == "year"){

      last_year <- df %>% filter(date >= Sys.Date() - years(1), currencyData == currency)
      ggplot2::ggplot(last_year, aes(x = date, y = value)) +
        geom_line() +
        ggtitle(paste0(currency, " value over last year (compared to ", base_currency, ")")) +
        xlab("Date") +
        ylab("Value") +
        theme_minimal()

    } else {
      stop('Please provide a correct time frame \"month\" or \"year\"')
    }
  } else {
    stop("One of the provided currency formats is not valid. Please use the ISO 4217 codes from the code colum here https://www.iban.com/currency-codes")
  }
}
