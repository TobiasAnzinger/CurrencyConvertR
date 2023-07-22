#' Get Historical Currency Data
#'
#' This function retrieves historical exchange rate data for a specified base currency from exchangerate.host.
#' The function retrieves data for the past year, based on the current system date.
#'
#' @param base_currency A character string specifying the ISO code of the base currency for which to retrieve data. Default is "EUR".
#'
#' @return Returns a data frame that contains exchange rate data for the specified base currency over the past year. The data frame includes a date column,
#' a currency_data column that specifies the currency to which the base currency is compared, and a value column that gives the exchange rate.
#' If the API request fails, the function will stop and print an error message.
#'
#' @import httr
#' @import jsonlite
#' @importFrom lubridate years
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' get_historical_currency_data(base_currency = "USD")
#' get_historical_currency_data() # defaults to "EUR"
#' }
get_historical_currency_data <- function(base_currency = "EUR") {
  url <- paste0('https://api.exchangerate.host/timeseries?start_date=', Sys.Date() - lubridate::years(1), '&end_date=', Sys.Date(), '&base=', base_currency)
  req <- httr::GET(url)

  if (req$status_code != 200){
    stop("API Request failed")
  }

  char <- rawToChar(req$content)
  hist_data <- jsonlite::fromJSON(char)
  hist_data <- hist_data$rates

  df <- purrr::map_dfr(hist_data, ~data.frame(currency_data = names(.x), value = unlist(.x)), .id = "date")
  df$date <- as.Date(df$date)

  return(df)
}




#' Plot Historical Currency Data
#'
#' This function retrieves historical exchange rate data from get_historical_currency_data function and plots it over the specified timeframe.
#' The function allows the user to choose a specific currency and the base currency for comparison. If the base currency is not specified, it defaults to "EUR".
#' The user can specify the time frame as "month" for the last 30 days or "year" for the past year.
#'
#' @param time_frame A character string specifying the timeframe over which to plot data. This must be either "month" or "year".
#' @param currency A character string specifying the ISO code of the currency to plot (e.g. "USD").
#' @param base_currency A character string specifying the ISO code of the base currency to which the other currency is compared. Defaults to "EUR" if not specified.
#'
#' @return Returns a ggplot2 object, which is a line plot of the currency's value over the specified timeframe.
#' If the provided currency or base currency is not valid, or if the provided timeframe is not "month" or "year",
#' the function will stop and print an error message.
#'
#' @importFrom ggplot2 ggplot aes geom_line ggtitle xlab ylab theme_minimal
#' @importFrom dplyr filter select distinct
#' @importFrom lubridate days years
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{get_historical_currency_data}} for the function used to retrieve the historical exchange rate data.
#'
#' @examples
#' \dontrun{
#' plot_historical_data(time_frame = "month", currency = "USD")
#' plot_historical_data(time_frame = "year", currency = "USD", base_currency = "GBP")
#' }
plot_historical_currency_data <- function(time_frame, currency, base_currency = "EUR") {

  df <- get_historical_currency_data(base_currency)

  currency_list <- df %>% dplyr::select(currency_data) %>% dplyr::distinct()

  if(any(currency_list == base_currency) && any(currency_list == currency)) {

    if(time_frame == "month") {
      last_30_days <- df %>% dplyr::filter(date >= Sys.Date() - days(30), currency_data == currency)

      ggplot2::ggplot(last_30_days, aes(x = date, y = value)) +
        geom_line() +
        ggtitle(paste0(currency, " value over last 30 days (compared to ", base_currency, ")")) +
        xlab("Date") +
        ylab("Value")+
        theme_minimal()

    } else if(time_frame == "year"){

      last_year <- df %>% dplyr::filter(date >= Sys.Date() - years(1), currency_data == currency)
      ggplot2::ggplot(last_year, aes(x = date, y = value)) +
        geom_line() +
        ggtitle(paste0(currency, " value over last year (compared to ", base_currency, ")")) +
        xlab("Date") +
        ylab("Value") +
        theme_minimal()

    } else {
      stop("Please provide a correct time frame \"month\" or \"year\"")
    }
  } else {
    stop("One of the provided currency formats is not valid. Please use the ISO 4217 codes from the code colum here https://www.iban.com/currency-codes")
  }
}
