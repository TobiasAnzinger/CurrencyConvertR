# CurrencyConvertR
CurrencyConvertR is an R package for performing currency conversions using current and historical conversion rates. This package leverages exchange rate data from 'exchangerate.host'.

# Installation
To install the package from GitHub, you can use the install_github function from the remotes package. If you haven't installed remotes, you can do so by running install.packages("remotes").

To install CurrencyConvertR, run the following:

```
remotes::install_github("TobiasAnzinger/CurrencyConvertR")
```

Then, to load the package, use:

```
library(CurrencyConvertR)
```
# Usage
## Basic Currency Conversion
The main function in this package is convert_currency(). When run without any parameters, it provides the exchange rate from EUR to USD for today:
```
convert_currency()
```

You can customize the conversion by specifying four parameters: amount, from, to, and date. Here's an example of converting 1 EUR to USD using the exchange rate for 2023-07-21:
```
convert_currency(amount = 1, from = "EUR", to = "USD", date = "2023-07-21")
```

## Historical Currency Data Retrieval
You can retrieve the historical currency data for a specified base currency using the get_historical_currency_data() function. The function retrieves data for the past year, based on the current system date. Here's an example:

```
get_historical_currency_data(base_currency = "USD")
```

## Historical Currency Data Plotting

The package also provides the plot_historical_data() function, which allows you to plot the value of a specified currency compared to a base currency over a specified time frame ("month" or "year").

```
plot_historical_data(time_frame = "month", currency = "USD", base_currency = "EUR")
```

# Support
For questions or issues, please open an issue on the GitHub repository.
