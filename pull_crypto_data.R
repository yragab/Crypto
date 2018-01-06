
library(jsonlite)
library(data.table)

# View a list of all cryptos
response = fromJSON('https://www.cryptocompare.com/api/data/coinlist')
cryptos = rbindlist(response$Data, fill = TRUE)
View(cryptos)

# Price of a single coin
response = fromJSON('https://min-api.cryptocompare.com/data/price?fsym=LTC&tsyms=USDT&e=Binance')

# Prices of multiple coins
response = fromJSON('https://min-api.cryptocompare.com/data/pricemulti?fsyms=XVG,ADA&tsyms=BTC&e=Binance')
response = fromJSON('https://min-api.cryptocompare.com/data/pricemulti?fsyms=ETH,DASH,NXT&tsyms=BTC,USD&e=Poloniex')

# Details of multiple coins
response = fromJSON('https://min-api.cryptocompare.com/data/pricemultifull?fsyms=XVG,ADA&tsyms=BTC&e=Binance')

# Top currencies exhanged to from a given currency
response = fromJSON('https://min-api.cryptocompare.com/data/top/pairs?fsym=BTC&limit=20')

# Details per minute
xvg_response = fromJSON('https://min-api.cryptocompare.com/data/histominute?fsym=XVG&tsym=BTC&aggregate=3&e=Binance')
ada_response = fromJSON('https://min-api.cryptocompare.com/data/histominute?fsym=ADA&tsym=BTC&aggregate=3&e=Binance')



# Basic data investigation
plotHighs <- function(crypto_data) {
  plot(crypto_data$time, crypto_data$high)
}

computeDiffHighs <- function(crypto_data, diff_interval_min) {
  d = diff(crypto_data$high, diff_interval_min)
  f = crypto_data$high[1:length(d)]
  c = round(100 * d / f, 2)
  c = c(rep(0, nrow(crypto_data) - length(c)), c)
  return(c)
}

processCryptoData <- function(crypto_data) {
  plotHighs(crypto_data)
  c_5 = computeDiffHighs(crypto_data, 5)
  c_15 = computeDiffHighs(crypto_data, 15)
  c_30 = computeDiffHighs(crypto_data, 30)
  c_60 = computeDiffHighs(crypto_data, 60)
  crypto_data = cbind(crypto_data, c_5, c_15, c_30, c_60)
  View(crypto_data)
  return(crypto_data)
}

vrg_data = processCryptoData(xvg_response$Data)
ada_data = processCryptoData(ada_response$Data)
bcc_response = fromJSON('https://min-api.cryptocompare.com/data/histominute?fsym=BCCOIN&tsym=USD&aggregate=3')
bcc_data = processCryptoData(bcc_response$Data)




diff(diff(bcc_data$high, 5), 5)



