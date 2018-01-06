
library(jsonlite)
library(TTR)
library(dplyr)


# CONFIG
TIME_CONSTANTS = c(7, 15, 30, 60)

# Generate URL
getHistoMinuteURL <- function(crypto) {
  URL = 
    sprintf("https://min-api.cryptocompare.com/data/histominute?fsym=%s&tsym=BTC&aggregate=3&e=Binance",
            crypto)
  return(URL)
}

# Compute the change over a time constant  
computeChange <- function(time_series, time_constant) {
  d = diff(time_series, time_constant)
  f = time_series[1:length(d)]
  c = round(100 * d / f, 2)
  c = c(rep(0, length(time_series) - length(c)), c)
  return(c)
}

# Compute change over a time constant features 
computeChangeFeatures <- function(time_series, feature_name) {
  # Compute change features
  change_features_temp = lapply(TIME_CONSTANTS, function(x) {
    computeChange(time_series, x)
  })
  # Rename features
  change_features = as.data.frame(
    change_features_temp,
    col.names = paste(feature_name, "_", TIME_CONSTANTS, "m", sep = ""))
  # Return
  return(change_features)
}

# Compute EMA features
computeEMAFeatures <- function(time_series, feature_name) {
  # Compute change features
  ema_features_temp = lapply(TIME_CONSTANTS, function(x) {
    EMA(time_series, x, wilder = FALSE, ratio = NULL)
  })
  # Rename features
  ema_features = as.data.frame(
    ema_features_temp,
    col.names = paste(feature_name, "_", TIME_CONSTANTS, "m", sep = ""))
  # Return
  return(ema_features)
}

# Compute change over a time constant features 
computeAllChangeFeatures <- function(crypto_data) {
  # Compute change of highs over preset time constants
  high_change_features = computeChangeFeatures(crypto_data$high, "high_change")
  # Compute change of lows over preset time constants
  low_change_features = computeChangeFeatures(crypto_data$low, "low_change")
  # Merge change features
  change_features = cbind(high_change_features, low_change_features)
  return(change_features)
}

# Compute all EMA features
computeAllEMAFeatures <- function(crypto_data) {
  # Compute change of highs over preset time constants
  high_EMA_features = computeEMAFeatures(crypto_data$high, "high_ema")
  # Compute change of lows over preset time constants
  low_EMA_features = computeEMAFeatures(crypto_data$low, "low_ema")
  # Compute change of highs over preset time constants
  close_EMA_features = computeEMAFeatures(crypto_data$close, "close_ema")
  # Merge change features
  ema_features = cbind(high_EMA_features, low_EMA_features, close_EMA_features)
  return(ema_features)
}

computeAllEMAChangeFeatures <- function(crypto_data) {
  # Compute change for EMA features
  ema_change_features = mapply(
    function(x, y) { 
      computeChange(x, y)
    }, 
    as.list(crypto_data), TIME_CONSTANTS, SIMPLIFY = FALSE)
  # Name features
  names(ema_change_features) =
    paste(names(ema_change_features), "_change_", TIME_CONSTANTS, "m", sep = "")
  # Return
  return(ema_change_features)
}

# Compute Change rates ad different intervals
computeFeatures <- function(crypto_data) {
  change_features = computeAllChangeFeatures(crypto_data)
  ema_features = computeAllEMAFeatures(crypto_data)
  ema_change_features = computeAllEMAChangeFeatures(ema_features)
  crypto_data = cbind(crypto_data, change_features, ema_features, ema_change_features)
  return(crypto_data)
}

# A rule that identifies buying opportunities 
opportunityRuleA <- function(augmented_crypto_data) {
  augmented_crypto_data = augmented_crypto_data %>%
    mutate(opportunityRuleA = 
             (high_change_7m > 0 &
                high_change_15m > 1 & low_change_15m > 1 &
                high_change_30m > 1 & low_change_30m > 1 & 
                high_ema_15m > high_ema_30m &
                high_ema_15m_change_15m > 1))
  return(augmented_crypto_data)
}

# Run all opportunity identification rules
identifyBuyOpportunities <- function(augmented_crypto_data) {
  augmented_crypto_data = augmented_crypto_data %>%
    opportunityRuleA()
  return(augmented_crypto_data)
}

# Plot the high of every interval 
plotPriceAction <- function(crypto_data, title) {
  plot(crypto_data$time, crypto_data$high, type = "l", main = title)
  lines(crypto_data$time, crypto_data$close_ema_7m, type = "l", col = "red")
  lines(crypto_data$time, crypto_data$close_ema_15m, type = "l", col = "orange")
  lines(crypto_data$time, crypto_data$close_ema_30m, col = "green")
  lines(crypto_data$time, crypto_data$opportunityRuleA, col = "blue")
}


# Verge
xvg_response = fromJSON(getHistoMinuteURL("XVG"))
xvg_data = xvg_response$Data
xvg_data_augmented = computeFeatures(xvg_data)
xvg_data_augmented = identifyBuyOpportunities(xvg_data_augmented)
plotPriceAction(xvg_data_augmented, "XVG / ETH")

# Cardano
ada_response = fromJSON(getHistoMinuteURL("ADA"))
ada_data = ada_response$Data
ada_data_augmented = computeFeatures(ada_data)
ada_data_augmented = identifyBuyOpportunities(ada_data_augmented)
plotPriceAction(ada_data_augmented, "ADA / ETH")

# NULS
nuls_response = fromJSON(getHistoMinuteURL("NULS"))
nuls_data = nuls_response$Data
nuls_data_augmented = computeFeatures(nuls_data)
nuls_data_augmented = identifyBuyOpportunities(nuls_data_augmented)
plotPriceAction(nuls_data_augmented, "NULS / ETH")








