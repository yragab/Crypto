

library(ggplot2)

title = "XVG / ETH"
crypto_data = xvg_data_augmented %>% 
  mutate(change = ifelse(open < close, "up", "down")) %>%
  filter(time < 1515160000)
opportunities = crypto_data %>% filter(opportunityRuleA) %>% .$time

  
pc <- ggplot(crypto_data, aes(x = time)) +
  geom_linerange(aes(ymin = low, ymax = high)) + 
  theme_bw() +
  labs(title = title) +
  geom_rect(
    aes(xmin = time - 160 * 0.6, 
        xmax = time + 160 * 0.6, 
        ymin = pmin(open, close), 
        ymax = pmax(open, close), 
        fill = change)) + 
  guides(fill = FALSE, colour = FALSE) + 
  scale_fill_manual(values = c("down" = "red", "up" = "green")) + 
  geom_line(aes(y = high_ema_15m), color = "blue", size = 0.6) + 
  geom_vline(xintercept = opportunities, color = "purple")

pc

plotPriceAction <- function(crypto_data, title) {
  plot(crypto_data$time, crypto_data$high, type = "l", main = title)
  lines(crypto_data$time, crypto_data$close_ema_7m, type = "l", col = "red")
  lines(crypto_data$time, crypto_data$close_ema_15m, type = "l", col = "orange")
  lines(crypto_data$time, crypto_data$close_ema_30m, col = "green")
  lines(crypto_data$time, crypto_data$opportunityRuleA, col = "blue")
}

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


# Handle special case of drawing a flat bar where OHLC = Open:
# if (any(FOSL$flat_bar)) pl <- pl + geom_segment(data = FOSL[FOSL$flat_bar,], aes(x = Date - width / 2 * 0.9, y = Close, yend = Close, xend = Date + width / 2 * 0.9))
# print(pl)
