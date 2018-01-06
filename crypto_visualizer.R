

library(ggplot2)
library(quantmod)
FOSL <- getSymbols("FOSL", from="2015-01-01", auto.assign=FALSE)
names(FOSL) <- gsub("^.+\\.","",names(FOSL))  # remove "FOSL." from column names

rng <- "2015-08"
FOSL <- FOSL[rng]
FOSL <- data.frame(Date=as.POSIXct(index(FOSL)), FOSL[,1:4])

FOSL$chg <- ifelse(Cl(FOSL) > Op(FOSL), "up", "dn")
FOSL$width <- as.numeric(periodicity(FOSL)[1])
FOSL$flat_bar <- FOSL[, "High"] == FOSL[, "Low"]

# Candle chart:
pl <- ggplot(FOSL, aes(x=Date)) +
  geom_linerange(aes(ymin=Low, ymax=High)) +
  theme_bw() +
  labs(title="FOSL") +
  geom_rect(aes(xmin = Date - width/2 * 0.9, xmax = Date + width/2 * 0.9, ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = chg)) + guides(fill = FALSE, colour = FALSE) + scale_fill_manual(values = c("dn" = "darkred", "up" = "darkgreen"))

# Handle special case of drawing a flat bar where OHLC = Open:
if (any(FOSL$flat_bar)) pl <- pl + geom_segment(data = FOSL[FOSL$flat_bar,], aes(x = Date - width / 2 * 0.9, y = Close, yend = Close, xend = Date + width / 2 * 0.9))

print(pl)
