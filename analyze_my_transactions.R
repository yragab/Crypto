
library(readr)
library(stringr)
library(dplyr)

raw_trans_set = read_csv("Data/bch_transactions.csv")

# Define Params
analysis_pair = "BCH/USD"

trans_set = raw_trans_set %>%
  filter(Pair == "BCH/USD", Type %in% c("buy", "sell")) %>%
  mutate(Rate = as.numeric(gsub(" USD", "", str_extract(Comment, "(\\d+\\.?\\d*)+\\s(USD$)")))) %>%
  mutate(CCR = ifelse(Type == "buy", Amount, -1 * Amount/Rate),
         USD = ifelse(Type == "sell", Amount, -1 * Amount * Rate - FeeAmount))
sum(trans_set$CCR)
sum(trans_set$USD)




