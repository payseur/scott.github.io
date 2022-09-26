install.packages("quantmod")
install.packages("tidyquant")
library(quantmod)
library(tidyquant)

library(ggplot2)

# Get the list of `get` options
tq_get_options()


tiingo_key <- "95801e1e7b29d26a20d32e1bb8b47a0c91afa947"
tiingo_api_key(tiingo_key)

test <- tq_get(c("SPY"),
       get = "tiingo.iex",
       from   = "2020-01-01",
       to     = "2020-01-15",
       resample_frequency = "5min")

# Get stock prices for a stock from Yahoo
vix <- tq_get("^vix")
vix %>%ggplot(aes(x=date, y=close)) + geom_line()

move <- tq_get("^move")
move %>% ggplot(aes(x=date, y=close)) + geom_line()

vols <- rbind(vix, move)

vols %>% ggplot(aes(x=date, y=close, color=symbol)) + geom_line()

test  %>% ggplot(aes(x=date, y=close))+geom_point()
