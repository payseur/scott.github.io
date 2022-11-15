
library(ggplot2)
library(dplyr)
library(lubridate)
library(googlesheets4)
library(formattable)
library(kableExtra)
library(dplyr)
library(quantmod)
library(tidyquant)
library(purrr)



dat <- read_sheet("1hMewH2nvYKniqzcS1CUQof0JgX--Ezvt9O93k9kzYjY")


vix <- tq_get(dat$ticker[[1]],
              from = "1990-01-01")


install.packages("quantmod")
install.packages("tidyquant")

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


PE_Ratio <- Quandl("MULTPL/SP500_PE_RATIO_MONTH") 
Shiller_PE_Ratio <- Quandl("MULTPL/SHILLER_PE_RATIO_MONTH")

Shiller_PE_Ratio %>% ggplot(aes(x=Date, y=Value)) + geom_line()
