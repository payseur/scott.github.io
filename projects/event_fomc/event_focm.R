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
library(Quandl)
library(PerformanceAnalytics)




dates_num <- c(
  #20180321, 20180613, 20180926, 20181219, 
  #  20190320, 20190619, 20190918, 20191211,
  #             20200610, 20200916, 20201216,
  20210317, 20210616, 20210922, 20211215,
  20220316, 20220615, 20220921, 20221214,
  20230322)

#dat <- read_sheet("1hMewH2nvYKniqzcS1CUQof0JgX--Ezvt9O93k9kzYjY")

# Get the list of `get` options
tq_get_options()


tiingo_key <- "95801e1e7b29d26a20d32e1bb8b47a0c91afa947"
tiingo_api_key(tiingo_key)

dates_<- ymd(dates_num)
org_tbl <- tibble(date_ = dates_)  %>% 
  mutate(event_type = "fomc") %>% 
  mutate(event_time = "t") 

org_tbl_tm1 <- org_tbl %>% 
  mutate(date_ = dates_ -1) %>% 
  mutate(event_time = "tm1")

org_tbl_tp1 <- org_tbl %>% 
  mutate(date_ = dates_ +1) %>% 
  mutate(event_time = "tp1")

org_tbl_tp2 <- org_tbl %>% 
  mutate(date_ = dates_ -1) %>% 
  mutate(event_time = "tp2")

event_tbl <- org_tbl_tm1 %>% bind_rows(org_tbl, org_tbl_tp1,org_tbl_tp2)


query_dates <- org_tbl %>% filter(event_time %in% c("t", "tp1"))
for(i in 1:nrow(query_dates)){
  
  market_data <- tq_get(c("SPY"),
                   get = "tiingo.iex",
                   from   = query_dates$date_[[i]],
                   to     = query_dates$date_[[i]],
                   resample_frequency = "5min")
  market_data <- market_data %>% 
    rename(market_date = date) %>% 
    mutate(date_ = query_dates$date_[[i]]) %>% 
    mutate(time_rank = rank(market_date)) %>% 
    mutate(rets = close/lag(close) - 1) %>%
    mutate(rets = ifelse(is.na(rets), 0.0, rets)) %>% 
    mutate(cumrets = cumprod(rets+1) - 1) %>% 
    select(symbol, market_date, time_rank, close, volume, date_, rets, cumrets)

  if(i == 1){
    total_event_tbl <- event_tbl %>% 
      right_join(market_data, by="date_", multiple="all")
  }else{
    total_event_tbl <- total_event_tbl %>% bind_rows(event_tbl %>% 
      right_join(market_data, by="date_", multiple="all"))
  }
  
}
  
total_event_tbl %>% ggplot(aes(x=time_rank, y=cumrets, color=factor(date_)))+geom_path()

  