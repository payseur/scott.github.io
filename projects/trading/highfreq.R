
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
library(viridis)

tiingo_key <- "95801e1e7b29d26a20d32e1bb8b47a0c91afa947"
tiingo_api_key(tiingo_key)


textTime2Min <- function(time_){
  hour_mins = (as.numeric(substr(time_,1,2)) - 9) * 60
  min_mins = as.numeric(substr(time_,4,5))
  return(hour_mins + min_mins - 30)
}



dat <- tq_get(c("TSLA"),
               get = "tiingo.iex",
               from   = "2022-01-01",
               to     = "2022-06-30",
               resample_frequency = "10min")


dat1 <- tq_get(c("TSLA"),
              get = "tiingo.iex",
              from   = "2022-01-01",
              to     = "2022-12-31",
              resample_frequency = "10min")

dat <- dat1 %>% bind_rows(dat)
dat <- dat %>% 
  mutate(spread = high - low) %>% 
  mutate(close_plot = high - close) %>% 
  mutate(date_ = lubridate::mdy(format(as.POSIXct(date,"America/New_York"), format = "%m/%d/%Y"))) %>% 
  mutate(mins = format(as.POSIXlt(date,"America/New_York"), format = "%H:%M") %>% textTime2Min(.) ) %>% 
  mutate(mon_num = month(date_)) %>% 
  mutate(mon_text = format(date_, "%h")) %>% 
  mutate(spread_adj = (spread-mean(spread))/STDEV(spread)) %>% 
  mutate(year_ = year(date_)) %>% 
  mutate(year_text = as.character(year_)) %>% 
  mutate(day_ = day(date_)) 
  


dat %>%
ggplot(aes(x=day_,y=mins, fill=close_plot)) +  
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="H")+ 
  facet_grid(year_text~mon_num)+
  theme_minimal(base_size = 8) +
  scale_y_continuous(trans = "reverse", breaks = unique(dat$mins)) +
  theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  ggExtra::removeGrid()


dat %>%
  ggplot(aes(y=spread,x=mins, color=date)) +
#  facet_wrap(~mon_num)+
  geom_point(size=.1) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),se=FALSE) +
  geom_smooth(dat, mapping=aes(y=close_plot, x=mins), method = lm, formula = y ~ splines::bs(x, 3),se=FALSE, color="darkblue") +
  scale_y_continuous(trans = "reverse", breaks = unique(dat$mins)) +
  coord_polar()
  
