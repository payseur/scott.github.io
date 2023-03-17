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
library(xts)

tlt <- tq_get("tlt", from="2000-01-01")
tlt %>%ggplot(aes(x=date, y=adjusted)) + geom_line()

#
# Convert to xts
#
tlt_xts <- xts(tlt$adjusted, order.by=ymd(tlt$date))
tlt_ret_xts <- PerformanceAnalytics::Return.calculate(tlt_xts)
tlt_ret_xts[is.na(tlt_ret_xts)] <- 0.0
dimnames(tlt_ret_xts)[[2]] <- "TLT"
PerformanceAnalytics::table.CalendarReturns(tlt_ret_xts)


tlt_q <- to.quarterly(tlt_xts)
barp <- (tlt_q$tlt_xts.Close/tlt_q$tlt_xts.Open) - 1
tlt_tbl <- data.frame(date_ = index(barp), rets = as.numeric(barp[,1])) %>% as.tibble()

tlt_q <- to.yearly(tlt_xts)
(tlt_q$tlt_xts.Close - tlt_q$tlt_xts.Open)/tlt_q$tlt_xts.Open

tlt_tbl %>% ggplot(aes(y=rets)) + geom_bar()



tlt_tbl %>% mutate(ind = rank(rets)) %>% ggplot(aes(x=ind, y=rets)) + geom_point()
tlt_tbl %>% mutate(ind = rank(rets)) %>% ggplot(aes(x=ind, y=rets)) + 
  geom_bar(stat= "identity") +
  theme_bw() 


library(ggrepel)

#
#  Yearly
#
symbol = "AGG"
name = "AGG"
from = "2000-01-01"
to = "2023-01-01"
tlt <- tq_get(symbol, from=from, to = to)
tlt_xts <- xts(tlt$adjusted, order.by=ymd(tlt$date))
tlt_ret_xts <- PerformanceAnalytics::Return.calculate(tlt_xts)
dimnames(tlt_ret_xts)[[2]] <- symbol
tlt_q <- to.yearly(tlt_xts)
barp <- (tlt_q$tlt_xts.Close/tlt_q$tlt_xts.Open) - 1

tlt_tbl <- data.frame(date_ = index(barp), rets = as.numeric(barp[,1])) %>%
  as.tibble()

tlt_tbl <- tlt_tbl %>%  
  mutate(ind = rank(rets)) %>% 
  mutate(label_ = as.character(year(date_))) %>% 
  mutate(label_used = if_else(year(date_) %in% c(2022, 2008), paste(as.character(year(date_)), "\n ", round(rets*100,1), "%"),  NA)) %>% 
  mutate(rets_for_color = if_else(!is.na(label_used) , 1, 0)) %>% 
  mutate(label_used_neg = if_else((!is.na(label_used) & (rets <= 0)), label_used, NA)) %>% 
  mutate(label_used_pos = if_else((!is.na(label_used) & (rets > 0)), label_used, NA))
 


tlt_tbl %>% 
  ggplot(aes(x=ind, y=rets, fill = rets_for_color)) + 
  geom_bar(stat= "identity") +
  theme_bw() +
#  geom_label_repel(aes(label=label_used_pos), size=2, data=tlt_tbl, y=-0.02) +
#  geom_label_repel(aes(label=label_used_neg), size=2, data=tlt_tbl, nudge_x=1, nudge_y = 2, y=0) +
  #geom_label(aes(label = "scott.payseur.phd@gmail.com", x=20, y= -.1), size=3) +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = NULL)  + 
  geom_label(aes(label = label_used_neg, y =  0.05), position = position_dodge(0.9), size=2.5) +
  geom_label(aes(label = label_used_pos, y =  -0.05), position = position_dodge(0.9), size=2.5) +
  theme(legend.position = "none")   

