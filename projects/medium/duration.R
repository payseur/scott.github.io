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

# tlt <- tq_get("tlt", from="2000-01-01")
# tlt %>%ggplot(aes(x=date, y=adjusted)) + geom_line()
# 
# #
# # Convert to xts
# #
# tlt_xts <- xts(tlt$adjusted, order.by=ymd(tlt$date))
# tlt_ret_xts <- PerformanceAnalytics::Return.calculate(tlt_xts)
# tlt_ret_xts[is.na(tlt_ret_xts)] <- 0.0
# dimnames(tlt_ret_xts)[[2]] <- "TLT"
# PerformanceAnalytics::table.CalendarReturns(tlt_ret_xts)
# 
# 
# tlt_q <- to.quarterly(tlt_xts)
# barp <- (tlt_q$tlt_xts.Close/tlt_q$tlt_xts.Open) - 1
# tlt_tbl <- data.frame(date_ = index(barp), rets = as.numeric(barp[,1])) %>% as.tibble()
# 
# tlt_q <- to.yearly(tlt_xts)
# (tlt_q$tlt_xts.Close - tlt_q$tlt_xts.Open)/tlt_q$tlt_xts.Open
# 
# tlt_tbl %>% ggplot(aes(y=rets)) + geom_bar()
# 
# 
# 
# tlt_tbl %>% mutate(ind = rank(rets)) %>% ggplot(aes(x=ind, y=rets)) + geom_point()
# tlt_tbl %>% mutate(ind = rank(rets)) %>% ggplot(aes(x=ind, y=rets)) + 
#   geom_bar(stat= "identity") +
#   theme_bw() 
# 

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




#
#  
#
library(macroTools)
library(fredr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

fred_key <- "cf80aa4b84f24ffb79b52ba1255b1803"
fredr_set_key(fred_key)

recs <- getFredRecessionData(fetch_symbol = "USREC", 
                             data_file_name = "us_recession", 
                             data_dir = "data", 
                             force_refresh = params$refresh_fred) 

data_file_name <- "fed_funds"
title <- "U.S. 10 Yr Yield"
sub_title <- "(percentage yield)"

fetch <- tribble(
  ~series_id, ~series_name,
  #"FEDFUNDS", "U.S. Fed Funds",
  "DGS10", "U.S. 10 Yr Yield"
)

data_file_path <- file.path("data", paste0(data_file_name,".RDS"))
if(params$refresh_fred  || !file.exists(data_file_path)){
  dat <- map_df(fetch$series_id, fredr, frequency="m", observation_start=ymd(20010101)) %>% 
    left_join(fetch) 
  write_rds(dat, data_file_path)
}else{
  dat <- read_rds(data_file_path)
}

dat %>%
  ggplot(mapping = aes(x=date, y=value, color="black")) + 
  geom_line(color="black") + 
  # geom_rect(data = recs %>% filter(end_date >= min(dat$date)), 
  #           aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=+Inf), 
  #           fill='lightgrey', alpha=0.4,inherit.aes = FALSE) +
  ggplot2::theme_bw() +
  ylab("") +
  xlab("") +
  guides(color = guide_legend(title = ""))+
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_x_continuous(label=c("2001", "2021","2022","2023"), breaks=c(ymd("2001-01-01"),ymd("2021-01-01"),ymd("2022-01-01"),ymd("2023-01-01")))+
  ggtitle(title, subtitle = sub_title)

