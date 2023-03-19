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


#
#  AGG
#
symbol = "AGG"
from = "2002-01-01"
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
  mutate(rets_for_color = if_else(!is.na(label_used) , "1", "0")) %>% 
  mutate(label_used_neg = if_else((!is.na(label_used) & (rets <= 0)), label_used, NA)) %>% 
  mutate(label_used_pos = if_else((!is.na(label_used) & (rets > 0)), label_used, NA))
 

watermark_x <- tlt_tbl %>% select(ind) %>% pull() %>% max()
watermark_y <- tlt_tbl %>% select(rets) %>% pull() %>% min()


 tlt_tbl %>% 
  ggplot(aes(x=ind, y=rets, fill = rets_for_color)) + 
  geom_bar(stat= "identity", ) +
  theme_bw() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = NULL)  + 
  geom_label(aes(label = label_used_neg, y =  0.01), position = position_dodge(0.9), size=2.5) +
  geom_label(aes(label = label_used_pos, y =  -0.01), position = position_dodge(0.9), size=2.5) +
  theme(legend.position = "none")  +
  scale_fill_manual("legend", values = c("1" = "lightgrey", "0" = "black")) +
  geom_text(aes(x = watermark_x, y = watermark_y, label = "scott.payseur.phd@gmail.com"), color="lightgrey", vjust = "inward", hjust = "inward", inherit.aes = FALSE)




#
#  SPY
#
symbol = "SPY"
from = "2003-01-01"
to = "2023-01-01"
spy <- tq_get(symbol, from=from, to = to)
spy_xts <- xts(spy$adjusted, order.by=ymd(spy$date))
spy_ret_xts <- PerformanceAnalytics::Return.calculate(spy_xts)
dimnames(spy_ret_xts)[[2]] <- symbol
spy_q <- to.yearly(spy_xts)
barp <- (spy_q$spy_xts.Close/spy_q$spy_xts.Open) - 1

spy_tbl <- data.frame(date_ = index(barp), rets = as.numeric(barp[,1])) %>%
  as.tibble()

spy_tbl <- spy_tbl %>%  
  mutate(ind = rank(rets)) %>% 
  mutate(label_ = as.character(year(date_))) %>% 
  mutate(label_used = if_else(year(date_) %in% c(2022, 2008), paste(as.character(year(date_)), "\n ", round(rets*100,1), "%"),  NA)) %>% 
  mutate(rets_for_color = if_else(!is.na(label_used) , "1", "0")) %>% 
  mutate(label_used_neg = if_else((!is.na(label_used) & (rets <= 0)), label_used, NA)) %>% 
  mutate(label_used_pos = if_else((!is.na(label_used) & (rets > 0)), label_used, NA))


watermark_x <- spy_tbl %>% select(ind) %>% pull() %>% max()
watermark_y <- spy_tbl %>% select(rets) %>% pull() %>% min()

spy_tbl %>% 
  ggplot(aes(x=ind, y=rets, fill = rets_for_color)) + 
  geom_bar(stat= "identity", ) +
  theme_bw() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = NULL)  + 
  geom_segment(aes(x = 2, y = 0, xend = 2.5, yend = .04), color = "lightgrey") + 
  geom_label(aes(label = "2022 \n -18.6%", y =  0.055, x=2.5), fill="lightgrey", position = position_dodge(1.5), size=2.5, inherit.aes = FALSE) +
  geom_label(aes(label = "2008 \n -36.2%", y =  0.035, x=1), fill="lightgrey", position = position_dodge(1.5), size=2.5, inherit.aes = FALSE) +
  theme(legend.position = "none")  +
  scale_fill_manual("legend", values = c("1" = "lightgrey", "0" = "black")) +
  geom_text(aes(x = watermark_x, y = watermark_y, label = "scott.payseur.phd@gmail.com"), color="lightgrey", vjust = "inward", hjust = "inward", inherit.aes = FALSE)



# 60/40
agg <- tlt_tbl %>% select(date_, rets) %>% rename(agg = rets)
spy <- spy_tbl %>% select(date_, rets) %>% rename(spy = rets)

sixty_forty <- agg %>% left_join(spy)
sixty_forty <- sixty_forty %>% 
  mutate(rets = 0.6 * spy + 0.4 * agg)

sixty_forty <- sixty_forty %>%  
  mutate(ind = rank(rets)) %>% 
  mutate(label_ = as.character(year(date_))) %>% 
  mutate(label_used = if_else(year(date_) %in% c(2022, 2008), paste(as.character(year(date_)), "\n ", round(rets*100,1), "%"),  NA)) %>% 
  mutate(rets_for_color = if_else(!is.na(label_used) , "1", "0")) %>% 
  mutate(label_used_neg = if_else((!is.na(label_used) & (rets <= 0)), label_used, NA)) %>% 
  mutate(label_used_pos = if_else((!is.na(label_used) & (rets > 0)), label_used, NA))


watermark_x <- sixty_forty %>% select(ind) %>% pull() %>% max()
watermark_y <- sixty_forty %>% select(rets) %>% pull() %>% min()

sixty_forty %>% 
  ggplot(aes(x=ind, y=rets, fill = rets_for_color)) + 
  geom_bar(stat= "identity", ) +
  theme_bw() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = NULL)  + 
  geom_segment(aes(x = 2, y = 0, xend = 2.5, yend = .04), color = "lightgrey") + 
  geom_label(aes(label = "2022 \n -16.2%", y =  0.04, x=2.5), fill="lightgrey", position = position_dodge(1.5), size=2.5, inherit.aes = FALSE) +
  geom_label(aes(label = "2008 \n -18.8%", y =  0.02, x=1), fill="lightgrey", position = position_dodge(1.5), size=2.5, inherit.aes = FALSE) +
  # geom_label(aes(label = label_used_neg, y =  0.02), position = position_dodge(1.5), size=2.5) +
  #geom_label(aes(label = label_used_pos, y =  -0.01), position = position_dodge(0.9), size=2.5) +
  theme(legend.position = "none")  +
  scale_fill_manual("legend", values = c("1" = "lightgrey", "0" = "black")) +
  geom_text(aes(x = watermark_x, y = watermark_y, label = "scott.payseur.phd@gmail.com"), color="lightgrey", vjust = "inward", hjust = "inward", inherit.aes = FALSE)




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
  dat <- map_df(fetch$series_id, fredr, frequency="m", observation_start=ymd(20020101)) %>% 
    left_join(fetch) 
  write_rds(dat, data_file_path)
}else{
  dat <- read_rds(data_file_path)
}


watermark_x <- ymd("2010-01-01")
watermark_y <- dat %>% select(value) %>% pull() %>% min(na.rm = TRUE)

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
  scale_x_continuous(label=c("2002", "2008", "2009", "2022","2023"), 
                     breaks=c(ymd("2002-01-01"),ymd("2008-01-01"),ymd("2009-01-01"),ymd("2022-01-01"),ymd("2023-01-01")),
                      minor_breaks = NULL)+
geom_text(aes(x = watermark_x, y = watermark_y, label = "scott.payseur.phd@gmail.com"), color="lightgrey", vjust = "inward", hjust = "inward", inherit.aes = FALSE)

