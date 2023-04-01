library(macroTools)
library(fredr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

params <- list()
params$refresh_fred <- TRUE
params$watermark <- "" #"scott.payseur.phd@gmail.com"
params$fred_key <- "cf80aa4b84f24ffb79b52ba1255b1803"
params$show_recession <- FALSE


fredr_set_key(params$fred_key)

recs <- getFredRecessionData(fetch_symbol = "USREC", 
                             data_file_name = "us_recession", 
                             data_dir = "data", 
                             force_refresh = params$refresh_fred) 

data_file_name <- "fed_funds_10_long"
title <- "U.S. Treasury Yields"
sub_title <- "(percentage yield)"
source <- "St. Louis Fed"
plot_start_date <- ymd(19700101) #19700101
periodicity = "m"

fetch <- tribble(
  ~series_id, ~series_name,
  "DFF", "Fed Funds",
  "DGS30", "30 Yr",
  "DGS10", "10 Yr",
  "DGS5", "5 Yr",
  "DGS2", "2 Yr",
  "DGS1", "1 Yr",
  
)

data_file_path <- file.path("data", paste0(data_file_name,".RDS"))
if(params$refresh_fred  || !file.exists(data_file_path)){
  dat <- map_df(fetch$series_id, fredr, frequency=periodicity, observation_start=ymd(19700101)) %>% 
    left_join(fetch) 
  write_rds(dat, data_file_path)
}else{
  dat <- read_rds(data_file_path)
}


dat_plot <- dat %>%
  filter(date >= plot_start_date )

p <- dat_plot  %>% 
  ggplot(mapping = aes(x=date, y=value, color=series_name)) + 
  geom_line() +
  ggplot2::theme_bw() +
  ylab("") +
  xlab("") 

if(params$show_recession){
  p <- p + geom_rect(data = recs %>% filter(end_date >= min(dat_plot$date)), 
                     aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=+Inf), 
                     fill='lightgrey', alpha=0.4,inherit.aes = FALSE) 
}
  
p<- p +  guides(color = guide_legend(title = "")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) 
 # scale_x_continuous(label=c("2002", "2008", "2009", "2022","2023"), 
#                     breaks=c(ymd("2002-01-01"),ymd("2008-01-01"),ymd("2009-01-01"),ymd("2022-01-01"),ymd("2023-01-01")),
 #                    minor_breaks = NULL)
if(nchar(params$watermark) > 0){
  watermark_x <- dat_plot %>% select(date) %>% pull() %>% min(na.rm = TRUE)
  watermark_y <- dat_plot %>% select(value) %>% pull() %>% min(na.rm = TRUE)
  p <- p + geom_text(aes(x = watermark_x, y = watermark_y, label = params$watermark), 
                   color="darkgrey", vjust = "inward", hjust = "inward", inherit.aes = FALSE)
}
p <- p + labs(title = title, subtitle = sub_title, caption = paste("Source:", source))

p
