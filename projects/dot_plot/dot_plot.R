library(lubridate)
library(rvest)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)



dates_ <- c(
            #20180321, 20180613, 20180926, 20181219, 
          #  20190320, 20190619, 20190918, 20191211,
         #             20200610, 20200916, 20201216,
            20210317, 20210616, 20210922, 20211215,
            20220316, 20220615, 20220921, 20221214,
            20230322)
          
            
url_start <- "https://www.federalreserve.gov/monetarypolicy/fomcprojtabl"


#url_ <- "https://www.federalreserve.gov/monetarypolicy/fomcprojtable20220316.htm"
#url_ <- "https://www.federalreserve.gov/monetarypolicy/fomcprojtabl20230322.htm"
css_selector <- "div.data-table:nth-child(12)"


for(date_ in dates_){
  if(date_ == 20220316){
    url_ <- paste0(url_start, "e", date_, ".htm")
  }else{
    url_ <- paste0(url_start, date_, ".htm")
  }
  
  print(url_)
  dot_plot_html <- 
    url_ %>% 
    read_html() %>% 
    html_elements(css = css_selector) %>% 
    html_table()
  
  dot_plot_html <- dot_plot_html[[1]]
  
  dot_plot_html <- dot_plot_html %>% 
    rename(percent = `Midpoint of target range or target level (Percent)`) %>% 
    pivot_longer(cols=-percent) %>% 
    mutate(date_ = ymd(date_))
  
  if(date_ == dates_[[1]]){
    dot_plot <- dot_plot_html
  }else{
    dot_plot <- dot_plot %>% bind_rows(dot_plot_html)
  }
}

dot_plot %>% ggplot(aes(y=percent, x=name, size=value, color=value)) + geom_point() + facet_wrap(~date_)
#dot_plot %>% ggplot(aes(y=percent, x=name, size=value)) + geom_boxplot() + facet_wrap(~date_)
