library(macroTools)
library(fredr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

params <- list(refresh_fred = TRUE)

fred_key <- "cf80aa4b84f24ffb79b52ba1255b1803"
fredr_set_key(fred_key)

recs <- getFredRecessionData(fetch_symbol = "USREC",
                             data_file_name = "us_recession",
                             data_dir = "data",
                             force_refresh = params$refresh_fred)

data_file_name <- "is_curve"
#title <- "US Employment"
#sub_title <- "(Monthly Change)"

fetch <- tribble(
  ~series_id, ~series_name,
  "FEDFUNDS", "Fed_Funds_Nom",
  "DGS10", "Ten_Yr_Nom",
  "GDPPOT", "Real_Pot_GDP",
  "GDPC1", "Real_GDP"
)



dat_1 <- getFredData(fetch_symbols = fetch$series_id,
                   data_file_name = data_file_name,
                   data_dir = "data",
                   force_refresh = params$refresh_fred,
                   frequency="q",
                   observation_start=ymd(19620101),
                   observation_end=ymd(20220401))  %>%
  left_join(fetch, by = "series_id")


fetch <- tribble(
  ~series_id, ~series_name,
  "CPIAUCSL", "CPI_All",
  "CPILFESL", "CPI_Core"
)

dat <- getFredData(fetch_symbols = fetch$series_id,
                     data_file_name = data_file_name,
                     data_dir = "data",
                     force_refresh = params$refresh_fred,
                     frequency="q",
                     observation_start=ymd(19620101),
                     observation_end=ymd(20220401),
                     units = "pc1")  %>%
  left_join(fetch, by = "series_id")


dat_long <- dat %>% 
  rbind(dat_1) %>% 
  select(-series_id)

#
# Look at data
#

#
# Transform data
# 1. Percentages all in decimal format
# 2. Create real 10 year (using core cpi)
# 3. Create real fed funds (using core cpi)
# 4. Create short-run output
#

#
# Transform data
# 1. Percentages all in decimal format
#
dat_wide <- dat_long %>% 
  tidyr::pivot_wider(names_from = c("series_name"))

dat_wide <- dat_wide %>% 
  mutate(CPI_All = CPI_All/100.0,
  CPI_Core = CPI_Core/100.0,
  Fed_Funds_Nom = Fed_Funds_Nom/100.0,
  Ten_Yr_Nom = Ten_Yr_Nom/100.0)

#
# Transform data
# 2. Create real 10 year (using core cpi)
#
dat_wide <- dat_wide %>% 
  mutate(Ten_Yr_Real = Ten_Yr_Nom - CPI_Core)

#
# Transform data
# 3. Create real fed funds (using core cpi)
#
dat_wide <- dat_wide %>% 
  mutate(Fed_Funds_Real = Fed_Funds_Nom - CPI_Core)

#
# Transform data
# 4. Create short-run output
#
dat_wide <- dat_wide %>% 
  mutate(Short_Run_Output = Real_GDP/Real_Pot_GDP-1)


#
# Plot 
#
dat_long <- dat_wide %>% 
  pivot_longer(-date)


dat_long %>%
  filter(name %in% c("Fed_Funds_Nom", "Ten_Yr_Nom")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") 



dat_long %>%
  filter(name %in% c("Fed_Funds_Real", "Ten_Yr_Real")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") 



dat_long %>%
  filter(name %in% c("Short_Run_Output")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") 



#
# IS Curve
#
dat_wide %>%
  ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Nom)) +
  geom_point() +
  ggplot2::theme_light() +
  ylab("Interest Rate") +
  xlab("Short-Run Output") +
  geom_smooth(method='lm', formula= y~x)


#
# IS Curve
#
dat_wide %>%
  filter(date < ymd(20000101)) %>%
  filter(date > ymd(19800101)) %>%
  ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Nom)) +
  geom_point() +
  ggplot2::theme_light() +
  ylab("Interest Rate") +
  xlab("Short-Run Output") +
  geom_smooth(method='lm', formula= y~x)


# dat_wide %>%
#   filter(date < ymd(20080101)) %>%
#   filter(date > ymd(19600101)) %>%
#   ggplot(mapping = aes(y=Short_Run_Output, x=Ten_Yr_Real)) +
#   geom_point() +
#   ggplot2::theme_light() +
#   ylab("") +
#   xlab("") +
#   geom_smooth(method='lm', formula= y~x)


# dat_wide %>%
#   filter(date > ymd(20000101)) %>%
#   ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Real)) +
#   geom_point() +
#   ggplot2::theme_light() +
#   ylab("") +
#   xlab("") +
#   geom_smooth(method='lm', formula= y~x)


regression_fit <- dat_wide %>%
  filter(date < ymd(20000101)) %>%
  filter(date > ymd(19800101)) %>%
  as.data.frame() %>% 
  lm(.,formula = Short_Run_Output~Ten_Yr_Nom)


plot(predict(regression_fit,newdata = data.frame(Ten_Yr_Nom=seq(0, .15, .01))),seq(0, .15, .01))
plot(seq(0, .1, .01), predict(regression_fit,newdata = data.frame(Short_Run_Output=seq(0, .1, .01))))

#guides(color = guide_legend(title = "")) +
  #theme(legend.position="top") +
  ##scale_y_continuous(labels = scales::percent_format(scale = 1))+
  #ggtitle(title, subtitle = sub_title)
