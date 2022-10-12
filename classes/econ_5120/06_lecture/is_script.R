library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

#
# Load data
#
is_dat_long <- readRDS('is_data.RDS')

#
# Create wide version
#
dat_wide <- is_dat_long %>% 
  tidyr::pivot_wider(names_from = c("series_name"))

#
# Look at data
#
dat_wide

dat_wide %>% tail()

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
# Multiple line plotting easier with wide data
#
dat_long <- dat_wide %>% 
  pivot_longer(-date)

#
# Plot CPI  
#
dat_long %>%
  filter(name %in% c("CPI_All", "CPI_Core")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))

#
# Plot Real vs Nominal 10 year 
#
dat_long %>%
  filter(name %in% c("Ten_Yr_Real", "Ten_Yr_Nom")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))


#
# Plot Fed Funds vs Ten Year Nominal
#
dat_long %>%
  filter(name %in% c("Fed_Funds_Nom", "Ten_Yr_Nom")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))


#
# Plot Fed Funds vs Ten Year Real
#
dat_long %>%
  filter(name %in% c("Fed_Funds_Real", "Ten_Yr_Real")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))


#
# Plot Real and Potential GDP
#
dat_long %>%
  filter(name %in% c("Real_GDP", "Real_Pot_GDP")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("Trillions $") +
  xlab("") +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  guides(color = guide_legend(title = ""))

#
# Plot Short Run Output
#
dat_long %>%
  filter(name %in% c("Short_Run_Output")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))


#
# IS Curve
#
dat_wide %>%
  ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Nom)) +
  geom_point() +
  ggplot2::theme_light() +
  ylab("Interest Rate") +
  xlab("Short-Run Output") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))  

#
# IS Curve
#
dat_wide %>%
  ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Nom, color=date)) +
  geom_point() +
  ggplot2::theme_light() +
  ylab("Interest Rate") +
  xlab("Short-Run Output")+  
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))  

#
# IS Curve
#
dat_wide %>%
  ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Nom, color=date)) +
  geom_point() +
  ggplot2::theme_light() +
  ylab("Interest Rate") +
  xlab("Short-Run Output") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))  +
  geom_smooth(method='lm', formula= y~x) 

#
# Plot Potential GDP and Nominal 10 year 
#
dat_long %>%
  filter(name %in% c("Short_Run_Output", "Ten_Yr_Nom")) %>% 
  ggplot(mapping = aes(x=date, y=value, color=name)) +
  geom_line() +
  ggplot2::theme_light() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))  
  


#
# IS Curve
#
dat_wide %>%
  filter(date < ymd(20000101)) %>%
  ggplot(mapping = aes(x=Short_Run_Output, y=Ten_Yr_Nom, color=date)) +
  geom_point() +
  ggplot2::theme_light() +
  ylab("Interest Rate") +
  xlab("Short-Run Output") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = guide_legend(title = ""))  +
  geom_smooth(method='lm', formula= y~x) 


#
# Regression values
#
dat_wide %>%
  filter(date < ymd(20000101)) %>%
  as.data.frame() %>% 
  lm(.,formula = Short_Run_Output~Ten_Yr_Nom)

