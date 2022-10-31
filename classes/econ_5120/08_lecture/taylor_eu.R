## Setup Environment

library(ggplot2)
library(fredr)
library(dplyr)
library(lubridate)
library(tidyr)

fred_key <- "cf80aa4b84f24ffb79b52ba1255b1803"
fredr_set_key(fred_key)

# download data
inflation <- fredr(series_id = "EA19CPALTT01GYM",
             observation_start = ymd(20000101),
             frequency = "m")
# Clean Data
inflation %>% head(5) %>% print()

inflation <- inflation %>% 
  select(date, series_id, value) %>% 
  rename(id = series_id) %>% 
  mutate(id = "INFLATION")

inflation %>% head(5) %>% print()

# download data
overnight <- fredr(series_id = "ECBDFR",
                  observation_start = ymd(20000101),
                  frequency = "m")
# Clean Data
overnight %>% head(5) %>% print()

overnight <- overnight %>% 
  select(date, series_id, value) %>% 
  rename(id = series_id) %>% 
  mutate(id = "OVERNIGHT")

fedfunds %>% head(5) %>% print()

# Combine two long series
dat <- bind_rows(inflation, overnight)

# Create wide serie 
dat_wide <- dat %>% pivot_wider(names_from = id)

dat_wide %>% head(5) %>% print()

# Create Taylor Rule Prediction
r_bar = 2       # MPC
m_bar = 0.5     # Sensitivity Parameter
pi_bar = 2      # Inflation target

dat_wide <- dat_wide %>% 
  mutate( PREDICTION = r_bar + INFLATION + m_bar*(INFLATION - pi_bar)) 

dat_long <- dat_wide %>% pivot_longer(-date)

# plot
dat_long %>% 
  filter(name %in% c("OVERNIGHT", "PREDICTION")) %>% 
  ggplot(aes(x=date, y=value, color=name)) +
  geom_line()
