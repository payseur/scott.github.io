## Setup Environment

library(ggplot2)
library(fredr)
library(dplyr)
library(lubridate)
library(tidyr)

fred_key <- "cf80aa4b84f24ffb79b52ba1255b1803"
fredr_set_key(fred_key)

# download data
cpi <- fredr(series_id = "CPILFESL",
             observation_start = ymd(19600101),
             units = "pc1")
# Clean Data
cpi %>% head(5) %>% print()

cpi <- cpi %>% 
  select(date, series_id, value) %>% 
  rename(id = series_id) %>% 
  mutate(id = "CPI")

cpi %>% head(5) %>% print()

# download data
fedfunds <- fredr(series_id = "FEDFUNDS",
                  observation_start = ymd(19600101))
# Clean Data
fedfunds %>% head(5) %>% print()

fedfunds <- fedfunds %>% 
  select(date, series_id, value) %>% 
  rename(id = series_id) 

fedfunds %>% head(5) %>% print()

# Combine two long series
dat <- bind_rows(cpi, fedfunds)

# Create wide serie 
dat_wide <- dat %>% pivot_wider(names_from = id)

dat_wide %>% head(5) %>% print()

# Create Taylor Rule Prediction
r_bar = 2       # MPC
m_bar = 0.5     # Sensitivity Parameter
pi_bar = 2      # Inflation target

dat_wide <- dat_wide %>% 
  mutate( PREDICTION = r_bar + CPI + m_bar*(CPI - pi_bar)) 

dat_long <- dat_wide %>% pivot_longer(-date)

# plot
dat_long %>% 
  filter(name %in% c("FEDFUNDS", "PREDICTION")) %>% 
  ggplot(aes(x=date, y=value, color=name)) +
  geom_line()