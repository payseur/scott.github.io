Welcome!

# This is a minimal example of a book based on R Markdown and **bookdown** (<https://github.com/rstudio/bookdown>).

```{r chunk-label, echo = FALSE, fig.cap = 'A figure caption.'}
1 + 1
rnorm(10)  # 10 random numbers
plot(dist ~ speed, cars)  # a scatterplot
```

```{r chunk-label, echo = FALSE, display = FALSE, fig.cap = 'A figure caption.'}

library(rvest)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

tlt_url = 'https://www.ishares.com/us/products/239454/ishares-20-year-treasury-bond-etf'
shy_url = 'https://www.ishares.com/us/products/239452/ishares-13-year-treasury-bond-etf'
#agg_url = 'https://www.ishares.com/us/products/239458/ishares-core-total-us-bond-market-etf'

base_url = 'https://www.ishares.com'
url_data = tlt_url
url_data = shy_url
#url_data = agg_url


css_selector <- "#holdings > div.holdings.fund-component-data-export > a.icon-xls-export"
css_link <- url_data %>% 
  read_html() %>% 
  html_elements(css = css_selector) %>% 
  html_attr('href') 

url_data_csv <- paste0(base_url, css_link)

br_csv <- read_csv(url_data_csv, skip = 0)
holdings_date <- br_csv[1,1] %>%  parse_date_time(.,'%b %d, %Y')

n_start <- which(apply(br_csv,1,function(x){substr(x,1,4)=="Name"}) == TRUE)
n_end <- max(which(apply(br_csv,1,function(x){str_count(x, ",")}) == 0)) - n_start -1
br_clean_csv <- read_csv(url_data_csv, skip = n_start, n_max = n_end)

with_dur <- br_clean_csv %>% 
  filter(Sector == "Treasuries") %>% 
  mutate(w_duration = `Weight (%)` * `Duration` / 100) %>% 
  mutate(days_to_mature = parse_date(`Maturity`,'%b %d, %Y'))

with_dur %>% filter(Sector == "Treasuries") %>% ggplot(., aes(x=`Duration`, y=`Coupon (%)`, color=`Price`)) + geom_point()


```

This template provides a skeleton file structure that you can edit to create your book.

The contents inside the .Rmd files provide some pointers to help you get started, but feel free to also delete the content in each file and start fresh.

Additional resources:

The **bookdown** book: <https://bookdown.org/yihui/bookdown/>

The **bookdown** package reference site: <https://pkgs.rstudio.com/bookdown>
