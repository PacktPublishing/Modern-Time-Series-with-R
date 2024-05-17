## ----chap4-loadpackage, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----
#install.packages("pacman")
library(pacman)
p_load("forecast"
       ,"fst"
       ,"fma"
       ,"Mcomp"
       ,"patchwork"
       ,"readabs"
       ,"readr"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
       )

# create data_chap4 folder 
if(!dir.exists("data/data_chap4")){
  dir.create("data/data_chap4/")
}



## ----chap4-read-abs-data, eval=FALSE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----
# Download ABS data using readabs package for plotting
# More info vignette(readabs)
# readabs::readabs_vignette	for explanation

# set directory path to save ABS data
if(!dir.exists("data/abs")){
  dir.create("data/abs/")
}

# ABS retail
abs_retail_dwnld <- read_abs("8501.0",
                       tables = 11,
                       path = "data/abs")
# keep required columns
abs_retail <- abs_retail_dwnld %>%
  mutate(month = yearmonth(date)) %>%
  rename(turnover = value) %>%
  select(month, series_id, series, turnover) %>%
  separate(series, c("category", "state", "industry"),
           sep = ";", extra = "drop") %>%
  mutate(
    state = trimws(state),
    industry = trimws(industry),
  ) %>%
  select(-category) %>%
  # filter(
  #   industry  != "Total (Industry)",
  #   state != "Total (State)") %>%
  as_tsibble(index = month, key = c(state, industry)) %>%
  filter(!is.na(turnover))

readr::write_rds(abs_retail,
          file = "data/data_chap4/abs_retail.rds")


## ----trend-1-data, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----

abs_retail <- readr::read_rds("data/data_chap4/abs_retail.rds") 

trend_seas_dt  <- abs_retail |> 
  filter(state == "Total (State)", industry == "Takeaway food services") |>
  index_by(year = ~year(.)) |> 
  filter(year >= 1983 , year <= 2022 ) 

turnover_trend_dt <- trend_seas_dt |> 
  summarise(total_turnover = sum(turnover))

turnover_trend_plot <- turnover_trend_dt |> 
  ggplot(aes(x = year, y = total_turnover)) +
  geom_line() + 
  labs(title = "Annual Australian takeaway food services turnover", y = "") +
  theme_bw()

m3003_trend_plot <- autoplot(M3[[3003]]$x) +
  labs(title = "M3 competition series O174", 
       subtitile = "Telecommunication data" ,
        x = "year [1Y]",
        y = "") +
  theme_bw()

  


## ----trend-1-plot, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, fig.cap="Examples of upward and downward trend", fig.width=12, fig.height=6, echo=FALSE----
# , out.width="100%"

(turnover_trend_plot | m3003_trend_plot)



## ----seasonal-data, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----

turnover_season_full_plot <- trend_seas_dt |>
  ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  labs(title = "Monthly Australian takeaway food services turnover",
       subtitle = "1983-2022",
       y = "") +
  theme_bw()

turnover_season_5yr_plot <- trend_seas_dt |>
  filter(year >= 2010, year <= 2015) |> 
   ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Australian takeaway food services turnover", 
       subtitle = "2010-2015",
       y = "") +
  theme_bw()

# make monthly milk production to quarterly data 
milk_dt <- milk |> 
  as_tsibble() |> 
    rename(month_milk = value) |> 
    mutate(qtr = quarter(index) ,
           yr = year(index)) |> 
    as_tibble() |> 
    mutate(quarter = make_yearquarter(year = yr, quarter = qtr, fiscal_start = 1)) |> 
    select(-index, -yr, -qtr) |> 
     group_by(quarter) |> 
    summarise(qr_milk = mean(month_milk)) |> 
    ungroup() |> 
    as_tsibble(index = quarter) 

milk_full_plot <- milk_dt |>
  ggplot(aes(x = quarter , y = qr_milk)) +
  geom_line() +
  labs(title = "Quarterly average milk production", 
       subtitle = "1962-1975",
       y = "") +
  theme_bw()


milk_5yr_plot <- milk_dt |>
    slice(1:20)  |> 
  #filter(year >= 1962, year <= 1965) |> 
  ggplot(aes(x = quarter , y = qr_milk)) +
  geom_line() +
  geom_point() +
  labs(title = "Quarterly average milk production", 
       subtitle = "1962 Q1-1965 Q4",
       y = "") +
  theme_bw()



## ----seasonal-1-plot, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, fig.cap="Examples of seasonal and trend",fig.width=12,fig.height=6,   echo=FALSE----
#out.width="100%", out.height="50%",

((turnover_season_full_plot | turnover_season_5yr_plot ) / (milk_full_plot | milk_5yr_plot))




## ----cyclical-1-data, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----

sunspot_plot <- sunspots |> 
  as_tsibble() |> 
  rename(month = index, sunspot = value) |> 
  ggplot(aes(x = month, y = sunspot)) +
  geom_line() +
  labs(title = "Monhtly Sunspots:1749-1983",
       y = "") +
  theme_bw()

lynx_plot <- autoplot(lynx) +
  labs(title = "Annual Canadian Lynx trapings: 1982-1934",
        y = "",
        x = "year [1Y]")

us_unemployed_plot <- economics |> 
  mutate(month = yearmonth(date),
         year = year(date)) |> 
  select(-date) |> 
  as_tsibble(index = month) |> 
  filter(year >= 1968, year <= 2014) |> 
  ggplot(aes(x = month, y = unemploy)) +
  geom_line() +
  labs(title = "US Number of unemployed (000K): 1968-2014", 
       y = "") +
  theme_bw()
  



## ----cyclical-1-plot, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, fig.cap="Examples of cyclical series",fig.width=12,fig.height=6,   echo=FALSE----
 
( (us_unemployed_plot | lynx_plot)/
 (  sunspot_plot)
)



## ----autocor-calc, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----

turnover_dcomp <- mstl(log(trend_seas_dt$turnover))
turnover_differenced <- diff(log(trend_seas_dt$turnover), 2)
turnover_seasadj <- seasadj(turnover_dcomp)

#turnover_detrend_seasadj <- diff(turnover_seasadj, 2)

# Acf of trend original turnover
acf1 <- ggAcf(trend_seas_dt$turnover) +
  labs(title = "ACF: Original data")

# acf2<- ggAcf(log(trend_seas_dt$turnover)) +
#   labs(title = "ACF: log(Y) - handling multiplicative structure")


# acf of monthly de-trended turnover -> seasonlity is prominent 
acf3 <- ggAcf(turnover_differenced) + 
  labs(title = "ACF: De-trended data - seasonality is prominent ")
  

## ----autocor-plot, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, fig.cap="Autocorrelation of monthly takeaway food turnover: original data and detrended data", fig.width=12, fig.height=4,   echo=FALSE----

(acf1 | acf3) 


