# install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"here"
       ,"forecast"
       ,"fst"
       ,"fma"
       ,"Mcomp"
       ,"patchwork"
       ,"readabs"
       ,"readr"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
       ,"scales"
       )

# create data_chap4 folder 
# if(!dir.exists("data/data_chap4")){
#   dir.create("data/data_chap4/")
# }
# conflicts_prefer(dplyr::filter
#                  )

# set plotting theme
theme_set(
  theme_bw() +
  theme(
    # Set the size for ALL axis text (tick labels) to 11
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),

    # Set the size for ALL axis titles to 12
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),

    # Set the size for the plot title to 12
    plot.title = element_text(size = 11),
    
    # strip text for facets
    strip.text = element_text(size = 11),
    
    theme(legend.position = "top")
  )
)



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
  scale_y_continuous(
    labels = label_number(scale=1e-3, suffix="K")) +
  labs(title = "Annual turnover (Food industry)", 
       y = "") 

m3003_trend_plot <- autoplot(M3[[3003]]$x) +
  scale_y_continuous(
    labels = label_number(scale=1e-3, suffix="K")) +
  labs(title = "Series O174 (M3 Competition)", 
       subtitile = "Telecommunication data" ,
        x = "year [1Y]",
        y = "") 

(turnover_trend_plot | m3003_trend_plot)  


turnover_season_full_plot <- trend_seas_dt |>
  ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  labs(title = "Food services turnover, 1983-2022",
        y = "") 

turnover_season_5yr_plot <- trend_seas_dt |>
  filter(year >= 2010, year <= 2015) |> 
   ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  geom_point() +
  labs(title = "Food services turnover, 2010-2015", 
        y = "")


turnover_season_full_plot | turnover_season_5yr_plot 



sunspot_plot <- sunspots |> 
  as_tsibble() |> 
  rename(month = index, sunspot = value) |> 
  ggplot(aes(x = month, y = sunspot)) +
  geom_line() +
  labs(title = "Monhtly Sunspots:1749-1983",
       y = "")

lynx_plot <- autoplot(lynx) +
  labs(title = "Annual Canadian Lynx trapings: 1821-1934",
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
       y = "") 
  


 
( (us_unemployed_plot | lynx_plot)/
 (  sunspot_plot)
)


#out.width="100%", out.height="50%",

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
  geom_point() +
  labs(title ="Average milk production/cow, Q1 1962 - Q4 1975",
       y = "", x = "") 


airpax_plot <- AirPassengers |>
   as_tsibble() |> 
  ggplot(aes(x = index  , y = value)) +
  geom_line() +
  geom_point() +
  labs(title = "Airline passengers: Jan 1949 - Dec 1960",
       y = "", x = "")


milk_full_plot | airpax_plot



turnover_dcomp <- mstl(log(trend_seas_dt$turnover))
turnover_differenced <- diff(log(trend_seas_dt$turnover), 2)
turnover_seasadj <- seasadj(turnover_dcomp)

# Acf of trend original turnover
acf1 <- ggAcf(trend_seas_dt$turnover) +
  labs(title = "ACF: Original data") 


acf3 <- ggAcf(turnover_differenced) +
  labs(title = "ACF: De-trended data")
 


(acf1 | acf3) 

