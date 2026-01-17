#install.packages("pacman")
library(pacman)
p_load( "conflicted"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"forecast"
       ,"GGally"
       ,"lubridate"
       ,"Mcomp"
       ,"nycflights13"
       ,"patchwork"
       ,"plotly"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata")

# manage conflicts
conflicts_prefer(
  dplyr::filter
  )


mismatch_dt <- read_csv(
  "data/data_chap5/mismatch_date.csv")


glimpse(mismatch_dt)

## Rows: 9
## Columns: 2
## $ date  <chr> "1/01/2015", "1/02/2015", "1/03…
## $ value <dbl> 1630, 826, 567, 139, 256, 113, …

year_count <- mismatch_dt |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  mutate(year = year(date)) |> 
  group_by(year) |> 
  tally()


year_count

## # A tibble: 2 × 2
##    year     n
##   <dbl> <int>
## 1    15     1
## 2  2015     8
## 

mismatch_dt |>
  mutate(date_accurate = dmy(date),
         date_inaccurate = ymd(date))

## # A tibble: 9 × 4
##   date      value date_accurate date_inaccurate
##   <chr>     <dbl> <date>        <date>
## 1 1/01/2015  1630 2015-01-01    NA
## 2 1/02/2015   826 2015-02-01    NA
## 3 1/03/2015   567 2015-03-01    NA
## 4 1/04/2015   139 2015-04-01    NA
## 5 1/05/2015   256 2015-05-01    NA
## 6 1/06/2015   113 2015-06-01    NA
## 7 1/07/2015   166 2015-07-01    NA
## 8 1/8/15      315 2015-08-01    NA
## 9 1/09/2015   710 2015-09-01    NA
## Warning message:
## There was 1 warning in `mutate()`.
## ℹ In argument: `date_inaccurate = ymd(date)`.
## Caused by warning:
## ! All formats failed to parse. No formats found.
## 

set.seed(45)
test_df <- tibble(
  date = as.Date(c("2019-01-01",
                   "2019-09-12",
                   "2020-01-01",
                   "2020-05-10")),
  value = runif(4) )

test_df |>
  mutate(year=year(date)
         ,day_year=yday(date)
         ,day_month=mday(date)
         ,month_name=month(date, label=TRUE))

## 
## # A tibble: 4 × 6
##   date       value  year day_year day_month month_name
##   <date>     <dbl> <dbl>    <dbl>     <int> <ord>
## 1 2019-01-01 0.633  2019        1         1 Jan
## 2 2019-09-12 0.318  2019      255        12 Sep
## 3 2020-01-01 0.241  2020        1         1 Jan
## 4 2020-05-10 0.378  2020      131        10 May

test_df |>
  mutate(year_month=yearmonth(date)
         ,year_quarter=yearquarter(date)
         ,year_week=yearweek(date))


## 
## # A tibble: 4 × 5
##   date       value year_month year_quarter year_week
##   <date>     <dbl>      <mth>        <qtr>    <week>
## 1 2019-01-01 0.633   2019 Jan      2019 Q1  2019 W01
## 2 2019-09-12 0.318   2019 Sep      2019 Q3  2019 W37
## 3 2020-01-01 0.241   2020 Jan      2020 Q1  2020 W01
## 4 2020-05-10 0.378   2020 May      2020 Q2  2020 W19





## ggplot(data = <DATA>, mapping = aes(x,y)) +
##   <GEOM_FUNCITONS>() +
##   <THEME_FUNCTIONS>()

class(aus_production)

## [1] "tbl_ts"     "tbl_df"     "tbl"        "data.frame"
## 

## autoplot(aus_production, Gas)
## 

## 
## aus_production |>
##   select(Quarter, Gas)  |>
## autoplot()
## 

aus_production |> 
  select(Quarter, Gas)  |> 
autoplot() +
  labs(title = "Australian Quarterly Gas Production" , 
       y = "Petajoules)") 



class(sunspots)
## [1] "ts"
## 
## autoplot(sunspots) +
##   labs(title = "Monthly Sunspots")
## 


flights <- 
  nycflights13::flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(sched_dep_datetime = 
     make_datetime(year, month, day, 
         sched_dep_time %/% 100, 
         sched_dep_time %% 100)
     ) |> 
  mutate(sched_dep_date = date(sched_dep_datetime)) |> 
  select(carrier, origin, dest, sched_dep_date, dep_delay) |> 
  mutate(is_delayed = ifelse(dep_delay > 0, 'y', 'n')) 
  

  
flights |> 
  filter(carrier %in% c("AA", "DL"),
         is_delayed == 'y') |> 
  count(sched_dep_date, carrier) |> 
  ggplot(aes(x = sched_dep_date, y = n, 
             group = carrier, 
             linetype = carrier)) +
  geom_line() +
  labs(x = " " ,
       y = "delayed flights") +
  theme(legend.position = "top")

  
flights |> 
  filter(carrier %in% c("AA", "DL"),
         is_delayed == 'y') |> 
  mutate(week_end_date = ceiling_date(sched_dep_date, "week")) |> 
  count(week_end_date, carrier) |> 
  ggplot(aes(x = week_end_date, 
             y = n, 
             group = carrier, 
             linetype = carrier)) +
  geom_line() +
  labs(x = " " ,
       y = "delayed flights") +
  theme(legend.position = "top")


# weekly average delayed time of all carriers from JFK
jfk_delay <- flights |> 
  filter(origin == "JFK" ,
         is_delayed == 'y' ) |> 
  mutate(sched_dep_week = ceiling_date(sched_dep_date, "week")) |> 
  group_by(sched_dep_week) |> 
  summarise(avg_jfk = mean(dep_delay, na.rm = T)) |> 
  ungroup() 


flight2_delay <- flights |> 
  filter(carrier %in% c("AA", "DL"), 
         is_delayed == 'y' ,
         origin == "JFK" ) |> 
  mutate(sched_dep_week = ceiling_date(sched_dep_date, "week")) |> 
  group_by(sched_dep_week, carrier, origin, dest) |> 
  summarise(avg_delay = mean(dep_delay, na.rm = T)) |> 
  ungroup()   



ggplot() +
  geom_line(data = flight2_delay |> 
            filter( dest %in% c("LAX", "SFO")), 
    aes(x = sched_dep_week, y = avg_delay), 
         linetype = 3, size = 0.9) +
  geom_line(data = jfk_delay, 
            aes(x = sched_dep_week,  y = avg_jfk), 
            size = 0.7) +
  facet_grid(carrier~dest ) + 
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b\n%Y") + 
  labs(x = "", y = "")



aus_production |> 
  select(Quarter, Gas) |> 
  gg_season(Gas) 


AirPassengers |> 
  as_tsibble() |> 
  gg_season(value, period = "year") +
  labs(x = "",
       y = "Total passengers") 



flights |> 
  filter(is_delayed == "y") |> 
  count(sched_dep_date) |> 
  as_tsibble(index = sched_dep_date) |> 
  gg_season(n, period = "week" ) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "number of delayed flights") +
  scale_x_date(
    date_breaks = "1 day", 
    labels = date_format("%a"))



takeaway_turnover <- readRDS("data/data_chap5/abs_retail.rds") |> 
  filter(
    industry == "Takeaway food services", 
    state %in% c("Victoria", "New South Wales"),
    year(month) >= 2015 ,
    year(month) <= 2022 ) |> 
  select(month, state, turnover)


takeaway_turnover |> 
  gg_subseries(turnover) +
  labs(x = "", y = "Turnover $M")



vic_retail <- 
  readRDS("data/data_chap5/abs_retail.rds") |> 
  filter(
    industry %in% 
     c("Clothing retailing",
       "Food retailing",
       "Electrical and electronic goods retailing",
       "Hardware, building and garden supplies retailing",
       "Newspaper and book retailing",
       "Supermarket and grocery stores"),
    state == "Victoria" ,
    year(month) >= 2015 ,
    year(month) <= 2022 ) |> 
  select(month, industry, turnover) |> 
  mutate(industry = sapply(
    str_split(string = industry,pattern = "[, ]+") ,'[', 1
    )) 
  


vic_retail |> 
  ggplot(aes(x = month, y = turnover)) +
  geom_line(size = 0.7) +
  facet_grid(vars(industry), scales = "free_y") +
  labs(x = " ") 



vic_retail |> 
  pivot_wider(names_from = industry,
              values_from = turnover) |>
  GGally::ggpairs(columns = 2:6, progress = F) 



vic_retail |> 
  filter(industry == "Clothing") |> 
  gg_lag(turnover, lags = 1:12, geom = "point") +
  labs(x = "lag(Turnover,l)",
       y = "Turnover")


vic_retail |> 
  filter(industry == "Clothing") |> 
  ACF(turnover) |> 
  autoplot() +
  labs(title = "" ) 



lag_turnover <- vic_retail |>
  filter(industry=="Clothing") |>
  gg_lag(turnover, lags = 1:12, geom = "point") +
  labs(x="lag(Turnover,l)",
       y="Turnover")

ggplotly(lag_turnover)

