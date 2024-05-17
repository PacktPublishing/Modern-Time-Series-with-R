## ----ch5-pacakges, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("fable"
       ,"fabletools"
       ,"feasts"
       ,"forecast"
       ,"lubridate"
       ,"Mcomp"
       ,"nycflights13"
       ,"patchwork"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
)



## ----ch5-datafolder

# create data_chap5 folder
if(!dir.exists("data/data_chap5")){
  dir.create("data/data_chap5/")
}


## ----ch5-load-data,eval=TRUE

correct_dt <- read_csv("data/data_chap5/correct_date.csv")
mismatch_dt <- read_csv("data/data_chap5/mismatch_date.csv")



## ----ch5-load-correct-format

correct_dt <- read_csv("data/data_chap5/correct_date.csv")
glimpse(correct_dt)



## ----ch5-load-mismatch-format

mismatch_dt <- read_csv("data/data_chap5/mismatch_date.csv")
glimpse(mismatch_dt)



## ----ch5-format-correct

correct_dt |>
  mutate(date1 = as.Date(date, format = "%d/%m/%Y") )



## ----ch5-format-mismatch

mismatch_dt |>
  mutate(date1 = as.Date(date, format = "%d/%m/%Y") )



## ----ch5-format-mismatch2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE----

mismatch_dt |>
  mutate(date1 = as.Date(date, format = "%d/%m/%y") )




## ----ch5-format-mismatch3

mismatch_dt2 <- mismatch_dt |>
  mutate(date1 = as.Date(date, format = "%d/%m/%Y")) |>
  mutate(year = year(date1))


mismatch_dt2 |>
  group_by(year) |>
  tally()



## ----ch5-format-mismatch4

mismatch_dt |>
  mutate(date2 = dmy(date),
         date3 = ymd(date))


## ----ch5-set-component

set.seed(45)
dt1 <- tibble(
  date = as.Date(c("2019-01-01",
                   "2019-09-12",
                   "2020-01-01",
                   "2020-05-10")),
  value = runif(4)
)

dt1 |>
  mutate(year=year(date)
         ,day_year=yday(date)
         ,day_month=mday(date)
         ,month_name=month(date, label=TRUE)
  )



## ----ch5-set-component-1

dt1 |>
  mutate(year_month=yearmonth(date)
         ,year_quarter=yearquarter(date)
         ,year_week=yearweek(date))



## ----ch5-timeplot-2

class(aus_production)

autoplot(aus_production, Gas) +
  labs(title = "Austalian Quarterly Gas Production" ,
       y = "Gas production(petajules)")



## ----ch5-timeplot-3

aus_production |>
  select(Quarter, Gas)  |>
autoplot() +
  labs(title = "Austalian Quarterly Gas Production" ,
       y = "Gas production(petajules)")


## ----ch5-timeplot-3-print

aus_production |>
  select(Quarter, Gas)  |>
autoplot() +
  labs(title = "Austalian Quarterly Gas Production" ,
       y = "Gas production(petajules)")




## ----ch5-timeplot-4

class(sunspots)


autoplot(sunspots) +
  labs(title = "Monthly Sunspots")



## ----ch5-timeplot-5

flights <- nycflights13::flights |>
   filter(!is.na(dep_time), !is.na(arr_time)) |>
   mutate(sched_dep_datetime = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100)) |>
   mutate(sched_dep_date = date(sched_dep_datetime)) |>
   select(carrier , origin, origin , dest, sched_dep_date, dep_delay) |>
   mutate(is_delayed = ifelse(dep_delay > 0, 'y', 'n'))



## ----ch5-timeplot-6

flights |>
  filter(carrier %in% c("AA", "DL"),
         is_delayed == 'y') |>
  count(sched_dep_date, carrier) |>
  ggplot(aes(x = sched_dep_date,
             y = n,
             group = carrier,
             linetype = carrier)) +
  geom_line() +
  labs(x = " " ,
       y = "# delayed flights") +
  theme_bw() +
  theme(legend.position = "top")



## ----ch5-timeplot-7-data

# average delayed time of all carriers from JFK
jfk_delay <- flights |>
  filter(origin == "JFK" ,
         is_delayed == 'y' ) |>
  group_by(sched_dep_date) |>
  summarise(avg_jfk = mean(dep_delay, na.rm = T)) |>
  ungroup()


flight2_delay <- flights |>
  filter(carrier %in% c("AA", "DL"),
         is_delayed == 'y' ,
         origin == "JFK" ) |>
  group_by(sched_dep_date, carrier, origin, dest) |>
  summarise(avg_delay = mean(dep_delay, na.rm = T)) |>
  ungroup()



## ----ch5-timeplot-7

ggplot() +
  geom_line(data = flight2_delay |>
                  filter( dest %in% c("LAX", "SFO")) ,
           aes(x = sched_dep_date,
                       y = avg_delay,
                       group = carrier,
                       color = carrier,
                       linetype = carrier)) +
  geom_line(data = jfk_delay,
              aes(x = sched_dep_date,  y = avg_jfk)) +
   facet_grid(carrier~dest, scales = "free_y") +
   theme_bw() +
  theme(legend.position = "top") +
  labs(title = " ")



## ----ch5-timeplot-8

aus_production |>
  select(Quarter, Gas) |>
  feasts::gg_season() +
  labs(title = "Australia Quarterly Gas production")



## ----ch5-timeplot-10

AirPassengers |>
  as_tsibble() |>
  gg_season(value, period = "year") +
  labs(x = "",
       y = "Total passengers") +
  theme_bw()



## ----ch5-timeplot-11

flights |>
  filter(is_delayed == "y") |>
  count(sched_dep_date) |>
  as_tsibble(index = sched_dep_date) |>
  gg_season(n, period = "week" ) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "# delayed flights")



## ----ch5-timeplot-12

takeaway_turnover <- readRDS("data/data_chap4/abs_retail.rds") |>
  filter(industry == "Takeaway food services",
         state %in% c("Victoria", "New South Wales"),
         year(month) >= 2015 ,
         year(month) <= 2022) |>
  select(month, state, turnover)


takeaway_turnover |>
  gg_subseries(turnover) +
  labs(x = "",
       y = "Turnover $M" )+
  theme_bw()



## ----ch5-timeplot-13

vic_retail <- readRDS("data/data_chap4/abs_retail.rds") |>
  filter(industry %in% c("Clothing retailing",
                         "Food retailing",
                         "Electrical and electronic goods retailing",
                         "Hardware, building and garden supplies retailing",
                         "Newspaper and book retailing",
                         "Supermarket and grocery stores"),
         state == "Victoria" ,
         year(month) >= 2015,
         year(month) <= 2022 ) |>
  select(month, industry, turnover) |>
  mutate(industry = sapply(str_split(string=industry,
                                     pattern="[, ]+")
                           ,'[', 1))


vic_retail |>
  ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  facet_grid(vars(industry), scales = "free_y") +
  theme_bw()



## ----imeplot-14
install.packages("GGally")
library(GGally)


## ----ch5-timeplot-15

vic_retail |>
pivot_wider(names_from=industry, values_from=turnover) |>
  GGally::ggpairs(columns = 2:6, progress = F)



## ----ch5-timeplot-16

vic_retail |>
  filter(industry=="Clothing") |>
  gg_lag(turnover, lags = 1:12, geom = "point") +
  labs(x="lag(Turnover,l)",
       y="Turnover")



## ----ch5-timeplot-17


vic_retail |>
  filter(industry=="Clothing") |>
  ACF(turnover) |>
  autoplot() +
  labs(title = "" )



## ----ch5-timeplot-18

install.packages("plotly")
library(plotly)

lag_turnover <- vic_retail |>
  filter(industry=="Clothing") |>
  gg_lag(turnover, lags = 1:12, geom = "point") +
  labs(x="lag(Turnover,l)",
       y="Turnover")

ggplotly(lag_turnover)


