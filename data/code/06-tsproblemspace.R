
#install.packages("pacman")
library(pacman)
p_load("conflicted",
       "dataseries",
       "fpp3",
       "tidyverse",
       "tsibble")

conflicts_prefer(
  dplyr::filter,
  fabletools::components
  )


# Swiss retail sale index with 2015 base
swiss_retail_sale <- ds("ch_fso_rtt.brut.n.noga0801", "ts") 
swiss_retail_sale_trend <- ds("ch_fso_rtt.tr.n.noga0801", "ts")

swiss_retail <- cbind(
  sale_index = swiss_retail_sale, 
  index_trend = swiss_retail_sale_trend ) |> 
  as_tsibble()



swiss_retail
## 
## # A tibble: 480 × 3
##    index    key        value
##    <chr>    <chr>      <dbl>
##  1 2000 Jan sale_index  81.5
##  2 2000 Feb sale_index  81.4
##  3 2000 Mar sale_index  84.0
##  4 2000 Apr sale_index  85.0
##  5 2000 May sale_index  92.6
##  6 2000 Jun sale_index  79.4
##  7 2000 Jul sale_index  83.5
##  8 2000 Aug sale_index  81.3
##  9 2000 Sep sale_index  87.5
## 10 2000 Oct sale_index  88.6
## # ℹ 470 more rows

swiss_retail |> 
  filter(key == "sale_index") |> 
  ggplot(aes(x = index, y = value)) +
  geom_line() +
  labs(x = "",
       y = "Index") 



swiss_retail |> 
  mutate(key = factor(key, 
                      levels = c("sale_index", "index_trend"))) |> 
  autoplot(linewidth = 0.9, color = "black") + 
  facet_wrap(key~., ncol = 1, scales = "fixed") +
  labs(x = "",
       y = "Index") +
  theme(legend.position = "none")



aus_accommodation |> 
  filter(State == "New South Wales") |> 
  select(Date, Occupancy) |> 
  autoplot() +
  labs(x = "",
       y = "% of room occupancy")



mod <- aus_accommodation |> 
  filter(State == "New South Wales") |> 
  select(Date, Occupancy) |> 
  model(stl = STL(Occupancy ~ trend(window=4)+
                    season(window="periodic"),
                  robust = TRUE)) 

mod |> 
  components() |> 
  autoplot() +
  labs(x = "")




swiss_sale <- swiss_retail |> 
  filter(key == "sale_index") |> 
  select(index, value)

swiss_mod <- swiss_sale |> 
  model(ARIMA(value)) |> 
   forecast(h = "5 years") 

swiss_mod |>
  autoplot(swiss_sale, 
           level = 95,
           linewidth = 0.9) 
