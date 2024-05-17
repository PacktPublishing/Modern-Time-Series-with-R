## ----ch11-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"Mcomp"
       ,"rsample"
       ,"patchwork"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
      )

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
)

theme_set(theme_bw())




## ----ch11-ses-1----

install.packages("Mcomp")
library(Mcomp)

M3$N0003




## ----ch11-ses-2

n3_train <-  
  M3$N0003$x |> 
  as_tsibble() |> 
  rename(year = index,
         sales = value)
  
n3_test <-  
  M3$N0003$xx |> 
  as_tsibble() |> 
  rename(year = index,
         sales = value)

autoplot(n3_train) +
  labs(title = "Series N0003(1975-1988) from M3 competition")



## ----ch11-ses-addv-3

ses_addtv_fit <-
  n3_train |>
  model(`SES additive` = ETS(sales ~ error("A") + trend("N") + season("N")))

report(ses_addtv_fit)



## ----ch11-ses-addv-4

ses_addtv_fcst <-
  ses_addtv_fit |>
  forecast(h = 6)

ses_addtv_acc <-
   bind_rows(
    accuracy(ses_addtv_fit) ,
    accuracy(ses_addtv_fcst,
             bind_rows(n3_train,
                       n3_test)))

## ----ch11-ses-5

ses_addtv_fcst |>
  autoplot(bind_rows(n3_train,
                     n3_test)) +
  geom_line(data = augment(ses_addtv_fit),
            aes(y = .fitted),
            col = "#D55E00") +
  geom_vline(xintercept = 1988, 
             color="grey", 
             linetype=2, 
             linewidth=0.97 ) +
  guides(colour = "none")+
  labs(title = "N0003 series: actual, fitted and forecasts from ETS(A,N,N) model")




## ----ch11-ses-6

ses_mult_fit <- 
  n5_train |> 
  model(`SES multiplicative` = ETS(sales ~ error("M") + trend("N") + season("N")))

ses_mult_fcst <-
  ses_mult_fit |> 
  forecast(h = 6)

ses_mult_fcst |>
  autoplot(bind_rows(n5_train,
                     n5_test)) +
  geom_line(data = augment(ses_mult_fit),
            aes(y = .fitted),
            col = "#D55E00") +
  geom_vline(xintercept = 1988, 
             color="grey", 
             linetype=2, 
             linewidth=0.97 ) +
  guides(colour = "none")+
  labs(title = "N0005 series: actual, fitted and forecasts from ETS(M,N,N) model")



## ----ch11-holt-1

n10_train <- 
  M3$N0010$x |> 
  as_tsibble()

n10_train |> 
  autoplot() +
  labs(title = "N0010 series from M3 competition")


## ----ch11-holt-2

n10_fit <-
 n10_train |>
  model('Holt additive' = ETS(value ~ error("A") + trend("A") + season("N")))

report(n10_fit)



## ----ch11-hlt-3

n36_train <- 
  M3$N0036$x |> 
  as_tsibble()

n36_train |> 
autoplot() +
  labs(title = "N0036 series from M3 competition")



## ----ch11-holt-4

n36_fit <-
  n36_train |>
  model(`Holt multiplicative` = ETS(value ~ error("M") + trend("A") + season("N")))

report(n36_fit)



## ----ch11-damp-1

n36_cv <-
  n36_train |> 
  stretch_tsibble(.init = 10) |> 
  model(SES = ETS(value ~ error("A") + trend("N") + season("N")),
        `Holt additive` = ETS(value ~ error("A") + trend("A") + season("N")),
        `Holt multi` = ETS(value ~ error("M") + trend("A") + season("N")),
        `Damped additive` = ETS(value ~ error("A") + trend("Ad") + season("N"))) |> 
  forecast(h = 1) |> 
  accuracy(n36_train)



## ----ch11-damp-2
# cross-validated performance
n36_cv



## ----ch11-damp-3

  n36_train |> 
   model(
        `Holt additive` = ETS(value ~ error("A") + trend("A") + season("N")),
        `Damped additive` = ETS(value ~ error("A") + trend("Ad") + season("N"))) |> 
  forecast(h = 6) |> 
  autoplot(n36_train, 
           level = NULL,
           linewidth = 0.98)



## ----ch11-hw-1
  
gas <- 
  aus_production |> 
  select(Quarter, Gas)|> 
  filter_index("1956 Q1" ~ "2009 Q4")

gas_train <-
  gas |> 
  filter_index("1956 Q1" ~ "2007 Q4")

gas_test <-
  gas |> 
  filter_index("2008 Q1" ~ "2009 Q4")

gas_train |> 
  autoplot() +
  labs(title = "Australina Gas production (1956 Q1-2009 Q4)")


## ----ch11-hw-2

# model fit
gas_fit <-
  gas_train |> 
  model(`HW additive` = ETS(Gas ~ error("A") + trend("A") + season("A")),
        `HW multiplicative` = ETS(Gas ~ error("M") + trend("A") + season("M")),
        `HW damped` = ETS(Gas ~ error("A") + trend("Ad") + season("M")))

gas_fcst <-
  gas_fit |> 
  forecast(h = "2 years")



## ----ch11-hw-3

accuracy(gas_fcst, gas)



## ----ch11-hw-4

  gas_fcst |> 
  autoplot(gas |> 
            filter_index("2005 Q1"~"2009 Q4")
     ,level = NULL
     ,size = 0.9) +
  labs(title = "Australian Gas production Forecasts using Holt-Winter's model",
       y = "Gas production") +
  guides(colour = guide_legend(title = "Forecast"))


## ----ch11-ets-automated
  
  
ets_sheep <-
  aus_livestock |>
  filter( Animal == "Sheep") |>
  model(ets = ETS(Count))


