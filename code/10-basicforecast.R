## ----ch10-packages

#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"rsample"
       ,"patchwork"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
      )

theme_set(theme_bw())


## ----ch10-resolve-conflicts----

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
)




## ----ch10-naive-2 -----

netuse |>
  autoplot() +
  labs(title = "WWWusage: Users connected to internet by minutes",
       x = "Minutes" ,
       y = "# of connections")


## ----ch10-naive-7----

nv_fit |>
  gg_tsresiduals() +
  labs(title = "Residual diagnostics of naïve method fitted on WWWusage")



## ----ch10-snaive-1----

elctr <-
  aus_production |>
  select(Quarter, Electricity) |>
  filter_index("2000-Q1" ~ "2009-Q4" )

elctr_train <-
  elctr |>
  filter_index("2000-Q1" ~ "2008-Q4" )

elctr_test <-
  elctr |>
 filter_index("2009-Q1" ~ "2009-Q4" )


## ----ch10-snaive-2----

snv_fit <-
  elctr_train |>
  model(snaive = SNAIVE(Electricity))

glance(snv_fit)


## ----ch10-snaive-3----

snv_fit |>
  gg_tsresiduals() +
  labs(title = "Residual diagnostics for snaïve method")


## ----ch10-snaive-4----

snv_fit |>
  augment() |>
  features(.innov,
           list(box_pierce,
                ljung_box )
           ,lag = 10)


## ----ch10-snaive-5----

snv_fcst <-
  snv_fit |>
    forecast(h = 4)



## ----ch10-snaive-5----

snv_comb <-
  elctr |>
  mutate(series = "observed") |>
  as_tibble() |>
  # fitted
  bind_rows(
      snv_fit |>
      augment() |>
      select(Quarter, .fitted) |>
      as_tibble() |>
      rename(Electricity = .fitted) |>
      mutate(series = "fitted")) |>
  # forecasts
  bind_rows(
     snv_fcst |>
      as_tibble() |>
      select(Quarter, .mean) |>
      rename(Electricity = .mean) |>
      mutate(series = "snaive")) |>
  mutate(series = factor(series,
         levels = c("observed","fitted","snaive")))


snv_comb |>
  ggplot(aes(x = Quarter,
             y = Electricity,
             color = series,
             lty = series )) +
  geom_line(lwd = 0.8) +
  labs(title = "Observed, fitted and seasonal naive forecasts of electricity production") +
  theme(legend.position = "top") +
  scale_color_viridis_d(option = "plasma", end = 0.7)


## ----ch10-snaive-6----

snv_acc <-
  bind_rows(
    accuracy(snv_fit) ,
    accuracy(snv_fcst, elctr))

snv_acc


## ----ch10-average-noshow----

# fit average model
avg_fit <-
  netuse_train |>
  model(mean = MEAN(value))

# augment average model
avg_aug <-
  avg_fit |>
  augment()

# average model residual test
avg_resid_test <-
  avg_aug |>
  features(.innov, list(box_pierce, ljung_box), lag = 10)

# forecast
avg_fcst <-
  avg_fit |>
  forecast(h = 5)

# accuracy
avg_acc <-
  fabletools::accuracy(avg_fit) |>
  bind_rows(
    fabletools::accuracy(avg_fcst, netuse))



## ----ch10-avg-4----

snv_avg_comb <-
  netuse |>
  as_tibble() |>
  mutate(series = "observed") |>
  bind_rows(
    nv_fcst |>
      as_tibble() |>
      select(index, .mean) |>
      rename(value = .mean) |>
      mutate(series = "naive")) |>
  bind_rows(
    avg_fcst |>
      as_tibble() |>
      select(index, .mean) |>
      rename(value = .mean) |>
      mutate(series = "average")) |>
  mutate(series = factor(series,
                       levels = c("observed", "naive","average")))



## ----ch10-avg-5----

p1 <-
  snv_avg_comb |>
  ggplot(aes(x = index,
             y = value,
             color = series,
             lty = series )) +
  geom_line(lwd = 0.9) +
  labs(title = "Full data") +
  scale_color_manual(name = "series",
                     values = c("observed" = "grey50",
                                "naive" = "blue",
                                "average" = "tomato"))

p2 <-
  snv_avg_comb |>
  filter(index > 95) |>
  ggplot(aes(x = index,
             y = value,
             color = series,
             lty = series )) +
  geom_line(lwd = 0.9) +
  labs(title = "Test set") +
  scale_color_manual(name = "series",
                     values = c("observed" = "grey50",
                                "naive" = "blue",
                                "average" = "tomato"))

comb_plot <-
  p1 + p2 & theme(legend.position = "bottom")

comb_plot +
   plot_layout(guides = "collect")





## ----ch10-tslm-1----

elctr_train |>
  ggplot(aes(x = Quarter, y = Electricity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Quarterly electricity production during 2000 - 2008")





## ----ch10-tslm-3----

lmcomp_fit |>
  gg_tsresiduals() +
  labs(title = "Residual diagnostic of TSLM with trend and seasonal dummies on electricity production ")





## ----ch10-tslm-5----

augment(lmcomp_fit) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Scatterplot of residuals versus fitted values",
       x = "Fitted",
       y = "Residuals")



## ----ch10-tslm-6-noshow----

lmcomp_fcst <-
  lmcomp_fit |>
  forecast(h = 4)

lmcomp_acc <-
  accuracy(lmcomp_fit) |>
  bind_rows(
    accuracy(lmcomp_fcst, elctr)
  )




## ----ch10-tslm-7----

lmcomp_fcst |>
  autoplot(elctr_train) +
  labs(title = "Forecasts of electricity production using time series components",
       y = "Electricity (Gwh)")



## ----ch10-tslm-8----

# tidy
dallas <- txhousing |>
  filter(city == "Dallas")|>
  mutate(year_month = yearmonth("2000 Jan")+ 0:186) |>
  as_tsibble(index = year_month )

# model fit
lm_fit <- dallas |>
  model(lm = TSLM(sales ~ listings + inventory + season()))

# future scenario
future_dt <- scenarios(
  "High listings" = new_data(dallas, 5) |>
    mutate(listings = 30000,inventory = 5),
  "Low listings" = new_data(dallas, 5) |>
    mutate(listings = 15000,inventory = 5)
)

# forecast scenario
scr_fcst <-
  forecast(lm_fit, new_data = future_dt)



## ----ch10-tslm-9----

dallas <- txhousing |>
  filter(city == "Dallas")|>
  mutate(year_month = yearmonth("2000 Jan")+ 0:186) |>
  as_tsibble(index = year_month )


## ----ch10-tslm-10----

lm_fit <- dallas |>
  model(lm = TSLM(sales ~ listings + inventory + season()))

report(lm_fit)


## ----ch10-tslm-11----

augment(lm_fit) |>
  ggplot(aes(x = year_month)) +
  geom_line(aes(y=sales, colour="Sales"), lwd=0.7) +
  geom_line(aes(y=.fitted, colour="Fitted"), lwd=0.8) +
  labs(title = "Dallas house sales actual and fitted values",
       x = "") +
  scale_color_manual(name="Series",
                     values = c(Sales="black",Fitted="tomato"))


## ----ch10-tslm-12----

future_dt <- 
  scenarios(
  "High listings" = new_data(dallas, 5) |>
    mutate(listings = 30000,inventory = 5),
  "Low listings" = new_data(dallas, 5) |>
    mutate(listings = 15000,inventory = 5)
)




## ----ch10-tslm-14, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Actual and fitted values from TSLM on Dallas housing sales", fig.width=8, fig.height=4, tidy=TRUE----

dallas |>
ggplot(aes(x = year_month)) +
  geom_line(aes(y = sales), color = "grey50") +
  geom_line(data = scr_fcst,
            aes(y = .mean, color = .scenario), lwd=0.8) +
  scale_color_manual(values = c("High listings"="tomato",
                                "Low listings" = "blue"))



## ----ch10-mult-accuracy-1,eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', tidy=TRUE----

nv_mult_fit <-
  netuse_train |>
  model(naive = NAIVE(value),
        mean = MEAN(value),
        drift = RW(value ~drift())
        )

nv_mult_fcst <-
  nv_mult_fit|>
  forecast(h = 5)

nv_mult_acc <-
  bind_rows(fabletools::accuracy(nv_mult_fit),
            fabletools::accuracy(nv_mult_fcst, netuse))



## ----ch10-mult-accuracy-2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', tidy=TRUE----

nv_mult_acc


## ----ch10-mult-accuracy-3,eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Forecasting performance of Naive, Mean and RW models", fig.width=8, fig.height=4, tidy=TRUE----

bind_rows(
nv_mult_fcst |>
    as_tsibble() |>
    select(.model, index,.mean) |>
    rename(value = .mean) |>
    mutate(set = "test")
  ,bind_cols(.model = "observed"
              ,netuse
             ,set = "original")
) |>
  ggplot(aes(x=index, y=value, colour=.model)) +
  geom_line(size=1.5) +
  geom_vline(xintercept = 96, linetype=2)+
  labs(title = "Forecasting performance on WWWusage series")



## ----ch10-tscv-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

netuse_cv <-
  netuse |>
  stretch_tsibble(.init = 60, .step = 5) |>
  relocate(.id)



## ----ch10-tscv-2-noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----

# forecasts and organise
nv_cv_fcst <-
  netuse_cv |>
  model(drift = RW(value ~ drift())) |>
  forecast(h = 5) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "value", distribution = value )

nv_cv_acc <-
  nv_cv_fcst |>
  accuracy(netuse, by = c("h",".model"))


