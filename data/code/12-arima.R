#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"patchwork"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata" )

conflicts_prefer(
  fabletools::accuracy,
  fabletools::forecast,
  dplyr::filter
)



set.seed(345876)
ar1_sim <- tibble(
  time_index = 1:120,
  values = c(arima.sim(model = list(ar=0.8),
             n = 120, 
             mean = 0,
             sd = 1))) |> 
  as_tsibble(index = time_index)



ar1_sim |>
  gg_tsdisplay(plot_type="partial")




ar_manual <- ar1_sim |> 
  model(manual = ARIMA(values ~ pdq(1,0,0)))

report(ar_manual)
## 
## Series: values
## Model: ARIMA(1,0,0)
## 
## Coefficients:
##         ar1
##       0.780
## s.e.  0.056
## 
## sigma^2 estimated as 0.9266:  log likelihood=-165.66
## AIC=335.33   AICc=335.43   BIC=340.9
## 

tidy(ar_manual)
## 
## # A tibble: 1 × 6
##   .model term  estimate std.error statistic  p.value
##   <chr>  <chr>    <dbl>     <dbl>     <dbl>    <dbl>
## 1 manual ar1      0.780    0.0560      13.9 7.51e-27

data("global_economy")
usa_gdp <- global_economy |> 
  filter(Code == "USA") |> 
  select(Year, GDP) |> 
  rename( year = Year, gdp = GDP) |>
  mutate(gdp = gdp/10^10, 
         gdp_diff_1 = difference(gdp, lag = 1, differences = 1),
         gdp_diff_2 = difference(gdp, lag = 1, differences = 2))



usa_gdp
## 
## # A tsibble: 58 x 4 [1Y]
##     year   gdp gdp_diff_1 gdp_diff_2
##    <dbl> <dbl>      <dbl>      <dbl>
##  1  1960  54.3      NA        NA
##  2  1961  56.3       2        NA
##  3  1962  60.5       4.18      2.18
##  4  1963  63.9       3.35     -0.830
##  5  1964  68.6       4.72      1.37
##  6  1965  74.4       5.79      1.07
##  7  1966  81.5       7.13      1.34
##  8  1967  86.2       4.67     -2.46
##  9  1968  94.2       8.08      3.41
## 10  1969 102.        7.74     -0.340
## # ℹ 48 more rows

 usa_gdp |> 
   pivot_longer(cols=c(gdp, gdp_diff_1, gdp_diff_2),
                names_to="diff_order",
                values_to="value") |> 
  mutate(order = factor(diff_order, levels = c("gdp", "gdp_diff_1", "gdp_diff_2"))) |>
  ggplot(aes(x = year, y = value)) +
  geom_line(size = 0.8) +
  facet_wrap(~ diff_order,
             ncol = 1, 
             scales = "free_y") +
  labs(x = "" , y = "") 



usa_gdp <- usa_gdp |> 
  mutate(gdp_log = log(gdp))

usa_gdp_diff <- usa_gdp |>
  select(year, gdp, gdp_log) %>% 
  pivot_longer(cols = c(gdp, gdp_log), 
               names_to = "series", 
               values_to = "values") |>
  group_by(series) |>
  features(values, unitroot_ndiffs )


usa_gdp_diff
## 
## # A tibble: 2 × 2
##   series  ndiffs
##   <chr>    <int>
## 1 gdp          2
## 2 gdp_log      2





usa_gdp <- usa_gdp |>
  mutate(gdp_log_diff_2 = difference(gdp_log, lag = 1, differences = 2))

usa_gdp |>
  select(year, gdp_log_diff_2)|>
  gg_tsdisplay(plot_type = "partial") +
  labs(x = "", y="")




n <- 120
set.seed(98765)
ma_sim <- tibble(
  time = 1:n,
  y = c(arima.sim(n = n,
         model=list(ar = 0.12, ma = c(0.8,0.45)),
         mean = 0 , sd = 1 ))) |>
  as_tsibble(index = time)

ma_sim |>
  gg_tsdisplay(plot_type = "partial")





ma_sim |>
  features(y, unitroot_ndiffs)


## # A tibble: 1 × 1
##   ndiffs
##    <int>
## 1      0



 glance(ma_model)
## 
## # A tibble: 2 × 8
##   .model   sigma2 log_lik   AIC  AICc   BIC ar_roots  ma_roots
##   <chr>     <dbl>   <dbl> <dbl> <dbl> <dbl> <list>    <list>
## 1 arma_mod   1.04   -171.  351.  351.  362. <cpl [1]> <cpl [2]>
## 2 ma_mod     1.03   -171.  349.  349.  357. <cpl [0]> <cpl [2]>

tidy(ma_model)
## 
## # A tibble: 5 × 6
##   .model   term  estimate std.error statistic  p.value
##   <chr>    <chr>    <dbl>     <dbl>     <dbl>    <dbl>
## 1 arma_mod ar1      0.111    0.242      0.456 6.49e- 1
## 2 arma_mod ma1      0.854    0.236      3.62  4.26e- 4
## 3 arma_mod ma2      0.407    0.145      2.80  6.02e- 3
## 4 ma_mod   ma1      0.956    0.0889    10.8   2.55e-19
## 5 ma_mod   ma2      0.466    0.0711     6.54  1.55e- 9


usagdp_full <- usa_gdp |>
  select(year, gdp)

usagdp_train <-usagdp_full |>
  filter_index(1960~2007)

usagdp_test <- usagdp_full|>
  filter_index(2008~2017)

usagdp_arima <-
  usagdp_train |>
  model(arima_121 = ARIMA(log(gdp) ~ pdq(1,2,1)),
        arima_221 = ARIMA(log(gdp) ~ pdq(2,2,1)),
        arima_auto = ARIMA(log(gdp)))

usagdp_fcst <- usagdp_arima |>
  forecast(h=10)

usagdp_fcst_eval <- bind_rows(
  accuracy(usagdp_arima),
  accuracy(usagdp_fcst, usagdp_full)
  )

nsarima_p1 <- usagdp_fcst |>
  autoplot(usagdp_train, level = NULL,size = 0.9)+
  labs(title="Forecasts with training set")

nsarima_p2 <- usagdp_fcst |>
  autoplot(usagdp_test,level = NULL,size = 0.9)

 nsarima_p1 + nsarima_p2+
   plot_layout(guides = "collect")+
  labs(title="Forecasts with test set")






usagdp_arima
## 
## # A mable: 1 x 3
##        arima_121      arima_221     arima_auto
##          <model>        <model>        <model>
## 1 <ARIMA(1,2,1)> <ARIMA(2,2,1)> <ARIMA(0,2,1)>
## 

glance(usagdp_arima)
## 
## # A tibble: 3 × 8
##   .model       sigma2 log_lik   AIC  AICc   BIC ar_roots  ma_roots
##   <chr>         <dbl>   <dbl> <dbl> <dbl> <dbl> <list>    <list>
## 1 arima_121  0.000372    117. -228. -228. -223. <cpl [1]> <cpl [1]>
## 2 arima_221  0.000374    118. -227. -226. -220. <cpl [2]> <cpl [1]>
## 3 arima_auto 0.000364    117. -230. -230. -227. <cpl [0]> <cpl [1]>

tidy(usagdp_arima)
## 
## # A tibble: 6 × 6
##   .model     term  estimate std.error statistic     p.value
##   <chr>      <chr>    <dbl>     <dbl>     <dbl>       <dbl>
## 1 arima_121  ar1     0.0113     0.206    0.0547 0.957
## 2 arima_121  ma1    -0.650      0.143   -4.54   0.0000402
## 3 arima_221  ar1    -0.107      0.248   -0.429  0.670
## 4 arima_221  ar2    -0.171      0.189   -0.908  0.369
## 5 arima_221  ma1    -0.528      0.222   -2.38   0.0216
## 6 arima_auto ma1    -0.645      0.107   -6.02   0.000000275



usagdp_fcst_eval |>
  select(.model, .type, ME, RMSE, MASE, ACF1)

## # A tibble: 6 × 6
##   .model     .type         ME   RMSE   MASE  ACF1
##   <chr>      <chr>      <dbl>  <dbl>  <dbl> <dbl>
## 1 arima_121  Training   -1.15   9.30  0.205 0.200
## 2 arima_221  Training   -1.01   9.24  0.203 0.204
## 3 arima_auto Training   -1.14   9.33  0.206 0.204
## 4 arima_121  Test     -299.   330.   10.1   0.623
## 5 arima_221  Test     -311.   343.   10.5   0.624
## 6 arima_auto Test     -299.   330.   10.1   0.623






retail <- aus_retail |>
  filter(`Series ID`=="A3349397X")  |>
  select(Month, Turnover) |>
  rename(month = Month, turnover = Turnover)

retail |>
  gg_tsdisplay(plot_type = "partial", lag = 36)

# log transformation, non-seasonal and seasonal differences
retail <- retail |>
  mutate(turnover_log = log(turnover),
         seas_diff = difference(turnover_log, differences = 1, lag = 1),
         nonseas_diff = difference(seas_diff, lag = 12))
retail |>
  gg_tsdisplay(nonseas_diff,
               plot_type="partial", lag=36)

# fit
sarima_fit <-
  retail |>
  model(manual = ARIMA(log(turnover)~ pdq(2,0,1) + PDQ(1,1,2)),
        auto = ARIMA(log(turnover)))

sarima_fit |>
  pivot_longer(everything(),
               names_to="model_name",
               values_to="model_order")

# forecast
sarima_fcst <- sarima_fit |>
  select(auto) |>
  forecast(h = 12)

sarima_fcst |>
  autoplot(retail |>
           filter(year(month) >= "2010"),
           size = 1.2)+
  labs(title = latex2exp::TeX(paste0("Forecasts of monthly NSW household goods retailing using $ARIMA(1,0,1)(2,1,2)_{12}$ with a drift term")))








retail |>
  features(log(turnover),
           list(unitroot_ndiffs, unitroot_nsdiffs))

## # A tibble: 1 × 2
##   ndiffs nsdiffs
##    <int>   <int>
## 1      1       1






sarima_fit <- retail |>
  model(manual = ARIMA(log(turnover)~ pdq(2,1,1) + PDQ(1,1,2)),
        auto = ARIMA(log(turnover)))

sarima_fit |>
  pivot_longer(everything(),
               names_to="model_name",
               values_to="model_order")

## # A mable: 2 x 2
## # Key:     model_name [2]
##   model_name                        model_order
##   <chr>                                 <model>
## 1 manual              <ARIMA(2,1,1)(1,1,2)[12]>
## 2 auto       <ARIMA(1,0,1)(2,1,2)[12] w/ drift>

tidy(sarima_fit)
## 
## # A tibble: 13 × 6
##    .model term     estimate std.error statistic   p.value
##    <chr>  <chr>       <dbl>     <dbl>     <dbl>     <dbl>
##  1 manual ar1      -0.422    0.255      -1.65   9.91e-  2
##  2 manual ar2      -0.271    0.0999     -2.71   6.96e-  3
##  3 manual ma1      -0.0222   0.271      -0.0820 9.35e-  1
##  4 manual sar1     -0.347    0.368      -0.942  3.47e-  1
##  5 manual sma1     -0.192    0.359      -0.534  5.93e-  1
##  6 manual sma2     -0.303    0.210      -1.44   1.50e-  1
##  7 auto   ar1       0.939    0.0203     46.4    3.41e-169
##  8 auto   ma1      -0.433    0.0520     -8.34   1.05e- 15
##  9 auto   sar1      0.735    0.323       2.27   2.35e-  2
## 10 auto   sar2     -0.251    0.0790     -3.17   1.61e-  3
## 11 auto   sma1     -1.28     0.331      -3.86   1.30e-  4
## 12 auto   sma2      0.500    0.216       2.32   2.08e-  2
## 13 auto   constant  0.00161  0.000250    6.42   3.60e- 10






elect_gas <- aus_production |>
  select(Quarter, Electricity, Gas) |>
  rename(quarter = Quarter,
         electricity = Electricity,
         gas = Gas)

elect_gas_train <- elect_gas |>
  filter(year(quarter) <= "2008")

elect_gas_test <-elect_gas |>
  filter(year(quarter) > "2008")

elect_gas_train |>
  pivot_longer(cols = c("electricity", "gas"),
               names_to = "sector",
               values_to = "production") |>
  ggplot(aes(x=quarter, y = production)) +
  geom_line() +
  facet_wrap(sector~., ncol=1, scales="free_y")+
  labs(y="")

# model fit
elect_gas_fit <-
  elect_gas_train |>
  model(arimax = ARIMA(electricity ~ gas))

report(elect_gas_fit)

# forecast
elect_gas_fcst <- elect_gas_fit |>
  forecast(h = 10,
           new_data = elect_gas_test |> select(quarter, gas))

elect_gas_fcst |>
  autoplot(elect_gas_train |>
             filter(year(quarter)>= 2000))



report(elect_gas_fit)
## 
## Series: electricity
## Model: LM w/ ARIMA(2,0,0)(1,1,2)[4] errors
## 
## Coefficients:
##          ar1     ar2    sar1     sma1    sma2      gas
##       0.4463  0.2223  0.9157  -1.4413  0.5236  37.8759
## s.e.  0.0687  0.0836  0.0795   0.1228  0.1032   9.9100
##       intercept
##        891.3641
## s.e.   121.5085
## 
## sigma^2 estimated as 411853:  log likelihood=-1637.14
## AIC=3290.28   AICc=3291   BIC=3316.98
