## ----ch12-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
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
       ,"tsibbledata"
      )

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
)

theme_set(theme_bw())


## ----ch12-ar-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

set.seed(345876)
ar1_dt <- tibble(Time = 1:120,
                 Values = c(arima.sim(n=120, 
                            model=list(ar=0.8),
                            mean = 26))) |> 
  as_tsibble(index = Time)



## ----ch12-ar-2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Line plot and ACF of an observed series",fig.width=10,fig.height=4, size='tiny'----

ar1_dt |> 
  gg_tsdisplay(plot_type="partial")
 


## ----ch12-ar-3, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## ar_manual <- ar1_dt |>
##   model(manual = ARIMA(Values ~ pdq(3,0,0)))
## 
## report(ar_manual)
## 
## # Series: Values
## # Model: ARIMA(3,0,0) w/ mean
## #
## # Coefficients:
## #           ar1     ar2      ar3  constant
## #       -0.4318  0.0549  -0.2367   24.7394
## # s.e.   0.1451  0.1644   0.1476    0.1505
## #
## # sigma^2 estimated as 1.06:  log likelihood=-61.94
## # AIC=133.87   AICc=135.45   BIC=142.79
## 
## 
## tidy(ar_manual)
## 
## # # A tibble: 4 × 6
## #   .model term   estimate std.error statistic p.value
## #   <chr>  <chr>     <dbl>     <dbl>     <dbl>   <dbl>
## # 1 arima  ar1      -0.432     0.145    -2.98    0.005
## # 2 arima  ar2       0.055     0.164     0.334   0.74
## # 3 arima  ar3      -0.237     0.148    -1.60    0.116
## # 4 arima  const…   24.7       0.151   164.      0
## 


## ----ch12-ar-4, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## ar_auto <- ar1_dt |>
##   model(auto = ARIMA(Values))
## 
## report(ar_auto)
## 
## # Series: Values
## # Model: ARIMA(1,0,0) w/ mean
## #
## # Coefficients:
## #           ar1  constant
## #       -0.5688   24.0479
## # s.e.   0.1206    0.1561
## #
## # sigma^2 estimated as 1.105:  log likelihood=-63.79
## # AIC=133.59   AICc=134.19   BIC=138.94
## 
## tidy(ar_auto)
## 
## # # A tibble: 2 × 6
## #   .model term   estimate std.error statistic p.value
## #   <chr>  <chr>     <dbl>     <dbl>     <dbl>   <dbl>
## # 1 auto   ar1      -0.569     0.121     -4.72       0
## # 2 auto   const…   24.0       0.156    154.         0
## 


## ----ch12-diff-noshow,  eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----

usa_gdp <- global_economy |> 
  filter(Code == "USA") |> 
  select(Year, GDP) |> 
  mutate(GDP = GDP/10^10, 
         `GDP->(d=1)` = difference(GDP,lag=1,differences=1),
         `GDP->(d=2)` = difference(GDP,lag=1,differences=2))

# ecm_plot <- usa_gdp |> 
#    pivot_longer(cols=c(GDP, 
#                       `GDP(d=1)`,
#                       `GDP(d=2)`),
#                 names_to="diff_order",
#                 values_to="value") |> 
#   ggplot(aes(x=Year,y=value)) +
#   geom_line() +
#   facet_wrap(~diff_order,
#              ncol = 1, 
#              scales = "free_y") +
#   labs(title = "USA GDP with varying degrees of differencing",
#        y="")



## ----ch12-diff-1,  eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
usa_gdp <- global_economy |> 
  filter(Code == "USA") |> 
  select(Year, GDP) |> 
  mutate(GDP = GDP/10^10, 
         `GDP->(d=1)` = difference(GDP,lag=1,differences=1),
         `GDP->(d=2)` = difference(GDP,lag=1,differences=2))


## ----ch12-diff-2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Impact of differencing on USA GDP (1960-2017) ",fig.width=8,fig.height=6, size='tiny'----
 usa_gdp |> 
   pivot_longer(cols=c(GDP, 
                      `GDP->(d=1)`,
                      `GDP->(d=2)`),
                names_to="diff_order",
                values_to="value") |> 
  ggplot(aes(x=Year,y=value)) +
  geom_line(size=0.8) +
  facet_wrap(~diff_order,
             ncol = 1, 
             scales = "free_y") +
  labs(title = "USA GDP with varying degrees of differencing",
       y="") +
  theme(axis.text=element_text(size=12)
       ,strip.text=element_text(size=12))



## ----ch12-diff-3,  eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Visualization of second order differenced USA GDP series",fig.width=10,fig.height=4, size='tiny'----
## 
## usa_gdp |>
##   select(Year, GDP) |>
##   features(GDP, unitroot_ndiffs )
## 
## # # A tibble: 1 × 1
## #   ndiffs
## #    <int>
## # 1      2
## 
## usa_gdp |>
##   select(Year, GDP) |>
##   mutate(`GDP->log` = log(GDP)) |>
##   features(`GDP->log`, unitroot_ndiffs )
## 
## # # A tibble: 1 × 1
## #   ndiffs
## #    <int>
## # 1      2
## 


## ----ch12-diff-4, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Visualization of second order differenced USA GDP series",fig.width=10,fig.height=4, size='tiny'----
usa_gdp |> 
  select(Year, `GDP->(d=2)`) |> 
  gg_tsdisplay(plot_type="partial") +
  labs(title = "GDP->(d=2)", 
       y = "")


## ----ch12-diff-5,eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="ACF and PACF of log(GDP) after second order differencing",fig.width=10,fig.height=4, size='tiny'----

usa_gdp <- usa_gdp |> 
  mutate(`GDP->log->(d=2)` = difference(log(GDP), lag=1, differences=2))
  
usa_gdp |> 
  select(Year, `GDP->log->(d=2)`)|> 
  gg_tsdisplay(plot_type="partial")+
  labs(title = "GDP->log->(d=2)", 
       y="")


## ----ch12-ma-1,eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Example of a moving average model",fig.width=10,fig.height=4, size='tiny'----
n <- 120
set.seed(98765)
ma_dt <- tibble(Time = 1:n,
                 y = c(arima.sim(n=n, 
                            model=list(ar=0.12,
                            ma=c(0.8,0.45)),
                            mean=30))) |> 
  as_tsibble(index = Time)

ma_dt |> 
  gg_tsdisplay(plot_type = "partial")



## ----ch12-ma-2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## ma_dt |>
##   features(y, unitroot_ndiffs)
## 
## # # A tibble: 1 × 1
## #   ndiffs
## #    <int>
## # 1      0
## 


## ----ch12-ma-3, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## ma_dt |>
##   model(ma = ARIMA(y ~ pdq(1,0,2))) |>
##    tidy() |>
##    mutate(across(where(is.numeric), round, 3))
## 
## # # A tibble: 4 × 6
## #   .model term  estimate std.error statistic p.value
## #   <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
## # 1 ma     ar1      0.109     0.243     0.448   0.655
## # 2 ma     ma1      0.855     0.236     3.62    0
## # 3 ma     ma2      0.407     0.146     2.79    0.006
## # 4 ma     cons…   68.3       0.206   332.      0
## 
## 
## ma_dt |>
##   model(ma = ARIMA(y ~ pdq(0,0,2))) |>
##   tidy() |>
##    mutate(across(where(is.numeric), round, 3))
## 
## # # A tibble: 3 × 6
## #   .model term  estimate std.error statistic p.value
## #   <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
## # 1 ma     ma1      0.956     0.089     10.8        0
## # 2 ma     ma2      0.465     0.071      6.53       0
## # 3 ma     cons…   76.6       0.221    347.         0
## 


## ----ch12-nsarima-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

usagdp_train <- 
  usa_gdp |> 
  select(Year, GDP) |> 
  filter_index(1960~2007)

usagdp_test <- 
  usa_gdp |> 
  select(Year, GDP) |> 
  filter_index(2008~2017)

usagdp_arima <- 
  usagdp_train |> 
  model(manual = ARIMA(log(GDP)~pdq(3,2,0)),
        auto = ARIMA(log(GDP)))



## ----ch12-nsarima-2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## glance(usagdp_arima)
## 
## # # A tibble: 2 × 8
## #   .model   sigma2 log_lik   AIC  AICc   BIC
## #   <chr>     <dbl>   <dbl> <dbl> <dbl> <dbl>
## # 1 manual 0.000380    117. -226. -225. -219.
## # 2 auto   0.000364    117. -230. -230. -227.
## 
## tidy(usagdp_arima)
## 
## # # A tibble: 4 × 6
## #   .model term  estimate std.error statistic p.value
## #   <chr>  <chr>    <dbl>     <dbl>     <dbl>   <dbl>
## # 1 manual ar1     -0.608     0.149     -4.08   0
## # 2 manual ar2     -0.44      0.158     -2.78   0.008
## # 3 manual ar3     -0.204     0.147     -1.39   0.171
## # 4 auto   ma1     -0.645     0.107     -6.02   0


## ----ch12-nsarima-3-noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
usagdp_fcst <- 
  usagdp_arima |> 
  forecast(h=10) 


## ----ch12-nsarima-3, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## usagdp_fcst <-
##   usagdp_arima |>
##   forecast(h=10)
## 
## accuracy(udagdp_fcst, usagdp_test) |>
##   select(.model, .type, ME, RMSE, MAPE, ACF1)
## 
## # # A tibble: 2 × 6
## #   .model .type    ME  RMSE  MAPE  ACF1
## #   <chr>  <chr> <dbl> <dbl> <dbl> <dbl>
## # 1 auto   Test  -299.  330.  17.4 0.623
## # 2 manual Test  -323.  357.  18.8 0.626
## 


## ----ch12-nsarima-4, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, results='asis', fig.cap="Forecasting USA GDP with an automatically selected ARIMA(0,2,1) and manually selected ARIMA(3,2,0) models",fig.width=10,fig.height=4, size='tiny'----
nsarima_p1 <-
  usagdp_fcst |> 
  autoplot(usagdp_train,
           level = NULL
            ,size = 0.9)+
  labs(title="Forecasts with training set")
  
nsarima_p2 <-
  usagdp_fcst |> 
  autoplot(usagdp_test,
           level = NULL
            ,size = 0.9)

 nsarima_p1 + nsarima_p2+
   plot_layout(guides = "collect")+
  labs(title="Forecasts with test set")
   


## ----ch12-sarima-1-noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----

retail <- aus_retail |> 
  filter(`Series ID`=="A3349397X")  |> 
  select(Month, Turnover)
  
# log and seasonally differenced
retail <-
  retail |> 
  mutate(Turnover_diff = difference(log(Turnover),
                                    lag=12))
# fit 
sarima_fit <- 
  retail_dt |> 
  model(manual = ARIMA(log(Turnover)~ pdq(2,0,1) + PDQ(1,1,2)),
        auto = ARIMA(log(Turnover)))




## ----ch12-sarima-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Monthly NSW household goods retailing, 1982-2018",fig.width=10,fig.height=4, size='tiny'----

retail <- 
  aus_retail |> 
  filter(`Series ID`=="A3349397X") |> 
  select(Month, Turnover)
  
retail |> 
  gg_tsdisplay(plot_type = "partial",
               lag = 36)



## ----ch12-sarima-2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## retail |>
##   features(log(Turnover), unitroot_ndiffs)
## # # A tibble: 1 × 1
## #   ndiffs
## #    <int>
## # 1      1


## ----ch12-sarima-3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Seasonally differenced monthly NSW household goods retailing",fig.width=10,fig.height=4, size='tiny'----

retail <-
  retail |> 
  mutate(Turnover_diff = difference(log(Turnover),
                                    lag=12))

retail |> 
  gg_tsdisplay(Turnover_diff,
               plot_type="partial",
               lag=36)


## ----ch12-sarima-4, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## sarima_fit <-
##   retail |>
##   model(manual = ARIMA(log(Turnover)~ pdq(2,0,1) + PDQ(1,1,2)),
##         auto = ARIMA(log(Turnover)))
## 
## sarima_fit |>
##   pivot_longer(everything(),
##                names_to="model_name",
##                values_to="model_order")
## 
## # # A mable: 2 x 2
## # # Key:     model_name [2]
## #   model_name                        model_order
## #   <chr>                                 <model>
## # 1 manual     <ARIMA(2,0,1)(1,1,2)[12] w/ drift>
## # 2 auto       <ARIMA(1,0,1)(2,1,2)[12] w/ drift>
## 


## ----ch12-sarima-5, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## tidy(sarima_fit) |>
##   mutate(across(where(is.numeric), round,3 ))
## 
## # # A tibble: 14 × 6
## #    .model term     estimate std.error statistic p.value
## #    <chr>  <chr>       <dbl>     <dbl>     <dbl>   <dbl>
## #  1 manual ar1         0.984     0.108     9.10    0
## #  2 manual ar2        -0.042     0.095    -0.444   0.657
## #  3 manual ma1        -0.475     0.096    -4.97    0
## #  4 manual sar1       -0.321     0.378    -0.85    0.396
## #  5 manual sma1       -0.21      0.367    -0.572   0.568
## #  6 manual sma2       -0.303     0.217    -1.40    0.163
## #  7 manual constant    0.004     0.001     7.35    0
## #  8 auto   ar1         0.939     0.02     46.4     0
## #  9 auto   ma1        -0.433     0.052    -8.34    0
## # 10 auto   sar1        0.735     0.323     2.27    0.023
## # 11 auto   sar2       -0.251     0.079    -3.17    0.002
## # 12 auto   sma1       -1.28      0.331    -3.86    0
## # 13 auto   sma2        0.5       0.216     2.32    0.021
## # 14 auto   constant    0.002     0         6.42    0
## 


## ----ch12-sarima-6, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Forecasts of monthly NSW household goods retailing using automatically selected ARIMA(1,0,1)(2,1,2)[12] model",fig.width=10,fig.height=4, size='tiny'----
sarima_fcst <- 
  sarima_fit |> 
  select(auto) |> 
  forecast(h = 12)

sarima_fcst |> 
  autoplot(retail |> 
           filter(year(Month) >= "2010"),
           size = 1.2)+
  labs(title = latex2exp::TeX(paste0("Forecasts of monthly NSW household goods retailing using $ARIMA(1,0,1)(2,1,2)_{12}$ model")))


## ----ch12-arimax-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Qaurterly Australian electricity and gas production, 1956 - 2008",fig.width=10,fig.height=4, size='tiny'----

elct_gas <- 
aus_production |> 
  select(Quarter, Electricity, Gas) 

elct_gas_train <- 
  elct_gas |> 
  filter(year(Quarter) <= "2008")

elct_gas_test <- 
  elct_gas |> 
  filter(year(Quarter) > "2008")

elct_gas_train |> 
  pivot_longer(cols = c("Electricity", "Gas"),
               names_to = "sector",
               values_to = "production") |> 
  ggplot(aes(x=Quarter, y = production)) +
  geom_line() +
  facet_wrap(sector~., ncol=1, scales="free_y")+
  labs(y="")



## ----ch12-arimax-2-noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE,size='tiny'----

elct_gas_fit <- 
  elct_gas_train |> 
  model(arimax = ARIMA(Electricity ~ Gas))


## ----ch12-arimax-2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## elct_gas_fit <-
##   elct_gas_train |>
##   model(arimax = ARIMA(Electricity ~ Gas))
## 
## report(elct_gas_fit)
## # Series: Electricity
## # Model: LM w/ ARIMA(2,0,0)(1,1,2)[4] errors
## #
## # Coefficients:
## #          ar1     ar2    sar1     sma1    sma2      Gas
## #       0.4463  0.2223  0.9157  -1.4413  0.5236  37.8759
## # s.e.  0.0687  0.0836  0.0795   0.1228  0.1032   9.9100
## #       intercept
## #        891.3641
## # s.e.   121.5085
## #
## # sigma^2 estimated as 411853:  log likelihood=-1637.14
## # AIC=3290.28   AICc=3291   BIC=3316.98
## 


## ----ch12-arimax-3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, results='asis', fig.cap="Forecasts of quarterly Australian electricity production using ARIMA and Gas as exogeneous variable",fig.width=10,fig.height=4, size='tiny'----
elct_gas_fcst <- 
  elct_gas_fit |> 
  forecast(h = 10,
           new_data = elct_gas_test |> 
             select(Quarter, Gas))

elct_gas_fcst |> 
  autoplot(elct_gas_train |> 
             filter(year(Quarter)>= 2000))

