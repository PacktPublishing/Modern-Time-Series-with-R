## ----ch13-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
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
       ,"forecastML"
       ,"glmnet"
       ,"hqreg"
       ,"fable.prophet"
      )

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,fabletools::components()
  ,dplyr::filter()
)

theme_set(theme_bw())




## ----ch13-2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Autocorrelation plot of lynx data.", fig.width=8,fig.height=4----
lynx_tsb <- lynx |> 
  as_tsibble() |> 
  rename(year = index,
         lynx_count = value)

ggAcf(lynx_tsb, lag.max = 36) + 
  labs(title = "")


## ----ch13-3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
lynx_train <- lynx_tsb |> filter(year < 1920)
lynx_test <- lynx_tsb |> filter(year >= 1920)


## ----ch13-4, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
set.seed(12345); lynx_mods <- 
  lynx_train |> 
  model(ets_model = ETS(sqrt(lynx_count)),
        arima_model = ARIMA(sqrt(lynx_count)),
        nnetar = NNETAR(sqrt(lynx_count)))


## ----ch13-5, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
set.seed(67890); lynx_fcst <- lynx_mods |> 
  forecast(h = nrow(lynx_test)*2)


## ----ch13-6, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Forecasted lynx count by alternative approaches.", fig.width=8,fig.height=4----
lynx_fcst |> 
    autoplot(lynx_train, level = NULL, size = 0.9) + 
    facet_wrap(~.model, ncol = 1, scales = "free_y") + 
    labs(y = "Lynx count", x = "Year") + 
    theme(legend.position = "none")


## ----ch13-7, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## lynx_mods |>
##   accuracy() |>
##   select(.model, .type, ME, RMSE, MAPE, MASE, ACF1)
## 
## # # A tibble: 3 × 7
## #   .model       .type        ME  RMSE  MAPE  MASE    ACF1
## #   <chr>        <chr>     <dbl> <dbl> <dbl> <dbl>   <dbl>
## # 1 ets_model    Training -242.  1318. 119.  1.02   0.374
## # 2 arima_model  Training   83.4  856.  61.7 0.631 -0.0395
## # 3 nnetar       Training   31.0  294.  24.1 0.208 -0.106
## 


## ----ch13-8, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## accuracy(lynx_fcst, lynx_test)  |>
##   select(.model, .type, ME, RMSE, MAPE, ACF1)
## 
## # # A tibble: 3 × 6
## #   .model       .type     ME  RMSE  MAPE  ACF1
## #   <chr>        <chr>  <dbl> <dbl> <dbl> <dbl>
## # 1 arima_model  Test   -43.4  893. 103.  0.578
## # 2 ets_model    Test   984.  1449.  49.6 0.689
## # 3 nnetar       Test  -472.  1166. 108.  0.686
## 


## ----ch13-9, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Foreasts with 95% prediction interval by alternative approaches.", fig.width=8,fig.height=4----
lynx_fcst |> 
  filter(.model  != "ets_model") |>
  autoplot(lynx_tsb, level = 95, size = 0.9) + 
  facet_wrap(~.model, ncol = 1, scales = "free_y") +
    labs(y = "Lynx count", x = "Year") + 
    theme(legend.position = "none")


## ----ch13-10, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
apDat <- AirPassengers |> 
  as_tsibble() |> 
  rename(passengers = value) |> 
  mutate(year = lubridate::year(index),
         month = lubridate::month(index)) |>
  as_tibble() |>
  select(-index)


## ----ch13-11, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## horizons <- 36


## ----ch13-12-noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
forecasts_store <- list(NA)
tau_vec <- c(0.05, 0.5, 0.95)
horizons <- 36
trn_idx <- 132
for (k in 1:length(tau_vec)) {
  # Step 1: Create data and build model
  data_train <- create_lagged_df(apDat |> 
                                   slice(1:trn_idx), 
                                 type = "train", 
                                 method = "direct",
                                 outcome_col = 1, 
                                 lookback = 1:(max(horizons) + 3), 
                                 horizons = 1:horizons)
  # ML model: wrapper for mean and quantile forecast
  model_fn_cust <- function(data) {
    x <- as.matrix(data[, -1, drop = FALSE])
    y <- as.matrix(data[, 1, drop = FALSE])
    if (tau_vec[[k]] == 0.5) {
      model <- cv.glmnet(x, y, type.measure = "mse")
    }else{
      model <- cv.hqreg(x, y, type.measure = "mse",
                        tau = tau_vec[k],
                        method = "quantile")
    }
  }
  windows <- create_windows(data_train,
                            window_length = 0)
  set.seed(54321);model_results <- 
    train_model(data_train, 
                windows, 
                model_name = "glmnetQuant", 
                model_function = model_fn_cust)
  
  # Step 2: forecast
  feature_data <- create_lagged_df(apDat |> 
                                        slice(1:trn_idx), 
                                      type = "forecast", 
                                      method = "direct",
                                      outcome_col = 1, 
                                      lookback = 1:(max(horizons) + 3), 
                                      horizons = 1:horizons)
  
  # Prediction function for ML model
  predict_fn <- function(model, data) {
    data_pred <- as.data.frame(predict(model, as.matrix(data)))
  }
  
  data_forecasts <- predict(model_results, 
                            prediction_function = list(predict_fn), 
                            data = feature_data)
  
  # Step 3: combine
  comb_forecasts <- combine_forecasts(data_forecasts)
  forecasts_store[[k]] <- comb_forecasts
}


## ----ch13-13, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## trn_idx <- 132
## data_train <- create_lagged_df(apDat |>
##                                  slice(1:trn_idx),
##                                type = "train",
##                                method = "direct",
##                                outcome_col = 1,
##                                lookback = 1:(max(horizons) + 3),
##                                horizons = 1:horizons)


## ----ch13-14, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## model_fn_init <- function(data){
##   x <- as.matrix(data[, -1, drop = FALSE])
##   y <- as.matrix(data[, 1, drop = FALSE])
##   model <- glmnet(x, y, type.measure = "mse")
## }


## ----ch13-15, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## model_fn_cust <- function(data, outcome_col = 1) {
##   x <- as.matrix(data[, -outcome_col, drop = FALSE])
##   y <- as.matrix(data[, outcome_col, drop = FALSE])
##   if(tau_vec[[k]] == 0.5){
##     model <- cv.glmnet(x, y, type.measure = "mse")
##   }else{
##     model <- cv.hqreg(x, y, type.measure = "mse",
##                       tau = tau_vec[k],
##                       method = "quantile")
##   }
## }


## ----ch13-16, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## windows <- create_windows(data_train,
##                           window_length = 0)
## set.seed(54321);model_results <-
##   train_model(data_train,
##               windows,
##               model_name = "glmnetQuant",
##               model_function = model_fn_cust)


## ----ch13-17, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## feature_data <- create_lagged_df(apDat |>
##                                       slice(1:trn_idx),
##                                     type = "forecast",
##                                     method = "direct",
##                                     outcome_col = 1,
##                                     lookback = 1:(max(horizons) + 3),
##                                     horizons = 1:horizons)
## 
## # Prediction function for ML model
## predict_fn <- function(model, data) {
##   data_pred <- as.data.frame(predict(model, as.matrix(data)))
## }
## 
## data_forecasts <- predict(model_results,
##                           prediction_function = list(predict_fn),
##                           data = feature_data)
## 
## # Step 3 of forecastML - combine
## data_forecasts <- combine_forecasts(data_forecasts)


## ----ch13-18, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## forecasts_store <- list(NA)
## tau_vec <- c(0.025, 0.5, 0.975)
## horizons <- 36
## trn_idx <- 132
## for (k in 1:length(tau_vec)) {
##   # Step 1: Create data and build model
##   data_train <- create_lagged_df(apDat |>
##                                    slice(1:trn_idx),
##                                  type = "train",
##                                  method = "direct",
##                                  outcome_col = 1,
##                                  lookback = 1:(max(horizons) + 3),
##                                  horizons = 1:horizons)
##   # Model wrapper for mean and quantile model
##   model_fn_cust <- function(data, outcome_col = 1) {
##     x <- as.matrix(data[, -outcome_col, drop = FALSE])
##     y <- as.matrix(data[, outcome_col, drop = FALSE])
##     if(tau_vec[[k]] == 0.5){
##       model <- cv.glmnet(x, y, type.measure = "mse")
##     }else{
##       model <- cv.hqreg(x, y, type.measure = "mse",
##                         tau = tau_vec[k],
##                         method = "quantile")
##     }
##   }
##   windows <- create_windows(data_train,
##                             window_length = 0)
##   set.seed(54321);model_results <-
##     train_model(data_train,
##                 windows,
##                 model_name = "glmnetQuant",
##                 model_function = model_fn_cust)
## 
##   # Step 2: forecast
##   feature_data <- create_lagged_df(apDat |>
##                                         slice(1:trn_idx),
##                                       type = "forecast",
##                                       method = "direct",
##                                       outcome_col = 1,
##                                       lookback = 1:(max(horizons) + 3),
##                                       horizons = 1:horizons)
## 
##   # Prediction function for ML model
##   predict_fn <- function(model, data) {
##     data_pred <- as.data.frame(predict(model, as.matrix(data)))
##   }
## 
##   data_forecasts <- predict(model_results,
##                             prediction_function = list(predict_fn),
##                             data = feature_data)
## 
##   # Step 3: combine
##   comb_forecasts <- combine_forecasts(data_forecasts)
##   forecasts_store[[k]] <- comb_forecasts
## }


## ----ch13-19, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Machine learning based forecast and 95% prediction interval for Airpassengers data.", fig.width=8,fig.height=4----
fore_yr_mnth <- yearmonth(zoo::as.Date(time(AirPassengers)))[trn_idx+1] + c(1:horizons)
obsfore <- apDat |> 
  mutate(year_month = yearmonth(zoo::as.Date(time(AirPassengers)))) |> 
  select(year_month, passengers) |> 
  full_join(forecasts_store[[1]] |>
              mutate(year_month = fore_yr_mnth) |> 
              select(year_month,
                     lower = passengers_pred)) |> 
  full_join(forecasts_store[[2]] |>
              mutate(year_month = fore_yr_mnth) |> 
              select(year_month,
                     mean = passengers_pred)) |> 
  full_join(forecasts_store[[3]] |>
              mutate(year_month = fore_yr_mnth) |> 
              select(year_month,
                     upper = passengers_pred))

obsfore |> 
  ggplot(aes(x = year_month, y = passengers)) +
  geom_line() +
  geom_line(aes(y = mean), col = "navy", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "navy", alpha = 0.3) +
  labs(y = "Air passengers count (in 000's)")


## ----ch13-20, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
aus_food_retail <- tsibbledata::aus_retail |> 
  filter(Industry == "Food retailing") |>
  index_by(Month) |> 
  summarise(National_turnover = sum(Turnover, na.rm=T)) |> 
  filter(Month > yearmonth("1999 Dec"))
aus_food_retail_train <- aus_food_retail |> 
  filter(Month < yearmonth("2016 Jan"))
aus_food_retail_test <- aus_food_retail |> 
  filter(Month >= yearmonth("2016 Jan"))


## ----ch13-21, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
fit <- aus_food_retail_train %>%
  model(
    arima = ARIMA(National_turnover),
    ets = ETS(National_turnover),
    prophet = prophet(National_turnover ~ growth("linear") + 
                        season(period = 12, order = 4, 
                               type = "multiplicative"))
  )
fc <- fit %>%
  forecast(h = "3 years")


## ----ch13-22, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Prophet decomposition of national retil turnover.", fig.width=8, fig.height=6----
fit |> 
  select(prophet) |> 
  fabletools::components() |> 
  autoplot()


## ----ch13-23, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Training accuracy
## fit |>
##   accuracy() |>
##   select(.model, .type, RMSE, MAPE, MASE, ACF1)
## 
## # # A tibble: 3 × 6
## #   .model  .type     RMSE  MAPE  MASE     ACF1
## #   <chr>   <chr>    <dbl> <dbl> <dbl>    <dbl>
## # 1 arima   Training 111.   1.24 0.230  0.0102
## # 2 ets     Training  95.8  1.14 0.205  0.00735
## # 3 prophet Training 204.   2.41 0.462 -0.616
## 
## # Test accuracy
## fc |>
##   accuracy(aus_food_retail) |>
##   select(.model, .type, RMSE, MAPE, MASE, ACF1)
## 
## # # A tibble: 3 × 6
## #   .model  .type  RMSE  MAPE  MASE     ACF1
## #   <chr>   <chr> <dbl> <dbl> <dbl>    <dbl>
## # 1 arima   Test   194.  1.65 0.466 -0.0837
## # 2 ets     Test   194.  1.67 0.469 -0.00199
## # 3 prophet Test   326.  2.72 0.773 -0.724


## ----ch13-24, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Residuals of Prophet method.", fig.width=8,fig.height=4----
fit |> 
  select(prophet) |> 
  gg_tsresiduals()


## ----ch13-25, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
ap_tibble <- AirPassengers |>  
  as_tsibble() |> 
  rename(month = index, passenger_count = value)
ap_train <- ap_tibble |> 
  filter(month < yearmonth("1957 Jan"))


## ----ch13-26, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
ap_models <- ap_train |>
  model(
    snaive = SNAIVE(passenger_count),
    tslm = TSLM(log(passenger_count) ~ trend() + season()),
    ets = ETS(log(passenger_count) ~ season("A")),
    arima = ARIMA(log(passenger_count)),
    prophet = prophet(passenger_count ~ growth("linear") + 
                        season(period = 12, order = 4, 
                               type = "multiplicative"))
    ) |>
  mutate(ensemble = (snaive + tslm + ets + arima + prophet) / 5)
# Derive forecast for 4 years
ap_fc <- ap_models |>
  forecast(h = "4 years") |> 
  mutate(.model = str_c(.model, "_forecast"))


## ----ch13-27, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Forecasts by alterantive appraoches.", fig.width=8,fig.height=4----
ap_fc |>
  autoplot(ap_tibble,
           level = NULL) +
  facet_wrap(.model~.) +
  labs(y = "Air passengers count (in 000's)", 
       x  = "Year-Month") +
  theme(legend.position = "none")


## ----ch13-28, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## ap_fc |>
##   accuracy(ap_tibble) |>
##   arrange(RMSE) |>
##   select(.model, .type, RMSE, MAPE, MASE)
## 
## # # A tibble: 6 × 5
## #   .model            .type  RMSE  MAPE  MASE
## #   <chr>             <chr> <dbl> <dbl> <dbl>
## # 1 ensemble_forecast Test   23.4  4.74 0.644
## # 2 prophet_forecast  Test   32.1  6.68 0.893
## # 3 ets_forecast      Test   39.4  8.08 1.12
## # 4 arima_forecast    Test   55.1 11.2  1.60
## # 5 tslm_forecast     Test   59.6 12.4  1.75
## # 6 snaive_forecast   Test   97.8 19.6  2.92


## ----ch13-28a, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # # A tibble: 5 × 5
## #   .model               .type  RMSE  MAPE  MASE
## #   <chr>                <chr> <dbl> <dbl> <dbl>
## # 1 combination_forecast Test   21.9  4.39 0.601


## ----ch13-29, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
set.seed(98765);ap_sims <- ap_train |>
  model(
    snaive = SNAIVE(passenger_count),
    tslm = TSLM(log(passenger_count) ~ trend() + season()),
    ets = ETS(log(passenger_count) ~ season("A")),
    arima = ARIMA(log(passenger_count))
  ) |>
  mutate(ensemble = (snaive + tslm + ets + arima) / 4) |>
  fabletools::generate(h = "4 years", times = 1000) |> 
  mutate(.model = str_c(.model, "_prediction_interval")) |> 
  as_tibble() |>
  group_by(month, .model) |>
  summarise(
    dist = distributional::dist_sample(list(.sim))) |>
  ungroup() |>
  as_fable(index = month, key = .model,
           distribution = dist, 
           response = "passenger_count")


## ----ch13-30, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Prediction interval for forecast ensemble.", fig.width=8,fig.height=4----
ap_sims |>
  filter(.model == "ensemble_prediction_interval") |>
  autoplot(ap_tibble,
           level = 95) +
  labs(y = "Air passengers count (in 000's)", 
       x  = "Year-Month") +
  theme(legend.position = "none")


## ----ch13-31, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## ap_sims |>
##   accuracy(ap_tibble,
##            measures = interval_accuracy_measures,
##            level = 95) |>
##   select(-pinball, -scaled_pinball) |>
##   arrange(winkler)
## 
## # # A tibble: 5 × 3
## #   .model                       .type winkler
## #   <chr>                        <chr>   <dbl>
## # 1 ensemble_prediction_interval Test     127.
## # 2 ets_prediction_interval      Test     190.
## # 3 arima_prediction_interval    Test     356.
## # 4 tslm_prediction_interval     Test     777.
## # 5 snaive_prediction_interval   Test    1653.

