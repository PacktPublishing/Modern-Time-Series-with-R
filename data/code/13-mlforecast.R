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
       ,"fable.prophet" )

conflicts_prefer(
  fabletools::accuracy,
  fabletools::forecast,
  fabletools::components,
  dplyr::filter,
  tsibble::difference,
  tsibble::index,
  dplyr::last,
  dplyr::select
)




lynx_tsb <- lynx |>
  as_tsibble() |>
  rename(year = index,
         lynx_count = value)

lynx_tsb |>
 gg_tsdisplay(plot_type = "partial", lag = 36)



lynx_train <- lynx_tsb |> filter(year < 1920)
lynx_test <- lynx_tsb |> filter(year >= 1920)

set.seed(12345)
lynx_mods <- lynx_train |> 
  model(ets_model = ETS(sqrt(lynx_count)),
        arima_model = ARIMA(sqrt(lynx_count)),
        nnetar = NNETAR(sqrt(lynx_count)))

lynx_mods
## 
## # A mable: 1 x 3
##      ets_model            arima_model      nnetar
##        <model>                <model>     <model>
## 1 <ETS(M,A,N)> <ARIMA(2,0,2) w/ mean> <NNAR(8,4)>

set.seed(67890); lynx_fcst <- lynx_mods |> 
  forecast(h = nrow(lynx_test)*2)



bind_rows(
  accuracy(lynx_mods),
  accuracy(lynx_fcst |> filter(year <= 1934),
           lynx_test)) |>
  select(.model, .type, ME, RMSE, MAPE, ACF1)


## # A tibble: 6 × 6
##   .model      .type        ME  RMSE  MAPE    ACF1
##   <chr>       <chr>     <dbl> <dbl> <dbl>   <dbl>
## 1 ets_model   Training -242.  1318. 119.   0.374
## 2 arima_model Training   83.4  856.  61.7 -0.0395
## 3 nnetar      Training   31.0  294.  24.1 -0.106
## 4 arima_model Test      -43.4  893. 103.   0.578
## 5 ets_model   Test      984.  1449.  49.6  0.689
## 6 nnetar      Test     -472.  1166. 108.   0.686
## 

lynx_fcst |>
  filter(.model  %in% c("arima_model", "nnetar")) |>
  autoplot(lynx_tsb, level = 95, size = 0.9) +
  facet_wrap(~.model, ncol = 1, scales = "free_y") +
    labs(y = "Lynx count", x = "") +
    theme(legend.position = "none")





apdf <- AirPassengers |> 
  as_tsibble() |> 
  rename(passengers = value) |> 
  mutate(year = lubridate::year(index),
         month = lubridate::month(index)) |>
   tibble() |> 
   select( - index)
  

trn_idx <- 132
horizons <- 36

forecasts_store <- list(NA)
tau_vec <- c(0.05, 0.5, 0.95)
horizons <- 36
trn_idx <- 132
cv_nfolds <- 10
for (k in 1:length(tau_vec)) {
  # Step 1: Create data and build model
  data_train <- create_lagged_df(apdf |> 
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
      model <- cv.glmnet(x, y, type.measure = "mse",
                         nfolds = cv_nfolds) 
    }else{
     model <- cv.hqreg(x, y, type.measure = "mse",
                        tau = tau_vec[k],
                        method = "quantile",
                         nfolds = cv_nfolds)
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
  feature_data <- create_lagged_df(apdf |> 
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
  forecasts_store[[k]] <- combine_forecasts(data_forecasts)
}

trn_idx <- 132
data_train <- create_lagged_df(
  apdf |> slice(1:trn_idx),
  type = "train",
  method = "direct",
  outcome_col = 1,
  lookback = 1:(max(horizons) + 3),
  horizons = 1:horizons)

dim(data_train[[1]])
## [1]  93 118
## 
dim(data_train[[36]])
## [1] 93 13



apdf_full <- AirPassengers |> 
  as_tsibble() |> 
  rename(year_month = index, passengers = value)

future_yrmn <- seq(yearmonth(apdf_full$year_month[trn_idx]+1), 
                     length.out = horizons, by = 1)

apdf_obs_pred <- apdf_full |> 
  filter(year_month >= yearmonth("1954 Jan")) |> 
  full_join(forecasts_store[[1]] |>
              mutate(year_month = future_yrmn) |> 
              select(year_month, lower = passengers_pred)) |> 
  full_join(forecasts_store[[2]] |>
              mutate(year_month = future_yrmn) |> 
              select(year_month, mean = passengers_pred)) |> 
   full_join(forecasts_store[[3]] |>
              mutate(year_month = future_yrmn) |> 
              select(year_month, upper = passengers_pred)) 
  
apdf_obs_pred |> 
  ggplot(aes(x = year_month, y = passengers)) +
  geom_line() +
  geom_line(aes(y = mean), col = "navy", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "navy", alpha = 0.3) +
   geom_vline(xintercept = as.Date(yearmonth("1960 Jan")), 
             color = "grey", linetype = "dashed", linewidth = 0.8)
  labs(y = "Air passengers count (in 000's)")


aus_food_retail <- tsibbledata::aus_retail |>
  filter(Industry == "Food retailing") |>
  index_by(Month) |>
  summarise(National_turnover = sum(Turnover, na.rm=T)) |>
  filter(Month > yearmonth("1999 Dec"))

aus_food_retail_train <- aus_food_retail |>
  filter(Month < yearmonth("2016 Jan"))

aus_food_retail_test <- aus_food_retail |>
  filter(Month >= yearmonth("2016 Jan"))

# model fit
foodretail_fit <- aus_food_retail_train %>%
  model(
    arima = ARIMA(National_turnover),
    ets = ETS(National_turnover),
    prophet = prophet(National_turnover ~ growth("linear") +
                        season(period = 12, order = 4,
                               type = "multiplicative"))
  )
foodretail_fc <- foodretail_fit %>%
  forecast(h = "3 years")

# prophet component plot
foodretail_fit |>
  select(prophet) |>
  fabletools::components() |>
  autoplot()

# accuracy
bind_rows(
    accuracy(foodretail_mod),
    accuracy(foodretail_fc,aus_food_retail))

# residual diagnostic
foodretail_fit |>
  select(prophet) |>
  gg_tsresiduals()





## # A tibble: 6 × 6
##   .model  .type     RMSE      MPE  MASE     ACF1
##   <chr>   <chr>    <dbl>    <dbl> <dbl>    <dbl>
## 1 arima   Training 111.  -0.0394  0.230  0.0102
## 2 ets     Training  95.8  0.00259 0.205  0.00735
## 3 prophet Training 204.  -0.0672  0.462 -0.616
## 4 arima   Test     194.  -1.23    0.466 -0.0837
## 5 ets     Test     194.  -1.40    0.469 -0.00199
## 6 prophet Test     327.  -1.31    0.776 -0.727

foodretail_fit |>
  select(prophet) |>
  gg_tsresiduals()



# training and test set
ap_tsb <- AirPassengers |>
  as_tsibble() |>
  rename(month = index, passenger_count = value)
ap_train <- ap_tsb |>
  filter(month < yearmonth("1957 Jan"))
ap_test <- ap_tsb |>
  filter(month >= yearmonth("1957 Jan"))

# fit model
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

# forecast
ap_fcst <- ap_models |>
  forecast(h = "4 years") |>
  mutate(.model = str_c(.model, "_forecast"))

# plot
ap_fcst |>
  autoplot(level = NULL, color = "black", linewidth = 0.9 ) +
  scale_color_manual(values = c("black"= "black")) +
  autolayer(ap_tsb |> filter(month >= yearmonth("1955 Jan")),
            linetype = 3 ) +
  facet_wrap(.model~.) +
  labs(x  = "",
       y = "Air passengers count (in 000's)") +
  theme(legend.position = "none")

# accuracy
ap_fcst |>
  accuracy(ap_tsb) |>
  select(.model, .type, RMSE, MAPE, MASE) |>
  arrange(RMSE)






ap_models
## 
## # A mable: 1 x 6
##     snaive    tslm          ets
##    <model> <model>      <model>
## 1 <SNAIVE>  <TSLM> <ETS(A,A,A)>
## 
##                               arima
##                             <model>
## 1 <ARIMA(2,0,0)(0,1,1)[12] w/ drift>
## 
##        prophet       ensemble
##        <model>        <model>
## 1    <prophet>  <COMBINATION>



ap_fcst |>
  filter(row_number() <= 6 | row_number() > (n()-6))

## # A fable: 12 x 4 [1M]
## # Key:     .model [2]
##    .model               month passenger_count .mean
##    <chr>                <mth>          <dist> <dbl>
##  1 snaive_forecast   1957 Jan    N(284, 1071)  284
##  2 snaive_forecast   1957 Feb    N(277, 1071)  277
##  3 snaive_forecast   1957 Mar    N(317, 1071)  317
##  4 snaive_forecast   1957 Apr    N(313, 1071)  313
##  5 snaive_forecast   1957 May    N(318, 1071)  318
##  6 snaive_forecast   1957 Jun    N(374, 1071)  374
##  7 ensemble_forecast 1960 Jul         608.542  609.
##  8 ensemble_forecast 1960 Aug        599.8902  600.
##  9 ensemble_forecast 1960 Sep        535.9316  536.
## 10 ensemble_forecast 1960 Oct        465.2943  465.
## 11 ensemble_forecast 1960 Nov        411.8913  412.
## 12 ensemble_forecast 1960 Dec         468.112  468.s





## # A tibble: 6 × 5
##   .model            .type  RMSE  MAPE  MASE
##   <chr>             <chr> <dbl> <dbl> <dbl>
## 1 ensemble_forecast Test   23.4  4.73 0.643
## 2 prophet_forecast  Test   32.2  6.70 0.896
## 3 ets_forecast      Test   39.4  8.08 1.12
## 4 arima_forecast    Test   55.1 11.2  1.60
## 5 tslm_forecast     Test   59.6 12.4  1.75
## 6 snaive_forecast   Test   97.8 19.6  2.92


# fit model
ap_mod_revised <- ap_train |>
  model(
    snaive = SNAIVE(passenger_count),
    tslm = TSLM(log(passenger_count) ~ trend() + season()),
    ets = ETS(log(passenger_count) ~ season("A")),
    arima = ARIMA(log(passenger_count))    ) |>
  mutate(ensemble = (snaive + tslm + ets + arima ) / 4)

# forecast
ap_fcst_revised <- ap_mod_revised |>
  forecast(h = "4 years") |>
  mutate(.model = str_c(.model, "_forecast"))

# accuracy
ap_accuracy_revised <- ap_fcst_revised |>
  accuracy(ap_tsb) |>
  select(.model, .type, RMSE, MAPE, MASE) |>
  arrange(MASE)

# prediction interval
set.seed(98765); ap_sims <- ap_mod_revised |>
  fabletools::generate(h = "4 years", times = 1000) |>
  mutate(.model = str_c(.model, "_prediction_interval")) |>
  tibble() |>
  group_by(month, .model) |>
  summarise(
    dist = distributional::dist_sample(list(.sim))) |>
  ungroup() |>
  as_fable(index = month, key = .model,
           distribution = dist,
           response = "passenger_count")


# interval accuracy
interval_accuracy <- ap_sims |>
  accuracy(ap_tsb,
           measures = interval_accuracy_measures,
           level = 95) |>
  select(-pinball, -scaled_pinball) |>
  arrange(winkler)






# fit model
ap_mod_revised <- ap_train |>
  model(
    snaive = SNAIVE(passenger_count),
    tslm = TSLM(log(passenger_count) ~ trend() + season()),
    ets = ETS(log(passenger_count) ~ season("A")),
    arima = ARIMA(log(passenger_count))    ) |>
  mutate(ensemble = (snaive + tslm + ets + arima ) / 4)

# forecast 
ap_fcst_revised <- ap_mod_revised |>
  forecast(h = "4 years") |> 
  mutate(.model = str_c(.model, "_forecast"))

# accuracy 
ap_accuracy_revised <- ap_fcst_revised |>
  accuracy(ap_tsb) |>
  select(.model, .type, RMSE, MAPE, MASE) |> 
  arrange(MASE) 

ap_accuracy_revised
## 
## # A tibble: 5 × 5
##   .model            .type  RMSE  MAPE  MASE
##   <chr>             <chr> <dbl> <dbl> <dbl>
## 1 ensemble_forecast Test   21.9  4.39 0.601
## 2 ets_forecast      Test   39.4  8.08 1.12
## 3 arima_forecast    Test   55.1 11.2  1.60
## 4 tslm_forecast     Test   59.6 12.4  1.75
## 5 snaive_forecast   Test   97.8 19.6  2.92

set.seed(98765); ap_sims <- ap_mod_revised |> 
  fabletools::generate(h = "4 years", times = 1000) |> 
  mutate(.model = str_c(.model, "_prediction_interval")) |> 
  tibble() |>
  group_by(month, .model) |>
  summarise(
    dist = distributional::dist_sample(list(.sim))) |>
  ungroup() |>
  as_fable(index = month, key = .model,
           distribution = dist, 
           response = "passenger_count")

ap_sims |>
  filter(.model == "ensemble_prediction_interval") |>
  autoplot(ap_tsb |> filter(month >= yearmonth("1954 Jan")),
           level = 95) +
  geom_vline(xintercept = as.Date(yearmonth("1957 Jan")), 
             color = "grey", linetype = "dashed", linewidth = 0.8) +
  labs(y = "Air passengers count (in 000's)", 
       x  = "") +
  theme(legend.position = "none")

interval_accuracy <- ap_sims |>
  accuracy(ap_tsb,
           measures = interval_accuracy_measures,
           level = 95) |>
  select(-pinball, -scaled_pinball) |>
  arrange(winkler)


interval_accuracy
## # A tibble: 5 × 3
##   .model                       .type winkler
##   <chr>                        <chr>   <dbl>
## 1 ensemble_prediction_interval Test     127.
## 2 ets_prediction_interval      Test     190.
## 3 arima_prediction_interval    Test     356.
## 4 tslm_prediction_interval     Test     777.
## 5 snaive_prediction_interval   Test    1653.
