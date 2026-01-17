#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"astsa"
       ,"forecast"
       ,"anomalize"
       ,"patchwork"
       ,"solitude"
       ,"slider"
       ,"tidyverse"
       ,"tsibble"
       ,"tsfeatures"
      ,"wesanderson" )

conflicts_prefer(
  fabletools::accuracy
  ,fabletools::forecast
  ,dplyr::filter )


astsa::gas |> 
  stats::stl(s.window = "periodic", robust = TRUE) |> 
  autoplot() 
  

gas_tbl <- astsa::gas %>%
  as_tsibble() |> 
  mutate(date = as.Date(index)) |> 
  as_tibble() |> 
  dplyr::select(-index, gas_price = value)

gas_anomalies_in_resid <- gas_tbl %>%
  time_decompose(gas_price, frequency = 52) %>%
  anomalize(remainder, method = "iqr", alpha = 0.15/3) |> 
  time_recompose()


colnames(gas_anomalies_in_resid )
## 
##  [1] "date"          "observed"      "season"
##  [4] "trend"         "remainder"     "remainder_l1"
##  [7] "remainder_l2"  "anomaly"       "recomposed_l1"
## [10] "recomposed_l2"

gas_anomalies_in_resid %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5) +
  theme_minimal() +
  geom_line() +
  labs(title = "Detected anomalies for SF = 2",
       y = "Gasoline spot price",
       x = "Time (week of year)")

gas_anomalies_in_resid_sf2 <- gas_tbl %>%
  time_decompose(gas_price, frequency = 52) %>%
  anomalize(remainder, method = "iqr", alpha = 0.15/2) |> 
  time_recompose()

anomaly_plot_SF3 <- gas_anomalies_in_resid %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5) +
  theme_minimal() +
   geom_line() +
  labs(title = "Detected anomalies for SF = 3",
       y = "Gasoline spot price",
       x = "Time (week of year)")

anomaly_plot_SF2 <- gas_anomalies_in_resid_sf2 %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5) +
  theme_minimal() +
   geom_line() +
  labs(title = "Detected anomalies for SF = 2",
       y = "Gasoline spot price",
       x = "Time (week of year)")


print(anomaly_plot_SF2/anomaly_plot_SF3)

# Create features for time-series context
create_features <- function(ts_data, win_wid) {
  n <- length(ts_data)
  X <- matrix(NA, nrow = n - win_wid + 1, 
                     ncol = win_wid + 7)
  for (i in win_wid:n) {
    win_data <- ts_data[(i - win_wid + 1):i]
    row_idx <- i - win_wid + 1
    # Window values
    X[row_idx, 1:win_wid] <- win_data
    holtPars <- holt_parameters(win_data)
    # Additional cross Mean, SD, Max and Min respectively
    X[row_idx, win_wid + 1] <- crossing_points(win_data)
    X[row_idx, win_wid + 2] <- entropy(win_data)
    X[row_idx, win_wid + 3] <- holtPars["alpha"]
    X[row_idx, win_wid + 4] <- holtPars["beta"]
    X[row_idx, win_wid + 5] <- lumpiness(win_data)
    X[row_idx, win_wid + 6] <- nonlinearity(win_data)
    X[row_idx, win_wid + 7] <- tsfeatures::stability(win_data)
  }
  # Assign column names to X
  colnames(X) <- c(paste0("lag_", (win_wid - 1):0), 
                 "xpoints", "entropy", "holt_1", "holt_2", 
                 "lumpiness",  "nonlinearity", "stability")
  return(as.data.frame(X))
}

win_wid <- ceiling(52.14)  
gas_features <- create_features(astsa::gas, win_wid)

# Train Isolation Forest model
iso_forest <- solitude::isolationForest$new(
  sample_size = nrow(gas_features),
  replace = TRUE,
  num_trees = 100,
  seed = 101,
  max_depth = ceiling(log2(nrow(gas_features)))
)
# fit
isof_fit <- iso_forest$fit(gas_features)
# predict
isof_pred <- iso_forest$predict(gas_features)


isof_pred
##         id average_depth anomaly_score
##      <int>         <num>         <num>
##   1:     1          8.71     0.5930571
##   2:     2          8.73     0.5923460
##   3:     3          8.78     0.5905721
##   4:     4          8.78     0.5905721
##   5:     5          8.68     0.5941253
##  ---
## 489:   489          8.83     0.5888035
## 490:   490          8.81     0.5895103
## 491:   491          8.87     0.5873924
## 492:   492          8.91     0.5859847
## 493:   493          8.84     0.5884504

# Subjective choice: Maximum 10% data are permitted to be anomaly
max_anom_prop <- 0.1 

gas_isof_anomaly <- gas_tbl |> 
  mutate(isof_anomscore = c(rep(NA, win_wid-1), 
                             isof_pred$anomaly_score)) |> 
  mutate(anomaly_threshold = quantile(
    isof_pred$anomaly_score, probs = 1 - max_anom_prop)) |> 
  mutate(is_anomaly = case_when(
    is.na(isof_anomscore)  ~  NA, 
    isof_anomscore > anomaly_threshold & !is.na(isof_anomscore)  ~ "Yes", 
    .default = "No"))


gas_isof_anomaly$anomaly_threshold |>
  unique()
## 
## [1] 0.614421

gas_isof_anomaly |>
  filter(is_anomaly == "Yes") |>
  select(gas_price, date, isof_anomscore)
## 
## # A tibble: 49 × 3
##    gas_price date       isof_anomscore
##        <dbl> <date>              <dbl>
##  1      56.5 2002-01-15          0.622
##  2      53.5 2002-02-12          0.618
##  3      54.5 2002-02-26          0.621
##  4      54.5 2002-03-12          0.619
##  5      55.7 2002-03-19          0.622
##  6      64.2 2002-03-26          0.615
##  7     216.  2005-10-01          0.617
##  8     194.  2005-10-08          0.632
##  9     220.  2006-08-06          0.616
## 10     180.  2006-09-24          0.624
## # ℹ 39 more rows



gas_isof_anomaly |> 
  ggplot() + 
  geom_line(aes(x = date, y = gas_price)) +
  geom_point(aes(x = date, y = gas_price, col = is_anomaly)) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  labs(y = "Gasoline spot price cents/gallon",
       x = "Time (week of year)") +
  theme(legend.position = "top")

gas_tsclean <- gas_tbl |> 
  mutate(clean_gas_price = 
           tsclean(astsa::gas, replace.missing = TRUE)) |> 
  mutate(is_anomaly = case_when(
    gas_price != clean_gas_price ~ "Yes", 
    .default = "No"))

gas_tsclean |> 
  ggplot(aes(x = date, y = gas_price)) +
  geom_point(aes(col = is_anomaly)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  geom_line(aes(y = clean_gas_price), col = "grey70") +
  theme_bw() +
  labs(y = "Gasoline spot price cents/gallon",
       x = "Time (week of year)")


gas_tsclean |> filter(is_anomaly == "Yes")
## # A tibble: 8 × 4
##   gas_price date       clean_gas_price is_anomaly
##       <dbl> <date>               <dbl> <chr>
## 1     270.  2005-09-24            200. Yes
## 2     336.  2008-07-30            334. Yes
## 3     116.  2008-12-17            125. Yes
## 4     103.  2009-01-01            113. Yes
## 5      97.9 2009-01-08            111. Yes
## 6     100.  2009-01-15            108. Yes
## 7      84.2 2009-01-22            106. Yes
## 8      92.4 2009-01-29            110. Yes
