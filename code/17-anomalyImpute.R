## ----ch17-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"forecast"
       ,"anomalize"
       ,"tidyverse"
       ,"solitude"
       ,"wesanderson"
      )

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
)

theme_set(theme_bw())


## ----ch17-stl-gas, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Time plot of gasoline price and its components", fig.width=8,fig.height=6----
gas_decomp_plot <- astsa::gas |> 
  stats::stl(s.window = "periodic", robust = TRUE) |> 
  autoplot() + 
  theme_bw() 
gas_decomp_plot


## ----ch17-gas-tibble, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
gas_data_tibble <- astsa::gas %>%
  as_tsibble() |> 
  mutate(date = as.Date(index)) |> 
  as_tibble() |> 
  select(-index, gas_price = value)


## ----ch17-gas-anomaly-comp, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
gas_anomalies_in_resid <- gas_data_tibble %>%
  time_decompose(gas_price, frequency = 52) %>%
  anomalize(remainder, method = "iqr", alpha = 0.15/3) |> 
  time_recompose()


## ----ch17-stl-gas-anomaly-plot-code, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis'----
## gas_anomalies_in_resid %>%
##   plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5) +
##   theme_minimal() +
##    geom_line() +
##   labs(title = "Detected anomalies (for choice of SF = 3)",
##        y = "Gasoline spot price",
##        x = "Time (week of year)")


## ----ch17-gas-anomaly-comp3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
gas_anomalies_in_resid_sf2 <- gas_data_tibble %>%
  time_decompose(gas_price, frequency = 52) %>%
  anomalize(remainder, method = "iqr", alpha = 0.15/2) |> 
  time_recompose()
anomaly_plot_SF3 <- gas_anomalies_in_resid %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5) +
  theme_minimal() +
   geom_line() +
  labs(title = "Detected anomalies (for choice of SF = 3)",
       y = "Gasoline spot price",
       x = "Time (week of year)")
anomaly_plot_SF2 <- gas_anomalies_in_resid_sf2 %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5) +
  theme_minimal() +
   geom_line() +
  labs(title = "Detected anomalies (for choice of SF = 2)",
       y = "Gasoline spot price",
       x = "Time (week of year)")


## ----ch17-stl-gas-anomaly-plot, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Detected anomalies in the gasoline price for alternative choice of SF", fig.width=8,fig.height=6----
library(patchwork)
plt_obj <- anomaly_plot_SF3/anomaly_plot_SF2
print(plt_obj)


## ----ch17-gas-anomaly-iforest-feature, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
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
    X[row_idx, win_wid + 7] <- stability(win_data)
  }
  # Assign column names to X
  colnames(X) <- c(paste0("lag_", (win_wid - 1):0), 
                 "xpoints", "entorpy", "holt_1", "holt_2", 
                 "lumpiness",  "nonlinearity", "stability")
  return(as.data.frame(X))
}


## ----ch17-gas-anomaly-iforest-feature-create, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
# Set window size/width parameters
win_wid <- 53  # ~ 1 year for weekly data as lag

# Create feature matrix
gas_features <- create_features(astsa::gas, win_wid)


## ----ch17-gas-anomaly-iforest-tune, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='hide'----
# Train Isolation Forest model
iso_forest <- solitude::isolationForest$new(
  sample_size = nrow(gas_features),
  num_trees = 100,
  seed = 101,
  max_depth = ceiling(log2(nrow(gas_features)))
)

iso_forest$fit(gas_features)

# Predict anomaly scores
predictions <- iso_forest$predict(gas_features)


## ----ch17-iforest-anomaly-plot, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Detected anomalies in the gasoline price using isolation forest", fig.width=8,fig.height=4----
# Subjective choice: Maximum 10% data are permitted to be anomaly
max_anom_prop <- 0.1 

# Pull relevant information for plotting
pull_4_plot <- gas_data_tibble |> 
  mutate(ifrst_anomScore = c(rep(NA, win_wid - 1), 
                            predictions$anomaly_score)) |> 
  mutate(anomaly_threshold = quantile(predictions$anomaly_score, 
                                      probs = 1 - max_anom_prop)) |> 
  mutate(anomaly = case_when(is.na(ifrst_anomScore)  ~  NA, 
                                ifrst_anomScore > anomaly_threshold & 
                                   !is.na(ifrst_anomScore)  ~ "Yes", 
                                .default = "No"))
# Plot the original gasoline data and highlight anomalies 
anom_plt <- pull_4_plot |> 
  ggplot() + 
  geom_line(aes(x = date, y = gas_price)) +
  geom_point(aes(x = date, y = gas_price, col = anomaly)) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  theme_bw() +
  labs(y = "Gasoline spot price cents/gallon",
       x = "Time (week of year)")

print(anom_plt)


## ----ch17-gas-anomaly-tsclean, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='hide'----
gas_price_and_cleanPrice <- gas_data_tibble |> 
  mutate(clean_gas_price = tsclean(astsa::gas, 
                                   replace.missing = TRUE)) |> 
  mutate(anomaly = case_when(gas_price != clean_gas_price ~ "Yes", 
                             .default = "No"))


## ----ch17-tsclean-anomaly-plot, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Detected anomalies in the gasoline price and thier imputation", fig.width=8,fig.height=4----
anom_imp_plot <- gas_price_and_cleanPrice |> 
  ggplot(aes(x = date, y = gas_price)) +
  geom_point(aes(col = anomaly)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  geom_line(aes(y = clean_gas_price), col = "grey70") +
  theme_bw() +
  labs(y = "Gasoline spot price cents/gallon",
       x = "Time (week of year)")
print(anom_imp_plot)

