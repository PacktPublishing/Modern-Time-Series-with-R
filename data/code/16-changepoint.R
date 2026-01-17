#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"tidyverse"
       ,"feasts"
       ,"patchwork"
       ,"bfast"
       ,"changepoint"
       ,"Rbeast" )

conflicts_prefer(
   strucchangeRcpp::boundary
  ,fabletools::accuracy
  ,fabletools::forecast
  ,fabletools::components
  ,dplyr::filter
  ,fable::VAR)



bfast_nile <- bfast(Nile, 
                      h = 0.15,
                      season = "none",
                      max.iter = 10) 

nile_bfast_magnitude <- bfast_nile$Magnitude


bfast_nile

## 
## TREND BREAKPOINTS
## 	Confidence intervals for breakpoints
## 	of optimal 2-segment partition:
## 
## Call:
## confint.breakpointsfull(object = bp.Vt, het.err = FALSE)
## 
## Breakpoints at observation number:
##   2.5 % breakpoints 97.5 %
## 1    26          28     31
## 
## Corresponding to breakdates:
##   2.5 % breakpoints 97.5 %
## 1  1896        1898   1901
## 
##   SEASONAL BREAKPOINTS:  None
## 

nile_bfast_magnitude
## 
## [1] -287.9431

## invisible(plot(bfast_Nile,
##                type = c("trend"), main = ""))
## 

par(mar = c(2, 2, 1, 1), cex=0.8)
invisible(plot(bfast_Nile,
  type = c("trend"), main = "")
  )

bfast_ap <- bfast(log(AirPassengers), 
                  h = round(frequency(AirPassengers)/
                             length(AirPassengers),3),
                  season = "harmonic",
                  max.iter = 10) 


bfast_ap
## 
##   TREND BREAKPOINTS
## 	 Confidence intervals for breakpoints
## 	 of optimal 4-segment partition:
## 
## Call:
## confint.breakpointsfull(object = bp.Vt, het.err = FALSE)
## 
## Breakpoints at observation number:
##   2.5 % breakpoints 97.5 %
## 1    22          23     32
## 2    52          54     59
## 3   107         109    114
## 
## Corresponding to breakdates:
##   2.5 %    breakpoints 97.5 %
## 1 1950(10) 1950(11)    1951(8)
## 2 1953(4)  1953(6)     1953(11)
## 3 1957(11) 1958(1)     1958(6)
## 
##   SEASONAL BREAKPOINTS:  None

par(mar = c(2, 2, 1, 1))
invisible(plot(bfast_ap, # type = c("trend"), 
               main = ""))



summary(pelt_nile)
## 
## Created Using changepoint version 2.2.4
## Changepoint type      : Change in mean and variance
## Method of analysis    : PELT
## Test Statistic  : Normal
## Type of penalty       : BIC with value, 13.81551
## Minimum Segment Length : 15
## Maximum no. of cpts   : Inf
## Changepoint Locations : 28

attributes(pelt_nile)$param.est
## 
## $mean
## [1] 1097.7500  849.9722
## 
## $variance
## [1] 17573.12 15352.92

par(mar = c(2, 2, 1, 1), cex=0.8)
invisible(plot(pelt_Nile, ylab = "Nile flow", xlab = ""))


ap_ts <- AirPassengers |> 
  as_tsibble() |> 
  mutate(log_passengers = log(value))

ap_decomp <- ap_ts |> 
  model(stl_decomp = STL(
    log_passengers ~ trend(window = 13) + 
      season(window = "periodic"))) |> 
  components()

ap_remainder_kpss <- ap_decomp |>  
  features(remainder, feasts::unitroot_kpss) 

ap_remainder_kpss

## # A tibble: 1 × 3
##   .model     kpss_stat kpss_pvalue
##   <chr>          <dbl>       <dbl>
## 1 stl_decomp    0.0264         0.1



summary(pelt_ap_remainder)
## 
## Created Using changepoint version 2.2.4
## Changepoint type      : Change in mean and variance
## Method of analysis    : PELT
## Test Statistic  : Normal
## Type of penalty       : BIC with value, 14.90944
## Minimum Segment Length : 12
## Maximum no. of cpts   : Inf
## Changepoint Locations : 62 109

par(mar = c(2, 2, 1, 1), cex=0.8)
invisible(plot(pelt_ap_remainder, 
               main ="")) 
abline(v= c(62,109), lty = 4, lwd=1)

pelt_ap_cp <- pelt_ap_remainder@cpts
pelt_ap_par <-pelt_ap_remainder@param.est


pelt_ap_cp
## 
## [1]  62 109 144
## 
pelt_ap_par
## $mean
## [1] -0.001516690  0.002190884 -0.000916221
## 
## $variance
## [1] 0.0012457391 0.0002361172 0.0009013271
## 
## 


pelt_ap_ref <- ap_ts |> 
  left_join( ap_decomp |> select(index, remainder),
             by = "index") |> 
  slice(pelt_ap_cp[1:2])


pelt_ap_ref
## 
## # A tsibble: 2 x 4 [1M]
##      index value log_passengers remainder
##      <mth> <dbl>          <dbl>     <dbl>
## 1 1954 Feb   188           5.24   -0.0640
## 2 1958 Jan   340           5.83    0.0191

ap_ts |>
  ggplot(aes(x = index, y = value)) +
  geom_line() +
  geom_vline(xintercept = as.Date(pelt_ap_ref$index),
             color = "grey70", 
             linetype = "dashed", 
             linewidth = 0.9) +
  labs( x = "", y = "Air passengers")


beast_nile <- beast(Nile, season = "none",
                      tcp.minmax = c(0, 10))

beast_nile_trend_cp <- tibble(
  location = beast_nile$trend$cp,
  prob = beast_nile$trend$cpPr,
  ci_lower = beast_nile$trend$cpCI[,1],
  ci_upper = beast_nile$trend$cpCI[,2],
  magnitude_change = beast_nile$trend$cpAbruptChange ) |>
  na.omit()


beast_nile_trend_cp
## 
## # A tibble: 9 × 5
##   location   prob ci_lower ci_upper magnitude_change
##      <dbl>  <dbl>    <dbl>    <dbl>            <dbl>
## 1     1899 0.968     1896.    1901.         -175.
## 2     1954 0.110     1950.    1957.            5.00
## 3     1966 0.0733    1962.    1968.           -2.77
## 4     1946 0.0725    1937.    1951.            2.28
## 5     1916 0.0644    1907.    1922.            1.61
## 6     1890 0.0350    1883.    1894.            1.65
## 7     1929 0.0349    1922.    1937.            0.559
## 8     1877 0.0345    1874.    1883.           -0.157
## 9     1961 0.0337    1956.    1964.            0.241

par(mar = c(2, 2, 1, 1), cex=0.8)
plot(beast_nile, main = "")

beast_ap <- beast(log(AirPassengers),
                  season = "harmonic",
                  tcp.minmax = c(0, 10))

# changepoints in trend
beast_ap_trend_cp <- tibble(
  location = beast_ap$trend$cp,
  prob = beast_ap$trend$cpPr,
  ci_lower = beast_ap$trend$cpCI[,1],
  ci_upper = beast_ap$trend$cpCI[,2],
  magnitude_change = beast_ap$trend$cpAbruptChange ) |>
  na.omit()

# changepoints in seasonality
beast_ap_seas_cp <- tibble(
  location = beast_ap$season$cp,
  prob = beast_ap$season$cpPr,
  ci_lower = beast_ap$season$cpCI[,1],
  ci_upper = beast_ap$season$cpCI[,2],
  magnitude_change = beast_ap$season$cpAbruptChange ) |>
  na.omit()


par(mar = c(2, 2, 1, 1), cex=0.8)
plot(beast_ap, main = "")

beast_ap_trend_cp
## 
## # A tibble: 6 × 5
##   location   prob ci_lower ci_upper magnitude_change
##      <dbl>  <dbl>    <dbl>    <dbl>            <dbl>
## 1    1958  0.979     1958.    1958.          -0.0412
## 2    1953. 0.969     1953.    1954.          -0.0353
## 3    1950. 0.798     1950.    1951.           0.0373
## 4    1956. 0.127     1956.    1957.           0.0145
## 5    1955. 0.0881    1954.    1955.           0.0135
## 6    1952. 0.0297    1952.    1952.           0.0121

beast_ap_seas_cp
## 
## # A tibble: 2 × 5
##   location  prob ci_lower ci_upper magnitude_change
##      <dbl> <dbl>    <dbl>    <dbl>            <dbl>
## 1    1954. 0.539    1953.    1954.         -0.00756
## 2    1953. 0.259    1952.    1953.         -0.0886
