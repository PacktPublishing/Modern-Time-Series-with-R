library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"mFilter"
       ,"FKF"
       ,"qcc"
       ,"tidyverse"
       ,"tstibble"
       ,"zoo" )



nile_dt <- Nile |>
  as_tsibble() |>
  rename(year = index, flow = value)

nile_dt |>
  ggplot(aes(year, flow)) +
  geom_line() +
  scale_y_continuous(limits = c(400, 1400), breaks = seq(400, 1400, by=400))




smooth_list <- list()
alpha_vec <- c(0.2, 0.4, 0.7)
for (i in seq_along(alpha_vec))
{
  smooth_list[[i]] <- ewmaSmooth(
    x = nile_dt$year, 
    y  =nile_dt$flow, 
    lambda = alpha_vec[i])$y 
}  
names(smooth_list) <- paste0("alpha_",alpha_vec)  

# join the smoothed series
nile_ewma <- nile_dt |> 
    bind_cols(smooth_list |> 
                bind_cols())



nile_ewma |> 
  pivot_longer(cols = starts_with("alpha_"),
               names_to = "smoothing_coef",
              values_to = "smoothed_flow") |> 
  ggplot(aes(x = year, y = smoothed_flow)) + 
  geom_line(linewidth = 0.6) +
  geom_line(aes(y = flow), color = "grey", linewidth = 0.7 ) +
  scale_y_continuous(limits = c(400, 1400), 
                     breaks = seq(400, 1400, by=400)) +
  facet_grid(smoothing_coef~.) +
  labs(x = "" ,y = "")



ma_filter <- nile_dt |> 
  mutate("MA(2)" = stats::filter(
            flow, 
            filter = c(0.6,0.4), 
            method="convolution", sides = 1)
         ,"MA(3)" = stats::filter(
           flow, 
           filter = rep(1/3,3), 
           method = "convolution", sides = 1))


ma_filter |> 
  pivot_longer(cols = starts_with("MA")
               ,names_to = "type"
               ,values_to = "filtered") |> 
  ggplot()  +
  geom_line(aes(year, flow), col="grey", linewidth=0.7) +
  geom_line(aes(year, filtered), linewidth = 0.6) +
  scale_y_continuous(limits = c(400, 1400), 
                     breaks = seq(400, 1400, by=400)) +
  facet_grid( type~.) +
  labs(x = ""  ,y = "" ) 


data(unemp)
unemp_hp <- mFilter(unemp, filter = "HP")

# organize
hp_filter <- unemp |> 
  as_tsibble() |> 
  mutate(trend = as.numeric(unemp_hp$trend),
         cycle = as.numeric(unemp_hp$cycle)) |> 
  rename(quarter = index,
         unemployment = value)


unemp_hp$lambda
## 
## [1] 129600


hp_filter |> 
  pivot_longer(cols = c(trend, cycle)
               ,names_to = "component"
               ,values_to = "estimate") |> 
  ggplot() +
  geom_line(aes(quarter, unemployment), linewidth=0.8, color ="grey") +
  geom_line(aes(quarter, estimate),
            linewidth = 0.7 ) +
  facet_grid(component~.) +
  labs(x = "", y = "") 



y <- Nile

## Set constant parameters:
dt <- ct <- matrix(0)
Zt <- Tt <- matrix(1)
a0 <- y[1] # Estimation of the first year flow
P0 <- matrix(100) # Variance of 'a0'

## Estimate parameters:
fit_fkf <- optim(c(HHt = var(y, na.rm = TRUE) * .5,
                   GGt = var(y, na.rm = TRUE) * .5),
                 fn = function(par, ...)
                   -fkf(HHt = matrix(par[1]), 
                        GGt = matrix(par[2]), ...)$logLik,
                 yt = rbind(y), 
                 a0 = a0, P0 = P0,
                 dt = dt, ct = ct,
                 Zt = Zt, Tt = Tt)
## Filter Nile data with estimated parameters:
fkf_obj <- fkf(a0, P0, dt, ct, Tt, Zt,
               HHt = matrix(fit_fkf$par[1]),
               GGt = matrix(fit_fkf$par[2]),
               yt = rbind(y))


fit_fkf$par
## 
##      HHt       GGt
## 1300.777 15247.773


kf_filter <- nile_dt |> 
  mutate("KF filtered" = as.numeric(fkf_obj$att) 
         ,"Kalman gain" = as.numeric(fkf_obj$Kt))



head(kf_filter, 5)
## 
## # A tsibble: 5 x 4 [1Y]
##    year  flow `KF filtered` `Kalman gain`
##   <dbl> <dbl>         <dbl>         <dbl>
## 1  1871  1120         1120        0.00652
## 2  1872  1160         1123.       0.0841
## 3  1873   963         1100.       0.145
## 4  1874  1210         1121.       0.187
## 5  1875  1160         1129.       0.214
## 
tail(kf_filter, 5)
## 
## # A tsibble: 5 x 4 [1Y]
##    year  flow `KF filtered` `Kalman gain`
##   <dbl> <dbl>         <dbl>         <dbl>
## 1  1966   746          907.         0.253
## 2  1967   919          910.         0.253
## 3  1968   718          862.         0.253
## 4  1969   714          824.         0.253
## 5  1970   740          803.         0.253
## 
