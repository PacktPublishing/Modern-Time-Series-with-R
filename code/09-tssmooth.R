## ----ch11-packages, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE,size="tiny"----
#install.packages("pacman")
library(pacman)
p_load("datasets"
       ,"mFilter"
       ,"FKF"
       ,"qcc"
       ,"tidyverse"
       ,"zoo"
        )

theme_set(theme_bw())



## ----ch11-nile-plot
nile_dt <- tibble(year = zoo::index(Nile)
                  ,flow = Nile)

nile_dt |> 
  ggplot() +
  geom_line(aes(year, flow)) +
  theme_bw() +
  labs(x=""
       ,y=latex2exp::TeX(paste0("Flow in $\\10^8m^3$"))
       ,title= "Annual flow of Nile, 1871-1970") +
  theme(axis.text = element_text(size=12))



## ----ch11-peds-sat-smooth
smooth_list <- list()
alpha_vec <- c(0.2, 0.4, 0.7)
for(i in seq_along(alpha_vec))
{
  smooth_list[[i]] <- ewmaSmooth(x=nile_dt$year, 
                                 y=nile_dt$flow, 
                                 lambda = alpha_vec[i])$y 
}  
names(smooth_list) <- paste0("alpha_",alpha_vec)  

# join the smoothed series
nile_ewma <- nile_dt |> 
    bind_cols(smooth_list |> 
                bind_cols())




## ----ch11-peds-sat-smooth-plot
nile_ewma |> 
  pivot_longer(cols = starts_with("alpha_"),
               names_to = "smooting_coef",
              values_to = "smoothed_flow") |> 
  ggplot(aes(x = year, y = smoothed_flow)) +
  geom_line(col = "blue", linewidth = 0.9) +
  facet_grid(smooting_coef~.) +
  geom_line(aes(y = flow)) +
  labs(x = "Year"
       ,y = "Observed and smoothed annual flow"
       , title = latex2exp::TeX(paste0("EWMA smoothing with $\\alpha=$ 0.2, 0.4 and 0.7"))
       ,subtitle = "River Nile's annual flow smothing") +
  theme_bw() +
  theme(strip.text = element_text(size=12)
        ,axis.text = element_text(size=12))



## ----ch11-mafilter-1
ma_filter <- nile_dt |> 
  mutate("MA(2)" = stats::filter(flow, filter = c(0.6,0.4), 
                                    method="convolution", sides=1)
         ,"MA(3)" = stats::filter(flow, filter = rep(1/3,3), 
                                     method="convolution", sides=1))


## ----ch11-mafilter-plot
ma_filter |> 
  pivot_longer(cols = starts_with("MA")
               ,names_to = "type"
               ,values_to = "filtered") |> 
  ggplot()  +
  geom_line(aes(year, flow)) +
  geom_line(aes(year, filtered),col="blue", linewidth=0.9) +
  facet_grid( type~.) +
  labs(x="Year"
       ,y="River Nile's annual flow smothing"
       ,title="Linear filtering using Moving Average on river Nile's annual flow") +
       theme_bw() +
  theme(axis.text = element_text(size=12)
        ,strip.text = element_text(size=12))




## ----ch11-hpfilter
data(unemp)
unemp_hp <- 
  mFilter(unemp, filter="HP")

# organize
hp_filter <- 
  tibble(Quarter = zoo::index(unemp)
         ,Unemployment = unemp
         ,`Estimated trend`= as.numeric(unemp_hp$trend)
         ,`Estimated cycle`= as.numeric(unemp_hp$cycle))


## ----ch11-hpfilter-plot
hp_filter |> 
  pivot_longer(cols = starts_with("Estimated")
               ,names_to = "Component"
               ,values_to = "Estimate") |> 
  ggplot() +
  geom_line(aes(Quarter, Unemployment)) +
  geom_line(aes(Quarter, Estimate), col="blue", linewidth=0.9) +
  facet_grid(Component~.) +
  labs(title = "Estimated trend and cycle from Hodrick-Prescott filter on U.S unemployment rate") +
  theme_bw() +
  theme(axis.text = element_text(size=12)
        ,strip.text = element_text(size=12))




## ----ch11-kf-1

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

# check the parameter estimate
fit_fkf$par



## ----ch11-kf-2

kf_filter <- nile_dt |> 
  mutate("KF filtered" = as.numeric(fkf_obj$att) 
         ,"Kalman gain" = as.numeric(fkf_obj$Kt))



## ----ch11-kf-3

head(kf_filter)

tail(kf_filter)



## ----ch11-nilekf-plot, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE, results='asis', fig.cap="Kalman filter on Nile data", fig.width=8, fig.height=4, size='tiny'----

kf_filter |> 
  ggplot() +
  geom_line(aes(year, flow)) +
  geom_line(aes(year, `KF filtered`), col="blue", linewidth=0.9) +
  labs(x=""
       ,y="Annual flow"
       ,title="Kalman filter using a local level model on Nile data") +
  theme(axis.text = element_text(size=12)
        ,strip.text = element_text(size=12))


