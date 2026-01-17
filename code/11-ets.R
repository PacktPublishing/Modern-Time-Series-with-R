#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"Mcomp"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata" )

conflicts_prefer(
   fabletools::accuracy,
   fabletools::forecast,
   dplyr::filter )







M3$N0003


## 
## Series: Y3
## Type of series: MICRO
## Period of series: YEARLY
## Series description: SALES ( CODE= ARC)
## 
## HISTORICAL data
## Time Series:
## Start = 1975
## End = 1988
## Frequency = 1
##  [1] 1461.57 1692.50 2193.82 2459.68 3246.80 4748.86
##  [7] 5559.46 5292.42 5029.40 4753.60 4344.60 2897.40
## [13] 3256.40 3525.20
## 
## FUTURE data
## Time Series:
## Start = 1989
## End = 1994
## Frequency = 1
## [1] 3070.2 3601.6 3407.4 3500.6 3437.8 3007.0


sesa_train <- as_tsibble(M3$N0003$x) |>
  rename(year = index, sales = value)

sesa_test <- as_tsibble( M3$N0003$xx) |>
  rename(year = index, sales = value)




sesa_fit <- sesa_train |>
  model(ses_additive =
  ETS(sales ~ error("A") + trend("N") + season("N")))


report(sesa_fit)
## 
## Series: sales
## Model: ETS(A,N,N)
##   Smoothing parameters:
##     alpha = 0.9998999
## 
##   Initial states:
##      l[0]
##  1461.183
## 
##   sigma^2:  549010
## 
##      AIC     AICc      BIC
## 225.8109 228.2109 227.7281

gg_tsresiduals(sesa_fit ) +
  labs(x = "")



sesa_fcst <- sesa_fit |>
  forecast(h = 6)

sesa_acc <-  bind_rows(
    accuracy(sesa_fit) ,
    accuracy(sesa_fcst,  bind_rows(sesa_train,sesa_test)))

sesa_acc
## 
## # A tibble: 2 × 10
##   .model       .type       ME  RMSE   MAE   MPE  MAPE  MASE
##   <chr>        <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 ses_additive Training  147.  686.  528.  4.36 15.1  0.929
## 2 ses_additive Test     -188.  290.  213. -6.10  6.81 0.375
## # ℹ 2 more variables: RMSSE <dbl>, ACF1 <dbl>


sesa_fitted_fcst  <- augment(sesa_fit) |> 
      select(year, .fitted) |> 
      bind_rows(as_tsibble(sesa_fcst) |> 
                  select(year, .mean)) 

full_df <- bind_rows(sesa_train, sesa_test)

sesa_comb_df <- full_df |> 
  left_join(sesa_fitted_fcst, by = "year")

sesa_comb_df |> 
  pivot_longer(cols = c(sales, .fitted, .mean),
               names_to = "type",
               values_to = "values") |> 
  mutate(type = factor(type, levels = c("sales", ".fitted", ".mean"))) |> 
  ggplot(aes(x = year, y = values, color = type)) + 
  geom_line( linewidth = 0.9) +
  geom_vline(xintercept = 1988, color = "grey",
             linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("sales"="grey", ".fitted"="black", ".mean"="black")) +
   labs(x = "", y = "sales") +
   theme(legend.position = "top") 


## 
## ETS(y ~ error("M") + trend("N") + season("N"))


hl_train <- as_tsibble(M3$N0011$x)
hl_test <- as_tsibble(M3$N0011$xx)


autoplot(hl_train ) +
  labs( x = "", y = "sales")



hl_fit <- hl_train |> 
  model(holt_additive = 
  ETS(value ~ error("A") + trend("A") + season("N")))

hl_fcst <- hl_fit |> 
  forecast(h = 6)

report(hl_fit)
## 
## Series: value
## Model: ETS(A,A,N)
##   Smoothing parameters:
##     alpha = 0.7801431
##     beta  = 0.7801431
## 
##   Initial states:
##      l[0]     b[0]
##  1195.501 135.7589
## 
##   sigma^2:  73647.98
## 
##      AIC     AICc      BIC
## 199.1349 206.6349 202.3302


ggplot() +
  geom_line(data = bind_rows(hl_train,hl_test),
             aes(x = index, y = value), color = "grey",
          linewidth = 1.1 ) +
  geom_line(data = augment(hl_fit), 
          aes(x=index, y = .fitted), color = "black", 
          linetype="dashed", linewidth = 1) +
  geom_line(data = as_tsibble(hl_fcst) |> 
              select(index, .mean),
            aes(x = index, y = .mean), linewidth = 1.1) +
geom_vline(xintercept = 1989, 
           color = "grey", linetype = "dashed", linewidth= 0.8) +
  labs(x = "", y = "sales")




report(hlm_fit)
## 
## Series: value
## Model: ETS(M,A,N)
##   Smoothing parameters:
##     alpha = 0.9442556
##     beta  = 0.0001000366
## 
##   Initial states:
##      l[0]     b[0]
##  4750.919 133.4805
## 
##   sigma^2:  0.0062
## 
##      AIC     AICc      BIC
## 666.9179 668.6322 675.4858
## 


ggplot() +
  geom_line(data = bind_rows(hlm_train,hlm_test),
             aes(x = index, y = value), color = "grey",
          linewidth = 1.1 ) +
  geom_line(data = augment(hlm_fit), 
          aes(x=index, y = .fitted), color = "black", 
          linetype="dashed", linewidth = 1) +
  geom_line(data = as_tsibble(hlm_fcst) |> 
              select(index, .mean),
            aes(x = index, y = .mean), linewidth = 1.1) +
geom_vline(xintercept = 1988, 
           color = "grey", linetype = "dashed", linewidth = 0.8)



s11_cv <- hl_train |> 
  stretch_tsibble(.init = 10) |> 
  model(
  `Linear trend` = 
    ETS(value ~ error("A") + trend("A") + season("N")),
  `Damped trend` = 
    ETS(value ~ error("A") + trend("Ad") + season("N"))) |> 
  forecast(h = 1) |> 
  accuracy(hl_train)


s11_cv
## 
## # A tibble: 2 × 10
##   .model       .type    ME  RMSE   MAE   MPE  MAPE  MASE
##   <chr>        <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 Damped trend Test   484.  594.  524.  11.5  12.8  1.82
## 2 Linear trend Test   539.  717.  602.  12.3  14.5  2.09
## # ℹ 2 more variables: RMSSE <dbl>, ACF1 <dbl>
## 


s11_fcst <-  hl_train |> 
   model(
 `Linear trend` = ETS(value ~ error("A") + trend("A") + season("N")),
  `Damped trend` = ETS(value ~ error("A") + trend("Ad") + season("N"))) |> 
  forecast(h = 6)


ggplot() +
  geom_line(data = bind_rows(hl_train,hl_test),
             aes(x = index, y = value), color = "grey",linewidth = 1.1 ) +
  geom_line(data = as_tsibble(s11_fcst) |> 
              select(.model, index, .mean),
            aes(x = index, y = .mean, linetype = .model), linewidth = 1.1)  +
  scale_linetype_manual(values = c("Damped trend" = "dashed", 
                                   "Linear trend" = "dotted")) +
    geom_vline(xintercept = 1989, 
           color = "grey", linetype = "dashed", linewidth = 0.8)  +
  
  theme(legend.position = "top") +
  labs(x = "", y = "sales")
  





gas <- aus_production |>
  select(Quarter, Gas)|>
  filter_index("1956 Q1" ~ "2009 Q4")

gas_train <- gas |>
  filter_index("1956 Q1" ~ "2007 Q4")

gas_test <- gas |>
  filter_index("2008 Q1" ~ "2009 Q4")


autoplot(gas_train) +
  labs(x = "")


gas_fit <- gas_train |> 
  model(
  `HW additive` = ETS(Gas ~ error("A") + trend("A") + season("A")),
  `HW multiplicative` = ETS(Gas ~ error("M") + trend("A") + season("M")),
  `HW damped` = ETS(Gas ~ error("A") + trend("Ad") + season("M")))

gas_fcst <- gas_fit |> 
  forecast(h = "2 years")

gas_acc <- accuracy(gas_fcst, gas)


gas_acc
## 
## # A tibble: 3 × 10
##   .model      .type    ME  RMSE   MAE   MPE  MAPE  MASE
##   <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 HW additive Test  -4.52  5.44  4.52 -2.07  2.07 0.806
## 2 HW damped   Test  -2.56  3.88  3.05 -1.11  1.37 0.545
## 3 HW multipl… Test  -7.08  8.57  7.70 -3.09  3.41 1.38
## # ℹ 2 more variables: RMSSE <dbl>, ACF1 <dbl>


gas_fcst |> 
  autoplot(gas |> 
            filter_index("2005 Q1"~"2009 Q4")
     ,level = NULL
     ,size = 0.9) +
  labs(x = "", y = "Gas production") +
  guides(colour = guide_legend(title = "models")) +
  theme(legend.position = "top")




auto_ets <- aus_livestock |> 
  filter( Animal == "Sheep") |> 
  model(ets = ETS(Count))


auto_ets
## 
## # A mable: 8 x 3
## # Key:     Animal, State [8]
##   Animal State                                  ets
##   <fct>  <fct>                              <model>
## 1 Sheep  Australian Capital Territory  <ETS(A,N,N)>
## 2 Sheep  New South Wales               <ETS(A,A,A)>
## 3 Sheep  Northern Territory           <ETS(A,Ad,N)>
## 4 Sheep  Queensland                   <ETS(A,Ad,A)>
## 5 Sheep  South Australia               <ETS(M,N,M)>
## 6 Sheep  Tasmania                      <ETS(M,A,M)>
## 7 Sheep  Victoria                     <ETS(M,Ad,M)>
## 8 Sheep  Western Australia             <ETS(A,N,A)>

auto_ets |>
     filter(State == "Victoria") |>
     report()
## 
## 
## Series: Count
## Model: ETS(M,Ad,M)
##   Smoothing parameters:
##     alpha = 0.766609
##     beta  = 0.000163292
##     gamma = 0.0004487799
##     phi   = 0.9776673
## 
##   Initial states:
##      l[0]     b[0]      s[0]     s[-1]     s[-2]
##  931806.9 11970.24 0.6549046 0.8560137 0.9431726
##     s[-3]    s[-4]    s[-5]    s[-6]    s[-7]    s[-8]
##  1.130084 1.293061 1.276138 1.175191 1.254486 1.201338
##    s[-9]    s[-10]   s[-11]
##  0.86756 0.6886154 0.659436
## 
##   sigma^2:  0.0401
## 
##      AIC     AICc      BIC
## 15763.26 15764.53 15841.10
