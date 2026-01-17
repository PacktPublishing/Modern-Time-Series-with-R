library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"here"
       ,"patchwork"
       ,"rsample"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata" )

conflicts_prefer(
  fabletools::accuracy,
  fabletools::forecast,
  dplyr::filter )



# tsibble series
netuse <- as_tsibble(WWWusage)

# split into training and testing set
netuse_split <- rsample::initial_time_split(netuse, prop = 0.95)
netuse_train <- training(netuse_split)
netuse_test <- testing(netuse_split)

# estimate the model using training set
nv_fit <- netuse_train |>
  model(naive = NAIVE(value))

# augmented fitted & residuals
nv_aug <-nv_fit |>
  augment()

# generate forecasts for 5 obs kept as test
nv_fcst <-nv_fit |>
  forecast(h = 5)

# accuracy
nv_acc <-
  bind_rows(
      fabletools::accuracy(nv_fit,
              measures = point_accuracy_measures),
      fabletools::accuracy(nv_fcst,
               netuse,
               measures = point_accuracy_measures))



## netuse
## 
## # A tsibble: 100 x 2 [1]
##    index value
##    <dbl> <dbl>
##  1     1    88
##  2     2    84
##  3     3    85
##  4     4    85
##  5     5    84
## # ℹ 95 more rows

## autoplot(netuse) +
##   labs(x = "Minutes", y = "Connections")

autoplot(netuse) +
  labs(x = "Minutes", y = "Connections")



## netuse_split
## 
## <Training/Testing/Total>
## <95/5/100>



## netuse_train
## 
## # A tsibble: 95 x 2 [1]
##    index value
##    <dbl> <dbl>
##  1     1    88
##  2     2    84
##  3     3    85
##  4     4    85
##  5     5    84
##  6     6    85
##  7     7    83
##  8     8    85
##  9     9    88
## 10    10    89
## # ℹ 85 more rows



## netuse_test
## 
## # A tsibble: 5 x 2 [1]
##   index value
##   <dbl> <dbl>
## 1    96   222
## 2    97   228
## 3    98   226
## 4    99   222
## 5   100   220
## 



## nv_fit
## 
## # A mable: 1 x 1
##     naive
##   <model>
## 1 <NAIVE>



## 
## # A tibble: 1 × 2
##   .model sigma2
##   <chr>   <dbl>
## 1 naive    32.8



## nv_aug
## 
## # A tsibble: 95 x 6 [1]
## # Key:       .model [1]
##    .model index value .fitted .resid .innov
##    <chr>  <dbl> <dbl>   <dbl>  <dbl>  <dbl>
##  1 naive      1    88      NA     NA     NA
##  2 naive      2    84      88     -4     -4
##  3 naive      3    85      84      1      1
##  4 naive      4    85      85      0      0
##  5 naive      5    84      85     -1     -1
##  6 naive      6    85      84      1      1
##  7 naive      7    83      85     -2     -2
##  8 naive      8    85      83      2      2
##  9 naive      9    88      85      3      3
## 10 naive     10    89      88      1      1
## # ℹ 85 more rows

nv_fit |>  gg_tsresiduals()




## # A tibble: 1 × 5
##   .model bp_stat bp_pvalue lb_stat lb_pvalue
##   <chr>    <dbl>     <dbl>   <dbl>     <dbl>
## 1 naive     138.         0    144.         0



## nv_fcst
## 
## # A fable: 5 x 4 [1]
## # Key:     .model [1]
##   .model index       value .mean
##   <chr>  <dbl>      <dist> <dbl>
## 1 naive     96  N(215, 34)   215
## 2 naive     97  N(215, 69)   215
## 3 naive     98 N(215, 103)   215
## 4 naive     99 N(215, 137)   215
## 5 naive    100 N(215, 171)   215



## # A tibble: 1 × 10
##   .model .type       ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
##   <chr>  <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 naive  Training  1.35  5.85  4.54 0.849  3.48     1     1 0.797
## 



## 
## # A tibble: 1 × 10
##   .model .type    ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
##   <chr>  <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 naive  Test    8.6  9.09   8.6  3.83  3.83  1.89  1.55 0.126
## 





## 
## elctr <- aus_production |>
##   select(Quarter, Electricity) |>
##   filter_index("2000-Q1" ~ "2009-Q4" )
## 
## elctr_train <- elctr |>
##   filter_index("2000-Q1" ~ "2008-Q4" )
## 
## elctr_test <- elctr |>
##  filter_index("2009-Q1" ~ "2009-Q4" )


autoplot(elctr) +
  labs(x = "")



snv_fit <- elctr_train |>
  model(snaive = SNAIVE(Electricity))


## 
## glance(snv_fit)
## 
## # A tibble: 1 × 2
##   .model   sigma2
##   <chr>     <dbl>
## 1 snaive 1861959.


snv_fit |>
  gg_tsresiduals() 

augment(snv_fit) |>
  features(.innov, list(box_pierce,ljung_box ),lag = 10)

## 
## # A tibble: 1 × 5
##   .model bp_stat bp_pvalue lb_stat lb_pvalue
##   <chr>    <dbl>     <dbl>   <dbl>     <dbl>
## 1 snaive    17.6    0.0623    21.9    0.0155


snv_fcst <- forecast(snv_fit, h = 4)


bind_rows(accuracy(snv_fit) ,
          accuracy(snv_fcst, elctr))

## 
## # A tibble: 2 × 10
##   .model .type        ME  RMSE   MAE   MPE  MAPE  MASE RMSSE   ACF1
##   <chr>  <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 snaive Training  1154. 1771. 1436.  2.04  2.56  1     1     0.321
## 2 snaive Test     -1830. 3013. 2236. -3.14  3.85  1.56  1.70 -0.339


snv_comb <- elctr |>
  mutate(source = "observed") |>
  as_tibble() |>
  bind_rows(
      augment(snv_fit) |>
      select(Quarter,Electricity = .fitted) |>
      as_tibble() |>
      mutate(source = "snaive")) |>
  bind_rows(
     as_tibble(snv_fcst) |>
      select(Quarter, Electricity =.mean) |>
      mutate(source = "snaive")) |>
  mutate(source = factor(source, levels = c("observed","snaive")))


snv_comb |> 
ggplot(aes(x = Quarter, y = Electricity, color = source)) +
  geom_line(linewidth=0.8) +
  scale_color_manual(values = c("observed"="grey", "snaive"="black")) +
  geom_vline(xintercept = as.Date(yearmonth("2009 Q1")), 
             color = "grey", linetype = "dashed", linewidth = 0.8) +
  labs(x = "", y = "Electricity") +
  theme(legend.position = "top")
 


# fit average model
avg_fit <-
  netuse_train |>
  model(mean = MEAN(value))

# augment average model
avg_aug <-
  avg_fit |>
  augment()

# average model residual test
avg_resid_test <-
  avg_aug |>
  features(.innov, list(box_pierce, ljung_box), lag = 10)

# forecast
avg_fcst <-
  avg_fit |>
  forecast(h = 5)

# accuracy
avg_acc <-
  fabletools::accuracy(avg_fit) |>
  bind_rows(
    fabletools::accuracy(avg_fcst, netuse))



tidy(avg_fit)
## 
## # A tibble: 1 × 6
##   .model term  estimate std.error statistic  p.value
##   <chr>  <chr>    <dbl>     <dbl>     <dbl>    <dbl>
## 1 mean   mean      133.      3.65      36.3 4.14e-57



## # A tibble: 1 × 5
##   .model bp_stat bp_pvalue lb_stat lb_pvalue
##   <chr>    <dbl>     <dbl>   <dbl>     <dbl>
## 1 mean      353.         0    373.         0



avg_acc
## 
## # A tibble: 2 × 10
##   .model .type        ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
##   <chr>  <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 mean   Train… 1.35e-14  35.4  29.9 -7.80  25.1  6.58  6.04 0.950
## 2 mean   Test   9.11e+ 1  91.1  91.1 40.7   40.7 20.0  15.6  0.126


snv_avg_comb <- as_tibble(netuse) |>
  mutate(series = "observed") |>
  bind_rows(
      as_tibble(nv_fcst) |>
      select(index, value =.mean) |>
      mutate(series = "naive")) |>
  bind_rows(
      as_tibble(avg_fcst ) |>
      select(index, value =.mean) |>
      mutate(series = "average")) |>
  mutate(series = factor(series,
         levels = c("observed", "naive","average")))



p3 <- snv_avg_comb |>
  ggplot(aes(x = index,  y = value,
             color = series, linetype = series )) +
  geom_line(lwd = 0.9) +
  labs(x = "Training and test periods") +
  scale_color_manual(name = "series",
                     values = c("observed" = "grey50",
                                "naive" = "blue",
                                "average" = "tomato"))

p4 <- snv_avg_comb |>
  filter(index > 95) |>
  ggplot(aes(x = index,  y = value,
             color = series, linetype = series )) +
  geom_line(lwd = 0.9) +
  labs(x = "Test period") +
  scale_color_manual(name = "series",
                     values = c("observed" = "grey50",
                                "naive" = "blue",
                                "average" = "tomato")) +
  scale_y_continuous(limits = c(120,240), breaks = seq(120, 240,40)) 

comb_plot <- p3 + p4 & theme(legend.position = "top")

comb_plot + plot_layout(guides = "collect")





elctr_train |>
  ggplot(aes(x = Quarter, y = Electricity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "", y = "")






report(lmcomp_fit)
## 
## Series: Electricity
## Model: TSLM
## 
## Residuals:
##      Min       1Q   Median       3Q      Max
## -2005.32  -792.10    40.98   737.06  3250.69
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)   49159.13     472.29 104.088  < 2e-16 ***
## trend()         278.89      17.65  15.797 2.23e-16 ***
## season()year2  -484.44     516.02  -0.939  0.35508
## season()year3  1896.11     516.92   3.668  0.00091 ***
## season()year4  -984.22     518.43  -1.898  0.06698 .
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 1094 on 31 degrees of freedom
## Multiple R-squared: 0.9025,	Adjusted R-squared: 0.8899
## F-statistic: 71.75 on 4 and 31 DF, p-value: 3.1907e-15


gg_tsresiduals(lmcomp_fit) 



augment(lmcomp_fit) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted", y = "Residuals")




lmcomp_acc
## 
## # A tibble: 2 × 10
##   .model .type           ME  RMSE   MAE     MPE  MAPE  MASE RMSSE    ACF1
##   <chr>  <chr>        <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>
## 1 tscomp Training  8.08e-13 1015.  783. -0.0299  1.42 0.545 0.573  0.0280
## 2 tscomp Test     -2.11e+ 3 2290. 2111. -3.64    3.64 1.47  1.29  -0.0947


lmcomp_fcst |>
  autoplot(elctr_train, level = 95) +
  labs(x = "", y = "Electricity (GWh)")


# tidy
dallas <- txhousing |>
  filter(city == "Dallas")|>
  mutate(year_month = yearmonth("2000 Jan")+ 0:186) |>
  as_tsibble(index = year_month )

# model fit
lm_fit <- dallas |>
  model(lm = TSLM(sales ~ listings + inventory + season()))

# future scenario
future_dt <- scenarios(
  "High listings" = new_data(dallas, 5) |>
    mutate(listings = 30000,inventory = 5),
  "Low listings" = new_data(dallas, 5) |>
    mutate(listings = 15000,inventory = 5)
)

# forecast scenario
scr_fcst <-
  forecast(lm_fit, new_data = future_dt)



report(lm_fit)
## 
## Series: sales
## Model: TSLM
## 
## Residuals:
##      Min       1Q   Median       3Q      Max
## -1150.95  -218.82    10.19   220.36  1110.83
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)
## (Intercept)     3.422e+03  1.216e+02  28.143  < 2e-16 ***
## listings        1.661e-01  8.513e-03  19.509  < 2e-16 ***
## inventory      -8.279e+02  3.773e+01 -21.944  < 2e-16 ***
## season()year2   6.205e+02  1.253e+02   4.953 1.74e-06 ***
## season()year3   1.769e+03  1.254e+02  14.110  < 2e-16 ***
## season()year4   1.857e+03  1.255e+02  14.796  < 2e-16 ***
## season()year5   2.417e+03  1.256e+02  19.244  < 2e-16 ***
## season()year6   2.674e+03  1.257e+02  21.280  < 2e-16 ***
## season()year7   2.451e+03  1.258e+02  19.486  < 2e-16 ***
## season()year8   2.294e+03  1.302e+02  17.629  < 2e-16 ***
## season()year9   1.430e+03  1.278e+02  11.193  < 2e-16 ***
## season()year10  1.284e+03  1.276e+02  10.062  < 2e-16 ***
## season()year11  8.124e+02  1.274e+02   6.376 1.62e-09 ***
## season()year12  9.790e+02  1.274e+02   7.687 1.11e-12 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 354.3 on 172 degrees of freedom
## Multiple R-squared: 0.8905,	Adjusted R-squared: 0.8822
## F-statistic: 107.6 on 13 and 172 DF, p-value: < 2.22e-16


augment(lm_fit) |>
  select(year_month, sales, .fitted) |> 
  pivot_longer(cols = c("sales",".fitted"), 
               names_to = "sources",
               values_to = "values" ) |> 
  as_tibble() |> 
  ggplot() +
  geom_line(aes(x = year_month, y = values, color = sources), lwd = 0.7) +
  scale_color_manual(name = "source",
    values = c(sales = "grey",".fitted" ="black")) +
   labs(x = "", y="house sales price") +
  theme(legend.position = "top")



future_dt <-scenarios(
"High listings" = new_data(dallas, 5) |>
    mutate(listings = 30000,inventory = 5),
"Low listings" = new_data(dallas, 5) |>
    mutate(listings = 15000,inventory = 5))



scr_fcst
## 
## # A fable: 10 x 7 [1M]
## # Key:     .scenario, .model [2]
##    .scenario     .model year_month           sales .mean listings inventory
##    <chr>         <chr>       <mth>          <dist> <dbl>    <dbl>     <dbl>
##  1 High listings lm       2015 Aug N(6560, 140137) 6560.    30000         5
##  2 High listings lm       2015 Sep N(5696, 139269) 5696.    30000         5
##  3 High listings lm       2015 Oct N(5549, 139298) 5549.    30000         5
##  4 High listings lm       2015 Nov N(5078, 139209) 5078.    30000         5
##  5 High listings lm       2015 Dec N(5244, 139462) 5244.    30000         5
##  6 Low listings  lm       2015 Aug N(4068, 137343) 4068.    15000         5
##  7 Low listings  lm       2015 Sep N(3204, 136923) 3204.    15000         5
##  8 Low listings  lm       2015 Oct N(3058, 136861) 3058.    15000         5
##  9 Low listings  lm       2015 Nov N(2586, 136901) 2586.    15000         5
## 10 Low listings  lm       2015 Dec N(2753, 136789) 2753.    15000         5
## 


dallas |>
ggplot(aes(x = year_month)) +
  geom_line(aes(y = sales), color = "grey50" ) +
  geom_line(data = scr_fcst,
            aes(y = .mean,  linetype=.scenario), color = "black",lwd=0.8) +
   scale_linetype_manual(values = 
                c("High listings" = "dashed",
               "Low listings" =  "dotted")) +
  labs(x = "") +
  theme(legend.position = "top")



nv_mult_fit <- netuse_train |>
  model(naive = NAIVE(value),
        mean = MEAN(value),
        drift = RW(value ~ drift()))

nv_mult_fcst <- nv_mult_fit |>
  forecast(h = 5)

nv_mult_acc <-bind_rows(
  accuracy(nv_mult_fit),
  accuracy(nv_mult_fcst, netuse))

nv_mult_acc
## 
## # A tibble: 6 × 10
##   .model .type          ME  RMSE   MAE    MPE  MAPE   MASE  RMSSE  ACF1
##   <chr>  <chr>       <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl>
## 1 naive  Training 1.35e+ 0  5.85  4.54  0.849  3.48  1      1     0.797
## 2 mean   Training 1.35e-14 35.4  29.9  -7.80  25.1   6.58   6.04  0.950
## 3 drift  Training 6.20e-15  5.70  4.42 -0.245  3.44  0.974  0.973 0.797
## 4 drift  Test     4.55e+ 0  6.19  5.25  2.01   2.33  1.16   1.06  0.358
## 5 mean   Test     9.11e+ 1 91.1  91.1  40.7   40.7  20.0   15.6   0.126
## 6 naive  Test     8.6 e+ 0  9.09  8.6   3.83   3.83  1.89   1.55  0.126
## 


bind_rows(
nv_mult_fcst |>
    as_tsibble() |>
    select(.model, index, value =.mean) |>
    mutate(set = "test")
   ,bind_cols(.model = "observed"
             ,netuse_test
             ,set = "original")) |>
  ggplot(aes(x = index, y = value, linetype = .model, color = .model)) +
  geom_line(linewidth = 0.9) +
  #geom_vline(xintercept = 96, linetype = 1) +
  # scale_color_manual(
  #   name = "series",
  #   values = c("observed" = "grey",
  #        "naive" = "black","mean" = "black", "drift" = "black")) +
  # scale_linetype_manual(
  #   name = "series",
  #   values = c("observed" = 1,
  #        "naive" = 2,"mean" = 4, "drift" = 3)) +
  theme(legend.position = "top") +
  labs(x = "", y = "connections")


netuse_cv <- netuse |>
  stretch_tsibble(.init = 60, .step = 5) |>
  relocate(.id)



netuse_cv |>
  group_by(.id) |>
  nest()


## 
## # A tibble: 9 × 2
## # Groups:   .id [9]
##     .id data
##   <int> <list>
## 1     1 <tbl_ts [60 × 2]>
## 2     2 <tbl_ts [65 × 2]>
## 3     3 <tbl_ts [70 × 2]>
## 4     4 <tbl_ts [75 × 2]>
## 5     5 <tbl_ts [80 × 2]>
## 6     6 <tbl_ts [85 × 2]>
## 7     7 <tbl_ts [90 × 2]>
## 8     8 <tbl_ts [95 × 2]>
## 9     9 <tbl_ts [100 × 2]>


# forecasts and organise
nv_cv_fcst <-
  netuse_cv |>
  model(drift = RW(value ~ drift())) |>
  forecast(h = 5) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "value", distribution = value)

nv_cv_acc <-
  nv_cv_fcst |>
  accuracy(netuse, by = c("h",".model"))




nv_cv_fcst
## 
## # A fable: 45 x 6 [1]
## # Key:     .id, .model [9]
##      .id .model index       value .mean     h
##    <int> <chr>  <dbl>      <dist> <dbl> <int>
##  1     1 drift     61  N(122, 33) 122.      1
##  2     1 drift     62  N(122, 66) 122.      2
##  3     1 drift     63 N(123, 101) 123.      3
##  4     1 drift     64 N(123, 137) 123.      4
##  5     1 drift     65 N(124, 174) 124.      5
##  6     2 drift     66   N(99, 33)  99.2     1
##  7     2 drift     67   N(99, 66)  99.3     2
##  8     2 drift     68 N(100, 101)  99.5     3
##  9     2 drift     69 N(100, 137)  99.7     4
## 10     2 drift     70 N(100, 174)  99.9     5
## # ℹ 35 more rows



nv_cv_acc
## 
## # A tibble: 5 × 11
##       h .model .type    ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
##   <int> <chr>  <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     1 drift  Test   3.10  7.33  6.53  1.63  4.86  1.44  1.26 0.521
## 2     2 drift  Test   5.19 14.7  12.6   1.65  9.23  2.78  2.54 0.498
## 3     3 drift  Test   5.92 18.4  15.5   1.41 11.7   3.41  3.17 0.502
## 4     4 drift  Test   6.51 20.3  16.7   1.88 12.9   3.70  3.50 0.487
## 5     5 drift  Test   9.73 23.6  19.6   4.29 14.4   4.33  4.07 0.458
