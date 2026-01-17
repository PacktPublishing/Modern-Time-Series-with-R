#install.packages("pacman")
library(pacman)
p_load("broom"
       ,"conflicted"
       ,"datasets"
       ,"desk"
       ,"fable"
       ,"forecast"
       ,"here"
       ,"janitor"
       ,"lmtest"
       ,"patchwork"
       ,"prais"
       ,"pwr"
       ,"sandwich"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata" )

conflicts_prefer(
  fabletools::accuracy
  ,fabletools::forecast
  ,dplyr::filter
  ,dplyr::select )


str(sleep)

## 'data.frame':	20 obs. of  3 variables:
##  $ extra: num  0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0 2 ...
##  $ group: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 .
##  $ ID   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4




slpd <- sleep |>
  tibble() |>
  clean_names() |>
  mutate(group = case_when(group == 1 ~ "Trt_1",
                            .default = "Trt_2") |>
           factor() |>
           fct_relevel(c("Trt_1", "Trt_2")))

sleep_summary <- slpd |>
   group_by(group) |>
   summarise(mean = mean(extra),
             variance = var(extra))

sleep_summary

## # A tibble: 2 × 3
##   group  mean variance
##   <fct> <dbl>    <dbl>
## 1 Trt_1  0.75     3.20
## 2 Trt_2  2.33     4.01

pair_ttest <- t.test(extra ~ group, data = slpd,
       paired = TRUE, conf.level = 0.95)

pair_ttest_tidy <- tidy(pair_ttest) |>
  select(method, alternative, estimate, p.value,
         conf.low, conf.high)
    

pair_ttest_tidy
## 
## # A tibble: 1 × 6
##   method        alternative estimate p.value conf.low conf.high
##   <chr>         <chr>          <dbl>   <dbl>    <dbl>     <dbl>
## 1 Paired t-test two.sided      -1.58 0.00283    -2.46    -0.700

slpdiff <- slpd |>
  pivot_wider(names_from = group,
               values_from = extra) |>
  mutate(diff_group =  Trt_1 - Trt_2)

onesamp_diff_ttest <- t.test(slpdiff$diff_group,
        conf.level = 0.95)

onesamp_diff_ttest_tidy <- tidy(onesamp_diff_ttest) |>
   select(method, alternative, estimate, p.value,
         conf.low, conf.high)


onesamp_diff_ttest_tidy
## 
## onesamp_diff_ttest_tidy
## # A tibble: 1 × 6
##   method            alternative estimate p.value conf.low conf.high
##   <chr>             <chr>          <dbl>   <dbl>    <dbl>     <dbl>
## 1 One Sample t-test two.sided      -1.58 0.00283    -2.46    -0.700

pair_diff_lm <- lm(diff_group ~ 1, data = slpdiff)

tidy(pair_diff_lm)
## 
## # A tibble: 1 × 5
##   term        estimate std.error statistic p.value
##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)    -1.58     0.389     -4.06 0.00283
## 
## round(confint(pair_diff_lm), 4)
## 
##               2.5 %  97.5 %
## (Intercept) -2.4599 -0.7001

twosamp_ttest <- t.test(extra ~ group, data = slpd,
                        var.equal = TRUE, conf.level = 0.95)

twosamp_test_tidy <- tidy(twosamp_ttest) |>
  select(method, alternative, estimate,
         p.value, conf.low, conf.high)

twosamp_test_tidy
## 
## # A tibble: 1 × 6
##   method          alternative estimate p.value conf.low conf.high
##   <chr>           <chr>          <dbl>   <dbl>    <dbl>     <dbl>
## 1 Two Sample t-t… two.sided      -1.58  0.0792    -3.36     0.204

twosamp_anova <- aov(
  extra ~ 1 + group,
  data = slpd |> mutate(
           group = relevel(group,ref = "Trt_2")))

tidy(twosamp_anova)
## 
## # A tibble: 2 × 6
##   term         df sumsq meansq statistic p.value
##   <chr>     <dbl> <dbl>  <dbl>     <dbl>   <dbl>
## 1 group         1  12.5  12.5       3.46  0.0792
## 2 Residuals    18  64.9   3.60     NA    NA

tukeyhsd_tidy <- tidy(TukeyHSD(twosamp_anova)) |>
  select(contrast,estimate, adj.p.value, 
         conf.low, conf.high )

tukeyhsd_tidy
## 
## # A tibble: 1 × 5
##   contrast    estimate adj.p.value conf.low conf.high
##   <chr>          <dbl>       <dbl>    <dbl>     <dbl>
## 1 Trt_1-Trt_2    -1.58      0.0792    -3.36     0.204

twogrp_lm <- lm(
  extra ~ 1 + group,
  data = slpd |> mutate(
    group = relevel(group,ref = "Trt_2")))


tidy(twogrp_lm)
## 
## # A tibble: 2 × 5
##   term        estimate std.error statistic p.value
##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)     2.33     0.600      3.88 0.00110
## 2 groupTrt_1     -1.58     0.849     -1.86 0.0792
## 
## round(confint(twogrp_lm), 4)
## 
##               2.5 %  97.5 %
## (Intercept)  1.0686  3.5914
## groupTrt_1  -3.3639  0.2039

set.seed(1)
n_obs <- 120 # Total number of observations
arima_ord <- c(0L, 0L, 0L) # Order for ARIMA model
n_trt <- 30 # Time points post intervention
base_level <- 100
level_increase <- 0.05

# Simulate two sets of data from ARIMA(0,0,0) model
a_mod2sim <- Arima(ts(rnorm(n_obs, sd = 3)), 
                   order = arima_ord) # Simulate from ARIMA model
sim_set1 <- simulate(a_mod2sim) + 
  base_level # 100 is the underlying level of the series
sim_set2 <- simulate(a_mod2sim) + 
 base_level# 100 is the underlying level of the series

# True impact size
impact_size <- base_level*level_increase # 5% increase in underlying level = 5

# Pre and post intervention metric
y_pre <- sim_set2[1:(n_obs - n_trt)]
y_post <- c(tail(sim_set2, n_trt)) + impact_size

# Working data as tibble
wd <- tibble(y_ctrl = sim_set1, # Control group data
             y_trt =  c(y_pre, y_post)) |> # Treatment group data
  mutate(tidx = 1:n()) |>  # A generic time index
  mutate(period = c(rep("_pre", (n_obs - n_trt)),
                    rep("_post", n_trt)) |> factor() |>
           fct_relevel("_pre"))  # Intervention indicator



wd
## 
## # A tibble: 120 × 4
##    y_ctrl y_trt  tidx period
##     <dbl> <dbl> <int> <fct>
##  1   99.0 102.      1 _pre
##  2  104.  103.      2 _pre
##  3   99.8 101.      3 _pre
##  4   99.9  98.0     4 _pre
##  5  100.  103.      5 _pre
##  6  102.   95.0     6 _pre
##  7  100.   98.9     7 _pre
##  8  100.   99.7     8 _pre
##  9   98.5  99.9     9 _pre
## 10   99.5 103.     10 _pre
## # ℹ 110 more rows

wd |>
  pivot_longer(cols = c(y_ctrl, y_trt),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(linetype = metric), linewidth = 0.6) +
  scale_linetype_manual(values = c(4,1) ) +
  scale_x_continuous(limits = c(0,121), breaks = seq(0,121,30)) +
  geom_vline(xintercept = 90) +
  labs(x = "Time index", y = "Response metric") +
  theme(legend.position = "top")



stationary_summary <- wd |>
  select(period, y_ctrl, y_trt) |>
  group_by(period) |>
  summarise_all(list(mean = mean, sd = sd))

stationary_summary
## 
## # A tibble: 2 × 5
##   period y_ctrl_mean y_trt_mean y_ctrl_sd y_trt_sd
##   <fct>        <dbl>      <dbl>     <dbl>    <dbl>
## 1 _pre         100.        101.      2.75     2.54
## 2 _post         99.7       105.      2.68     2.75

stationary_ttest <- wd |>
  filter(period == "_post") |>
  select(y_ctrl, y_trt) |>
  pivot_longer(cols = c(y_ctrl, y_trt),
              names_to = "metric") |>
  mutate(metric = factor(metric) |>
           fct_relevel(c("y_trt", "y_ctrl"))) %>%
  t.test(value ~ metric, data = .,
         var.equal = TRUE, conf.level = 0.95)

stationary_ttest_tidy <- tidy(stationary_ttest ) |>
     select(estimate, p.value, conf.low, conf.high, 
             alternative)

stationary_ttest_tidy
## 
## # A tibble: 1 × 5
##   estimate  p.value conf.low conf.high alternative
##      <dbl>    <dbl>    <dbl>     <dbl> <chr>
## 1     5.63 5.39e-11     4.23      7.03 two.sided

stationary_lm <- lm(y_trt ~ period, dat = wd)

tidy(stationary_lm)
## 
## # A tibble: 2 × 5
##   term        estimate std.error statistic   p.value
##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)   101.       0.274    368.   2.09e-182
## 2 period_post     4.67     0.547      8.53 5.78e- 14
## 
round(confint(stationary_lm),4)
## 
##                2.5 %   97.5 %
## (Intercept) 100.1214 101.2049
## period_post   3.5835   5.7506
## 

stationary_lm_comp <- augment(stationary_lm)
# residuals vs fitted
st_lm_resp <- ggplot(stationary_lm_comp, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linewidth=0.9, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals")


# Normal Q-Q Plot
st_lm_qqp <- ggplot(stationary_lm_comp, aes(sample = .resid)) +
  stat_qq(shape = 1) +
  stat_qq_line(linewidth=0.9, color = "grey", linetype = "dashed") +
  labs(title = "Normal Q-Q",
       x = "Theoretical Quantiles",
       y = "Standarized residuals")

st_lm_resp+st_lm_qqp

stationary_lm_ar1 <- dwtest(
  stationary_lm, alternative = "two.sided")

tidy(stationary_lm_ar1) |>
  select(statistic, p.value, alternative)
## 
## # A tibble: 1 × 3
##   statistic p.value alternative
##       <dbl>   <dbl> <chr>
## 1      2.15   0.454 true autocorrelation is not 0

pwr_analysis_lm <- pwr.f2.test(
  u = 1, f2 = 0.35,
  sig.level = 0.05, 
  power = 0.80)

min_sample_size <- ceiling(
  pwr_analysis_lm$u + pwr_analysis_lm$v + 1)

min_sample_size
## 
## [1] 25

# Extract R-squared
rsq <- summary(stationary_lm)$r.squared 
achieved_effect <- rsq/(1-rsq)

achieved_effect
## 
## [1] 0.6165695

trend_size <- 0.1 # Effect of trend
wd <- wd |> 
  mutate(y_c_trnd = y_ctrl + trend_size*tidx,
         y_t_trnd =  y_trt + trend_size*tidx) 

wd |>
  select(y_c_trnd, y_t_trnd, period, tidx) |>
  pivot_longer(cols = c(y_c_trnd, y_t_trnd),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(linetype = metric), linewidth = 0.6) +
  scale_linetype_manual(values = c(4,1)) +
  scale_x_continuous(limits = c(0,121), breaks = seq(0,121,30)) +
  geom_vline(xintercept = 90, color = "grey", linetype = "dashed")+
  labs(x = "Time index", y = "Response metric") +
  theme(legend.position = "top")

trend_summary <- wd |> 
  select(period, y_c_trnd, y_t_trnd) |>
  group_by(period) |>
  summarise_all(list(mean = mean, sd = sd))


trend_summary
## 
## # A tibble: 2 × 5
##   period y_c_trnd_mean y_t_trnd_mean y_c_trnd_sd y_t_trnd_sd
##   <fct>          <dbl>         <dbl>       <dbl>       <dbl>
## 1 _pre            105.          105.        4.14        3.74
## 2 _post           110.          116.        2.79        2.92

trend_ttest <- wd |>
  filter(period == "_post") |>
  select(y_c_trnd, y_t_trnd) |>
  pivot_longer(cols = c(y_c_trnd, y_t_trnd),
              names_to = "metric") |>
  mutate(metric = factor(metric) |>
           fct_relevel(c("y_t_trnd", "y_c_trnd"))) %>%
  t.test(value ~ metric, data = .,
         var.equal = TRUE, conf.level = 0.95)


trend_ttest_tidy <- tidy(trend_ttest) |>
  select(estimate,p.value, conf.low, conf.high, alternative)


trend_ttest_tidy
## 
## # A tibble: 1 × 5
##   estimate  p.value conf.low conf.high alternative
##      <dbl>    <dbl>    <dbl>     <dbl> <chr>
## 1     5.63 2.54e-10     4.15      7.11 two.sided

trnd_trt_lm <- lm(y_t_trnd ~ period + tidx, dat = wd)


tidy(trnd_trt_lm)
## 
## # A tibble: 3 × 5
##   term        estimate std.error statistic   p.value
##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)  100.       0.546     184.   6.39e-146
## 2 period_post    4.34     0.830       5.23 7.68e-  7
## 3 tidx           0.106    0.0104     10.2  8.47e- 18
## 
## 
round(confint(trnd_trt_lm),4)

##               2.5 %   97.5 %
## (Intercept) 99.3307 101.4931
## period_post  2.6925   5.9792
## tidx         0.0850   0.1261

trend_lm_comp <- augment(trnd_trt_lm)
# residuals vs fitted
trnd_lm_resp <- ggplot(trend_lm_comp, aes(x = .fitted, y = .resid)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, linewidth=0.9, 
             linetype = "dashed", color = "grey") +
  geom_smooth(method = "loess", se = FALSE, color="black") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals")


# Normal Q-Q Plot
trnd_lm_qqp <- ggplot(trend_lm_comp, aes(sample = .resid)) +
  stat_qq(shape = 1) +
  stat_qq_line(linewidth=0.9, color = "grey", linetype="dashed") +
  labs(title = "Normal Q-Q",
       x = "Theoretical Quantiles",
       y = "Standarized residuals")

trnd_lm_resp+trnd_lm_qqp


set.seed(2)
arima_ord <- c(1L, 0L, 0L)
# Simulate two sets of data from ARIMA(1,0,0) model
arima2sim <- Arima(ts(rnorm(n_obs, sd = 3)), 
                         order = arima_ord,
                         fixed = c(0.5, NA))
sim_set1_ar <- simulate(arima2sim) + 100
sim_set2_ar <- simulate(arima2sim) + 100

# Pre and post intervention metric
y_pre <- sim_set2_ar[1:(n_obs - n_trt)]
y_post <- c(tail(sim_set2_ar, n_trt)) + impact_size

# Working data as tibble
wd_ar <- tibble(y_ar1_ctrl = sim_set1_ar,
             y_ar1_trt =  c(y_pre, y_post)) |>
  mutate(tidx = 1:n()) |> 
  mutate(period = c(rep("_pre", (n_obs - n_trt)),
                    rep("_post", n_trt)) |> factor() |>
           fct_relevel("_pre"))

wd_ar |>
  select(y_ar1_ctrl, y_ar1_trt, period, tidx) |>
  pivot_longer(cols = c(y_ar1_ctrl, y_ar1_trt),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(linetype = metric), linewidth = 0.6) +
  scale_linetype_manual(values = c(3,1)) +
  scale_x_continuous(limits = c(0,121), breaks = seq(0,121,30)) +
  geom_vline(xintercept = 90, color = "grey", linetype = "dashed")+
  labs(x = "Time index", y = "Response metric") +
  theme(legend.position = "top")

ar_summary <- wd_ar |>
  group_by(period) |>
  summarise(mean = mean(y_ar1_trt),
            sd = sd(y_ar1_trt))


ar_summary
## 
## # A tibble: 2 × 3
##   period  mean    sd
##   <fct>  <dbl> <dbl>
## 1 _pre    101.  4.72
## 2 _post   109.  3.32
## 

ar_ttest <- wd_ar |>
  filter(period == "_post") |>
  select(y_ar1_ctrl, y_ar1_trt) |>
  pivot_longer(cols = c(y_ar1_ctrl, y_ar1_trt),
               names_to = "metric") |>
  mutate(metric = factor(metric) |>
           fct_relevel(c("y_ar1_trt", "y_ar1_ctrl"))) %>%
  t.test(value ~ metric, data = .,
         var.equal = TRUE, conf.level = 0.95)


tidy(ar_ttest) |>
  select(estimate,p.value,
         conf.low, conf.high, alternative)
## 
## # A tibble: 1 × 5
##   estimate  p.value conf.low conf.high alternative
##      <dbl>    <dbl>    <dbl>     <dbl> <chr>
## 1     7.82 4.41e-10     5.73      9.91 two.sided
## 

ar_lm <- lm(y_ar1_trt ~ period, dat = wd_ar)


tidy(ar_lm)
## 
## # A tibble: 2 × 5
##   term        estimate std.error statistic   p.value
##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)   101.       0.465    218.   1.40e-155
## 2 period_post     7.41     0.931      7.96 1.18e- 12
## 
round(confint(ar_lm),4)
## 
##                2.5 %   97.5 %
## (Intercept) 100.4325 102.2753
## period_post   5.5657   9.2513

acf_plot <- ar_lm$residuals |> 
  ggAcf() + labs(title = "")
pacf_plot <- ar_lm$residuals |> 
  ggPacf() + labs(title = "")

acf_plot|pacf_plot


# Adjustment by Cochran-Orcutt
lm_CO_adj <- desk::cochorc(ar_lm, iter = 100)

# Derive 95% confidence interval
lm_CO_adj_int <- tibble(bind_rows(lm_CO_adj$results[,"coef"] - 
  qnorm(0.975)*lm_CO_adj$results[,"std.err."], 
  lm_CO_adj$results[,"coef"] + 
  qnorm(0.975)*lm_CO_adj$results[,"std.err."])) |> 
  t() |> 
  `colnames<-`(c("2.5 %",    "97.5 %"))

lm_CO_adj
## 
## Cochrane-Orcutt estimation given AR(1)-autocorrelated errors
## -------------------------------------------------------------
## 
##                  coef  std.err.   t.value  p.value
## (Intercept)  101.3394    0.8928  113.5036  < 1e-04
## period_post    7.3009    1.6742    4.3607  < 1e-04
## 
## 
## Number of iterations performed:       3
## Final rho value:                 0.5898
## 


lm_CO_adj_int
## 
##                2.5 %    97.5 %
## (Intercept) 99.58950 103.08933
## period_post  4.01946  10.58239

## # Adjustment by Cochran-Orcutt
## lm_PW_adj <- prais::prais_winsten(
##   y_ar1_trt ~ period,
##    dat = wd_ar, index = 'tidx')
## 
## # Estimated coefficient and significance test
lm_PW_adj_sum <- lm_PW_adj |> summary()
## 
## # Derive 95% confidence interval
## lm_PW_adj_int <- tibble(
##   bind_rows(lm_PW_adj_sum$coefficients[,"Estimate"] -
##   qnorm(0.975)*lm_PW_adj_sum$coefficients[,"Std. Error"],
##   lm_PW_adj_sum$coefficients[,"Estimate"] +
##   qnorm(0.975)*lm_PW_adj_sum$coefficients[,"Std. Error"])) |>
##   t() |>
##   `colnames<-`(c("2.5 %",    "97.5 %"))
## 

lm_PW_adj$rho |> last() |> c()
## [1] 0.5898135
## 

coef(lm_PW_adj_sum) |> round(3)

##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  101.339      0.893 113.504        0
## period_post    7.301      1.674   4.361        0
## 

round(lm_PW_adj_int,4)
## 
##               2.5 %   97.5 %
## (Intercept) 99.5895 103.0893
## period_post  4.0195  10.5824
## 

lm_NW_adj <- lmtest::coeftest(
  ar_lm, 
  vcov = NeweyWest(ar_lm)) 

tidy(lm_NW_adj)
## 
## # A tibble: 2 × 5
##   term        estimate std.error statistic   p.value
##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)   101.        1.00    101.   1.68e-116
## 2 period_post     7.41      1.38      5.37 4.04e-  7
## 
## 
round(confint(lm_NW_adj),4)
## 
##               2.5 %   97.5 %
## (Intercept) 99.3698 103.3380
## period_post  4.6755  10.1415

aa_trt <- wd_ar |> 
  as_tsibble(index = tidx) |> 
  model(arima = ARIMA(y_ar1_trt ~ period, stepwise=FALSE))

aa_trt_coef <- tidy(aa_trt) |> 
  mutate(
  lower_95 = estimate - qnorm(0.975) * std.error,
  upper_95 = estimate + qnorm(0.975) * std.error )|> 
  mutate(across(where(is.numeric), \(x) sprintf("%.3f", x)))

aa_trt
## # A mable: 1 x 1
##                         arima
##                       <model>
## 1 <LM w/ ARIMA(1,0,0) errors>
## 

aa_trt_coef |>
  select(.model, term, estimate, std.error, p.value)
## 
## # A tibble: 3 × 5
##   .model term        estimate std.error p.value
##   <chr>  <chr>       <chr>    <chr>     <chr>
## 1 arima  ar1         0.587    0.073     0.000
## 2 arima  period_post 7.305    1.652     0.000
## 3 arima  intercept   101.339  0.880     0.000

aa_trt_coef |>
  select(.model, term, lower_95, upper_95)
## 
## # A tibble: 3 × 4
##   .model term       lower_95  upper_95
##   <chr>  <chr>         <chr>     <chr>
## 1 arima  ar1           0.444     0.730
## 2 arima  period_post   4.067    10.542
## 3 arima  intercept    99.615   103.063
## 

set.seed(3)
trnd_coef_pre <- 0.5
trnd_coef_post <- 1.0

wd_slope <- wd_ar |> 
  mutate(tidx2 = c(rep(0,(n_obs - n_trt)), 1:n_trt)) |> 
  mutate(
  y_slope_trt = case_when(
      tidx <= (n_obs - n_trt) ~ y_ar1_trt  + trnd_coef_pre*tidx,
      tidx > (n_obs - n_trt) ~ y_ar1_trt + trnd_coef_pre*tidx + trnd_coef_post*tidx2))

wd_slope |>
  select(y_slope_trt, period, tidx) |>
  ggplot(aes(x = tidx, y =y_slope_trt)) +
  geom_line(linewidth = 0.8 ) + 
  scale_x_continuous(limits= c(0,121), breaks = seq(0,121,30)) +
  geom_vline(xintercept = 90, color = "grey",linetype = 3, linewidth = 1) +
  labs(x = "Time index", y = "Response metric") 


aa_ar1slope <- wd_slope |> 
  as_tsibble(index = tidx) |> 
  model(arima = 
        ARIMA(y_slope_trt ~ period + tidx + tidx:period,
              stepwise = FALSE ))

aa_ar1slope_coef <- tidy(aa_ar1slope) |> 
  mutate(
  lower_95 = estimate - qnorm(0.975) * std.error,
  upper_95 = estimate + qnorm(0.975) * std.error ) |> 
  mutate(across(where(is.numeric), \(x) sprintf("%.3f", x)))


aa_ar1slope
## 
## # A mable: 1 x 1
##                         arima
##                       <model>
## 1 <LM w/ ARIMA(1,0,0) errors>
## 
## 
aa_ar1slope_coef |>
  select(.model, term, estimate, std.error, p.value)
## 
## # A tibble: 5 × 5
##   .model term             estimate std.error p.value
##   <chr>  <chr>            <chr>    <chr>     <chr>
## 1 arima  ar1              0.582    0.074     0.000
## 2 arima  period_post      -90.321  15.876    0.000
## 3 arima  tidx             0.508    0.032     0.000
## 4 arima  period_post:tidx 1.069    0.156     0.000
## 5 arima  intercept        100.922  1.719     0.000

aa_ar1slope_coef |>
 select(.model, term, lower_95, upper_95)
## 
## # A tibble: 5 × 4
##   .model term            lower_95   upper_95
##   <chr>  <chr>               <chr>     <chr>
## 1 arima  ar1                 0.437     0.726
## 2 arima  period_post      -121.438   -59.205
## 3 arima  tidx                0.445     0.571
## 4 arima  period_post:tidx    0.763     1.375
## 5 arima  intercept          97.554   104.291


aa_ar1slope |>
  gg_tsresiduals()


aa_ar1slope_lb <- aa_ar1slope |> 
  residuals() |> 
  features(.resid, ljung_box)


aa_ar1slope_lb
## 
## # A tibble: 1 × 3
##   .model lb_stat lb_pvalue
##   <chr>    <dbl>     <dbl>
## 1 arima    0.123     0.726
