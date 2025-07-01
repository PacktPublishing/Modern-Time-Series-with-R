## ----ch15-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"broom"
       ,"fable"
       ,"janitor"
       ,"desk"
       ,"patchwork"
       ,"tidyverse"
       ,"lmtest"
       ,"sandwich"
       ,"prais"
       ,"pwr"
      )

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
)
theme_set(theme_bw())


## ----ch15-exp-vs-obs, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
slpd <- sleep |>
  tibble() |>
  clean_names() |>
  mutate(group = case_when(group == 1 ~ "Trt_1",
                            .default = "Trt_2") |>
           factor() |>
           fct_relevel(c("Trt_1", "Trt_2")))

# pair t-test 
pair_test <- t.test(extra ~ group, data = slpd,
       paired = TRUE, conf.level = 0.95)

stats2print <- c("estimate", "conf.low", "conf.high")

# difference between treatment 
slpdiff <- slpd |>
  pivot_wider(names_from = group,
               values_from = extra) |>
  mutate(diff_group =  Trt_1 - Trt_2)
# t-test in difference between treatment 
paired_diff_test_2 <- t.test(slpdiff$diff_group,
        conf.level = 0.95)

# linear model in paired difference  
lm_pair_diff <- lm(diff_group ~ 1,
                   data = slpdiff)

# two sample test 
two_samp_test <- t.test(extra ~ group, data = slpd,
                        var.equal = TRUE, conf.level = 0.95)

# two sample anova 
two_samp_anova <- aov(extra ~ 1 + group,
             data = slpd |>
                  mutate(group = relevel(group,
                                         ref = "Trt_2")))

# two groups linear regression 
lm_2_grps <- lm(extra ~ 1 + group,
                data = slpd |>
                  mutate(group = relevel(group,
                                         ref = "Trt_2")))




## ----ch15-exp-vs-obs-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## slpd <- sleep |>
##   tibble() |>
##   clean_names() |>
##   mutate(group = case_when(group == 1 ~ "Trt_1",
##                             .default = "Trt_2") |>
##            factor() |>
##            fct_relevel(c("Trt_1", "Trt_2")))
## 
## slpd |>
##    group_by(group) |>
##    summarise(mean = mean(extra),
##              variance = var(extra))
## # # A tibble: 2 × 3
## #   group  mean variance
## #   <fct> <dbl>    <dbl>
## # 1 Trt_1  0.75     3.20
## # 2 Trt_2  2.33     4.01
## 


## ----ch15-ttest-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## pair_test <- t.test(extra ~ group, data = slpd,
##        paired = TRUE, conf.level = 0.95)
## 
## stats2print <- c("estimate", "conf.low", "conf.high")
## 
## tidy(pair_test) |>
##   select(all_of(c(stats2print, "p.value")))
## # # A tibble: 1 × 4
## #   estimate conf.low conf.high p.value
## #      <dbl>    <dbl>     <dbl>   <dbl>
## # 1    -1.58    -2.46    -0.700 0.00283
## 


## ----ch15-diff-ttest-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## slpdiff <- slpd |>
##   pivot_wider(names_from = group,
##                values_from = extra) |>
##   mutate(diff_group =  Trt_1 - Trt_2)
## 
## paired_diff_test_2 <- t.test(slpdiff$diff_group,
##         conf.level = 0.95)
## tidy(paired_diff_test_2) |>
##   select(all_of(c(stats2print, "p.value")))
## # # A tibble: 1 × 4
## #   estimate conf.low conf.high p.value
## #      <dbl>    <dbl>     <dbl>   <dbl>
## # 1    -1.58    -2.46    -0.700 0.00283
## 


## ----ch15-linear-reg-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## lm_pair_diff <- lm(diff_group ~ 1,
##                    data = slpdiff)
## 
## tidy(lm_pair_diff)
## # # A tibble: 1 × 5
## #   term        estimate std.error statistic p.value
## #   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
## # 1 (Intercept)    -1.58     0.389     -4.06 0.00283
## 
## confint(lm_pair_diff)
## #                  2.5 %     97.5 %
## # (Intercept) -2.459886 -0.7001142
## 


## ----ch15-2samp-test-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## two_samp_test <- t.test(extra ~ group, data = slpd,
##                         var.equal = TRUE, conf.level = 0.95)
## tidy(two_samp_test) |>
##   select(all_of(c(stats2print, "p.value")))
## 
## # # A tibble: 1 × 4
## #   estimate conf.low conf.high p.value
## #      <dbl>    <dbl>     <dbl>   <dbl>
## # 1    -1.58    -3.36     0.204  0.0792
## 


## ----ch15-2samp-anova-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## two_samp_anova <- aov(extra ~ 1 + group,
##              data = slpd |>
##                   mutate(group = relevel(group,
##                                          ref = "Trt_2")))
## tidy(two_samp_anova)
## # # A tibble: 2 × 6
## #   term         df sumsq meansq statistic p.value
## #   <chr>     <dbl> <dbl>  <dbl>     <dbl>   <dbl>
## # 1 group         1  12.5  12.5       3.46  0.0792
## # 2 Residuals    18  64.9   3.60     NA    NA
## 
## 
## tidy(TukeyHSD(two_samp_anova)) |>
##   select(all_of(c("contrast", stats2print)))
## # # A tibble: 1 × 4
## #   contrast    estimate conf.low conf.high
## #   <chr>          <dbl>    <dbl>     <dbl>
## # 1 Trt_1-Trt_2    -1.58    -3.36     0.204
## 


## ----ch15-lm2-anova-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## lm_2_grps <- lm(extra ~ 1 + group,
##                 data = slpd |>
##                   mutate(group = relevel(group,
##                                          ref = "Trt_2")))
## tidy(lm_2_grps)
## # # A tibble: 2 × 5
## #   term        estimate std.error statistic p.value
## #   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
## # 1 (Intercept)     2.33     0.600      3.88 0.00110
## # 2 groupTrt_1     -1.58     0.849     -1.86 0.0792
## 
## confint(lm_2_grps)
## #                 2.5 %   97.5 %
## # (Intercept)  1.068611 3.591389
## # groupTrt_1  -3.363874 0.203874


## ----ch15-stTSgen, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
set.seed(1)
n_obs <- 120
arima_ord <- c(0L, 0L, 0L)
n_trt <- 30 # Time points with intervention

# Simulate two 2 sets of data from ARIMA(0,0,0) model
a_mod2sim <- Arima(ts(rnorm(n_obs, sd = 3)), order = arima_ord)
sim_set1 <- simulate(a_mod2sim) + 100
sim_set2 <- simulate(a_mod2sim) + 100

# True impact size
impact_size <- 100*0.05 # 5% increase in level

# Pre and post intervention metric
y_pre <- sim_set2[1:(n_obs - n_trt)]
y_post <- c(tail(sim_set2, n_trt)) + impact_size

# Working data as tibble
wd <- tibble(y_ctrl = sim_set1, # Control
             y_trt =  c(y_pre, y_post)) |> # Treatment
  mutate(tidx = 1:n()) |>  # Time index
  mutate(period = c(rep("_pre", (n_obs - n_trt)),
                    rep("_post", n_trt)) |> factor() |>
           fct_relevel("_pre"))  # Intervention index


## ----ch15-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Time plot of a metric from treatment and control group prior and post of intervention", fig.width=8,fig.height=4----
wd |>
  pivot_longer(cols = c(y_ctrl, y_trt),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(col = metric), linewidth = 1, alpha = 0.6) +
  geom_point(aes(shape = period), alpha = 0.5, size = 2) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  labs(x = "Time id", y = "Response metric") +
  theme(legend.position = "bottom")


## ----ch15-stTSgen_data_sum, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
wd |>
  select(period, y_ctrl, y_trt) |>
  group_by(period) |>
  summarise_all(list(mean = mean, sd = sd))


## ----ch15-stTSgen_data_sum-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## wd |>
##   select(period, y_ctrl, y_trt) |>
##   group_by(period) |>
##   summarise_all(list(mean = mean, sd = sd))
## # # A tibble: 2 × 5
## #   period y_ctrl_mean y_trt_mean y_ctrl_sd y_trt_sd
## #   <fct>        <dbl>      <dbl>     <dbl>    <dbl>
## # 1 _pre         100.        101.      2.75     2.54
## # 2 _post         99.7       105.      2.68     2.75


## ----ch15-impac_measure, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
impact_t_test <- wd |>
  filter(period == "_post") |>
  select(y_ctrl, y_trt) |>
  pivot_longer(cols = c(y_ctrl, y_trt),
              names_to = "metric") |>
  mutate(metric = factor(metric) |>
           fct_relevel(c("y_trt", "y_ctrl"))) %>%
  t.test(value ~ metric, data = .,
         var.equal = TRUE, conf.level = 0.95)


## ----ch15-impac_measure-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## impact_t_test <- wd |>
##   filter(period == "_post") |>
##   select(y_ctrl, y_trt) |>
##   pivot_longer(cols = c(y_ctrl, y_trt),
##               names_to = "metric") |>
##   mutate(metric = factor(metric) |>
##            fct_relevel(c("y_trt", "y_ctrl"))) %>%
##   t.test(value ~ metric, data = .,
##          var.equal = TRUE, conf.level = 0.95)
## 
## tidy(impact_t_test) |>
##   select(estimate,p.value, conf.low, conf.high, alternative)
## # # A tibble: 1 × 5
## #   estimate  p.value conf.low conf.high alternative
## #      <dbl>    <dbl>    <dbl>     <dbl> <chr>
## # 1     5.63 5.39e-11     4.23      7.03 two.sided


## ----ch15-impac_measure_lm, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
impact_lm <- lm(y_trt ~ period, dat = wd)


## ----ch15-impac_measure_lm-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## impact_lm <- lm(y_trt ~ period, dat = wd)
## tidy(impact_lm)
## # # A tibble: 2 × 5
## #   term        estimate std.error statistic   p.value
## #   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## # 1 (Intercept)   101.       0.274    368.   2.09e-182
## # 2 period_post     4.67     0.547      8.53 5.78e- 14
## 
## confint(impact_lm)
## #                  2.5 %     97.5 %
## # (Intercept) 100.121357 101.204876
## # period_post   3.583545   5.750584


## ----ch15-2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Residuals analysis for checking adequacy of fitted linear model for measuring impact for stationary data", fig.width=8,fig.height=4----
par(mfrow = c(1,2))
plot(impact_lm, which = c(1:2))


## ----ch15-dw_test, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
lm_ar1_test <- dwtest(impact_lm, 
                           alternative = "two.sided")


## ----ch15-dw_test-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## lm_ar1_test <- dwtest(impact_lm,
##                            alternative = "two.sided")
## tidy(lm_ar1_test) |> select(-method)
## # # A tibble: 1 × 3
## #   statistic p.value alternative
## #       <dbl>   <dbl> <chr>
## # 1      2.15   0.454 true autocorrelation is not 0


## ----ch15-power-note, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
pwr_analysis <- pwr.anova.test(k = 2, f = 0.4, 
                               # use large impact threshold
               power = 0.8, sig.level = 0.05)



## ----ch15-power-note_noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## pwr_analysis <- pwr.anova.test(k = 2, f = 0.4,
##                power = 0.8, sig.level = 0.05)
## tidy(pwr_analysis)
## # # A tibble: 1 × 3
## #       n sig.level power
## #   <dbl>     <dbl> <dbl>
## # 1  25.5      0.05   0.8


## ----------------------------------------------
f <- sd(c(mean(wd$y_ctrl), mean(wd$y_trt)))/sd(wd$y_ctrl)


## ----ch15-ts_trnd, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
trend_size <- 0.1 # Effect of trend
wd <- wd |> 
  mutate(y_c_trnd = y_ctrl + trend_size*tidx,
         y_t_trnd =  y_trt + trend_size*tidx) 


## ----ch15-3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Time plot of a metric from treatment and control group prior and post of intervention with trend", fig.width=8,fig.height=4----
wd |>
  select(y_c_trnd, y_t_trnd, period, tidx) |>
  pivot_longer(cols = c(y_c_trnd, y_t_trnd),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(col = metric), linewidth = 1, alpha = 0.6) +
  geom_point(aes(shape = period), alpha = 0.5, size = 2) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  labs(x = "Time id", y = "Response metric") +
  theme(legend.position = "bottom")


## ----ch15-ts_trnd-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## trend_size <- 0.1 # 0.05
## wd <- wd |>
##   mutate(y_c_trnd = y_ctrl + trend_size*tidx,
##          y_t_trnd =  y_trt + trend_size*tidx)
## 
## # Summary of the control and treatment group data
## wd |>
##   select(period, y_c_trnd, y_t_trnd) |>
##   group_by(period) |>
##   summarise_all(list(mean = mean, sd = sd))
## # # A tibble: 2 × 5
## #   period y_c_trnd_mean y_t_trnd_mean y_c_trnd_sd y_t_trnd_sd
## #   <fct>          <dbl>         <dbl>       <dbl>       <dbl>
## # 1 _pre            105.          105.        4.14        3.74
## # 2 _post           110.          116.        2.79        2.92


## ----ch15-impac_measure_trnd, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
impact_trend_t_test <- wd |>
  filter(period == "_post") |>
  select(y_c_trnd, y_t_trnd) |>
  pivot_longer(cols = c(y_c_trnd, y_t_trnd),
              names_to = "metric") |>
  mutate(metric = factor(metric) |>
           fct_relevel(c("y_t_trnd", "y_c_trnd"))) %>%
  t.test(value ~ metric, data = .,
         var.equal = TRUE, conf.level = 0.95)


## ----ch15-impac_measure_trnd-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## impact_trend_t_test <- wd |>
##   filter(period == "_post") |>
##   select(y_c_trnd, y_t_trnd) |>
##   pivot_longer(cols = c(y_c_trnd, y_t_trnd),
##               names_to = "metric") |>
##   mutate(metric = factor(metric) |>
##            fct_relevel(c("y_t_trnd", "y_c_trnd"))) %>%
##   t.test(value ~ metric, data = .,
##          var.equal = TRUE, conf.level = 0.95)
## 
## tidy(impact_trend_t_test) |>
##   select(estimate,p.value, conf.low, conf.high, alternative)
## # # A tibble: 1 × 5
## #   estimate  p.value conf.low conf.high alternative
## #      <dbl>    <dbl>    <dbl>     <dbl> <chr>
## # 1     5.63 2.54e-10     4.15      7.11 two.sided


## ----ch15-impac_measure_trend_lm, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
lmfit_trt_trnd <- lm(y_t_trnd ~ period + tidx, dat = wd)


## ----ch15-impac_measure_trend_lm-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## lmfit_trt_trnd <- lm(y_t_trnd ~ period + tidx, dat = wd)
## tidy(lmfit_trt_trnd)
## # # A tibble: 3 × 5
## #   term        estimate std.error statistic   p.value
## #   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## # 1 (Intercept)  100.       0.546     184.   6.39e-146
## # 2 period_post    4.34     0.830       5.23 7.68e-  7
## # 3 tidx           0.106    0.0104     10.2  8.47e- 18
## 
## confint(lmfit_trt_trnd)
## #                   2.5 %      97.5 %
## # (Intercept) 99.33072979 101.4931211
## # period_post  2.69247947   5.9791671
## # tidx         0.08497817   0.1260632


## ----ch15-4, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Residuals analysis for checking adequacy of fitted linear model for measuring impact for stationary data", fig.width=8,fig.height=4----
par(mfrow = c(1,2))
plot(lmfit_trt_trnd, which = c(1:2))


## ----ch15-ts_ar1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
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


## ----ch15-5, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Time plot of a AR(1) serially dependent metric from treatment and control group prior and post of intervention", fig.width=8,fig.height=4----
wd_ar |>
  select(y_ar1_ctrl, y_ar1_trt, period, tidx) |>
  pivot_longer(cols = c(y_ar1_ctrl, y_ar1_trt),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(col = metric), linewidth = 1, alpha = 0.6) +
  geom_point(aes(shape = period), alpha = 0.5, size = 2) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  labs(x = "Time id", y = "Response metric") +
  theme(legend.position = "bottom")


## ----ch15-ts_ar_sum-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Summary for the treatment group pre and post intervention
## wd_ar |> group_by(period) |>
##   summarise(mean = mean(y_ar1_trt),
##             sd = sd(y_ar1_trt))
## # # A tibble: 2 × 3
## #   period  mean    sd
## #   <fct>  <dbl> <dbl>
## # 1 _pre    101.  4.72
## # 2 _post   109.  3.32


## ----ch15-impac_measure_ARts, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
impact_t_test_with_ar1 <- wd_ar |>
  filter(period == "_post") |>
  select(y_ar1_ctrl, y_ar1_trt) |>
  pivot_longer(cols = c(y_ar1_ctrl, y_ar1_trt),
               names_to = "metric") |>
  mutate(metric = factor(metric) |>
           fct_relevel(c("y_ar1_trt", "y_ar1_ctrl"))) %>%
  t.test(value ~ metric, data = .,
         var.equal = TRUE, conf.level = 0.95)


## ----ch15-impac_measure_ARts-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## impact_t_test_with_ar1 <- wd_ar |>
##   filter(period == "_post") |>
##   select(y_ar1_ctrl, y_ar1_trt) |>
##   pivot_longer(cols = c(y_ar1_ctrl, y_ar1_trt),
##                names_to = "metric") |>
##   mutate(metric = factor(metric) |>
##            fct_relevel(c("y_ar1_trt", "y_ar1_ctrl"))) %>%
##   t.test(value ~ metric, data = .,
##          var.equal = TRUE, conf.level = 0.95)
## 
## tidy(impact_t_test_with_ar1) |>
##   select(estimate,p.value, conf.low, conf.high, alternative)
## # # A tibble: 1 × 5
## #   estimate  p.value conf.low conf.high alternative
## #      <dbl>    <dbl>    <dbl>     <dbl> <chr>
## # 1     7.82 4.41e-10     5.73      9.91 two.sided


## ----ch15-impac_measure_AR1_lm, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
impact_lm_with_ar1 <- lm(y_ar1_trt ~ period, dat = wd_ar)


## ----ch15-impac_measure_AR1_lm-noEval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## impact_lm_with_ar1 <- lm(y_ar1_trt ~ period, dat = wd_ar)
## 
## tidy(impact_lm_with_ar1)
## # # A tibble: 2 × 5
## #   term        estimate std.error statistic   p.value
## #   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## # 1 (Intercept)   101.       0.465    218.   1.40e-155
## # 2 period_post     7.41     0.931      7.96 1.18e- 12
## 
## confint(impact_lm_with_ar1)
## #                  2.5 %     97.5 %
## # (Intercept) 100.432507 102.275299
## # period_post   5.565719   9.251304


## ----ch15-6, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Autocorretaion function of residuals from linear model fit on AR(1) data for ITS analysis", fig.width=8,fig.height=4----
impact_lm_with_ar1$residuals |> 
  ggAcf() + labs(title = "")


## ----ch15-cochra_adjust, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
# Adjustment by Cochran-Orcutt
lm_CO_adj <- desk::cochorc(impact_lm_with_ar1, iter = 100)


## ----ch15-cochra_adjusts-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Adjustment by Cochran-Orcutt
## lm_CO_adj <- desk::cochorc(impact_lm_with_ar1, iter = 100)
## lm_CO_adj
## # Cochrane-Orcutt estimation given AR(1)-autocorrelated errors
## # -------------------------------------------------------------
## #
## #                  coef  std.err.   t.value  p.value
## # (Intercept)  101.3394    0.8928  113.5036  < 1e-04
## # period_post    7.3009    1.6742    4.3607  < 1e-04
## #
## #
## # Number of iterations performed:       3
## # Final rho value:                 0.5898
## 
## # Calculate 95% confidence interval
## tibble(bind_rows(lm_CO_adj$results[,"coef"] -
##   qnorm(0.975)*lm_CO_adj$results[,"std.err."],
##   lm_CO_adj$results[,"coef"] +
##   qnorm(0.975)*lm_CO_adj$results[,"std.err."])) |>
##   t() |>
##   `colnames<-`(c("2.5 %",    "97.5 %"))
## #                2.5 %    97.5 %
## # (Intercept) 99.58950 103.08933
## # period_post  4.01946  10.58239


## ----ch15-prais_adjust, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
# Adjustment by Cochran-Orcutt
lm_PW_adj <- prais::prais_winsten(y_ar1_trt ~ period, 
                                  dat = wd_ar, index = 'tidx')


## ----ch15-prais_adjusts-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Adjustment by Prais-Winsten
## lm_PW_adj <- prais::prais_winsten(y_ar1_trt ~ period,
##                                   dat = wd_ar, index = 'tidx')
## 
## # Estimated coefficient and significance test
## lm_PW_adj_sum <- lm_PW_adj |> summary()
## coef(lm_PW_adj_sum) |> round(3)
## #             Estimate Std. Error t value Pr(>|t|)
## # (Intercept)  101.339      0.893 113.504        0
## # period_post    7.301      1.674   4.361        0
## 
## # Estimated correlation coefficient
## lm_PW_adj $rho |> last() |> c()
## # [1] 0.5898135
## 
## # Calculate 95% confidence interval
## tibble(bind_rows(lm_PW_adj_sum$coefficients[,"Estimate"] -
##   qnorm(0.975)*lm_PW_adj_sum$coefficients[,"Std. Error"],
##   lm_PW_adj_sum$coefficients[,"Estimate"] +
##   qnorm(0.975)*lm_PW_adj_sum$coefficients[,"Std. Error"])) |>
##   t() |>
##   `colnames<-`(c("2.5 %",    "97.5 %"))
## #                2.5 %    97.5 %
## # (Intercept) 99.58950 103.08933
## # period_post  4.01946  10.58239


## ----ch15-nw_adjust, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
# Adjustment by Newey-West
lm_NW_adj <- lmtest::coeftest(impact_lm_with_ar1, 
                              vcov = NeweyWest(impact_lm_with_ar1)) 


## ----ch15-nw_adjusts-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Adjustment by Newey-West
## lm_NW_adj <- lmtest::coeftest(impact_lm_with_ar1,
##                               vcov = NeweyWest(impact_lm_with_ar1))
## 
## # Estimated coefficient and significance test
## lm_NW_adj |> tidy()
## # # A tibble: 2 × 5
## #   term        estimate std.error statistic   p.value
## #   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
## # 1 (Intercept)   101.        1.00    101.   1.68e-116
## # 2 period_post     7.41      1.38      5.37 4.04e-  7
## 
## # 95% confidence interval
## confint(lm_NW_adj)
## #                 2.5 %    97.5 %
## # (Intercept) 99.369823 103.33798
## # period_post  4.675517  10.14151


## ----ch15-aa_adjust, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
# Fitting ARIMA model automatically
aa_trt <- auto.arima(wd_ar$y_ar1_trt, 
                     xreg = as.numeric(wd_ar$period), 
                     stepwise = T)


## ----ch15-aa_adjusts-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Fitting ARIMA model automatically
## aa_trt <- auto.arima(wd_ar$y_ar1_trt,
##                      xreg = as.numeric(wd_ar$period),
##                      stepwise = F)
## 
## # Model summary
## aa_trt
## # Series: wd_ar$y_ar1_trt
## # Regression with ARIMA(1,0,0) errors
## #
## # Coefficients:
## #          ar1  intercept    xreg
## #       0.5870    94.0349  7.3046
## # s.e.  0.0732     2.2128  1.6517
## #
## # sigma^2 = 12.8:  log likelihood = -321.91
## # AIC=651.82   AICc=652.17   BIC=662.97
## 
## # Estimated coefficient and significance test
## sig_test <- coeftest(aa_trt)
## sig_test
## # z test of coefficients:
## #
## #            Estimate Std. Error z value  Pr(>|z|)
## # ar1        0.586966   0.073163  8.0227 1.034e-15 ***
## # intercept 94.034854   2.212777 42.4963 < 2.2e-16 ***
## # xreg       7.304574   1.651702  4.4225 9.759e-06 ***
## # ---
## # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## # Calculate 95% confidence interval
## tibble(bind_rows(sig_test[,"Estimate"] -
##                    qnorm(0.975)*sig_test[,"Std. Error"],
##        sig_test[,"Estimate"] +
##                    qnorm(0.975)*sig_test[,"Std. Error"])) |>
##   t() |>
##   `colnames<-`(c("2.5 %",    "97.5 %"))
## #                2.5 %     97.5 %
## # ar1        0.4435686  0.7303627
## # intercept 89.6978909 98.3718167
## # xreg       4.0672971 10.5418502


## ----ch15-ts_ar1_slope, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
set.seed(3)

# Working data as tibble
wd_slope <- tibble(y_ar1_ctrl = sim_set1_ar,
                y_ar1_trt =  sim_set2_ar) |>
  mutate(tidx = 1:n()) |>
  mutate(period = c(rep("_pre", (n_obs - n_trt)),
                    rep("_post", n_trt)) |> factor() |>
           fct_relevel("_pre"))

trnd_coef_pre <- 0.5
trnd_coef_post <- 1.0

wd_slope <- wd_ar |> 
  mutate(tidx2 = c(rep(0,(n_obs - n_trt)), 1:n_trt)) |> 
  mutate(y_Slope_ctrl = y_ar1_ctrl  + 
           trnd_coef_pre*tidx,
         y_Slope_trt =  
           case_when(tidx <= 
                       (n_obs - n_trt) ~  y_ar1_trt  +
                       trnd_coef_pre*tidx,
                     tidx > (n_obs - n_trt) ~  y_ar1_trt + 
                       trnd_coef_pre*tidx + trnd_coef_post*tidx2))


## ----ch15-7, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Time plot of a AR(1) serially dependent metric from treatment and control group prior and post of intervention", fig.width=8,fig.height=4----
wd_slope |>
  select(y_Slope_ctrl, y_Slope_trt, period, tidx) |>
  pivot_longer(cols = c(y_Slope_ctrl, y_Slope_trt),
               names_to = "metric") |>
  ggplot(aes(x = tidx, y = value)) +
  geom_line(aes(col = metric), linewidth = 1, alpha = 0.6) +
  geom_point(aes(shape = period), alpha = 0.5, size = 2) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) +
  labs(x = "Time id", y = "Response metric") +
  theme(legend.position = "bottom")


## ----ch15-aa_ar1slope, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
# Fitting ARIMA model automatically
xreg4arima <- model.matrix(y_Slope_trt ~ period + 
                             tidx + tidx:period,
                           data = wd_slope)[,-1]
aa_ar1slope <- auto.arima(wd_slope$y_Slope_trt, 
                          xreg = xreg4arima, 
                          stepwise = F)


## ----ch15-aa_ar1slop_p1-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Fitting ARIMA model automatically
## xreg4arima <- model.matrix(y_Slope_trt ~ period +
##                              tidx + tidx:period,
##                            data = wd_slope)[,-1]
## aa_ar1slope <- auto.arima(wd_slope$y_Slope_trt,
##                           xreg = xreg4arima,
##                           stepwise = F)
## 
## # # Model summary
## # aa_ar1slope
## # Series: wd_slope$y_Slope_trt
## # Regression with ARIMA(1,0,0) errors
## #
## # Coefficients:
## #          ar1  intercept  period_post    tidx  period_post:tidx
## #       0.5818   100.9222     -90.3215  0.5082            1.0689
## # s.e.  0.0737     1.7186      15.8760  0.0322            0.1562
## #
## # sigma^2 = 12.98:  log likelihood = -321.73
## # AIC=655.45   AICc=656.19   BIC=672.18


## ----ch15-aa_ar1slop_p2-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Estimated coefficient and significance test
## sig_test_aaslope <- coeftest(aa_ar1slope)
## sig_test_aaslope |> round(3)
## # z test of coefficients:
## #
## #                  Estimate Std. Error z value  Pr(>|z|)
## # ar1                 0.582      0.074   7.895 < 2.2e-16
## # intercept         100.922      1.719  58.722 < 2.2e-16
## # period_post       -90.321     15.876  -5.689 < 2.2e-16
## # tidx                0.508      0.032  15.771 < 2.2e-16
## # period_post:tidx    1.069      0.156   6.845 < 2.2e-16


## ----ch15-aa_ar1slop_p3-noeval, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## # Calculate 95% confidence interval
## tibble(bind_rows(sig_test_aaslope[,"Estimate"] -
##                    qnorm(0.975)*sig_test_aaslope[,"Std. Error"],
##        sig_test_aaslope[,"Estimate"] +
##                    qnorm(0.975)*sig_test_aaslope[,"Std. Error"])) |>
##   t() |>
##   `colnames<-`(c("2.5 %",    "97.5 %"))
## #                         2.5 %      97.5 %
## # ar1                 0.4373278   0.7261849
## # intercept          97.5537890 104.2907077
## # period_post      -121.4377790 -59.2051339
## # tidx                0.4450559   0.5713761
## # period_post:tidx    0.7628334   1.3750001

