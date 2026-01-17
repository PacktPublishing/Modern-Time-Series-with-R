#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"dataseries"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"here"
       ,"readr"
       ,"seasonal"
       ,"slider"
       ,"tidyverse"
       ,"tsibble")

#manage conflicts
conflicts_prefer(
  dplyr::filter,
  fabletools::components )







# Set the seed for reproducibility
set.seed(3302)
visitors <- tibble(
  visit_date = seq(as.Date("2021-01-01"),
                 as.Date("2022-12-31"), 
                 by = "1 day")) |> 
  mutate(visitor = rpois(n = n(), lambda = 950)) |> 
  as_tsibble() |> 
  group_by_key() |> 
  index_by(year_month = ~ yearmonth(.) ) |> 
  summarize(total_visitor = sum(visitor),
             avg_visitor = mean(visitor))

## # save data to share via git repo
## write_csv(visitors,
##           here("data", "data_chap7", "visitors.csv"))

visitors |> 
   pivot_longer(cols = c("total_visitor", "avg_visitor"),
                names_to = "vars",
                values_to = "values") |> 
  mutate(vars = factor(vars, 
                       levels = c("total_visitor","avg_visitor"))) |> 
  ggplot(aes(x = year_month,y = values)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars~., scales = "free_y", ncol = 1) +
  scale_x_yearmonth(date_breaks = "1 month",
                    date_labels = "%b\n%y",
                    expand = expansion(mult = c(0.025,0.025))) +
  theme(axis.text.x = element_text(size = 9)) +
  labs(x = "",
       y = "")



set.seed(3302)
days_in_sim <- 730
clicks_data <- tibble(
  visit_date = seq(as.Date("2021-01-01"), 
                   as.Date("2022-12-31"),
                   by = "1 day"),
  stable_clicks = rpois(n = days_in_sim, lambda = 100),
  base_visitors = rpois(n = days_in_sim, lambda = 5000)) |> 
  # introduce annual population growth
  mutate(
    date_index = as.numeric(visit_date - min(visit_date)),
    raw_visits = base_visitors * (1 + 0.2 * (date_index/365))
    ) |> 
  mutate(
    raw_clicks = stable_clicks * (1 + 0.2 * (date_index/365)),
    pop_adjusted_clicks = raw_clicks/raw_visits * mean(raw_visits)) |> 
  as_tsibble() |> 
  index_by(year_month = ~ yearmonth(.)) |> 
  summarize(
    unadjusted_clicks = mean(raw_clicks),
    adjusted_clicks = mean(pop_adjusted_clicks)
  )




clicks_data |> 
  pivot_longer(cols = c(unadjusted_clicks, adjusted_clicks),
               names_to = "series",
               values_to = "clicks") |> 
  ggplot(aes(x = year_month, 
             y = clicks, 
             linetype = series)) +
  geom_line(lwd = 1.1) +
  scale_x_yearmonth(date_breaks = "1 month",
                    date_labels = "%b\n%y",
                    expand = expansion(mult = c(0.025,0.025))) +
  labs(x = "" ,y = "average monthly clicks") +
  theme(legend.position = "top")



swiss_employment <- cbind(
  male = ds("ch_comb_emp.ttl.m.tot", "ts")
  ,female = ds("ch_comb_emp.ttl.f.tot","ts")) |>
  as_tsibble()

# # save data to share via git repo
# write_csv(swiss_employment, here("data", "data_chap7","swiss_employment.csv"))


# # load data 
# swiss_employment <- read_csv(here("data", "data_chap7","swiss_employment.csv"))



swiss_employment <- cbind(
  male = ds("ch_comb_emp.ttl.m.tot", "ts")
  ,female = ds("ch_comb_emp.ttl.f.tot","ts")) |> 
  as_tsibble() |> 
  filter_index("2000 Q1" ~ "2019 Q4") |> 
  index_by(year = ~ year(.) ) |> 
  group_by_key() |> 
  summarize(employee = sum(value)) |> 
  group_by_key() |> 
  mutate(employee_base_2000  =  round(100*employee/employee[1]))



swiss_employment |> 
  pivot_longer(cols = c(employee,employee_base_2000)
               ,names_to = "vars"
               ,values_to = "values") |> 
  ggplot(aes(x = year,y = values,linetype = key)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(vars~., scales = "free_y",ncol = 1) +
  labs(x = "", y = "") +
  theme(legend.position = "top")



indian_tourism <- ds("ch_fso_hesta.62.1", "ts") |>
  as_tsibble() |>
  filter_index("2005 Jan" ~ "2019 Dec") |>
  rename(arrivals = value) |>
  mutate(log_arrivals = log(arrivals))


## write_csv(indian_tourism,
##           here("data", "data_chap7", "indian_tourism.csv"))

# # reload from the local saved file
# indian_tourism <- read_csv(
#           here("data", "data_chap7", "indian_tourism.csv"))

# indian_tourism <- indian_tourism |> 
#   mutate(index = yearmonth(index)) |> 
#   as_tsibble(index = index) 


indian_tourism |> 
  pivot_longer(cols = c(arrivals, log_arrivals)
               ,names_to = "measure"
               ,values_to = "values") |> 
  mutate(vars = factor(measure, levels = c("arrivals","log_arrivals"))) |> 
  ggplot(aes(x = index, y = values)) + 
  geom_line(lwd= 0.9, color = "black") +
  facet_wrap(measure~., scales = "free_y",ncol = 1) +
  labs(x = ""
      ,y = "# arrivals in 1000's") +
  theme(legend.position = "none")


lambda_auto_selection <- feasts::guerrero(indian_tourism$arrivals)


round(lambda_auto_selection,1)
## 
## lambda_guerrero
##            0.4

indian_tourism_bc <- indian_tourism |> 
  mutate(
 `lambda 0(manual)` = box_cox(arrivals,lambda = 0),
 `lambda 0.7(manual)` = box_cox(arrivals,lambda = 0.7),
 `lambda 0.4(auto)` = box_cox(arrivals,
         lambda = feasts::guerrero(arrivals))) 


indian_tourism_bc |> 
  pivot_longer(cols = c(arrivals, 
                      `lambda 0(manual)`,
                      `lambda 0.7(manual)`,
                      `lambda 0.4(auto)` ),
                names_to = "lambda_value",
                values_to = "value") |> 
  ggplot(aes(x = index,y = value)) +
  geom_line() +
  facet_wrap(~lambda_value,
             ncol = 2, 
             scales = "free_y") +
  labs(x = "", y = "") 



abs_data <- read_rds(here("data", "data_chap7","abs_retail.rds"))

takeaway_turnover  <- abs_data  |>
  filter(state == "Total (State)", 
         industry == "Takeaway food services") |>
  filter(year(month) >= 2015, year(month) <= 2022 ) |>
  select(month, turnover)


takeaway_turnover |>
  ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  geom_point() +
  labs(x = "" ,y = "Turnover($M)")




turnover_dcomp <- takeaway_turnover |>
  model(stl = STL(turnover))


turnover_dcomp
## 
## # A mable: 1 x 1
##       stl
##   <model>
## 1   <STL>

components(turnover_dcomp)


## 
## # A dable: 96 x 7 [1M]
## # Key:     .model [1]
## # :        turnover = trend + season_year + remainder
##    .model    month turnover trend season_year remainder season_adjust
##    <chr>     <mth>    <dbl> <dbl>       <dbl>     <dbl>         <dbl>
##  1 stl    2015 Jan    1378  1354.        4.13    20.4           1374.
##  2 stl    2015 Feb    1203. 1357.     -163.       8.97          1366.
##  3 stl    2015 Mar    1328. 1360.      -27.2     -4.74          1355.
##  4 stl    2015 Apr    1307. 1363.      -67.4     11.5           1374.
##  5 stl    2015 May    1314. 1366.      -52.6     -0.341         1366.
##  6 stl    2015 Jun    1312. 1370.      -57.5     -0.724         1370.
##  7 stl    2015 Jul    1410. 1374.       34.6      1.60          1376.
##  8 stl    2015 Aug    1373. 1379.       19.5    -25.2           1353.
##  9 stl    2015 Sep    1388. 1383.       21.6    -16.9           1366.
## 10 stl    2015 Oct    1432  1388.       62.2    -17.8           1370.
## # ℹ 86 more rows


components(turnover_dcomp) |>
  autoplot(linewidth = 1) +
  labs(x = "") 



components(turnover_dcomp) |>
  as_tsibble() |>
  autoplot(turnover, colour = "gray", linewidth = 1.2) +
  geom_line(aes(y = trend), colour = "black", linewidth = 1.2) +
  labs(x = "" ,y = "Turnover")




smooth_turnover <- takeaway_turnover |>
  index_by(year = ~year(.)) |>
  summarise(turnover = sum(turnover)) |>
  mutate(
    `3-MA` = slide_dbl(
      .x = turnover,
      .f = mean,
      .before = 1,
      .after = 1,
      .complete = FALSE),
    `5-MA` = slide_dbl(
      .x = turnover,
      .f = mean,
      .before = 2,
      .after = 2,
      .complete = FALSE ))



smooth_turnover |>
 pivot_longer(cols = c(`3-MA`, `5-MA`),
              names_to = "smoother",
              values_to = "smoothed") |>
  mutate(smmother = factor(smoother, levels = c("3-MA", "5-MA"))) |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = turnover, color = "original", linetype = "original"),
            linewidth = 1.1) + 
  geom_line(aes(y = smoothed, colour = smoother, linetype = smoother),
            linewidth = 0.9) +
  scale_color_manual(name = "series",
                     values = c("original" = "gray",
                            "3-MA" = "dodgerblue4",
                            "5-MA" = "purple")) + 
  scale_linetype_manual(name = "series",
                        values = c("original" = "solid",
                                   "3-MA" = "solid",
                                   "5-MA" = "dotted")) +
  labs(x = "" ,y = "Turnover") +
  theme( legend.position = "top")



turnover_monthly_ma <- takeaway_turnover |>
  mutate(
    `12-MA` = slide_dbl(
      .x = turnover,
      .f = mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
      ),
    `2x12-MA` = slide_dbl(
      .x = `12-MA`,
      .f = mean,
      .before = 1,
      .after = 0,
      .complete = TRUE))


turnover_monthly_ma |>
  ggplot(aes(x = month)) +
  geom_line(aes(y = turnover, 
                color = "original"),
            linewidth = 0.8 ) +
  geom_line(aes(y = `2x12-MA`, 
            color = "2x12-MA"),
            linewidth = 0.8 )+
  scale_color_manual(name = "series",
                     values = c("original" = "gray",
                            "2x12-MA" = "black")) + 
  labs( x = "", y = "" ) +
  theme(legend.position = "top")





takeaway_turnover |>
  model(classical_decomposition(turnover, type = "additive"))  |>
  components() |>
   autoplot(linewidth = 1) +
  labs(x = "")



turnover_model_decomp <- takeaway_turnover |>
  model(x11 = X_13ARIMA_SEATS(turnover ~ x11()),
        seats = X_13ARIMA_SEATS(turnover ~ seats()))





turnover_model_decomp |>
  select(x11) |>
  components() |>
   autoplot(linewidth = 1) +
  labs(x = "")


turnover_stl <- takeaway_turnover |>
  model(stl_dcomp = 
          STL(turnover ~ trend(window = 12) +
                        season(window="periodic"),
                        robust = TRUE  ))
