## ----ch7-pacakges, eval=FALSE, cache=TRUE,warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("dataseries"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"readr"
       ,"seasonal"
       ,"slider"
       ,"tidyverse"
       ,"tsibble")


theme_set(theme_bw())



## ----ch7-calendar-adjust-1

# Set the seed for reproducibility
set.seed(3302)

visitors <- tibble(
  visit_date=seq(as.Date("2021-01-01"),
                 as.Date("2022-12-31"), 
                 by = "1 day")) |> 
  mutate(visitor = rpois(n=n(), lambda=950)) |> 
  as_tsibble() |> 
  group_by_key() |> 
  index_by(year_month = ~ yearmonth(.) ) |> 
  summarize(total_visitor = sum(visitor),
             avg_visitor = mean(visitor))


## ----ch7-calendar-adjust-2
# Visualize the synthetic time series
visitors |> 
   pivot_longer(cols=c("total_visitor", "avg_visitor"),
                names_to="vars",
                values_to="values") |> 
  mutate(vars=factor(vars, levels=c("total_visitor","avg_visitor"))) |> 
  ggplot(aes(x=year_month,y=values)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars~., scales="free_y", ncol=1) +
  theme(axis.text=element_text(size=11),
        strip.text=element_text(size=11)) +
  labs(x="",
       y="# visitors")



## ----ch7-population-adjust-1

set.seed(3302)

ctr <- tibble(
  visit_date=seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by="1 day")) |> 
  mutate( visitor=rpois(n=n(), lambda=950)
          ,ctr=runif(n=n(), min=0.02, max=0.05)) |> 
  as_tsibble() |> 
  mutate(adjusted_ctr= 1000 * ctr / visitor) |> 
  group_by_key() |> 
  index_by(year_month=~ yearmonth(.) ) |> 
  summarize( ctr=mean(ctr)
             ,adjusted_ctr=mean(adjusted_ctr))


## ----ch7-population-adjust-2


ctr |> 
  ggplot(aes(x = year_month)) +
  geom_line(aes(y = ctr * 100), 
            linetype = "solid", 
            size = 1, 
            alpha = 0.7, 
            group = 1) +
  geom_line(aes(y = adjusted_ctr * 100), 
            linetype = "dashed", 
            size = 1, 
            alpha = 0.7, 
            group = 2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "CTR vs. Population-Adjusted CTR"
       ,x = ""
       ,y = "% CTR") +
  theme(axis.text = element_text(size = 12))



## ----ch7-base-adjust

swiss_employment <- cbind(
  male=dataseries::ds("ch_comb_emp.ttl.m.tot", "ts")
  ,female=dataseries::ds("ch_comb_emp.ttl.f.tot","ts")) |> 
  as_tsibble() |> 
  filter_index("2000 Q1" ~ "2019 Q4") |> 
  index_by(year=~ year(.) ) |> 
  group_by_key() |> 
  summarize(employee=sum(value)) |> 
  group_by_key() |> 
  mutate(employee_base_2000 = round(100*employee/employee[1]))

swiss_employment |> 
  pivot_longer(cols=c(employee,employee_base_2000)
               ,names_to="vars"
               ,values_to="values") |> 
  ggplot(aes(x=year,y=values,linetype=key)) +
  geom_line(linewidth=0.9)+
  facet_wrap(vars~., scales="free_y",ncol=1)+
  labs(title = "Switzerland female and male employee during 2000-2019"
       ,x=""
       , y="") +
  theme(axis.text=element_text(size=12)
        ,strip.text=element_text(size=12)
        ,legend.position="top")




## ----ch7-tourist-log


indian_tourism <- dataseries::ds("ch_fso_hesta.62.1", "ts") |> 
  as_tsibble() |> 
  filter_index("2005 Jan" ~ "2019 Dec") |> 
  rename(arrivals = value) |> 
  mutate(log_arrivals = log(arrivals))
  
indian_tourism |> 
  pivot_longer(cols=c(arrivals, log_arrivals)
               ,names_to = "vars"
               ,values_to = "values") |> 
  mutate(vars = factor(vars, levels=c("arrivals","log_arrivals"))) |> 
  autoplot(linewidth=0.9) +
  facet_wrap(vars~., scales="free_y",ncol=1) +
  labs(title = "Monthly tourist arrivals in India 2005-2019"
       ,x =""
       ,y="# arrivals in 1000's") +
  theme(axis.text=element_text(size=12)
       ,strip.text=element_text(size=12)
       ,legend.position="none")



## ----ch7-tourism-lambda-1

# auto selection 
lambda_auto_selection <- feasts::guerrero(indian_tourism$arrivals)

indian_tourism <- dataseries::ds("ch_fso_hesta.62.1", "ts") |> 
  as_tsibble() |> 
  filter_index("2005 Jan" ~ "2019 Dec") |> 
  rename(arrivals = value)  |>  
  mutate(
         lambda_0=box_cox(arrivals,lambda=0),
         lambda_0.7=box_cox(arrivals,lambda=0.7),
         lambda_auto=box_cox(arrivals,lambda=feasts::guerrero(arrivals))
         ) 


## ----ch7-tourism-lambda-2

indian_tourism |> 
  pivot_longer(cols=c(arrivals, 
                      lambda_0,
                      lambda_0.7,
                      lambda_auto),
                names_to="lambda_value",
                values_to="data") |> 
  ggplot(aes(x=index,y=data)) +
  geom_line() +
  facet_wrap(~lambda_value,
             ncol = 2, 
             scales = "free_y") + 
  labs(title = latex2exp::TeX(paste0("Box-Cox transformations with $\\lambda=0$,$\\lambda=0.7$ and $\\lambda=",round(lambda_auto_selection,1),"(auto selection)"))
       ,x = ""
       ,y = "") +
  theme(axis.text=element_text(size=12)
       ,strip.text=element_text(size=12)
       ,legend.position="none")



## ----ch7-takeawayretail-plot

abs_data <- read_rds("data/data_chap4/abs_retail.rds")

takeaway_turnover  <- abs_data  |>
  filter(state == "Total (State)", industry == "Takeaway food services") |>
  filter(year(month) >= 2015 , year(month) <= 2022 ) |>
  select(month, turnover)

takeaway_turnover |>
  ggplot(aes(x = month, y = turnover)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Australian takeaway food services turnover: 2015-2022",
       x = "Turnover($M)",
        y = "") +
  theme_bw()



## ----ch7-turnover-dcomp-mable-run

turnover_dcomp <- takeaway_turnover |>
  model(turnover_stl = STL(turnover))



## ----ch7-turnover-dcomp-mable
turnover_dcomp <- takeaway_turnover |>
  model(turnover_stl = STL(turnover))

turnover_dcomp



## ----ch7-turnover-dcomp-dable

components(turnover_dcomp)



## ----ch7-turnover-dcomp-plot
components(turnover_dcomp) |>
  autoplot(linewidth=1) +
  labs(x="") +
  theme(strip.text = element_text(size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))



## ----ch7-turnover-dcomp-actual-plot
components(turnover_dcomp) |>
  as_tsibble() |>
  autoplot(turnover, colour="gray", linewidth=1.2) +
  geom_line(aes(y=trend), colour = "black", linewidth=1.2) +
  labs(title = "Takeaway food turnover and estimated trend",
       y = "Turnover",
       x="") +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  theme_bw()



## ----ch7-turnover-ma-smoothing-plot
takeaway_turnover |>
  index_by(year = ~year(.)) |>
  summarise(turnover= sum(turnover)) |>
  mutate(`3_MA` = slide_dbl(.x = turnover,
                           .f = mean,
                           .before = 1,
                           .after = 1,
                           .complete = FALSE),
         `5_MA` = slide_dbl(.x = turnover,
                           .f = mean,
                           .before = 2,
                           .after = 2,
                           .complete = FALSE)) |>
 pivot_longer(cols=c(`3_MA`, `5_MA`),
              names_to = "metric",
              values_to = "values") |>
  ggplot(aes(x=year, y=turnover)) +
  geom_line(colour="black", linewidth=1.2) +
  geom_line(aes(x=year, y=values, linetype=metric), linewidth=1.2, colour="blue") +
  facet_grid(~metric) +
  labs(title = "Moving average smoothing of annual takeaway food turnover",
       x="") +
  theme(strip.text = element_text(size=12),
        legend.position = "none")



## ----ch7-turnover-ma-seasonal-plot
turnover_monthly_ma <- takeaway_turnover |>
  mutate(`12_MA` = slide_dbl(.x = turnover,
                           .f = mean,
                           .before = 5,
                           .after = 6,
                           .complete = TRUE),
         `2x12_MA` = slide_dbl(.x = `12_MA`,
                           .f = mean,
                           .before = 1,
                           .after = 0,
                           .complete =TRUE))


turnover_monthly_ma |>
  autoplot(turnover,  colour="gray", linewidth=1.2) +
  geom_line(aes(y = `2x12_MA`), linetype="dashed", linewidth=1.2) +
  labs(title = "Autralian takeaway food turnover",
       y="",
       x="") +
  theme(legend.position = "none")



## ----ch7-turnover-classicaldecomp-plot
takeaway_turnover |>
  model(classical_decomposition(turnover, type = "additive"))  |>
  components() |>
   autoplot(linewidth=1) +
  labs(x="") +
  theme(strip.text=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))



## ----ch7-turnover-modelbased
turnover_model_decomp <- takeaway_turnover |>
  model(x11=X_13ARIMA_SEATS(turnover ~ x11()),
        seats=X_13ARIMA_SEATS(turnover ~ seats()))



## ----ch7-turnover-x11decomp-plot
turnover_model_decomp |>
  select(x11) |>
  components() |>
   autoplot(linewidth=1) +
  labs(x = "") +
  theme(strip.text=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))


## ----ch7-turnover-stl-plot
turnover_stl <- takeaway_turnover |>
  model(stl_dcomp = STL(turnover ~ trend(window=11) +
                        season(window="periodic"),
                        robust=TRUE ))


