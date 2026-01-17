#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"patchwork"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
       ,"vars"
       ,"WDI"
       ,"vars" )

conflicts_prefer(
  fabletools::accuracy
  ,fabletools::forecast
  ,dplyr::filter
  ,dplyr::select
  ,dplyr::lag
  ,fable::VAR
  ,feasts::unitroot_kpss
)


# download for the first time ,save as csv, load for subsequent code run
gdp_data <- WDI(indicator = 'NY.GDP.PCAP.KD',
                country = c('AU', 'US'),
                start = "1960", end = "2022") |>
  as_tibble() |>
  select(country, year, GDP = 'NY.GDP.PCAP.KD')

write_csv(gdp_data, here("data","data_chap14", "gdp_data.csv"))


# load again 
gdp_data <- read_csv(here("data","data_chap14", "gdp_data.csv")) |> 
  tibble() 


# gdp_data <- WDI(
#   indicator = 'NY.GDP.PCAP.KD',
#   country = c('AU', 'US'),
#   start = "1960", end = "2022" ) |>
#   tibble() |>
#   select(country, year, GDP = 'NY.GDP.PCAP.KD')

GDP_norm <- gdp_data |> 
  left_join(gdp_data |> 
              filter(year == max(year)) |> 
              rename(gdp2022 = GDP) |>
              select(-year)) |> 
  mutate(GDP_index = (GDP/gdp2022)*100) |> 
  select(-gdp2022) |> 
  as_tsibble(key = country, index = year) 

GDP_norm |> 
  group_by(country) |> 
  autoplot(.vars = GDP_index, col = "black") +
  facet_wrap(country~., nrow = 2) +
  labs(x = "") +
  theme(legend.position = "none")

# unitroot tests
GDP_norm |>
  features(GDP_index,
           list(unitroot_kpss,
                unitroot_ndiffs))

# GDP growth
GDP_change <- GDP_norm |>
  group_by(country) |>
  mutate(GDP_growth = ((GDP_index/lag(GDP_index) - 1)*100)) |>
  ungroup() |>
  as_tsibble(key = country, index = year)

# unitroot tests on GDP growth
GDP_change |>
  features(GDP_growth,
           list(unitroot_kpss,
                unitroot_ndiffs))

# tiem plot of stationary GDP growth
GDP_change |>
  group_by(country) |>
  autoplot(.vars = GDP_growth, col = "black") +
  facet_wrap(country~., nrow = 2) +
  labs(x = "")+
  theme(legend.position = "none")


# CCF
ccf_plt_growth <- GDP_change |>
  select(GDP_growth) |>
  pivot_wider(names_from = country, values_from = GDP_growth) |>
  CCF() |>
  autoplot() +
  labs(y = "Correlation", x = "Lag",
       title = "Cross correlation plot") +
  theme_bw()

ccf_plt_growth


# Create dataset for fitting VAR model
GDP_growth_dt <- GDP_change |>
  select(GDP_growth) |>
  pivot_wider(names_from = country, values_from = GDP_growth) |>
  as_tsibble() |> na.omit()

var_fit <- GDP_growth_dt |>
  model(
    var_aicc = VAR(vars(Australia , `United States`), ic = "aicc"),
    var_bic = VAR(vars(Australia , `United States`), ic = "bic")
  )




## 
## # A tibble: 2 × 4
##   country       kpss_stat kpss_pvalue ndiffs
##   <chr>             <dbl>       <dbl>  <int>
## 1 Australia          1.66        0.01      1
## 2 United States      1.68        0.01      1





## # A tibble: 2 × 4
##   country       kpss_stat kpss_pvalue ndiffs
##   <chr>             <dbl>       <dbl>  <int>
## 1 Australia         0.258         0.1      0
## 2 United States     0.332         0.1      0



## 
## # A mable: 1 x 2
##           var_aicc          var_bic
##            <model>          <model>
## 1 <VAR(1) w/ mean> <VAR(1) w/ mean>

var_fit |>
 select(var_aicc)
|> report()
## 
## Series: Australia, United States
## Model: VAR(1) w/ mean
## 
## Coefficients for Australia:
##       lag(Australia,1)  lag(United States,1)  constant
##                 0.0791                0.4085    0.8971
## s.e.            0.1152                0.0951    0.3124
## 
## Coefficients for United States:
##       lag(Australia,1)  lag(United States,1)  constant
##                -0.0856                0.1878    1.8064
## s.e.            0.1602                0.1323    0.4348
## 
## Residual covariance matrix:
##               Australia United States
## Australia        2.2076        0.5304
## United States    0.5304        4.2746
## 
## log likelihood = -237.57
## AIC = 495.14	AICc = 499.54	BIC = 516.25
## 





varmat <- GDP_growth_dt |>
  as_tibble() |> 
  na.omit() |>
  select(-year) |>
  as.matrix()

varfit <- vars::VAR(varmat, p = 1) 
OzCausesUS <- vars::causality(varfit, cause = "Australia")$Granger
USCausesOZ <- vars::causality(varfit, cause = "United.States")$Granger

causality_test_rs <- as_tibble(
  cbind(Hypothesis = c(OzCausesUS$method, USCausesOZ$method), 
  `p-value` = round(c(OzCausesUS$p.value, USCausesOZ$p.value),2)))


causality_test_rs |> 
   kable(align = "lr",
        format = "pandoc",
        caption = "Granger causality test on the GDP growth")

# IRF: US -> Australia
irf_fit_us_2_oz <- vars::irf(varfit,
                     impulse = "United.States",
                     response = "Australia",
                     n.ahead = 10, 
                     ortho = TRUE)

# IRF: Australia -> US
irf_fit_oz_2_us <- vars::irf(varfit,
                             impulse = "Australia", 
                             response =  "United.States",
                             n.ahead = 10, 
                             ortho = TRUE)


tibble(period = 1:11, 
                  bind_cols(irf_fit_us_2_oz$irf, 
                       irf_fit_oz_2_us$irf) |> 
                round(3)) |> 
   kable(align = "lrr",
        format = "pandoc",
        col.names = c("Year", "USA -> Australia", "Australia -> USA"),
        caption = "Impulse response coefficeints of Australia and the USA GDP data")



# IRF: US -> Australia
irfp_us2oz <- as_tibble(
  cbind(irf = unlist(irf_fit_us_2_oz$irf), 
        lower = unlist(irf_fit_us_2_oz$Lower), 
        upper = unlist(irf_fit_us_2_oz$Upper))) |> 
  mutate(period = 0:(n() - 1)) |>
  ggplot(aes(x = period, y = irf)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.1) +
  scale_x_continuous(limits=c(0,10), breaks=seq(0,10,2))+
  theme_bw() +
  labs(x = "Year", y = "Response in Australia",
       title = "Orthogonal impulse response from United States")

# IRF: Australia -> US
irfp_oz2us <- as_tibble(
  cbind(irf = unlist(irf_fit_oz_2_us$irf), 
        lower = unlist(irf_fit_oz_2_us$Lower), 
        upper = unlist(irf_fit_oz_2_us$Upper))) |> 
  mutate(period = 0:(n() - 1)) |>
  ggplot(aes(x = period, y = irf)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.1) +
  scale_x_continuous(limits=c(0,10), breaks=seq(0,10,2))+
  theme_bw() +
  labs(x = "Year", y = "Response in United States",
       title = "Orthogonal impulse response from Australia")

irfp_us2oz / irfp_oz2us


var_fit |>
    select(var_aicc) |>
    forecast(h = 8) |>
    autoplot(GDP_growth_dt, level = 95, linewidth = 0.9) 



retail_sel <- aus_retail |>
  filter(Month >= yearmonth("2008 Jan") ) |> 
  filter(State %in% c("Queensland", "Victoria")) |> 
  filter(Industry %in%
           c("Clothing retailing", "Food retailing",
             "Other recreational goods retailing" )) |> 
  mutate(Industry  = case_when(
    Industry == "Clothing retailing" ~ "Clothing",
    Industry == "Food retailing" ~ "Food",
    Industry == "Other recreational goods retailing" ~ "Recreational",
    .default = Industry )) |> 
  rename(month = Month, 
         state = State, 
         industry = Industry, 
         turnover = Turnover) |> 
  aggregate_key((state/industry), turnover = sum(turnover)) 

## retail_sel
## 
## # A tsibble: 1,188 x 4 [1M]
## # Key:       state, industry [9]
##       month state        industry     turnover
##       <mth> <chr*>       <chr*>          <dbl>
##  1 2008 Jan <aggregated> <aggregated>    3799.
##  2 2008 Feb <aggregated> <aggregated>    3509.
##  3 2008 Mar <aggregated> <aggregated>    3778.
##  4 2008 Apr <aggregated> <aggregated>    3649.
##  5 2008 May <aggregated> <aggregated>    3762.
##  6 2008 Jun <aggregated> <aggregated>    3546.
##  7 2008 Jul <aggregated> <aggregated>    3843.
##  8 2008 Aug <aggregated> <aggregated>    3891.
##  9 2008 Sep <aggregated> <aggregated>    3812.
## 10 2008 Oct <aggregated> <aggregated>    4076.
## # ℹ 1,178 more rows




retail_sel_train <- retail_sel |>
  filter(month <= yearmonth("2016 Dec"))

retail_sel_test <- retail_sel |>
  filter(month >= yearmonth("2017 Jan"))


hfm_base_fit <- retail_sel_train |> 
  model(base = ETS(turnover)) 

hfm_td_fit <- hfm_base_fit |>
  reconcile(td_hp = top_down(base, 
            method = c("average_proportions")))

hfm_td_fcst <- hfm_td_fit |> 
   forecast(h = "2 years") 


 hfm_td_fit
## 
## # A mable: 9 x 4
## # Key:     state, industry [9]
##   state        industry              base td_hp
##   <chr*>       <chr*>             <model> <model>
## 1 Queensland   Clothing     <ETS(M,Ad,M)> <ETS(M,Ad,M)>
## 2 Queensland   Food         <ETS(M,Ad,M)> <ETS(M,Ad,M)>
## 3 Queensland   Recreational  <ETS(A,N,A)> <ETS(A,N,A)>
## 4 Queensland   <aggregated> <ETS(M,Ad,M)> <ETS(M,Ad,M)>
## 5 Victoria     Clothing     <ETS(M,Ad,M)> <ETS(M,Ad,M)>
## 6 Victoria     Food         <ETS(M,Ad,M)> <ETS(M,Ad,M)>
## 7 Victoria     Recreational  <ETS(A,A,A)> <ETS(A,A,A)>
## 8 Victoria     <aggregated> <ETS(M,Ad,M)> <ETS(M,Ad,M)>
## 9 <aggregated> <aggregated> <ETS(M,Ad,M)> <ETS(M,Ad,M)>


hfm_td_fcst  |> 
  filter(.model %in% c( "base","td_hp")) |> 
  autoplot(retail_sel |>
           filter(month >= yearmonth("2015 Jan")),
              level = NULL, linewidth = 1) +
    facet_wrap(state ~ industry, scales = "free_y") +
    scale_x_yearmonth(
      date_breaks = "1 year",
      date_labels = "%b\n%Y") +
  labs(x = "", y = "") +
  theme(legend.position = "top")

hfm_base_fit
## 
## # A mable: 9 x 3
## # Key:     state, industry [9]
##   state        industry              base
##   <chr*>       <chr*>             <model>
## 1 Queensland   Clothing     <ETS(M,Ad,M)>
## 2 Queensland   Food         <ETS(M,Ad,M)>
## 3 Queensland   Recreational  <ETS(A,N,A)>
## 4 Queensland   <aggregated> <ETS(M,Ad,M)>
## 5 Victoria     Clothing     <ETS(M,Ad,M)>
## 6 Victoria     Food         <ETS(M,Ad,M)>
## 7 Victoria     Recreational  <ETS(A,A,A)>
## 8 Victoria     <aggregated> <ETS(M,Ad,M)>
## 9 <aggregated> <aggregated> <ETS(M,Ad,M)>

hfm_bu_fit <- hfm_base_fit |> 
  reconcile(bu_hp = bottom_up(base))

hfm_bu_fcst <- hfm_bu_fit |> 
   forecast(h = "2 years") 


hfm_bu_fcst  |> 
  filter(.model %in% c( "base","bu_hp")) |> 
    autoplot( retail_sel |>
                  filter(month >= yearmonth("2015 Jan")),
              level = NULL, linewidth = 1) +
    scale_x_yearmonth(date_breaks = "1 year",
                      date_labels = "%b\n%Y") +
    facet_wrap(state ~ industry, scales = "free_y") +
    labs(x = "", y = "") +
    theme(legend.position = "top")


hfm_base_fit |> slice(c(4,8))
## 
## # A mable: 2 x 3
## # Key:     state, industry [2]
##   state      industry              base
##   <chr*>     <chr*>             <model>
## 1 Queensland <aggregated> <ETS(M,Ad,M)>
## 2 Victoria   <aggregated> <ETS(M,Ad,M)>

hfm_mo_fit <- hfm_base_fit |> 
  reconcile(mo_hp = middle_out(base, 1))

hfm_mo_fcst <- hfm_mo_fit |> 
   forecast(h = "2 years") 




hfm_mint_fit <- retail_sel |>
  filter(month <= yearmonth("2016 Dec") ) |> 
  model(base = ETS(turnover)) |>
  reconcile(ols = min_trace(base, method = "ols"),
            mint_cov = min_trace(base, method = "mint_cov"))

hfm_mint_fcst <- hfm_mint_fit |> 
   forecast(h = "2 years") 


hfm_mint_fcst  |> 
  filter(.model %in% c( "base","ols", "mint_cov")) |> 
    autoplot( retail_sel |>
                  filter(month >= yearmonth("2015 Jan")),
              level = NULL, linewidth = 1) +
    scale_x_yearmonth(date_breaks = "1 year",
                      date_labels = "%b\n%Y") +
    facet_wrap(state ~ industry, scales = "free_y") +
    labs(x= "", y = "") +
    theme(legend.position = "top")




fit_hfm <- retail_sel_train |>
   model(base = ETS(turnover)) |>
  reconcile(
    td_hp =  top_down(base, method = c("average_proportions")),
    bu = bottom_up(base),
    mo = middle_out(base, 1),
    ols = min_trace(base, method = "ols"),
    mint_cov = min_trace(base, method = "mint_cov")
  )

acc_hfm <- fit_hfm |> 
  forecast(h = "2 years") |> 
  accuracy(data = retail_sel |> 
             filter(month >= yearmonth("2012 Jan"))) 


fore_accu <- acc_hfm |> 
  filter(.model != "base") |> 
  select(.model, state, industry, MASE) |> 
  pivot_wider(names_from = .model, values_from = MASE) %>%
  `colnames<-`(c("State", "Industry", "Bottom-up",
                "Tr.min. (cov)", "Middle-out", "OLS",
                "Top-down")) |> 
  select(`State`, `Industry`, `Top-down`, `Bottom-up`,
         `Middle-out`, `OLS`, `Tr.min. (cov)`)

fore_accu$Industry[c(4,8,9)] <- "All industries"
fore_accu$State[9] <- "All states"
