## ----ch14-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
       ,"fable"
       ,"fabletools"
       ,"feasts"
       ,"Mcomp"
       ,"rsample"
       ,"patchwork"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata"
       ,"WDI"
       ,"kableExtra"
      )

conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
  ,dplyr::lag()
)

theme_set(theme_bw())


## ----ch14-var-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
gdp_data <- WDI::WDI(indicator = 'NY.GDP.PCAP.KD', 
                country = c('AU', 'US'), 
                start = "1960", end = "2022") |> 
  as_tibble() |> 
  select(country, year, GDP = 'NY.GDP.PCAP.KD')


## ----ch14-var-1a, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
GDP_norm <- gdp_data |> 
  left_join(gdp_data |> 
              filter(year == max(year)) |> 
              rename(gdp2022 = GDP) |>
              select(-year)) |> 
  mutate(GDP_index = (GDP/gdp2022)*100) |> 
  select(-gdp2022) |> 
  as_tsibble(key = country, index = year) 


## ----ch14-var-1b, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="GDP (normalized) for Australia and United States.", fig.width=8,fig.height=4----
GDP_norm |> 
  group_by(country) |> 
  autoplot(.vars = GDP_index, col = "black") +
  facet_wrap(country~., nrow = 2) +
  theme(legend.position = "none")


## ----ch14-var-2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## GDP_norm |>
##   features(GDP_index,
##            list(unitroot_kpss,
##                 unitroot_ndiffs))
## # # A tibble: 2 × 4
## #   country       kpss_stat kpss_pvalue ndiffs
## #   <chr>             <dbl>       <dbl>  <int>
## # 1 Australia          1.66        0.01      1
## # 2 United States      1.68        0.01      1


## ----ch14-var-3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
GDP_change <- GDP_norm |> 
  group_by(country) |> 
  mutate(GDP_lag = lag(GDP_index)) |> 
  mutate(GDP_growth = ((GDP_index/GDP_lag) - 1)*100,
         GDP_diff = c(NA, diff(GDP_index))) |> 
  ungroup() |> 
  as_tsibble(key = country, index = year) 


## ----ch14-var-3a, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## GDP_change |>
##   features(GDP_growth,
##            list(unitroot_kpss,
##                 unitroot_ndiffs))
## # # A tibble: 2 × 4
## #   country       kpss_stat kpss_pvalue ndiffs
## #   <chr>             <dbl>       <dbl>  <int>
## # 1 Australia         0.260      0.1         0
## # 2 United States     0.353      0.0976      0


## ----ch14-var-4, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Cross correlation plot for growth in GDP.", fig.width=10,fig.height=8----
gdp_YoY_growth <- GDP_change |> 
  autoplot(.vars = GDP_growth, col = "black") +
  facet_wrap(country~., nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  labs(y = "Growth", x = "Year",
       title = "Time plot of year-on-year growth in GDP") +
  theme(legend.position = "none") 

ccf_plt_growth <- GDP_change |> 
  select(GDP_growth) |> 
  pivot_wider(names_from = country, values_from = GDP_growth) |> 
  CCF() |> 
  autoplot() +
  labs(y = "Correlation", x = "Lag",
       title = "Cross correlation plot") +
  theme_bw()
# Plot as patchwork
gdp_YoY_growth / ccf_plt_growth



## ----ch14-var-5, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
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
# # A mable: 1 x 2
#           var_aicc          var_bic
#            <model>          <model>
# 1 <VAR(1) w/ mean> <VAR(1) w/ mean>


## ----ch14-var-6, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Autocorreation in the residual for VAR model by alternative information criteria.", fig.width=8,fig.height=6----
var_fit |>
  augment() |>
  ACF(.innov) |>
  autoplot()


## ----ch14-var-7, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## coef(var_fit) |>
##   filter(.model == "var_aicc") |>
##   select(.response, term, estimate)
## 
## # # A tibble: 6 × 3
## #   .response     term                 estimate
## #   <chr>         <chr>                   <dbl>
## # 1 Australia     lag(Australia,1)       0.0716
## # 2 Australia     lag(United States,1)   0.401
## # 3 Australia     constant               0.928
## # 4 United States lag(Australia,1)      -0.0815
## # 5 United States lag(United States,1)   0.176
## # 6 United States constant               1.81
## 




## ----ch14-var-8-show, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
varmat <- GDP_growth_dt |>
  as_tibble() |> 
  na.omit() |>
  select(-year) |>
  as.matrix()

varfit <- vars::VAR(varmat, p = 1) # `VAR()` from package `vars`
OzCausesUS <- vars::causality(varfit, cause = "Australia")$Granger
USCausesOZ <- vars::causality(varfit, cause = "United.States")$Granger

causality_test_rs <- as_tibble(cbind(Hypothesis = c(OzCausesUS$method, 
                               USCausesOZ$method), 
                `p-value` = round(c(OzCausesUS$p.value, 
                                    USCausesOZ$p.value),2)))


## ----ch14-var-8a-show, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## causality_test_rs
## # # A tibble: 2 × 2
## #   Hypothesis                                   `p-value`
## #   <chr>                                         <chr>
## # 1 Australia do not Granger-cause United States  0.62
## # 2 United.States do not Granger-cause Australia  0


## ----ch14-var-9, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
irf_fit_us_2_oz <- vars::irf(varfit,
                     impulse = "United.States",
                     response = "Australia",
                     n.ahead = 10, 
                     ortho = TRUE)

irfp_us2oz <- as_tibble(cbind(irf = unlist(irf_fit_us_2_oz$irf), 
                lower = unlist(irf_fit_us_2_oz$Lower), 
                upper = unlist(irf_fit_us_2_oz$Upper))) |> 
  mutate(period = 0:(n() - 1)) |>
  ggplot(aes(x = period, y = irf)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.1) +
  theme_bw() +
  labs(x = "Year", y = "Response in Australia",
       title = "Orthogonal impulse response from United States")


## ----ch14-var-10, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='asis', fig.cap="Orthogonal impulse response by countries.", fig.width=8,fig.height=6----

irf_fit_oz_2_us <- vars::irf(varfit,
                             impulse = "Australia", 
                             response =  "United.States",
                             n.ahead = 10, 
                             ortho = TRUE)
irfp_oz2us <- as_tibble(cbind(irf = unlist(irf_fit_oz_2_us$irf), 
                              lower = unlist(irf_fit_oz_2_us$Lower), 
                              upper = unlist(irf_fit_oz_2_us$Upper))) |> 
  mutate(period = 0:(n() - 1)) |>
  ggplot(aes(x = period, y = irf)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.1) +
  theme_bw() +
  labs(x = "Year", y = "Response in United States",
       title = "Orthogonal impulse response from Australia")

irfp_us2oz / irfp_oz2us



## ----ch14-var-11, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Forecasts from VAR model.", fig.width=8,fig.height=4----
var_fit |>
    select(var_aicc) |>
    forecast(h = 8) |>
    autoplot(GDP_growth_dt)




## ----ch14-hgts-2-noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
retail_sel <- aus_retail |>
  filter(Month >= yearmonth("2008 Jan") ) |> 
  filter(State %in% c("Queensland", "Victoria")) |> 
  filter(Industry %in%
           c("Clothing retailing", "Food retailing",
             "Other recreational goods retailing" )) |> 
  aggregate_key((State/Industry), Turnover = sum(Turnover))

fit_hfm <- retail_sel |>
  filter(Month <= yearmonth("2016 Dec") ) |> 
  model(base = ETS(Turnover)) |>
  reconcile(
    td_hp =  top_down(base, method = c("average_proportions")),
    bu = bottom_up(base),
    mo = middle_out(base, 1),
    ols = min_trace(base, method = "ols"),
    mint_cov = min_trace(base, method = "mint_cov")
  )

hts_fcast <- fit_hfm |> forecast(h = "2 years")


## ----ch14-hgts-2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
retail_sel <- aus_retail |>
  filter(Month >= yearmonth("2008 Jan") ) |> 
  filter(State %in% c("Queensland", "Victoria")) |> 
  filter(Industry %in%
           c("Clothing retailing", "Food retailing",
             "Other recreational goods retailing" )) |> 
  aggregate_key((State/Industry), Turnover = sum(Turnover))


## ----ch14-hgts-3, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='asis', fig.cap="Time plot of hierarchical retail turnonver data.", fig.width=8,fig.height=6----
retail_sel |> 
  autoplot(col = "black") + 
  facet_wrap(State ~ Industry, scales ="free_y") + 
  theme(legend.position = "none")


## ----ch14-hgts-4, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
fit_hfm <- retail_sel |>
  filter(Month <= yearmonth("2016 Dec") ) |> 
  model(base = ETS(Turnover)) |>
  reconcile(td_hp =  top_down(base, 
                              method = c("average_proportions")))


## ----ch14-hgts-5, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Top-down forecasts of retail turnover.", fig.width=8,fig.height=6----
fit_hfm |> forecast(h = "2 years") |> 
  filter(.model %in% c( "base","td_hp")) |> 
    autoplot( retail_sel |>
                  filter(Month >= yearmonth("2012 Jan")),
              level = NULL ) +
    labs(y = "Retail turnover in $Million AUD") +
    facet_wrap(State ~ Industry, scales = "free_y") +
  theme(legend.position = "bottom")


## ----ch14-hgts-6, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
fit_hfm <- retail_sel |>
  filter(Month <= yearmonth("2016 Dec") ) |> 
  model(base = ETS(Turnover)) |>
  reconcile(bu = bottom_up(base))


## ----ch14-hgts-7, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Bottom-up forecasts of retail turnover.", fig.width=8,fig.height=6----
fit_hfm |> forecast(h = "2 years") |> 
  filter(.model %in% c( "base","bu")) |> 
    autoplot( retail_sel |>
                  filter(Month >= yearmonth("2012 Jan")),
              level = NULL ) +
    labs(y = "Retail turnover in $Million AUD") +
    facet_wrap(State ~ Industry, scales = "free_y") +
  theme(legend.position = "bottom")


## ----ch14-hgts-8, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
fit_hfm <- retail_sel |>
  filter(Month <= yearmonth("2016 Dec") ) |> 
  model(base = ETS(Turnover)) |>
  reconcile(mo = middle_out(base, 1))


## ----ch14-hgts-9, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Middle-out forecasts of retail turnover.", fig.width=8,fig.height=6----
fit_hfm |> forecast(h = "2 years") |> 
  filter(.model %in% c( "base","mo")) |> 
    autoplot( retail_sel |>
                  filter(Month >= yearmonth("2012 Jan")),
              level = NULL ) +
    labs(y = "Retail turnover in $Million AUD") +
    facet_wrap(State ~ Industry, scales = "free_y") +
  theme(legend.position = "bottom")


## ----ch14-hgts-10, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
fit_hfm <- retail_sel |>
  filter(Month <= yearmonth("2016 Dec") ) |> 
  model(base = ETS(Turnover)) |>
  reconcile(ols = min_trace(base, method = "ols"),
            mint_cov = min_trace(base, method = "mint_cov"))


## ----ch14-hgts-11, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='asis', fig.cap="Optimal reconciliation forecasts of retail turnover.", fig.width=8,fig.height=6----
fit_hfm |> forecast(h = "2 years") |> 
  filter(.model %in% c( "base","ols", "mint_cov")) |> 
    autoplot( retail_sel |>
                  filter(Month >= yearmonth("2012 Jan")),
              level = NULL ) +
    labs(y = "Retail turnover in $Million AUD") +
    facet_wrap(State ~ Industry, scales = "free_y") +
  theme(legend.position = "bottom")


## ----ch14-hgts-12, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny'----
fore_accu <- fit_hfm |> 
  forecast(h = "2 years") |> 
  accuracy(data = retail_sel |> 
             filter(Month >= yearmonth("2012 Jan"))) |> 
  filter(.model != "base") |> 
  select(.model, State, Industry, MASE) |> 
  pivot_wider(names_from = .model, values_from = MASE) %>%
  `colnames<-`(c("State", "Industry", "Bottom-up",
                "Tr.min. (cov)", "Middle-out", "OLS",
                "Top-down"))
kable(fore_accu, digits = 3)

