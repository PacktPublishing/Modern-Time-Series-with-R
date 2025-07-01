## ----ch16-packages, eval=TRUE, warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load(
  "conflicted"
  , "tidyverse"
  , "ggplot"
  , "bfast"
  , "changepoint"
  , "Rbeast"
)
conflicts_prefer(
  fabletools::accuracy()
  ,fabletools::forecast()
  ,dplyr::filter()
)

theme_set(theme_bw())


## ----ch16-bfast_apply_1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
bfast_Nile <- bfast(Nile, 
                      h = 0.15,
                      season = "none",
                      max.iter = 10) 
bfast_Nile


## ----ch16-bfast_result_1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
bfast_Nile$Magnitude


## ----ch16-bfast-nonseas-plot-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny' , results='asis', fig.cap="Changepoint detection of annual Nile flow by BFAST", fig.width=8,fig.height=5----
# plot(bfast_Nile)
invisible(plot(bfast_Nile, main = ""))



## ----ch16-bfast_apply_2_run, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', results='hide'----
bfast_ap <- bfast(log(AirPassengers), 
                  h = round(frequency(AirPassengers)/
                             length(AirPassengers),3),
                  season = "harmonic",
                  max.iter = 10) 
bfast_ap


## ----ch16-bfast-plot-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Changepoints detection by BFAST method on seasoanl AirPassenger data", fig.width=8,fig.height=6----
invisible(plot(bfast_ap, main = ""))


## ----ch16-pelt_apply_1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
pelt_Nile <- cpt.meanvar(Nile, 
                         method = "PELT",  
                         penalty = "BIC", 
                         minseglen = length(Nile)*0.15)
pelt_Nile


## ----ch16-pelt_apply_1a, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
attributes(pelt_Nile)$param.est$mean


## ----ch16-pelt-plot-1a, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## plot(pelt_Nile)


## ----ch16-pelt_apply_2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
pelt_ap <- cpt.meanvar(log(AirPassengers), 
                       method = "PELT",  
                       penalty = "BIC",
                       minseglen = 12)
pelt_ap


## ----ch16-beast_apply_1_noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
beast_nile <- beast(Nile, season = "none",
                      tcp.minmax = c(0, 10))


## ----ch16-beast_apply_1, eval=FALSE, cache=FALSE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## beast_nile <- beast(Nile, season = "none",
##                       tcp.minmax = c(0, 10))
## # beast_nile


## ----ch16-beast_apply_1b, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## 
## # modal number of changepoints
## beast_nile$trend$ncp_mode
## 
## # [1] 1
## 
## # location of changepoints
## beast_nile$trend$cp[1:beast_nile$trend$ncp_mode]
## 
## # [1] 1899
## 


## ----ch16-beast-plot-1, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Bayesian estimation of trend changepoints in annual flow of the river Nile", fig.width=8,fig.height=6----
plot(beast_nile, main = "")


## ----ch16-beast_apply_1z, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## beast_nile$trend$cpCI[1,]


## ----ch16-beast_apply_1a, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
beast_nile$trend$cpAbruptChange[beast_nile$trend$ncp_mode]

# [1] -182.8789


## ----ch16-beast_apply_2_noshow, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny', results='hide'----
beast_ap <- beast(log(AirPassengers), 
                  season = "harmonic",
                    tcp.minmax = c(0, 10))


## ----ch16-beast_apply_2, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
## beast_ap <- beast(log(AirPassengers),
##                   season = "harmonic",
##                     tcp.minmax = c(0, 10))
## # beast_ap


## ----ch16-beast-plot-2, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE, size='tiny' , results='asis', fig.cap="Bayesian estimation of trend and seasonal changepoints in AirPassengers data", fig.width=8,fig.height=8----
plot(beast_ap, main = "")

