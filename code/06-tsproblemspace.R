## ----ch6-pacakges, eval=FALSE, cache=TRUE,warning=FALSE, message=FALSE, echo=TRUE, purl=TRUE----
#install.packages("pacman")
library(pacman)
p_load("dataseries"
       ,"tidyverse"
       ,"tsibble")


## ----ch6-trend-analysis-data-example

# Swiss retail sale index with 2015 base
swiss_retail_sale <- dataseries::ds("ch_fso_rtt.brut.n.noga0801", "ts") 
swiss_retail_sale_trend <- dataseries::ds("ch_fso_rtt.tr.n.noga0801", "ts")

swiss_retail <- cbind(sale_index = swiss_retail_sale, 
                      index_trend = swiss_retail_sale_trend) |> 
  as_tsibble()


swiss_retail |> 
filter(key == "sale_index") |> 
ggplot(aes(x=index,y=value)) +
geom_line() +
labs(title = "Swiss retail sale index with base (2015 = 100)",
       x = "",
       y = "Index") +
theme_bw() +
theme(axis.text=element_text(size=13),
      plot.title=element_text(size=13))



## ----ch6-underlying-trend-example

swiss_retail |> 
  filter(key %in% c("sale_index", "index_trend")) |> 
  mutate(key = factor(key, levels = c("sale_index", "index_trend"))) |> 
  autoplot(linewidth = 0.9) +
  facet_wrap(key~., ncol = 1, scales = "free_y") +
  labs(title = "Swiss retail sale index with base (2015 = 100)",
       x = "",
       y = "Index") +
  theme_bw() +
  theme(axis.text=element_text(size=13),
        plot.title=element_text(size=13),
        strip.text=element_text(size=13),
        legend.position = "none")



