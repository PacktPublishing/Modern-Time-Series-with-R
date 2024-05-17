## ----ch10-loadpackage, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----
#install.packages("pacman")
library(pacman)
p_load("bundle"
       ,"factoextra"
       ,"feasts"
       ,"fpp3"
       ,"fs"
       ,"readr"
       ,"tidyverse"
       ,"tidymodels"
       ,"uwot"
       )

# create data_chap10 folder 
if(!dir.exists("data/data_chap10")){
  dir.create("data/data_chap10/")
}



## ----ch10-acf-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----
# load data
abs_retail <- readr::read_rds("data/data_chap4/abs_retail.rds")

acf_features <- abs_retail |>
  features(turnover, feat_acf )



## ----ch10-acf-features-out, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

acf_features


## ----ch10-pacf-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

abs_retail |>
  features(turnover, feat_pacf )



## ----ch10-stl-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

abs_retail |>
  features(turnover, feat_stl)




## ----ch10-tiled-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

abs_retail |>
  features(turnover, list(var_tiled_mean, var_tiled_var ))



## ----ch10-slide-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

abs_retail |>
  filter(industry == "Food retailing") |>
  features(turnover,
           list(shift_level_max,
                shift_var_max,
                shift_kl_max))


## ----ch10-unitroot-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

abs_retail |>
   filter(industry == "Food retailing") |>
  features(turnover,
           list(unitroot_kpss,
                unitroot_pp,
                unitroot_ndiffs,
                unitroot_nsdiffs))


## ----ch10-summary-features, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

abs_retail |>
  features(turnover,
           list(mean=mean,
                var=var,
                quantile))

## ----ch10-m3-monthly-download, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

m3_monthly <- monash_forecasting_repository(4656298)



## ----ch10-m3-monthly-transform-month, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

m3_monthly <- m3_monthly |>
  mutate(month = yearmonth(start_timestamp)) %>%
  as_tsibble(index=month) %>%
  select(-start_timestamp)


## ----ch10-m3-features, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

monthly_features <- m3_monthly |> 
  features(value, feature_set(pkgs = "feasts"))



## ----ch10-pca-recipe, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

pca_recp <- recipe(~., data = monthly_features) |> 
  update_role(series_name, new_role = "id") |> 
  step_nzv(all_predictors()) |> 
  step_normalize(all_predictors()) |> 
  step_pca(all_predictors(), num_comp = 4)



## ----ch10-pca-prep, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

pca_prep <- prep(pca_recp)

pca_prep



## ----ch10-pca-contrib, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', fig.width=12, fig.height=8, fig.cap="Contribution of features on PCA of M3 monthly data"----

tidied_pca <- tidy(pca_prep,3)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(x = "Absolute value of contribution"
       ,y = NULL 
       ,fill = "Positive?") +
  theme_bw() +
  theme(axis.text = element_text(size=11))


## ----ch10-pca-space, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny', fig.width=12, fig.height=6, fig.cap="First two principal components space of features computed from M3 monthly data"----

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = series_name)) +
  geom_point( alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size=12))



## ----ch10-umap, eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

nzv_var <- c("zero_run_mean","zero_start_prop","zero_end_prop")
id_var <- "series_name"

set.seed(121123)
get_umap <-uwot::umap(
  X = monthly_features %>% 
    select(-all_of(c(id_var, nzv_var))),  
  metric = "euclidean" ,
  scale = TRUE, 
  approx_pow = TRUE,
  init = "spca",
  n_components = 2,  
  min_dist = 0,     
  n_trees = 25,
  nn_method = "annoy", 
  n_threads = 12,
  ret_nn = TRUE , 
  ret_model = TRUE, 
  verbose = FALSE
)



## ----ch10-umap-save, eval=FALSE, cache=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=TRUE, size='tiny'----

library(bundle)
library(fs)

temp_file <- fs::file_temp(pattern = "umap", ext = "rds")
bundle(get_umap) |>  write_rds(temp_file)

saved_umap <- read_rds(temp_file)
unbundle(saved_umap)



## ----ch10-umap-embed
umap_embed <- monthly_features |> 
  select(series_name) |> 
  dplyr::bind_cols(get_umap$embedding |> 
                     as_tibble())


umap_embed |>
  ggplot(aes(x=V1, y=V2)) +
  geom_point(size=2, alpha=0.8) + 
  geom_text(aes(label = series_name)
            ,check_overlap = TRUE
            ,hjust = "inward"
             ,size=4) +
  labs(color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size=12))



## ----ch10-umap-kmean
kmean_list <- list()

for(j in 1:10)
{
kmean_list[[j]] <- factoextra::eclust(umap_embed |> 
    select(-series_name),
    FUNcluster="kmeans", 
    k=j,
    k.max = j, 
    graph = FALSE,
    nboot = 0, 
    verbose = FALSE)
}

km_totwss <- tibble(clust_num = seq(1:10),
 totwss =  do.call(c, lapply(kmean_list, '[[', "tot.withinss")))


km_totwss %>% 
  ggplot(aes(clust_num, totwss))+
  geom_line(linewidth=0.9) + 
  geom_point(size=4)+
  scale_x_continuous(limits = c(1,10), breaks = seq(1,10,1))+
  labs(x = "Number of clusters", 
       y = "Total within sum of squares")+
  geom_vline(xintercept =4, linetype = 2)+
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12))


## ----ch10-umap-kmean-optimal
final_km <- kmean_list[[4]]

# add the cluster allocation 
umap_embed <- umap_embed |> 
  mutate(cluster_no = factor(final_km$cluster, levels = c(1:4))) 
  
umap_embed |>
  ggplot(aes(x=V1, y=V2)) +
  geom_point(aes(colour=cluster_no, shape=cluster_no), size=2, alpha=0.8) + 
  geom_text(aes(label = series_name)
            ,check_overlap = TRUE
            ,hjust = "inward"
             ,size=3) +
  labs(x = "Embedded dimension 1",
       y = "Embedded dimension 2",
       title ="M3 Monthly data segmented into 4 clusters") +
  theme_bw() +
  theme(axis.text = element_text(size=12))



## ----ch10-umap-cluster-raw
# add the cluster allocation with the raw observation
m3_monthly_cluster <- m3_monthly |> 
  left_join(umap_embed |> 
            select(series_name, cluster_no), 
            by = "series_name") 

srname <- c("T527", "T1045","T29","T278")

m3_monthly_cluster |> 
  filter(series_name %in% all_of(srname)) |> 
  mutate(series = paste0("Cluster no:", cluster_no, " & Series no:", series_name)) |>
  ggplot(aes(x=month, y=value)) +
  geom_line() +
  facet_wrap(~series, scales="free_y",ncol=1 ) +
  theme_bw() +
  theme(axis.text = element_text(size=12)
       ,strip.text = element_text(size=12))


