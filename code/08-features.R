
library(pacman)
p_load("bundle"
       ,"conflicted"
       ,"factoextra"
       ,"fabletools"
       ,"feasts"
       ,"fpp3"
       ,"fracdiff"
       ,"fs"
       ,"here"
       ,"readr"
       ,"tidyverse"
       ,"tidymodels"
       ,"urca"
       ,"uwot" )

conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  fabletools::components,
  fabletools::accuracy )

conflict_prefer_all("feasts", "tsfeatures")

# load data
abs_retail <- read_rds("data/data_chap8/abs_retail.rds") 

retail_df <- abs_retail |> 
  filter(state == "Victoria", 
         industry %in% c("Department stores", 
                         "Supermarket and grocery stores")) |> 
  mutate(industry = ifelse(
   industry == "Supermarket and grocery stores" 
      ,"Supermarket stores" , industry )) |> 
  filter_index("2000 Jan" ~ "2022 Dec") |> 
  select(industry, month, turnover)

retail_df |> 
  ggplot(aes(x = month, y = turnover)) +
  geom_line() + 
  facet_wrap(industry~., scales = "free_y", ncol = 1) +
  labs(x = "", y = "")


stat_feat <- retail_df |> 
  features(turnover, 
           list(mean=mean, var=var))

stat_feat
## 
## # A tibble: 2 × 3
##   industry            mean     var
##   <chr>              <dbl>   <dbl>
## 1 Department stores   356.  11900.
## 2 Supermarket stores 1751. 305344.
## 


quant_feat <- retail_df |> 
  features(turnover, quantile)


quant_feat
## 
## # A tibble: 2 × 6
##   industry            `0%` `25%` `50%` `75%` `100%`
##   <chr>              <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 Department stores   170.  298.  334.  370.   763.
## 2 Supermarket stores  886  1266. 1676. 2149.  3278.




acf_pacf_feat <- retail_df |> 
  features(turnover, 
           list(feat_acf ,feat_pacf))


apcf_feat_tab <- acf_pacf_feat |> 
  mutate(across(where(is.numeric), \(x) round(x,3))) |> 
  t() 
apcf_feat_tbl <- tibble(
  Features = rownames(apcf_feat_tab)[-1],
  `Department stores` = as.numeric(apcf_feat_tab[-1,1]),
  `Supermarket stores` = as.numeric(apcf_feat_tab[-1,2]))


knitr::kable(
  apcf_feat_tbl
  ,format = "pandoc"
  ,align = "lrr"
   ,caption = "ACF and PACF features computed from Department and Supermarket stores turnover"
  )




stl_feat <- retail_df |> 
  features(turnover, feat_stl) 



stl_feat |> 
  select(industry,
         trend_strength,
         seasonal_strength_year,
         linearity,
         curvature,
         spikiness,
         stl_e_acf1,
         stl_e_acf10) |> 
  pivot_longer(cols = 2:8,
               names_to = "STL Features",
               values_to = "value") |> 
  mutate(value = round(value,2)) |> 
  pivot_wider(names_from = "industry",
              values_from = "value") |> 
  kable(align = "lrr",
        format = "pandoc",
        caption = "STL features of two retail series")



retail_df |>
  filter(industry == "Supermarket stores") |>
  model(STL(turnover)) |>
  components() |>
  autoplot() +
  labs(x="")



tiled_feat <- retail_df |>
  features(turnover, list(var_tiled_mean, var_tiled_var ))


tiled_feat
## 
## # A tibble: 2 × 3
##   industry        var_tiled_mean var_tiled_var
##   <chr>                   <dbl>         <dbl>
## 1 Department stores       0.113       0.105
## 2 Supermarket stores      0.997       0.00125


sw_feat <- retail_df |> 
   features(turnover, 
           list(shift_level_max,
                shift_var_max,
                shift_kl_max))


# sw_feat |> 
#   pivot_longer(cols = 2:7,
#                names_to = "Features",
#                values_to = "value") |> 
#   mutate(value_formatted = ifelse(str_detect(Features, "index"), 
#                         as.character(as.integer(value)), 
#                         as.character(round(value,2)))) |> 
#   select(-value) |> 
#   pivot_wider(names_from = "industry",
#               values_from = "value_formatted") |> 
#   kable(align = "lrr",
#         format = "pandoc",
#         caption = "Sliding window features of two retail series")
# 


series_idx <- retail_df |>
 filter(industry %in%
  c("Department stores", "Supermarket stores")) |>
  group_by(industry) |>
  mutate(row_index = row_number()) |>
  ungroup()


sw_feat |>
  select(c("industry", ends_with("index"))) |>
  pivot_longer(cols = ends_with("index")) |>
  left_join(
    series_idx,
    by = c( "industry" = "industry", "value" = "row_index")) |>
  arrange(industry, month)


## # A tibble: 6 × 5
##   industry     name             value    month turnover
##   <chr>        <chr>            <dbl>    <mth>    <dbl>
## 1 Department   shift_kl_index     262 2021 Oct     260
## 2 Department   shift_var_index    264 2021 Dec     680.
## 3 Department   shift_level_index  265 2022 Jan     359.
## 4 Supermarket  shift_kl_index     237 2019 Sep    2304.
## 5 Supermarket  shift_level_index  242 2020 Feb    2385.
## 6 Supermarket  shift_var_index    244 2020 Apr    2428.


uroot_feat <- retail_df |>
  features(turnover,
           list(unitroot_kpss,
                unitroot_pp,
                unitroot_ndiffs,
                unitroot_nsdiffs))


# uroot_feat_tab <- uroot_feat |> 
#   mutate(across(where(is.numeric), \(x) round(x,3))) |> 
#   t() 
# uroot_feat_tbl <- tibble(
#   Features = rownames(uroot_feat_tab)[-1],
#   `Department stores` = as.numeric(uroot_feat_tab[-1,1]),
#   `Supermarket stores` = as.numeric(uroot_feat_tab[-1,2]))
# 
# knitr::kable(
#   uroot_feat |> select(-1) |> t()
#   ,digits = 4
#   ,format = "pandoc"
#   ,align = "lrr"
#   ,col.names = c("Feature", "Department stores", "Supermarket stores")
#   ,caption = "Features to test the existence of a unitroot in department and supermarket turnover"
# )



retail_df |>
  model(stl = STL(turnover)) |>
  components() |>
  features(remainder,
           list(ljung_box, box_pierce)) |>
  mutate(across(3:6, \(x)round(x ,2))) |>
  select( - .model)

## # A tibble: 2 × 5
##   industry          lb_stat lb_pvalue bp_stat bp_pvalue
##   <chr>               <dbl>     <dbl>   <dbl>     <dbl>
## 1 Department stores   10.9       0      10.8       0
## 2 Supermarket stores  2.87      0.09    2.84      0.09

## 
## m3_monthly <- monash_forecasting_repository(4656298)
## saveRDS(m3_monthly,  here("data", "data_chap8", "m3_monthly.rds"))
## 




m3_monthly <- m3_monthly |>
  mutate(month = yearmonth(start_timestamp)) |>
  as_tsibble(index = month) |>
  select(-start_timestamp)



monthly_features <- m3_monthly |>
  features(value, feature_set(pkgs = "feasts"))


## # save locally
## saveRDS(monthly_features,  here("data", "data_chap8", "monthly_features.rds"))


pca_recp <- recipe(~., data = monthly_features) |>
  update_role(series_name, new_role = "id") |>
  step_nzv(all_predictors()) |>
  step_normalize(all_predictors()) |>
  step_pca(all_predictors(), num_comp = 4)



pca_prep <- prep(pca_recp, training = monthly_features)





tidied_pca <- tidy(pca_prep,3)

tidied_pca |>
  filter(component %in% paste0("PC", 1:4)) |>
  mutate(component = fct_inorder(component)) |>
  ggplot(aes(value, terms), fill = "grey" ) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(x = "Absolute value of contribution"
       ,y = NULL )




juice(pca_prep)|>
  ggplot(aes(PC1, PC2, label = series_name)) +
  geom_point( alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL)


nzv_var <- c("zero_run_mean","zero_start_prop","zero_end_prop")
id_var <- "series_name"

set.seed(121123)
umap_mod <- uwot::umap(
  X = monthly_features |>
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



temp_file <- fs::file_temp(pattern = "umap", ext = "rds")
umap_save_path <- here("data","data_chap8","umap_mod.rds")

# save
save_uwot(umap_mod, file = umap_save_path, unload = TRUE)

# reload
umap_mod <-load_uwot(file = umap_save_path)






kmean_list <- list()
for (j in 2:8){
kmean_list[[j]] <- factoextra::eclust(umap_embed |>
    select(-series_name),
    FUNcluster = "kmeans",
    k = j,
    k.max = j,
    graph = FALSE,
    nboot = 0,
    verbose = FALSE)
}
names(kmean_list) <- 2:8


km_totwss <- tibble(clust_num = seq(2,8,1),
 totwss =  do.call(c, lapply(kmean_list, '[[', "tot.withinss")))

opt_cluster <- km_totwss |>
  mutate( first_diff = totwss - lag(totwss)) |>
  mutate(second_diff = first_diff - lag(first_diff)) |>
  drop_na() |>
  slice_max(second_diff)


km_totwss
## 
## # A tibble: 7 × 2
##   clust_num totwss
##       <dbl>  <dbl>
## 1         2 10454.
## 2         3  5115.
## 3         4  3465.
## 4         5  3083.
## 5         6  2492.
## 6         7  1803.
## 7         8  1411.
## 
opt_cluster
## 
## # A tibble: 1 × 4
##   clust_num totwss first_diff second_diff
##       <dbl>  <dbl>      <dbl>       <dbl>
## 1         4  3465.     -1650.       3689.
## 




km_totwss |>
  ggplot(aes(clust_num, totwss)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(2,8), breaks = seq(2,8,1)) +
  labs(x = "Number of clusters",
       y = "Total within sum of squares") +
  geom_vline(xintercept = opt_cluster$clust_num, linetype = 2)




final_km <- kmean_list[[opt_cluster$clust_num]]

# add the cluster allocation
umap_embed <- umap_embed |>
  mutate(cluster_no = factor(final_km$cluster, levels = c(1:4)))

umap_embed |>
  ggplot(aes(x = umap1, y = umap2)) +
  geom_point(aes(colour = cluster_no, shape = cluster_no)
             ,size = 2
             ,alpha = 0.8) +
  geom_text(aes(label = series_name)
            ,check_overlap = TRUE
            ,hjust = "inward"
            ,size = 3) +
  labs(x = "UMAP Embedded dimension 1",
       y = "UMAP Embedded dimension 2") +
  theme(legend.position = "top")





# add the cluster allocation with the raw observation
m3_monthly_cluster <- m3_monthly |>
  left_join(umap_embed |>
            select(series_name, cluster_no),
            by = "series_name")

srname <- c("T527", "T1045","T29","T278")

m3_monthly_cluster |>
  filter(series_name %in% all_of(srname)) |>
  mutate(series = paste0("Cluster no:", cluster_no, " & Series no:", series_name)) |>
  ggplot(aes(x = month, y = value)) +
  geom_line() +
  facet_wrap(~series, scales = "free_y",ncol = 1 )

