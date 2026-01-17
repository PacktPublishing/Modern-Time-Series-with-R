## R.home()
## Sys.getenv("R_HOME")
## 
## Sys.getenv("HOME")
## path.expand("~")
## getwd()
## 
## .libPaths()

## R.home()
## Sys.getenv("HOME")
## .libPaths()

## sudo apt update -qq
## sudo apt install --no-install-recommends software-properties-common dirmngr

## sudo add-apt repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

## sudo apt install --no-install-recommends r-base r-base-core r-recommended r-base-dev

## wget https://download1.rstudio.org/electron/jammy/amd64/rstudio-2023.03.0-386-amd64.deb

## sudo apt install -f ./rstudio-2023.03.0-386-amd64.deb

## rstudio

## apt search "^r-.*" | sort

## sudo apt update
## 
## sudo apt install --no-install-recommends software-properties-common dirmngr gnupg ca-certificates
## 
## curl -sSL "keyserver.ubuntu.com" | sudo gpg --dearmor -o /usr/share/keyrings/cran-archive-keyring.gpg
## 
## echo "deb [signed-by=/usr/share/keyrings/cran-archive-keyring.gpg] https://cloud.r-project.org/bin/linux/debian bookworm-cran40/" | sudo tee /etc/apt/sources.list.d/cran.list
## 

## sudo apt update
## sudo apt install r-base r-base-core r-base-dev

## wget https://rstudio.org/download/latest/stable/desktop/jammy/rstudio-latest-amd64.deb -O rstudio-latest-amd64.deb
## 
## sudo apt install -f ./rstudio-latest-amd64.deb
## rstudio



## file.edit("~/.Rprofile")

## .First <- function(){
##   path_to_package = "D:/<Username>/R/RPackages"
##   .libPaths(c(path_to_package, .libPaths()))
## }





## # install a single package
## install.packages("dplyr")
## 
## # install multiple packages
## install.packages(
##   c("DBI", "tibble", "dplyr","ggplot2","fst","knitr"))

## 
## if (!requireNamespace("pacman", quietly = TRUE)) {
##   install.packages("pacman") }
## 
## pacman::p_load(
##   DBI
##   ,tibble
##   ,dplyr
##   ,ggplot2
##   ,fst
##   ,knitr)

## R

## install.packages("dplyr")



## sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
## sudo apt install --no-install-recommends r-cran-dplyr



## 
## # using remotes package
## remotes::install_github(repo = "tidyverse/purrr")
## 
## # using devtools package
## devtools::install_github(repo = "tidyverse/purrr")
## 
## # using xfun package
## xfun::install_github(repo = "tidyverse/purrr")
## 
## # using pacman package
## pacman::p_load_gh(repo = "tidyverse/purrr")
## 

## # load an installed package
## library(dplyr)



## # list of installed packages
## pkg_list = as.data.frame(installed.packages(.libPaths()[1]),
##                          stringsAsFactors = F)
## 
## # re-install packages
## install.packages(pkg_list$Package)

## install.packages("installr")
## installr::updateR()





## install.packages("tidyverse")

## tidyverse::tidyverse_packages(include_self = FALSE)



## install.packages("dlstats")


library(pacman)
p_load(tidyverse
       ,ggplot2
       ,dlstats
       ,scales
       ,conflicted)

conflicts_prefer(
  dplyr::filter()
)


dwnld_stat <- cran_stats(c("ggplot2",
                           "dplyr", 
                           #"tidyr", 
                           "readr",
                           #"purrr",
                           "tibble"
                           #"stringr", 
                           #"forcats"
                           ))
write_csv(dwnld_stat, 
          file.path("data", "data_chap1", "dwnld_stat.csv"))



dwnld_stat <- readr::read_csv("data/data_chap1/dwnld_stat.csv") |> 
  tibble()

dwnld_stat |>
  filter(start <= "2025-11-01") |> 
  group_by(package) |>
  dplyr::filter(row_number() <= n() - 1) |>
  ungroup() |>
  ggplot(aes(end, downloads, group = package)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(package~., ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(title = ""
       , x = ""
       , y = "Number of downloads")

