# chapter: R, RStudio and R packages 

# section: Install R and RStudio with admin access ----
# subsection: Windows
R.home()
Sys.getenv("R_HOME")
Sys.getenv("HOME")
path.expand("~")
getwd()
.libPaths()


# section: Installation in a custom location----
.libPaths()
file.edit("~/.Rprofile")

.First <- function(){
  path_to_package = "users_to_provide_their_path"
  .libpaths(c(path_to_package, .libPaths()))
}


# section: Installing R packages----
# subsection: Package installation via R console

# install a single package 
install.packages("dplyr")

# install multiple packages
install.packages("DBI", "tibble", "dplyr","ggplot2","fst","knitr")

# use of pacman library
install.packages("pacman")
pacman::p_load(
  "DBI"        #R database interface 
  ,"tibble"    #Simple Data Frames
  ,"dplyr"     #Data manipulation 
  ,"ggplot2"   #visualization
  ,"fst"       #fast file read and write
  ,"knitr"     #dynamic report generation
)

# subsection: Install developing R packages
# run only one
# using remotes package
remotes::install_github(repo = "tidyverse/purrr")

# using devtools package 
devtools::install_github(repo = "tidyverse/purrr")

# using xfun package
xfun::install_github(repo = "tidyverse/purrr")

# using pacman package
pacman::p_load_gh(repo = "tidyverse/purrr")

# subsection: Load installed packages
library(dplyr)


# section: Updating software and packages----
# subsection: Check and update R packages

# list of installed packages
pkg_list = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors = F)
# re-install packages 
install.packages(pkg_list$Package)

# subsection: Check and update R and RStudio
install.packages("installr")
installr::updateR()

# section: Tidyverse package collection----
install.packages("tidyverse")
tidyverse::tidyverse_packages(include_self = FALSE)

# download package stats and create plot---- 
install.packages("dlstats")

# assuming tidyverse is already installed. 
library(dplyr)
library(ggplot2)
library(dlstats)

dwnld_stat <- cran_stats(c("ggplot2",
                           "dplyr", 
                           "readr",
                           "tibble"
                           ))

dwnld_stat |> 
  group_by(package) |> 
  filter(row_number() <= n() - 1) |> 
  ungroup() |> 
  ggplot(aes(end, downloads, group = package)) + 
  geom_line(size = 1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(package~., ncol = 2, scales = "free_y") + 
  theme_bw() + 
  labs(title = "" 
       , x = "" 
       , y = "Number of downloads per month (in millions)")