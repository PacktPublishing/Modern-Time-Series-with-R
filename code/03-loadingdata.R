## ----chap3-loadpackage, eval=TRUE, warning=FALSE, message=FALSE, purl=TRUE, echo=FALSE----
#install.packages("pacman")
library(pacman)
p_load("forecast"
       )



##----data-io1, eval=FALSE, warning=FALSE, message=FALSE, purl=TRUE---------------

# lists of objects in global environment
ls()

## clear R workspace
 rm(list = ls())

## prints root directory
 getwd()


## ----data-io2
# check existence of data folder
 dir.exists("data")
 dir.create("data")


## ----data-io3
 
 if(dir.exists(paste0(getwd(), "/data/data_chap3"))){
  data_path <- paste0(getwd(), "/data/data_chap3")
}else{
  dir.create(paste0(getwd(), "/data/data_chap3"))
  data_path <- paste0(getwd(), "/data/data_chap3")
}


## ----data-io4
 
 saveRDS(mtcars
        ,file = paste0(data_path,"/mtcars.rds"))

saveRDS(list(mtcars, austres)
        ,file = paste0(data_path,"/mtcars_austres.rds"))


## ----data-io5
readRDS(paste0(data_path,"/mtcars.rds"))

singl_obj <- readRDS(paste0(data_path,"/mtcars.rds"))
singl_obj


## ----data-io6
mult_obj <- readRDS(paste0(data_path,"/mtcars_austres.rds"))
df1 <- mult_obj[[1]]
df2 <- mult_obj[[2]]


## ----data-io7
data(package="forecast")


## ----data-io8
gas
# # Error: object 'gas' not found
#
install.packages("forecast")
library(forecast)
gas |> head()


## ----data-io9
p_load("tidyverse")

# add a character column to mtcars data
mtcars2 <- rownames_to_column(mtcars, "make") |>
  as_tibble()

write_csv(mtcars2
          ,file = paste0(data_path,"/mtcars2.csv"))

rm("mtcars2")

mtcars2 <- read_csv(paste0(data_path,"/mtcars2.csv"))



## ----data-io11
parse_date("10/05/2023", format = "%m/%d/%Y")

parse_date("10/05/2023", format = "%d/%m/%Y")

parse_date("10/05/2023", format = "%Y-%m-%d")


## ----data-io12
parse_datetime("2023-05-14 16:42")

parse_datetime("14 May, 2023", "%d %B, %Y")


## ----data-io13
parse_time("4:42pm")



## ----data-io14
mtcars3 <- mtcars2 |>
  mutate(date = "31/12/2000")

write_csv(mtcars3, paste0(data_path,"/mtcars3.csv"))
rm("mtcars3")

# the column type for date is changed by providing proper formatting option
mtcars3 <- read_csv(paste0(data_path, "/mtcars3.csv")
                    ,col_types = list(date = col_date(format = "%d/%m/%Y")))

mtcars3 |>
  select(date, make, mpg)


## ----data-io15,
## # location of an example file from readr package
file_loc <- readr_example("challenge.csv")
spec_csv(file_loc)

read_csv(file_loc, col_types = list(y=col_logical()))

spec_csv(file_loc, guess_max = 1100)

read_csv(file_loc, col_types = list(y=col_date())) |>
 slice(1:1003) |>
  tail()


## ----data-io16
p_load("readxl")
# use an example file from readxl package
file_loc2 <- readxl_example("type-me.xlsx")

# reading a file without specifying date column types
read_excel(file_loc2
           ,sheet = "date_coercion")

read_excel(file_loc2
           ,sheet = "date_coercion"
           ,col_types = c("date", "text"))


## ----data-io17
p_load("fst")

write_fst(mtcars3,
          path = paste0(data_path, "/mtcars3.fst")
          , compress = 100)

read_fst(path = paste0(data_path,"/mtcars3.fst")
         ,columns = c("make", "mpg", "cyl")
         ,from = 5
         ,to = 10)



## ----data-io18
read_fst(path = paste0(data_path,"/mtcars3.fst")
         ,columns = c("make", "mpg", "cyl")
         ,from = 5
         ,to = 10) |>
  as_tibble()





## ----dbconnet-1, eval=FALSE, warning=FALSE, message=FALSE, purl=TRUE-------------
p_load("DBI"
       ,"odbc"
       ,"dplyr"
       ,"RSQLite"
       ,"nycflights13")

# setup a in memory SQLite connection
sqlite_con <- DBI::dbConnect(drv=RSQLite::SQLite(),
                             dsn = ":memory:")
sqlite_con


## ----dbconnet-2
# write weather table
copy_to(sqlite_con
        ,weather
        ,"WEATHER")


## ----dbconnet-3
# Connecting to Snowflake
snowflake_conn <- DBI::dbConnect(drv = odbc::odbc()
                                ,dsn = "Snowflake"
                                ,warehouse = "warehouse_name"
                                ,database = "database_name"
                                ,schema = "schema_name"
                                ,uid = "user id"
                                ,pwd  = "never include pwd in script")


## ----dbconnet-4
## Do not run example
p_load("ROracle")

# loading ROracle library before connection avoids unexpected errors
protocol <- "protocol"
host <- "host of the database"
port <- "port of the database"
sid <- "database name"

connect.string <- paste(
                        "(DESCRIPTION=",
                        "(ADDRESS=(PROTOCOL=" , protocol,
                        ")(HOST=", host,
                        ")(PORT=", port, "))",
                        "(CONNECT_DATA=(SID=", sid, ")))",
                sep = "" )

oracle_con <- DBI::dbConnect(dbDriver("oracle")
                             ,dbname = connect.string
                             ,uid
                             ,pwd )


## ----security-1
p_load("keyring")

# set a key prompts a box to insert password
key_set(service = "database_service_name"
        ,username = "user_id")

# the retrieved credentials are used in the connection directly
snowflake_conn <- DBI::dbConnect(drv = odbc::odbc()
                                ,dsn = "database_service_name"
                                ,warehouse = "warehouse_name"
                                ,database = "database_name"
                                ,schema = "schema_name"
                                ,uid = kingring::key_list("database_service_name")[1,2]
                                ,pwd  = kingring::key_get("database_service_name",
                                                         "user_id"))


## ----security-2
snowflake_conn <- DBI::dbConnect(drv = odbc::odbc()
                                ,dsn = "database_service_name"
                                ,warehouse = "warehouse_name"
                                ,database = "database_name"
                                ,schema = "schema_name"
                                ,uid = rstudioapi::askForPassword("type user id")
                                ,pwd  = rstudioapi::askForPassword("type password")
                               )


## ----sql-1
p_load("DBI"
       ,"dplyr"
       ,"dbplyr"
       ,"glue")


## ----sql-2
sqlite_con <- DBI::dbConnect(drv=RSQLite::SQLite(),
                             dsn = ":memory:")


## ----sql-3
q1 <- dbGetQuery(sqlite_con,
                statement = "select *
                 from weather
                 where origin = 'EWR' ") |>
  as_tibble()
q1


## ----sql-4
# the full query should be enclosed by double quotes
# to avoid cancellation with single quote around EWR.
dbGetQuery(sqlite_con,
           statement = "select *
                 from weather
                 where origin = 'EWR' ")


## ----sql-5
# select *
# from weather
# where temp >= 50
# and wind_dir > 250


## ----sql-6

q2 <- dbGetQuery(sqlite_con,
                 statement = readr::read_file(paste0(data_path,"/weather.sql"))) |>
  as_tibble()

q2


## ----sql-7

q3 <- tbl(sqlite_con, "WEATHER") |>
  group_by(month, origin) |>
  tally() |>
  ungroup() |>
  pivot_wider(id_cols = "month"
              ,names_from = "origin"
              ,values_from = "n") |>
  collect()

q3


## ----sql-8

origin_par <- c("EWR", "JFK" ,"LGA")
origin_sql <- glue_sql("select
                       origin
                       ,year
                       ,temp
                        from weather
                        where origin = ?")

origin_data <- list()
for(i in seq_along(origin_par)){
  origin_query <- dbSendQuery(sqlite_con, origin_sql)
  dbBind(origin_query, list(origin_par[i]))
  origin_data[[i]] <- dbFetch(origin_query) |>
                     as_tibble()
  dbClearResult(origin_query)
}

origin_data[[1]] |>
  head(3)

origin_data[[2]] |>
  head(3)



## ----sql-9

first_date <- "2013-01-01"
second_date <- "2013-05-31"

query <- "select *
from(
  select
  origin
  ,strftime('%Y-%m-%d', datetime(time_hour,'unixepoch', 'localtime')) as date
  ,temp
  from weather
 )
where date >= ?start_ts
and date <= ?end_ts
"
quey_interpolate <- sqlInterpolate(sqlite_con,
                                   query,
                                    start_ts = first_date
                                   ,end_ts = second_date )

q4 <- dbGetQuery(sqlite_con, quey_interpolate) |>
  as_tibble()

q4

