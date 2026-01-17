#install.packages("pacman")
p_load(
  "DBI",
  "dbplyr",
  "forecast",
  "fst",
  "glue",
  "keyring",
  "nycflights13",
  "readxl",
  "RSQLite",
  "ROracle",
  "tidyverse" )

# lists of objects in global environment
ls()

# clear R workspace
rm(list = ls())

# prints root directory
getwd()

# check existence of data folder
dir.exists("data")
dir.create("data")


# Check if the folder exists and create if it doesn't

if (dir.exists(file.path(getwd(), "data", "data_chap3"))){
  data_path <- file.path(getwd(), "data", "data_chap3")
} else {
  dir.create(file.path(getwd(), "data", "data_chap3"), 
             recursive = TRUE)
  data_path <- file.path(getwd(), "data", "data_chap3")
}


saveRDS(mtcars,
        file = file.path(data_path,"mtcars.rds"))

saveRDS(list(mtcars, austres),
        file = file.path(data_path, "mtcars_austres.rds"))

singl_obj <- readRDS(file.path(data_path, "mtcars.rds"))
singl_obj

mult_obj <- readRDS(file.path(data_path,"/mtcars_austres.rds"))
df1 <- mult_obj[[1]]
df2 <- mult_obj[[2]]


data(package="forecast")




data(gas)

head(gas,10)
## 
## Time Series:
## Start = c(2000, 1)
## End = c(2000, 10)
## Frequency = 52
##  [1] 70.636 71.040 68.490 65.137 67.918
##  [6] 75.117 72.970 76.106 78.158 82.272
## 


mtcars2 <-
  rownames_to_column(mtcars, "make") |>
  as_tibble()

write_csv(
  mtcars2,
  file = file.path(data_path,"mtcars2.csv"))

rm("mtcars2")

mtcars2 <- read_csv(
  file.path(data_path,"mtcars2.csv"))

spec_csv(file.path(data_path,"mtcars2.csv"))
## 
## cols(
##   make = col_character(),
##   mpg = col_double(),
##   cyl = col_double(),
##   disp = col_double(),
##   hp = col_double(),
##   drat = col_double(),
##   wt = col_double(),
##   qsec = col_double(),
##   vs = col_double(),
##   am = col_double(),
##   gear = col_double(),
##   carb = col_double()
## )
## 

parse_date("10/05/2023", format="%m/%d/%Y")
## 
## [1] "2023-10-05"

parse_date("20/05/2023", format="%d/%m/%Y")
## 
## [1] "2023-05-20"
## 

parse_date("10/05/2023", format="%Y-%m-%d")
## 
## [1] NA
## 

parse_date("2023-05-10", format = "%Y-%m-%d")
## 
## [1] "2023-05-10"

parse_datetime("2023-05-14 16:42")
## 
## [1] "2023-05-14 16:42:00 UTC"

parse_datetime("14 May, 2023", "%d %B, %Y")
## 
## [1] "2023-05-14 UTC"


parse_time("4:42pm")
## 
## 16:42:00

typeme <- readxl_example("type-me.xlsx")


read_excel(typeme ,sheet = "date_coercion")
## 
## # A tibble: 7 × 2
##   `maybe a datetime?` explanation
##   <chr>               <chr>
## 1 NA                  "empty"
## 2 41051               "date only format"
## 3 41026.479166666664  "date and time format"
## 4 TRUE                "boolean true"
## 5 cabbage             "\"cabbage\""
## 6 4.3                 "4.3 (numeric)"
## 7 39448               "another numeric"

read_excel(typeme
           ,sheet = "date_coercion"
           ,col_types = c("date", "text"))
## 
## # A tibble: 7 × 2
##   `maybe a datetime?` explanation
##   <dttm>              <chr>
## 1 NA                  "empty"
## 2 2016-05-23 00:00:00 "date only format"
## 3 2016-04-28 11:30:00 "date and time format"
## 4 NA                  "boolean true"
## 5 NA                  "\"cabbage\""
## 6 1904-01-05 07:12:00 "4.3 (numeric)"
## 7 2012-01-02 00:00:00 "another numeric"
## Warning messages:
## 1: Expecting date in A5 / R5C1: got boolean
## 2: Expecting date in A6 / R6C1: got 'cabbage'
## 3: Coercing numeric to date in A7 / R7C1
## 4: Coercing numeric to date in A8 / R8C1
## 


write_fst(
  as_tibble(mtcars, rownames= "make"),
  path = file.path(data_path, "mtcars3.fst"),
  compress = 100)


read_fst(
  path = file.path(data_path,"mtcars3.fst"),
  columns = c("make", "mpg", "cyl"),
  from = 5,
  to = 10) |>
  as_tibble()
## 
## # A tibble: 6 × 3
##   make                mpg   cyl
##   <chr>             <dbl> <dbl>
## 1 Hornet Sportabout  18.7     8
## 2 Valiant            18.1     6
## 3 Duster 360         14.3     8
## 4 Merc 240D          24.4     4
## 5 Merc 230           22.8     4
## 6 Merc 280           19.2     6
## 




sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")


copy_to(sqlite_con
        ,weather
        ,"WEATHER")

# Connecting to Snowflake
snowflake_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "Snowflake" ,
  warehouse = "<warehouse name>",
  database = "<database name>",
  schema = "<schemaname>",
  uid = "<user id>",
  pwd  = "<securely_retrieve_password_here>",
  authenticator="externalbrowser"
)

# Loading library prior to setup avoids unexpected errors
library(ROracle)

protocol <- "protocol"
host <- "<host of the database>"
port <- "<port of the database>"
sid <- "<database name>"

connect.string <- paste(
   "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=" , protocol,
  ")(HOST=", host,
  ")(PORT=", port, "))",
   "(CONNECT_DATA=(SID=", sid, ")))",
  sep = "" )

oracle_con <- DBI::dbConnect(
  dbDriver("oracle"),
  dbname = connect.string,
  uid,
  pwd )


key_set(service = "<database service name>",
        username = "<user id>")

# Retrieved credentials are used in the connection directly
snowflake_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "<database service name>",
  warehouse = "<warehouse name>",
  database = "<database name>",
  schema = "<schema name>",
  uid = keyring::key_list("<database service name>")[1,2],
  pwd = keyring::key_get("<database service name>","<user id>"))





## snowflake_conn <- DBI::dbConnect(drv = odbc::odbc()
##                                 ,dsn = "<database service name>"
##                                 ,warehouse = "<warehouse name>"
##                                 ,database = "<database name>"
##                                 ,schema = "<schema name>"
##                                 ,uid = rstudioapi::askForPassword("type user id")
##                                 ,pwd  = rstudioapi::askForPassword("type password")
##                                )


sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Write weather table
copy_to(sqlite_con ,weather ,"WEATHER")

weather_ewr <- dbGetQuery(sqlite_con,
                statement = "select *
                  from weather
                  where origin = 'EWR' ") |>
  as_tibble()


weather_ewr
## 
## # A tibble: 8,703 × 15
##    origin  year month   day  hour  temp
##    <chr>  <int> <int> <int> <int> <dbl>
##  1 EWR     2013     1     1     1  39.0
##  2 EWR     2013     1     1     2  39.0
##  3 EWR     2013     1     1     3  39.0
##  4 EWR     2013     1     1     4  39.9
##  5 EWR     2013     1     1     5  39.0
##  6 EWR     2013     1     1     6  37.9
##  7 EWR     2013     1     1     7  39.0
##  8 EWR     2013     1     1     8  39.9
##  9 EWR     2013     1     1     9  39.9
## 10 EWR     2013     1     1    10  41
## # ℹ 8,693 more rows
## # ℹ 9 more variables: dewp <dbl>,
## #   humid <dbl>, wind_dir <dbl>,
## #   wind_speed <dbl>, wind_gust <dbl>,
## #   precip <dbl>, pressure <dbl>,
## #   visib <dbl>, time_hour <dbl>

select *
from weather
where temp >= 50
and wind_dir > 250

weather_temp <- dbGetQuery(sqlite_con,
                 statement = readr::read_file(
                   file.path(data_path,"weather.sql"))) |>
  as_tibble()


weather_summary <- tbl(sqlite_con, "WEATHER") |>
  group_by(month, origin) |>
  tally() |>
  ungroup() |>
  pivot_wider(id_cols = "month"
              ,names_from = "origin"
              ,values_from = "n") |>
  collect()



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





start_date <- "2013-01-01"
end_date <- "2013-05-31"

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
query_interpolate <- sqlInterpolate(
  sqlite_con,
  query,
  start_ts = start_date,
  end_ts = start_date )

query_extract <- dbGetQuery(
  sqlite_con, query_interpolate) |>
  as_tibble()

