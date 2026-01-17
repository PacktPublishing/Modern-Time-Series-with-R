#install.packages("pacman")
library(pacman)
p_load("conflicted"
       ,"datasets"
      ,"gapminder"
       ,"purrr"
       ,"sloop"
       ,"tidyverse"
       ,"tsibble"
       ,"tsibbledata")

conflicts_prefer(
  dplyr::filter,
  purrr::compose)


x1 <- c(1,2,3,4,5)
x1

x2 <- 1:5
x2

.x <- 1:5
.x

x_y <- 1:5
x_y

## _y <- 1:5
## # Error: unexpected symbol in "_y"

## 1x <- 2:6
## #Error: unexpected symbol in "1x"

## if <- 2:6
## #Error: unexpected assignment in "if <-"

## a b <- 2:6
## #Error: unexpected symbol in "a b"

`_y` <- 1:5
`_y`

`a b` <- 2:6
`a b`

"a b" <- 1:5
"a b"

dbl_vec <- c(7, 10.0, 19.025)
dbl_vec 

typeof(dbl_vec)

is.double(dbl_vec)

is.integer(dbl_vec)

attr(dbl_vec, "names") <- c('a', 'b', 'c')
dbl_vec

attr(dbl_vec, "names")

attributes(dbl_vec)

int_vec <- c(0L,7L,11L)
int_vec

typeof(int_vec)

int_vec2 <- c(0,7,11)
typeof(int_vec2)

lg_vec <- c(TRUE, FALSE, TRUE) 
typeof(lg_vec)

is.logical(lg_vec)

which(lg_vec)

abs(lg_vec)

T <- 5 
T

char_vec <- c("this is", "a", "section of a book")
typeof(char_vec)

is.character(char_vec)

char_vec2 <- c("This book\t discussed \n\"Time Series Forecasting\".")
print(char_vec2)

writeLines(char_vec2)

nchar(char_vec)

lst1 <- list(
  dbl_vec
  ,int_vec
  ,lg_vec
  ,char_vec
)
lst1

is.list(lst1)

emp_lst <- list()
emp_lst[[1]] <- 1:5
emp_lst[[2]] <- c(letters[1:4])
emp_lst

str(emp_lst)

# create a nested list 
lst2 <- list(
  lst1
  ,list(
    seq(0,20,5)
    ,c(TRUE, TRUE, FALSE)
  ))

glimpse(lst2)


attributes(lst2)

attributes(lst2[[1]][[1]])

mat_a <- 1:9
dim(mat_a) <- c(3,3)
mat_a

attributes(mat_a)

mat_b <- matrix(
  c(rep(TRUE,3), rep(TRUE,3), rep(FALSE,3))
   ,nrow=3
   ,ncol=3)
mat_b


rownames(mat_a) <- c("a", "b","c")
colnames(mat_a) <- paste0("col_",1:3)

mat_a

dimnames(mat_b) <- list(
  paste0("row_",1:3), paste0("col_",1:3))
mat_b


diag(3)


b_arry <- array(1:18, c(3,3,2)) 
b_arry


dim(b_arry)


# integer overwrite logical
cr1 <- c(1L, TRUE, FALSE, 5L)
typeof(cr1)

# character coerce all other data types
cr2<- c("Pen", 5.30, TRUE )
typeof(cr2)


# character coerce all other data types
as.double(c(TRUE, FALSE, FALSE))

# character coerce all other data types
as.double(c(TRUE, FALSE, FALSE, 1.5))

as.double(c(TRUE, FALSE, "Paper", 1.5))


ch_vec <- rep(c("a", "b","c"), times=2) 
fct_vec <- factor(ch_vec)
fct_vec

attributes(fct_vec)

cust_resp1 <- ordered(
  c("bad", "good", "good", "fair")
  ,levels = c("bad", "fair", "good"))
cust_resp1

cust_resp2 <- factor(
  c("bad", "good", "good", "fair")
  ,levels = c("bad", "fair", "good")
  ,labels = c("Bad", "Fair", "Good"))
cust_resp2


score <- c(0, 10, 2, 6, 8, 8, 1, 9)
nps_score <- cut(
  score 
  ,breaks=c(-Inf, 7, 9, Inf)
  ,labels = c("Detractors", "Neutral", "Promoters")
  ,include_lowest = TRUE)

nps_score

table(nps_score)

otype(fct_vec)


otype(ch_vec)

date_vec <- as.Date("2023-01-01")
typeof(date_vec)


attributes(date_vec)


otype(date_vec)

unclass(Sys.Date())

dtm_vec <- as.POSIXct("2023-01-01 22:41:00", 
                      tz="Australia/Melbourne")
dtm_vec

attributes(dtm_vec)

as.POSIXct(8^10, origin="1970-01-01")


dur1 <- as.Date("2025-12-07") - as.Date("2023-03-28")
dur1

as.numeric(dur1)

typeof(dur1)

attributes(dur1)

dur2 <- as.difftime(14, units="weeks")
dur2


pop_df <- data.frame(
  country = c(rep("USA",2), rep("AUS",2))
  ,date = as.Date(rep(c("2000-01-01","2023-01-01"), times =2))
  ,population_mil =c(282.2, 334.3, 19.03, 26.6) )


## # > pop_df
## 
## #   country       date population_mil
## # 1     USA 2000-01-01         282.20
## # 2     USA 2023-01-01         334.30
## # 3     AUS 2000-01-01          19.03
## # 4     AUS 2023-01-01          26.60

typeof(pop_df)


attributes(pop_df)

## data.frame(id = paste0("00", c(1:6))
##            ,score= c(10:14))



## df_ok <- data.frame(id = paste0("00", c(1:6))
##            ,score= c(10:12))

df_nm <- data.frame(
  `1` = c("rock", "paper", "scissors"),
  `2` = c(5,3,2))

names(df_nm)

df_wnm <- data.frame(
    `1` = c("rock", "paper", "scissors")
   ,`2` = c(5,3,2)
   ,check.names = FALSE)

names(df_wnm)

char_become_factor <- data.frame(
  option = c("rock", "paper", "scissors"),
  n = c(5,3,2),
  stringsAsFactors = TRUE)

## glimpse(char_become_factor)
## 
## # Rows: 3
## # Columns: 2
## # $ option <fct> rock, paper, scissors
## # $ n      <dbl> 5, 3, 2

char_remain_char <- data.frame(
  option = c("rock", "paper", "scissors"),
  n = c(5,3,2),
  stringsAsFactors = FALSE)


## glimpse(char_remain_char)
## 
## # Rows: 3
## # Columns: 2
## # $ option <chr> "rock", "paper", "scissors"
## # $ n      <dbl> 5, 3, 2

row.names(char_remain_char) <- paste0("game_", 1:3)



## char_remain_char
## 
## #          option n
## # game_1     rock 5
## # game_2    paper 3
## # game_3 scissors 2

## gapminder::gapminder



set.seed(02)
large_tbl <- tibble(x = 1:1e6, y=rnorm(1e6))

## large_tbl |> head(5)
## 
## # # A tibble: 5 × 2
## #       x       y
## #   <int>   <dbl>
## # 1     1 -0.897
## # 2     2  0.185
## # 3     3  1.59
## # 4     4 -1.13
## # 5     5 -0.0803
## 

## str(large_tbl)
## 
## # tibble [1,000,000 × 2] (S3: tbl_df/tbl/data.frame)
## #  $ x: int [1:1000000] 1 2 3 4 5 6 7 8 9 ...
## #  $ y: num [1:1000000] -0.8969 0.1848 1.5878 -1.1304 ...
## 

ref_tbl <- tibble(
  x = 0:4,
  y = x^2,
  z = 2*y + x )


## ref_tbl
## 
## # # A tibble: 5 × 3
## #       x     y     z
## #   <int> <dbl> <dbl>
## # 1     0     0     0
## # 2     1     1     3
## # 3     2     4    10
## # 4     3     9    21
## # 5     4    16    36
## 

rwnam_tbl <- as_tibble(ref_tbl, rownames = "counters")


## rwnam_tbl
## 
## # # A tibble: 5 × 4
## #   counters     x     y     z
## #   <chr>    <int> <dbl> <dbl>
## # 1 1            0     0     0
## # 2 2            1     1     3
## # 3 3            2     4    10
## # 4 4            3     9    21
## # 5 5            4    16    36
## 


mtcars_tbl <- as_tibble(mtcars)

# create a list column tibble
mtcars_lm <- mtcars_tbl |> 
       group_by(gear)|>
       nest() |> 
       mutate(model = map(.x = data, 
                       ~lm(mpg~., data = .x)))

## mtcars_lm
## 
## # # A tibble: 3 × 3
## # # Groups:   gear [3]
## #    gear data               model
## #   <dbl> <list>             <list>
## # 1     4 <tibble [12 × 10]> <lm>
## # 2     3 <tibble [15 × 10]> <lm>
## # 3     5 <tibble [5 × 10]>  <lm>

mv_vec1 <- c(2, NA, 1, NA, 0)

is.na(mv_vec1)

anyNA(mv_vec1)


mv_vec2 <- c("NA", NA)
is.na(mv_vec2)

mv_lst <- list(1:3
               ,mv_vec1
               ,mv_vec2)
sapply(mv_lst, is.na)


is.nan(0/0)

is.nan(5/0)

is.nan(c(5, NA))

set.seed(100)
month_ts <- ts(rnorm(36,25,7)
          , start = "2020"
          , frequency = 12)

## month_ts
## 
## #           Jan      Feb      Mar
## # 2020 21.48465 25.92072 24.44758
## # 2021 23.58856 30.17888 25.86366
## # 2022 19.29935 21.93085 19.95845
## #           Apr      May      Jun
## # 2020 31.20749 25.81880 27.23041
## # 2021 24.79478 22.27802 28.57599
## # 2022 26.61661 16.89589 26.72953
## #           Jul      Aug      Sep
## # 2020 20.92747 30.00173 19.22318
## # 2021 18.60330 41.17208 21.93337
## # 2022 24.36221 37.30163 24.03449
## #           Oct      Nov      Dec
## # 2020 22.48097 25.62920 25.67392
## # 2021 30.34842 26.83373 30.41383
## # 2022 24.22165 20.16990 23.44744

attributes(month_ts)

set.seed(101)
sales_tsb <- tsibble(
  year = 2000:2023,
  sales = rnorm(24,500, 80),
  index = year
)



gpm_tsb <- gapminder |> 
   as_tsibble(index = year
              ,key = c(continent, country))







a <- c(1,3,5)
b <- c(1,6,10)

a[a < 3]

a >= b

a[a!=b]

## 
## gapminder |>
##   filter(year == 1992)
## 
## # # A tibble: 142 × 6
## #    country     continent  year lifeExp
## #    <fct>       <fct>     <int>   <dbl>
## #  1 Afghanistan Asia       1992    41.7
## #  2 Albania     Europe     1992    71.6
## #  3 Algeria     Africa     1992    67.7
## #  4 Angola      Africa     1992    40.6
## #  5 Argentina   Americas   1992    71.9
## #  6 Australia   Oceania    1992    77.6
## #  7 Austria     Europe     1992    76.0
## #  8 Bahrain     Asia       1992    72.6
## #  9 Bangladesh  Asia       1992    56.0
## # 10 Belgium     Europe     1992    76.5
## # # ℹ 132 more rows
## # # ℹ 2 more variables: pop <int>,
## # #   gdpPercap <dbl>

## b <- c(1,6,10)
## 
## b[(b < 3) | (b >= 10)]
## 
## gapminder |>
##   filter((lifeExp < 40) | (lifeExp >= 80))
## 
## gapminder |>
##   filter((lifeExp < 40) & (lifeExp >= 80))
## 
## gapminder |>
##   filter((lifeExp >= 40) & (lifeExp < 80))
## 

x <- 10
if(x > 0){
  x + 10
}

x <- -5
if(x > 0){
  x + 10
}else{
  abs(x)
}


check_zlogic <- function(z){
  if(z <= 0){
    abs(z)
 }else if(z > 0 & z <= 9){
  z^2
 }else if(z > 10 & z <= 20){
   z + 10
 }else{
  print("z is beyond limit")
 }
}



check_zlogic(z = -4)
check_zlogic(z = 12)
check_zlogic(z = 35)

select_model <- function(x){
  switch(x,
  lm = print("Selected model: Linear regression"),
  ets = print("Selected model: Exponential smoothing"),
  arima = print("Selected model: ARIMA"),
  stop(str_c("A ",str_to_title(x), " model is not available")))
}


select_model("arima")


##  select_model("deep learning")
## 



score_cat <- tibble(
  score = 1:6,
  category = case_when(
                score < 3 ~ "Low",
                score %in% c(3,4) ~ "Medium",
               TRUE ~ "High"))


## score_cat
## 
## # # A tibble: 6 × 2
## #   score category
## #   <int> <chr>
## # 1     1 Low
## # 2     2 Low
## # 3     3 Medium
## # 4     4 Medium
## # 5     5 High
## # 6     6 High

## x <- 1
## while (x <= 5) {
##      print(x)
##      x <- x + 1
##      if (x == 4) {
##          stop("x reached 4. Loop terminated.")
##      }
## }

x <- 10:13
for(i in 1:4){
  print(x[i]^2)
}

mat_dt <- matrix(1:9, nrow=3,ncol=3)
mat_dt[2,3] <- NA
apply(mat_dt, 2, sum, na.rm=TRUE)


comb_lst <- list(
    dbl_vec = c(7, 10.0, 19.025),
    int_vec = c(0L,  7L,  11L),
    lg_vec = c(TRUE, FALSE,  TRUE),
    char_vec = c("this is", "a", "section of a book"))



lapply(comb_lst, summary)

set.seed(102)
dist_list <- list(
  normal = rnorm(n=50,mean=0,sd=1),
  binomial = rbinom(n=80, size=20, prob=0.3),
  poisson = rpois(30, lambda=4))


sapply(dist_list, mean)

vapply(dist_list, function(x){round(mean(x),2)}, numeric(1))

v1 <- 1:3
v2 <- 5:7

vec_multiply <- function(x, y) {
  return(x * y)
}



mapply(vec_multiply, v1, v2)


map_tab1 <- tribble(
  ~"Function name", ~ "Input", ~"Output" ,
  "\`map()`", "A list or vector" , "A list with the same length as the input list",
  "\`map_lgl()\`", "A list or vector" , "A logical vector", 
  "\`map_int()\`", "A list or vector" , "An integer vector",
  "\`map_dbl()\`", "A list or vector" , "A double(numeric) vector" ,
  "\`map_chr()\`" , "A list or vector" , "A character vector"
)



knitr::kable(map_tab1
             ,align = "ccl"
             ,format = "pandoc"
             ,caption = "Variations of the `map()` function" ) 


set.seed(103)
wtbl <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10))


## 
## map_dbl(wtbl, mean)
## 
## #           a           b           c           d
## # -0.35070052  0.17806119  0.07854021 -0.02179665
## 

## 
## wtbl |>
##   map_dbl(function(x){round(min(x)^2, 2)})
## 
## #    a    b    c    d
## # 3.48 0.87 2.18 4.60
## 
