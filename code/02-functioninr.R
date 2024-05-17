# Chapter: Functions in R  

# section: Objects and its name ----
x1 <- c(1,2,3,4,5)
x1
x2 <- 1:5
x2


.x <- 1:5
.x

x_y <- 1:5
x_y

## The below lines are commented to avoid breaking this script
# _y <- 1:5
# 
# 1x <- 1:5
# 
# if <- 1:5
# 
# a b <- 1:5


`_y` <- 1:5
`_y`

`a b` <- 1:5
`a b`


# avoid using double quotes
"a b" <- 1:5
"a b"


# section: Data types
# subsection: Atomic Vectors

# Double
dbl_vec <- c(7, 10.0, 19.025)
dbl_vec

typeof(dbl_vec)

is.double(dbl_vec)

is.integer(dbl_vec)

# set the names attributes to the object dbl_vec
attr(dbl_vec, "names") <- c('a', 'b', 'c')
dbl_vec

# get the names attributes from the dbl_vec object
attr(dbl_vec, "names")

# equivalently
attributes(dbl_vec)


# Integer
int_vec <- c(0L,7L,11L)
int_vec
#[1]  0  7 11
##
typeof(int_vec)
#[1] "integer"
##
int_vec2 <- c(0,7,11)
typeof(int_vec2)
# [1] "double"
##


# Logical
lg_vec <- c(TRUE, FALSE, TRUE)

typeof(lg_vec)

is.logical(lg_vec)

# which returns the indices of TRUE
which(lg_vec)

abs(lg_vec)

sum(lg_vec)

mean(lg_vec)



# Character
char_vec <- c("this is", "a", "section of a book")

typeof(char_vec)

is.character(char_vec)


# Lists
char_vec2 <- c("This book\t discussed \n\"Time Series Forecasting\".")
print(char_vec2)

writeLines(char_vec2)


# calculate number of characters in each element
nchar(char_vec)
#[1]  7  1 17


lst1 <- list(
  dbl_vec
  ,int_vec
  ,lg_vec
  ,char_vec
)

lst1

typeof(lst1)

is.list(lst1)

# create an empty list and assign list items
emp_lst <- list()
emp_lst[[1]] <- 1:5
emp_lst[[2]] <- c(letters[1:4])

emp_lst


str(lst1)

glimpse(emp_lst)


# create a nested list
lst2 <- list(
  lst1
  ,list(
    seq(0,20,5)
    ,c(TRUE, TRUE, FALSE)
  )
)
# better display of the internal structure
glimpse(lst2)



attributes(lst2)

attributes(lst2[[1]][[1]])


# Matrix
a <- 1:9
dim(a) <- c(3,3)
a

attributes(a)

is.matrix(a)

a_mat <- matrix(c(rep(TRUE,3), rep(TRUE,3), rep(FALSE,3))
                ,nrow=3
                ,ncol=3)
a_mat

attributes(a_mat)

rownames(a) <- c("a", "b","c")
colnames(a) <- paste0("col_",1:3)

a

dimnames(a_mat) <- list(paste0("row_",1:3),
                    paste0("col_",1:3))
a_mat

diag(3)


# Array
b_arry <- array(1:18, c(3,3,2))
b_arry

dim(b_arry)

# Coercion
# Integer overwrite logical
cr1 <- c(1L, TRUE, FALSE, 5L)
typeof(cr1)
#[1] "integer"
##
# Character coerce all other data types
cr2 <- c("Pen", 5.30, TRUE )
cr2
typeof(cr2)


as.double(c(TRUE, FALSE, FALSE))

as.double(c(TRUE, FALSE, FALSE, 1.5))

as.double(c(TRUE, FALSE, "Paper", 1.5))


# Factor1

ch_vec <- rep(c("a", "b","c"), times=2)
typeof(ch_vec)

# create factor
fct_vec <- factor(ch_vec)

attributes(fct_vec)

typeof(fct_vec)

cust_resp1 <- ordered(c("bad", "good", "good", "fair")
                     ,levels = c("bad", "fair", "good"))
cust_resp1

cust_resp2 <- factor(c("bad", "good", "good", "fair")
                     ,levels = c("bad", "fair", "good")
                     ,labels = c("Bad", "Fair", "Good"))
cust_resp2


score <- c(0, 10, 2, 6, 8, 8, 1, 9)

fct_score <- cut(score
                 ,breaks=c(-Inf, 7, 9, Inf)
                 ,labels = c("Distractor", "Neutral", "Promoter")
                 ,include_lowest = TRUE)
fct_score

table(fct_score)

sloop::otype(fct_vec)

sloop:otype(ch_vec)


# Dates

date_vec <- as.Date("2023-01-01")
typeof(date_vec)

attributes(date_vec)

sloop::otype(date_vec)

unclass(Sys.Date())


# Date-times
dtm_vec <- as.POSIXct("2023-01-01 22:41:00", tz = "Australia/Melbourne")
dtm_vec

attributes(dtm_vec)

# origin needs to supply to convert a numeric to Dates
as.POSIXct(8^10, origin = "1970-01-01")


# Duration
dur1 <- as.Date("2022-10-30") - as.Date("2019-03-16")
dur1

typeof(dur1)

attributes(dur1)

dur2 <- as.difftime(14, units="weeks")
dur2


# section: S3 lists 
# Data frames
df1 <- data.frame(country = c(rep("USA",2), rep("AUS",2))
                  ,date = as.Date(rep(c("2000-01-01","2023-01-01"), times = 2))
                  ,population_mil = c(282.2, 334.3, 19.03, 26.6) )

df1

typeof(df1)

attributes(df1)


data.frame(id = paste0("00", c(1:6))
           ,score = c(10:14))

data.frame(id = paste0("00", c(1:6))
           ,score = c(10:12))



df2 <- data.frame(`1` = c("rock", "paper", "scissors")
                  ,`2` = c(5,3,2))
names(df2)

df3 <- data.frame(`1` = c("rock", "paper", "scissors")
                  ,`2` = c(5,3,2)
                  ,check.names = FALSE)

names(df3)

df4 <- data.frame(option = c("rock", "paper", "scissors")
                  ,n = c(5,3,2)
                  ,stringsAsFactors = TRUE)

attributes(df4$option)


row.names(df4) <- paste0("game_", 1:3)
df4

# Tibbles
install.packages("tibble")
library(tibble)

tb1 <- tibble(country = c(rep("USA",2), rep("AUS",2))
                  ,date = as.Date(rep(c("2000-01-01","2023-01-01"), times = 2))
                  ,population_mil = c(282.2, 334.3, 19.03, 26.6) )
tb1

typeof(tb1)

attributes(tb1)

set.seed(02)
tb2 <- tibble(x = 1:1e6, y = rnorm(1e6))

tb2 |> head()

tb3 <- tibble(option = c("rock", "paper", "scissors")
                  ,n = c(5,3,2))

str(tb3)

tb4 <- tibble(x = 0:4
              ,y = x^2
              ,z = 2*y )
tb4

tb5 <- as_tibble(df4, rownames = "game_name")
tb5

install.packages("tidyverse")
library(tidyverse)

# make a tibble of mtcars data set
tb_mtcars <- as_tibble(mtcars)

# create a list column tibble
tb6 <- tb_mtcars |>
     group_by(gear) |>
    nest() |>
    mutate(model = map(.x = data,
                       ~lm(mpg~., data = .x)))

tb6

# section:  Missing values

mv_vec1 <- c(2, NA, 1, NA, 0)
is.na(mv_vec1)

anyNA(mv_vec1)

mv_vec2 <- c("NA", NA)
is.na(mv_vec2)


mv_lst <- list(1:3
               ,mv_vec1
               ,mv_vec2)

# wrong application
is.na(mv_lst)

anyNA(mv_lst)

# correct application
sapply(mv_lst, is.na)

sapply(mv_lst, anyNA)

is.nan(0/0)

is.nan(5/0)

is.nan(c(5, NA))

# use of NULL
c()

my_list <- NULL
my_list


#section:  Time Series Specific Objects
# `ts` object
set.seed(100)
ts1 <- ts(rnorm(36,25,7)
          ,start = "2020"
          ,frequency=12)

ts1

sloop::otype(ts1)

typeof(ts1)

attributes(ts1)

as.ts(c(1:100))


# tsibble
install.packages("tsibble")
library(tsibble)

set.seed(101)
tsb1 <- tsibble(
  year = 2000:2023
  ,sales = rnorm(24,500, 80)
  ,index = year
)

tsb1 |> head()


class(austres)

austres_tsb <- as_tsibble(austres)

head(austres_tsb)



install.packages("gapminder")
library(gapminder)

gpm_tsb <- gapminder |>
            as_tsibble(index = year
                       ,key = c(continent, country))

gpm_tsb

df5 <- data.frame(country = c(rep("USA",4))
                  ,date = as.Date(rep(c("2000-01-01","2023-01-01"), times = 2))
                  ,population_mil = c(282.2, 334.3, 19.03, 26.6) )

df5_tsb <- as_tsibble(df5
                      ,index=date
                      ,key = country)

duplicates(df5)

df5_c <- data.frame(country = c(rep("USA",2), rep("AUS",2))
                  ,date = as.Date(rep(c("2000-01-01","2023-01-01"), times =2))
                  ,population_mil =c(282.2, 334.3, 19.03, 26.6) )
as_tsibble(df5_c
           ,index=date
           ,key = country)


# section:  Functions in R 

my_func <- function(arg1,arg2){

  # body full of codes

  .....

##

  # return output

}


# Comparisons 
a <- c(1,3,5)
b <- c(1,6,10)

a[a < 3]

a >= b

a[a!=b]


install.packages("dplyr")
library(dplyr)
##
# data for the year 1992 only
gapminder |>
  filter(year == 1992)

gapminder |>
  filter(continent != "Asia")


# Conditions 
b <- c(1,6,10)
##
b[(b < 3) | (b >= 10)]
# [1]  1 10
##
gapminder |>
  filter((lifeExp < 40) | (lifeExp >= 80))

gapminder |>
  filter((lifeExp < 40) & (lifeExp >= 80))

gapminder |>
  filter((lifeExp >= 40) & (lifeExp < 80))



# Conditional Execution 

# if statement
x <- 10
if(x > 0){
  x + 10
}

# if-else
x <- -5
if(x > 0){
  x + 10
}else{
  abs(x)
}



# multiple if-else
multi_cond <- function(z){
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

multi_cond(z=-4)

multi_cond(35)

# switch
choose_fruit <- function(x){
  switch(x,
  apple = print("Selected fruit: Apple"),
  banana = print("Selected fruit: Banana"),
  orange = print("Selected fruit: Orange"),
  stop("Unknown fruit in the basket")
)
}

choose_fruit("apple")

choose_fruit("strawberry")



library(dplyr)
tb7 <- tibble(score = 1:6
              ,category = case_when(
                score < 3 ~ "Low",
                score %in% c(3,4) ~ "Medium",
               TRUE ~ "High")
              )
tb7



# Iterations

# while
x <- 1
# Using while loop with stop to increment x until it reaches 5
while (x <= 5) {
  # expression1
  print(x)
  # expression2
  x <- x + 1
  # Check if x is equal to 4 and terminate the loop
  if (x == 4) {
    stop("x reached 4. Loop terminated.")
  }
}

# for loop
x <- 10:13
for(i in 1:3){
  print(x[i]^2)
}


# apply 
mat_dt <- matrix(1:9, nrow=3,ncol=3)
# allocate NA to show the use of ... inside apply
mat_dt[2,3] <- NA
mat_dt

# apply sum on each column with na.rm sa additional argument of sum
apply(mat_dt, 2, sum, na.rm=TRUE)

# Create a data frame
df6 <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6), z = c(7, 8, 9))

# A custom function to calculate the product of elements in a vector
prod_func <- function(vec) {
  return(prod(vec))
}

# Apply the custom function to each row of the data frame
apply(df6, 1, prod_func)
# [1]  28  80 162


glimpse(lst1)

lapply(lst1, summary)



set.seed(102)
lst3 <- list(d1 = rnorm(n=5,mean=0,sd=1)
             ,d2 = rbinom(n=8, size=20, prob=0.3)
             ,d3 = rpois(3, lambda=4))

glimpse(lst3)

sapply(lst3, range )


# vapply example with a custom function
vapply(lst3, function(x){round(min(x),1)}, numeric(1))


# maplly
# Create two vectors
vec1 <- 1:3
vec2 <- 5:7

# Define a function to multiply corresponding elements of two vectors
multiply_function <- function(x, y) {
  return(x * y)
}

# Apply the multiply_function to corresponding elements of the vectors using mapply
mapply(multiply_function, vec1, vec2)


# map 
set.seed(103)
wdt <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

map_dbl(wdt, mean)

# OR using piping
wdt |>
  map_dbl(function(x){min(x)^2})


