#Exploratory Data  Analysis
1/(1000^3)
4 / 2*2
16 ^ 1/2
16 ^ (1/2)
x<-c(12,14,16,18,20)
x[2:4]
x[c(1,3,5)]
x[-3]

x <- factor(c("mild" ,"mild", "none", "severe"))
x
x <- factor(c("mild" ,"mild", "none", "severe"),
            levels = c("none", "mild" ,"moderate", "severe"))
x
responses <- c("yes","no","no")
responses <- factor(c("yes","no","no"))
responses

# Exercise 4.2 ----
responses <- factor(c("yes","no","no"),
                    levels = c("yes","no","undecided"))
responses

# ----
x <- 1:10
mean(x)
y <- c(5, 10, 6, 11, 7, 12)
sort(y)
sort(y, decreasing = TRUE)

seq(from = 1, to = 10, length = 19)
?seq
seq(1, 9, by = 2)
seq(1, 9, by = pi)
seq(1, 6, by = 3)

# Exercise 5.1 ----
sample(1:6, size = 6)
?sample
sample(1:6, size = 6, replace = TRUE)

# ----
mtcars
head(mtcars)
head(CO2)
mtcars$cyl
mtcars[2, ]
mtcars[2, 2]
?tibble
?tbl
mylist <- list(a = 1:10, b = "Monday")
names(mylist)
mylist$b
mylist$a
table(mylist)
morley
?morley

# Exercise 6.1 ----
z <- morley$Speed
mean(z)

# ----
sample(1:6,
       size = 6,
       replace = TRUE)

# Exercise 7.1 ----
x <- sample(2 * 1:10, size = 1)
y <- sample(2 * 1:10, size = 1)
x * y

# ----
x <- rep(2:4, each = 3)
sum(unique(x))
x <- 10:20
x
zoo::rollmean(x, k = 4)
library(zoo)
rollmean(x, k = 4)

# ----
getwd()
read_csv("myData.csv")

# R programming class----

# ----
x <- 12
y <- x**x
z <- x ^ 12
help(sum)
sum(4,7)
search()
library(wrangle)
help("constant")
x <- 2
x_squared <- x ^ 2
y_sequence <- 1:x_squared
even_sequence <- seq(2, 10, by = 2)
rnorm(n = 10, mean = 1,sd = 3)
search()
library(MASS)
