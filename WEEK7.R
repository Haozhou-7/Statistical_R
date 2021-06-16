# 17----
x <- c(2, 4, NA, 6, NA)
is.na(x)
mean(x)
mean(x, na.rm = TRUE)
mean(na.omit(x))
y <- 11:15
myData <- data.frame(x, y)
myData
na.omit(myData)
plot(x)

set.seed(123)
x <- signif(1:10 + rnorm(10), 3)
x[c(3, 4, 8)] <- NA
imputeTS::na_mean(x)
imputeTS::na_locf(x)
imputeTS::na_interpolation(x)
imputeTS::na_kalman(x)
imputeTS::ggplot_na_distribution(x)
imputeTS::ggplot_na_imputations(x,
                                imputeTS::na_kalman(x))

# 17.1----
head(airquality)
imputeTS::ggplot_na_distribution(airquality$Solar.R)
imputeTS::ggplot_na_imputations(airquality$Ozone,
                                imputeTS::na_locf(airquality$Ozone))
imputeTS::ggplot_na_imputations(airquality$Ozone,
                                imputeTS::na_kalman(airquality$Ozone))

# 20----
library(leaflet)









