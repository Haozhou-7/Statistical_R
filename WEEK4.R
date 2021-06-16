library(tidyverse)
install.packages("devtools")
devtools::install_github("OakleyJ/MAS6005")

#这种图不能出现在报告里，要有轴标签，单位和标题
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(x = "Weight (lb/1000)", y = "Miles / (US) gallon") 

#图片里加title和caption
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(x = "Weight (lb/1000)", y = "Miles / (US) gallon",
       title = "Fuel economy and weight for 32 car models",
       subtitle = "Heavier cars tend to be less fuel efficient",
       caption = "Source: Motor Trend (1974)") 

#refining a plot: an example
search()
library(MAS6005)
library(gridExtra)

p1 <- mvscores %>%
  filter(innings == "first", captain == "yes") %>%
  ggplot(aes(x = runs))+
  labs(x = "First innings runs, captain") +
  geom_histogram()

p2 <- mvscores %>%
  filter(innings == "first", captain == "no") %>%
  ggplot(aes(x = runs))+
  labs(x = "First innings runs, not captain") +
  geom_histogram()

p3 <- mvscores %>%
  filter(innings == "second", captain == "yes") %>%
  ggplot(aes(x = runs))+
  labs(x = "Second innings runs, captain") +
  geom_histogram()

p4 <- mvscores %>%
  filter(innings == "second", captain == "no") %>%
  ggplot(aes(x = runs))+
  labs(x = "Second innings runs, not captain") +
  geom_histogram()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

#上面的改进，刻度一致化，加lable
ggplot(mvscores, aes(x = runs, y = ..density..))+
  labs(x = "Runs scored") +
  geom_histogram() +
  facet_grid(rows = vars(captain),
             col = vars(innings),
             labeller = label_both)

#使用箱型图表示
ggplot(mvscores, aes(x = innings, y = runs))+
  geom_boxplot(aes(color = captain) )

# Exercise 19.1 ----
#1
?airquality
ggplot(airquality, aes(x = Temp, y = Ozone)) +
  geom_point()+
  labs(x = "Maximum daily temperature in degrees Fahrenheit", 
       y = "Mean ozone in parts per billion from 1300 to 1500 hours")

#2 repel可以自动在图中加标注 注意如何把x轴刻度对数化
ggplot(MAS6005::medals, aes(x = population / 1000000, y = gold, label = country)) +
  geom_point()+
  labs(x = "population size(millions)", y = "gold medals won") +
  scale_x_log10() + 
  ggrepel::geom_text_repel(data = filter(MAS6005::medals, population > 10^9))

#3 
colVector <- rep("grey", 36)
colVector[35] <- "red"

ggplot(inequality, aes(y = reorder(country,gini), x = gini)) +
  geom_col(fill = colVector) +
  labs(y = "", x = "Gini coefficient") +
  theme_classic() +
  geom_vline(xintercept = c(0.1, 0.2, 0.3, 0.4), col = "white") +
  theme(axis.line = element_blank())
  
