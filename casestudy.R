library(tidyverse)
pgd <- read_csv("EDAcasestudy/payGapData.csv")
head(pgd)

a <-(pgd %>%
  filter(CurrentName == "UNIVERSITY OF SHEFFIELD"))

payGapData %>%
  filter(payGapMeasure == "DiffMeanHourlyPercent",
         year == "2019/20") #%>%
  summarise(meanGap = mean(value))

b <- (payGapData %>%
  filter(payGapMeasure == "DiffMeanHourlyPercent") %>%
  group_by(year, institution) %>%
  summarise(meanGap = mean(value)))

b1 <- (b %>%
         filter(institution == "pre-92"))

b2 <- (b %>%
         filter(institution == "post-92"))

ggplot(data = b1, aes(x = year, y = meanGap)) + 
  geom_histogram(colour = "blue", fill = "white", stat ='identity', binwidth = 1) +
  labs(x = "year", y = "meanGap")

ggplot(data = b2, aes(x = year, y = meanGap)) + 
  geom_histogram(colour = "blue", fill = "white", stat ='identity', binwidth = 1) +
  labs(x = "year", y = "meanGap")

ggplot(data = b1, aes(x = year, y = meanGap)) +
  geom_point(size = 3, alpha = 0.5, colour = "red") +
  labs(x = "",
       y = "Hourly wages pay gap of pre-92 universities(%)") +
  annotate("text", label = "20.27", x = "2017/18",
           y = 20.27, colour = "red", 
           hjust = -0.3, vjust = 1) +
  annotate("text", label = "19.51", x = "2018/19",
           y = 19.51, colour = "red", 
           hjust = -0.3, vjust = 1) +
  annotate("text", label = "18.44", x = "2019/20",
           y = 18.44, colour = "red", 
           hjust = -0.3, vjust = 1)

ggplot(data = b2, aes(x = year, y = meanGap)) +
  geom_point(size = 3, alpha = 0.5, colour = "red") +
  labs(x = "",
       y = "Hourly wages pay gap of post-92 universities(%)") +
  annotate("text", label = "13.33", x = "2017/18",
           y = 13.33, colour = "red", 
           hjust = -0.3, vjust = 1) +
  annotate("text", label = "12.64", x = "2018/19",
           y = 12.64, colour = "red", 
           hjust = -0.3, vjust = 1) +
  annotate("text", label = "12.43", x = "2019/20",
           y = 12.43, colour = "red", 
           hjust = -0.3, vjust = 1)

