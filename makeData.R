# Import the data ----

paste0("EDAcasestudy/", dir("EDAcasestudy/"))

library(tidyverse)

pay17 <- read_csv("EDAcasestudy/UK Gender Pay Gap Data - 2017 to 2018.csv")
pay18 <- read_csv("EDAcasestudy/UK Gender Pay Gap Data - 2018 to 2019.csv")
pay19 <- read_csv("EDAcasestudy/UK Gender Pay Gap Data - 2019 to 2020.csv")
universities <- read_csv("EDAcasestudy/universities.csv")

# Fix errors in the universities data ----

man <- which(universities$EmployerName == "The University Of Manchester")
universities[man, ]

south <- which(universities$EmployerName == "University of Southampton")
universities[south,]

universities[man, "institution"] <- "pre-92"
universities[south[2], "EmployerName"] <- "Solent University"

# Merge the datasets ----

## How to handle different years? 

pay17 %>%
  mutate(year = "2017/18") %>%
  inner_join(universities, by = "EmployerName") %>%
  select(EmployerName, institution, year, DiffMeanHourlyPercent) %>%
  pivot_longer(-c(EmployerName, institution, year), 
               names_to = "payGapMeasure",
               values_to = "value")

## Making the long format dataframes

colnames(pay17)
nonPayGapColumns <- colnames(pay17)[c(1:4, 19:25)]

pay17Long <- pay17 %>%
  mutate(year = "2017/18") %>%
  inner_join(universities, by = "EmployerName") %>%
  pivot_longer(-c(nonPayGapColumns, institution, year), 
               names_to = "payGapMeasure",
               values_to = "value")
pay17Long %>%
  select(EmployerName, year, payGapMeasure, value)


pay18Long <- pay18 %>%
  mutate(year = "2018/19") %>%
  inner_join(universities, by = "EmployerName") %>%
  pivot_longer(-c(nonPayGapColumns, institution, year), 
               names_to = "payGapMeasure",
               values_to = "value")

pay19Long <- pay19 %>%
  mutate(year = "2019/20") %>%
  inner_join(universities, by = "EmployerName") %>%
  pivot_longer(-c(nonPayGapColumns, institution, year), 
               names_to = "payGapMeasure",
               values_to = "value")

payGapData <- rbind(pay17Long, pay18Long, pay19Long)

write_csv(payGapData, "EDAcasestudy/payGapData.csv")


# Compute some summary statistics ----

unique(payGapData$payGapMeasure)


payGapData %>%
  filter(payGapMeasure == "DiffMeanHourlyPercent",
         year == "2019/20") %>%
  summarise(meanGap = mean(value))

payGapData %>%
  filter(payGapMeasure == "DiffMeanHourlyPercent") %>%
  group_by(year, institution) %>%
  summarise(meanGap = mean(value))

