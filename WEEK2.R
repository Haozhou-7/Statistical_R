library(tidyverse)
getwd()
medals <- read_csv("EDAdata/RIOolympics.csv", col_names = TRUE, skip = 0)
head(medals)
?read_csv

population <- readxl::read_excel("EDAdata/population.xlsx")#注意sheet和range两个参数
head(population)

flints <- read_table("EDAdata/flintsdata.txt")
head(flints)

maths <- read_csv("http://www.jeremy-oakley.staff.shef.ac.uk/mas113/maths.csv")
head(maths)

write_csv(maths, path = "EDAdata/myCopyOfMaths.csv")
download.file(url = "http://www.jeremy-oakley.staff.shef.ac.uk/mas113/maths.csv",
              destfile = "EDAdata/myCopyOfMaths.csv")

population$`Country Name`
population$`2015`
population$`Country Code`
population$`Country Code`
medals$`code`

?inner_join
olympics <- inner_join(population, medals,
                       by = c("Country Code" = "code"))
olympics <- inner_join(medals, population,
                       by = c("code" = "Country Code"))

head(olympics)

df1 <- data.frame(x = c(10, 11), y = c(100, 101))
df2 <- data.frame(x = c(12, 13), y = c(102, 103))
df1
df2
rbind(df1, df2)
myList <- list(df1, df1)
myList
do.call(rbind.data.frame, myList)

olympics <- rename(olympics, `population2015` = `2015`)
olympics <- rename(olympics, `2015` = `population2015`)
head(olympics)

countries <- read_csv("EDAdata/populationMultiple.csv")
head(countries)
countriesLong <- pivot_longer(countries,
                              cols = -`Country Name`,
                              names_to = "year",
                              values_to = "population")
head(countriesLong)

countriesWide <- pivot_wider(countriesLong, 
                             names_from = year,
                             values_from = population)
head(countriesWide)

# Exercise 12.1 ----
votes <- read_csv("EDAdata/BrexitVotes.csv")
head(votes)
edu <- read_csv("EDAdata/education.csv")
head(edu)
Brexit <- inner_join(votes, edu,
                       by = "Area_Code")
head(Brexit)

cancer <- readxl::read_excel("EDAdata/cancer.xlsx")
head(cancer)
cancerLong <- pivot_longer(cancer,
                              cols = everything(),
                              names_to = "organ",
                              values_to = "survival time")
head(cancerLong)
# ----

?xtable
library(xtable)
xtable(flints)

# ----
maths <- read_csv("http://www.jeremy-oakley.staff.shef.ac.uk/mas113/maths.csv")
maths %>%
  arrange(score)

maths %>%
  arrange(desc(score))
a <- arrange(maths, desc(score))

maths %>%
  filter(start.age == 5)
maths %>%
  filter(country == "United Kingdom")

maths %>%
  select(score, country)

# extract the values from a column
maths$score
mean(maths$score)

# Creating new columns in a data frame with the mutate() command
maths$gdp > 17000
maths <- maths %>%
  mutate(wealthiest = maths$gdp > 17000)
maths
maths %>%
  select(country, gdp, wealthiest)

maths %>%
  group_by(continent) %>%
  summarise(meanscore = mean(score))

maths %>%
  group_by(continent, wealthiest) %>%
  summarise(meanscore = mean(score)) 

maths %>%
  group_by(continent, wealthiest) %>%
  summarise(meanscore = mean(score)) %>%
  pivot_wider(names_from = wealthiest, values_from = meanscore)

# Counting observations within groups
maths %>%
  count(continent)

maths %>%
  count(continent, start.age)

maths %>%
  count(continent, start.age) %>%
  pivot_wider(names_from = start.age, values_from = n)

# Exercise 13.1 ----
#1
Brexit %>%
  arrange(desc(Pct_Remain))

#2
Brexit %>%
  group_by(Region) %>%
  summarise(meanpercentage = mean(Pct_Remain))

Brexit %>%
  group_by(Region) %>%
  summarise(meanpercentage = mean(Pct_Remain)) %>%
  arrange(desc(meanpercentage))


#3
B <- Brexit %>%
  mutate(majorityRemain = Pct_Remain > 50)
B

B <- Brexit %>%
  mutate(majorityRemain = Pct_Remain > 50) %>%
  select(-district)#去掉这一列
head(B)
mean(B$level4)

Brexit %>%
  mutate(majorityRemain = Pct_Remain > 50) %>%
  group_by(majorityRemain) %>%
  summarise(meanlevel4 = mean(level4))
