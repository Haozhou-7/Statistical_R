library(tidyverse)
search()
maths <- read_csv("http://www.jeremy-oakley.staff.shef.ac.uk/mas113/maths.csv") %>%
  mutate(wealthiest = gdp > 17000)
# 半透明（alpha）散点图 带横纵坐标解释
ggplot(data = maths, aes(x = gdp, y = score)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(x = "GDP per capita (US$)",
       y = "Mean PISA mathematics score, 2015")

# 默认实心散点 无坐标注释
ggplot(data = maths, aes(x = gdp, y = score)) +
  geom_point()

# 加图中注释 高亮某个散点
ggplot(data = maths, aes(x = gdp, y = score)) +
  geom_point() +
  labs(x = "GDP per capita (US$)",
       y = "Mean PISA mathematics score, 2015") +
  annotate("point", x = 39899, y = 492,
           colour = "red", size = 2) +
  annotate("text", label = "UK", x = 39899,
           y = 492, colour = "red", 
           hjust = -0.2, vjust = 1)

# 加入趋势，蓝线是估计曲线，灰色区域是趋势不确定区域，散点越少，区域越宽
ggplot(data = maths, aes(x = gdp, y = score)) +
  geom_point() +
  labs(x = "GDP per capita (US$)", 
       y = "Mean PISA mathematics score, 2015") +
  annotate("point", x = 39899, 
           y = 492, colour = "red", 
           size = 2) +
  annotate("text", label = "UK", 
           x = 39899, y = 492, 
           colour = "red", 
           hjust = -0.2, vjust = 1) +
  geom_smooth()

# 线性趋势
ggplot(data = maths,  
       aes(x = gini, y = score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Gini coeffcient", 
       y = "Mean PISA mathematics score, 2015") 

# 按大洲分配散点颜色（aes函数内嵌好的，也可以自己定义）
ggplot(data = maths, 
       aes(x = gdp, y = score)) +
  geom_point(aes(colour = continent)) +
  labs(x = "GDP per capita (US$)", 
       y = "Mean PISA mathematics score, 2015")

# 按开始年龄分配散点颜色（aes函数内嵌好的）
ggplot(data = maths, 
       aes(x = gdp, y = score)) +
  geom_point(aes(colour = start.age)) +
  labs(x = "GDP per capita (US$)", 
       y = "Mean PISA mathematics score, 2015")

# 上面年龄的色标是连续的，但实际上这里的年龄只有三个选项，所以应用定性的色标，用factor函数可以做到
ggplot(data = maths, 
       aes(x = gdp, y = score)) +
  geom_point(aes(colour = factor(start.age))) +
  labs(x = "GDP per capita (US$)", 
       y = "Mean PISA mathematics score, 2015")

# 默认直方图
ggplot(data = maths, aes(x = score)) + 
  geom_histogram()

# 默认概率密度直方图（所有区域的面积和是1）
ggplot(data = maths, aes(x = score, y = ..density..)) + 
  geom_histogram()

# 个性化直方图，可改变外框颜色，填充颜色，bar的宽度，还可以加图中注释
ggplot(data = maths, aes(x = score)) + 
  geom_histogram(colour = "blue", fill = "white", binwidth = 10) +
  labs(x = "Mean PISA mathematics score, 2015") +
  annotate("point", x = 492, y = 0,  size = 4, colour = "red") 

# 箱型图
ggplot(data = maths, aes(x = continent, y = score)) +
  geom_boxplot() +
  labs(y = "Mean PISA mathematics score, 2015")

# 坐标互换，因为很难将左右标签放在x轴
ggplot(data = maths, aes(y = continent, x = score)) +
  geom_boxplot() +
  labs(x = "Mean PISA mathematics score, 2015",
       y = "") 

# do a box plot of ‘score’ by ‘start.age’
ggplot(data = maths, aes(x = as.factor(start.age), y = score)) +
  geom_boxplot() +
  labs(x = "School starting age", 
       y =  "Mean PISA mathematics score, 2015")

# 将观测值添加到箱形图中，如果组内点的分布是双峰的，则也可以显示单个观察值。
ggplot(data = maths, aes(x = as.factor(start.age), y = score)) +
  geom_boxplot() +
  labs(x = "School starting age", 
       y =  "Mean PISA mathematics score, 2015") +
  geom_jitter(width = 0.1)

# 小提琴图
ggplot(data = maths, aes(x = as.factor(start.age), y = score)) +
  geom_violin() +
  labs(x = "School starting age", 
       y =  "Mean PISA mathematics score, 2015")

# ‘全局’美学 更清楚地判段变量
ggplot(data = maths, 
       aes(x = gini, y = score)) +
  geom_point(aes(colour = wealthiest)) +
  labs(x = "Income inequality (Gini coefficient)", 
       y = "Mean PISA mathematics score, 2015") + 
  geom_smooth(method = "lm")

# 变量颜色在不同的函数中会有不一样的效果，注意和上面的区别
ggplot(data = maths, 
       aes(x = gini, y = score, colour = wealthiest)) +
  geom_point() +
  labs(x = "Income inequality (Gini coefficient)", 
       y = "Mean PISA mathematics score, 2015") + 
  geom_smooth(method = "lm")

# facets 创建一个具有多个并排或以网格排列的图形。如果图的类型相同，坐标轴相同，则可以使用facets
ggplot(maths, aes(x = gdp, y = score)) + 
  geom_point() + 
  facet_grid(cols = vars(start.age))

# Exercise 18.1 ----
#1
ggplot(data = Brexit, 
       aes(x = Pct_Remain, y = level4)) +
  geom_point(aes(colour = Region), alpha = 0.5) +
  labs(x = " percentage voting to remain in the EU (within each local authority)", 
       y = "percentage of adults with Level 4 qualifications") + 
  geom_smooth(method = "lm")
#注意上下两种的区别
ggplot(data = Brexit, 
       aes(x = Pct_Remain, y = level4, colour = Region)) +
  geom_point(alpha = 0.5) +
  labs(x = " percentage voting to remain in the EU (within each local authority)", 
       y = "percentage of adults with Level 4 qualifications") + 
  geom_smooth(method = "lm")

#2
Brexit %>%
  filter(Area == "Sheffield")

ggplot(data = Brexit, aes(x = Pct_Remain)) + 
  geom_histogram(colour = "blue", fill = "white", bins = 10) +
  labs(x = "voting to remain") +
  annotate("point", x = 49, y = 0,  size = 4, colour = "red") 

#3
ggplot(data = Brexit, aes(x = Region, y = Pct_Remain)) +
  geom_boxplot() +
  labs(y = "percentage voting to remain in the EU")


ggplot(data = Brexit, aes(y = Region, x = Pct_Remain)) +
  geom_boxplot() +
  labs(y = "", x = "percentage voting to remain in the EU") 

#根据median重新排序
ggplot(data = Brexit,
       aes(y = reorder(Region, Pct_Remain, FUN = median),
           x = Pct_Remain)) +
  geom_boxplot() +
  labs(y = "", x = "percentage voting to remain in the EU") 
