##analyze gapminder data for life expectancy and climate pollution versus population
library(tidyverse)

##read csv
gapminder_1997 <- read_csv("un-report/gapminder_1997.csv")


##plotting data
ggplot(data=gapminder_1997)+
         aes(x=gdpPercap) + aes(y=lifeExp) +labs(x="GDP Per Capita")+ labs(y="Life Expectancy (yrs)")+ geom_point() +
  labs(title="Do people in wealthier countries live longer?") +
  aes(color=continent) +
  scale_color_brewer(palette="Set1") +
  aes(size=pop/1000000) + labs(size= "Population in millions") +
  aes(shape=continent)

##edit shapes in geom_point
##same as above, but in short hand

ggplot(data=gapminder_1997, 
       aes(x=gdpPercap, y=lifeExp, color= continent, size= pop)) +
         labs(x="GDP Per Capita", y= "Life expectancy (yrs)",
              title="Do people in wealthier countries live longer?", size="population in millions")+
  geom_point()

##Read in all of the data from gapminder (beyond 1997)

gapminder_data<- read_csv("un-report/gapminder_data.csv")

##view dimensions
dim(gapminder_data)

##view heading
head(gapminder_data) ##returns a tibble with first few rows and columns

##and can look at the bottom
tail(gapminder_data)

##alternate way to view data with structure command
str(gapminder_data)



##plotting with ggplot

ggplot(data=gapminder_data) + aes(x=year, y=lifeExp, color=continent) +
geom_point()

##create a boxplot 
?geom_boxplot

ggplot(data=gapminder_data) + aes(x=continent, y=lifeExp, color=continent) +
  geom_boxplot()

##make it a line graph
ggplot(data=gapminder_data) + aes(x=year, y=lifeExp, color=continent, group=country) +
  geom_line()

##violin plots; geom_jitter adds individual points
##use 'fill' and 'color' arguments within geom_violin to change color of violin plots

ggplot(data=gapminder_1997) + aes(x=continent, y=lifeExp, color=continent) +
  geom_violin() + geom_jitter(aes(size=pop)) 

##switch order of violin and jitter commands --> violin covers points

ggplot(data=gapminder_data) + aes(x=continent, y=lifeExp, color=continent) +
  geom_jitter() + geom_violin() 

##univariate plots (i.e., histogram)

ggplot(data=gapminder_1997)+ aes(x=lifeExp)+geom_histogram(bins=20) +
  theme_minimal()

##using facets

ggplot(data=gapminder_1997) + 
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_wrap(vars(continent))  ##variables=vars

ggplot(data=gapminder_1997) + 
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_grid(rows=vars(continent))

##saving plots

ggsave("awesome_plot.jpg", width=6, height=4)

##ggsave will save the last plot created by default, but can assign previous plots to an object then save that object

