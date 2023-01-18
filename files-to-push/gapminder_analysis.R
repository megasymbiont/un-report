library(tidyverse)

getwd()
summarise(gapminder_data, averageLifeExp=mean(lifeExp), medianlifeExp=mean(lifeExp))

##piping

gapminder_sum<-gapminder_data%>%summarise(averageLifeExp=mean(lifeExp))

##filtering
gapminder_sum_2007<-gapminder_data%>%
  filter(year==2007)%>%summarize(average=mean(lifeExp))
##double = because it's a condition, not an argument (== logical operator)

##life exp in first year of data

gapminder_lifeExp_1952<- gapminder_data%>%
  filter(year==1952)%>%summarise(average=mean(lifeExp))

##alternative way to find earliest year
gapminder_data%>%
  summarize(Firstyear=min(year))

##using group_by()

gapminder_data%>%
  group_by(year)%>%
  summarize(average=mean(lifeExp))

gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average=mean(lifeExp),
            error=sd(lifeExp))

##mutate function-- adds new columns onto dataframe

gapminder_data%>%
  mutate(gdp=pop*gdpPercap)

gapminder_data%>%
  mutate(popmil=pop/1000000)

##select function

gapminder_data%>%
  select(pop, year)

##can use select to remove columns

gapminder_data%>%
  select(-continent)

##transforming data (long -> wide form)

##Pivot_wider
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from=year, values_from=lifeExp)%>%
  View()

##working with messy data

read_csv("un-report/co2-un-data.csv")

co2_emissions_messy<-read_csv("un-report/co2-un-data.csv", skip=2,
         col_names=c("region", "country", "year", "series",
                     "value", "footnotes", "source"))
co2_emissions_cleaned<-co2_emissions_messy%>%
  select(country, year, series, value)%>%
  mutate(series=recode(series, "Emissions (thousand metric tons of carbon dioxide)"="total_emissions",
                       "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%
  pivot_wider(names_from=series, values_from = value)

## combine emissions data with the gapminder_data. they have country and year in common

##discrepancies between years (2005 only in the emissions data, 2007 for the population data) so need to filter

co2_emissions<-co2_emissions_cleaned%>%
  filter(year==2005)%>%
  select(-year)


#note: filter is for rows; select is for columns

##bringing in 2007 pop data

gapminder_2007<-read.csv("un-report/gapminder_data.csv")%>%
  filter(year==2007)%>%
  select(country, pop, lifeExp, gdpPercap)

##merge tables using join 
##inner_join discards data not shared between tables (i.e., variable is shared between them ('country') but not a certain value, like a certain country)

inner_join(co2_emissions, gapminder_2007, by="country")

##anti_join identifies values that are not shared between two data frames

anti_join(co2_emissions, gapminder_2007, by="country")
##prints table of countries that do not match gapminder_2007

##order makes a difference with anti_join
anti_join(gapminder_2007, co2_emissions, by="country")

full_join(co2_emissions, gapminder_2007)

##using left join

co2_emissions%>%
  left_join(gapminder_2007)

##want to find relationship between emissions and GDP, so using the inner_join data frame


joined_co2_pop<-inner_join(co2_emissions, gapminder_2007, by="country")

##save this object to a csv using write
write_csv(joined_co2_pop, file= "un-report/joined_co2_pop.csv")

##practicing reading the file that was just made
co2_and_pop<-read_csv("un-report/joined_co2_pop.csv")

##plot

ggplot(data=co2_and_pop)+
  aes(x=per_capita_emissions) + aes(y=gdpPercap) +labs(x="Emissions per capita")+ labs(y="GDP Per Capita")+ labs(title="Relationship between GDP and CO2 Emission")+ geom_point(color="cornflower blue") + theme_bw()

##histogram for each variable to see distributions

ggplot(data=co2_and_pop)+
  aes(x=per_capita_emissions)+ geom_histogram() 

ggplot(data=co2_and_pop)+
  aes(x=gdpPercap)+ geom_histogram()+theme_bw()

ggsave("gdp_emission_plot.jpg", width=6, height=4)
