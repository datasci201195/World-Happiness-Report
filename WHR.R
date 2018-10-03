
#Necessary Packages

library(ggplot2)
library(dplyr)
library(stringr)
library(formattable)
library(ggthemes)
library(corrplot)

#Importing data set

happiness <- read.csv('2017.csv',stringsAsFactors = F)
openness  <- read.csv('countries.csv',stringsAsFactors = F)

#Exploring data set

str(happiness)
str(openness)

summary(happiness)
summary(openness)

View(happiness)
View(openness)

#To check for unique rows using pipe operator
happiness %>%
group_by(Country) %>%
mutate(n = n()) %>%
filter(n > 1)

openness %>%
group_by(Country.Name) %>%
mutate(n = n()) %>%
filter(n > 1)

#correcting error for Russia country
openness$Country.Name[62] <- "Russia"

#changing coloum names

colnames(openness)[2] <- "Country"

#Joining two data table by "Country"

open_happy <- happiness %>%
              left_join(openness,by = "Country") %>%
              mutate(Country = factor(Country)) %>%
              select(Country,Happiness.Score,X2015.Score,Economy..GDP.per.Capita.,Family,Health..Life.Expectancy.,Freedom,Generosity,Trust..Government.Corruption.,Dystopia.Residual)

#changine Colnames to better names

colnames(open_happy) <- c("country","happiness","openness","gdp","family","health","freedom","generosity","goverment","dyst_residual")


#checking NAs for `openness`
sum(is.na(open_happy$openness))


#making nice table to get relationship between happiness and openness
library(formattable)

open_happy %>%
  arrange(desc(openness)) %>%
  mutate_each(funs(round(., 2)), -c(country,openness)) %>%
  head(10) %>%
  formattable(list(
    openness = color_bar("yellow"),
    happiness = color_bar("lightgreen"),
      gdp = color_bar("deepskyblue"),
    family = color_bar("deepskyblue"),
    health = color_bar("deepskyblue"),
    freedom = color_bar("deepskyblue"),
    goverment = color_bar("deepskyblue"),
    generosity = color_bar("deepskyblue"),
    dyst_residual = color_bar("deepskyblue")
  ), align = "l")

#countries group

g8 <- c("Canada","France","Germany","Italy","Japan","United Kingdom","United States","Russia")
brics <- c("India","China","South Africa","Brazil","Russia")
asean <- c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar","Philippines", "Singapore", "Thailand","Vietnam")

#some changes for plotting the graph
rownames(open_happy) <- open_happy$country

#Is Happiness related with openness?

ggplot(open_happy, 
       aes(x = openness, 
           y = happiness)) +
  geom_point() +
  geom_point(data = open_happy[g8,],aes(colour = "G8"),size = 5) +
  geom_point(data = open_happy[brics,],aes(colour = "BRICS"),size = 5) +
  geom_point(data = open_happy[asean,],aes(colour = "ASEAN"),size = 5) +
  geom_smooth(method="lm") +
  labs(x = "Openness Score",
       y = "Happiness Score",
       title = "Are open data friendly countries happy countries?",
       subtitle = "Data openness and happiness by country in 2015") +
    theme_minimal() +
  theme(text = element_text(size=16))


#Other variables correlated with openness variable

open_data_corr <- open_happy %>%
  select(openness, happiness, gdp, family, health, 
         freedom,goverment,generosity, dyst_residual) %>%
  mutate(openness = as.numeric(openness))

od_corr <- cor(open_data_corr, use = "complete", method = "pearson")

corrplot(od_corr)
