library(tidyverse)
library(ggplot2)
library(dtwclust)
library(fpc)
library(NbClust)
library(ggthemes)
library(plotly)

setwd("D:/Capstone/data")
daily_precip <- read_csv("dailyWatershedPrecip1956-2019.csv")

#mL/day precip data for 2012-2013
daily_precip <- daily_precip %>% 
  filter(watershed == 3,
         DATE > "2012-01-01",
         DATE < "2013-01-01")

#precipitation plot over 2012
ggplot(daily_precip,
       aes(x = DATE,
           y = Precip)) + geom_line()

#filtering for days with precip > 50mL
high_precip <- daily_precip %>% 
  filter(Precip > 50)

late_may <- daily_precip %>% 
  filter(DATE > "2012-05-10",
         DATE < "2012-06-10")

ggplot(late_may,
       aes(x = DATE,
           y = Precip)) + geom_line()

# 2012-09-17 to 2012-09-19
precip1 <- daily_precip %>% 
  filter(DATE >= "2012-09-17",
         DATE <= "2012-09-19")
ggplot(precip1,
       aes(x = DATE,
           y = Precip)) + geom_point() +
  labs(x = "Date",
       y = "Precipitation (mL)",
       title = "Precipation from 2012/09/17 to 2012/09/19") +
  theme_economist()

# 2012-06-01 to 2012-06-03
precip2 <- daily_precip %>% 
  filter(DATE >= "2012-06-01",
         DATE <= "2012-06-03")
ggplot(precip2,
       aes(x = DATE,
           y = Precip)) + geom_line() + geom_point() +
  labs(x = "Date",
       y = "Precipitation (mL)",
       title = "Precipation from 2012/06/01 to 2012/06/03") +
  theme_economist()

# making interactive precip plot with plotly
precip_2012_2013 <- ggplot(daily_precip,
                           aes(x = DATE,
                               y = Precip)) + 
  geom_line() +
  labs(x = "Date",
       y = "Precipitation (mL)",
       title = "Daily Recorded Precipitation for Watershed 3 from 2012 to 2013")
precip_2012_2013
precip_plotly <- ggplotly(precip_2012_2013)
precip_plotly
  