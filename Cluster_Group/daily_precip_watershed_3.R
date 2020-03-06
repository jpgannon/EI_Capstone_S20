library(tidyverse)
library(ggplot2)
library(dtwclust)
library(fpc)
library(NbClust)

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

high_precip <- daily_precip %>% 
  filter(Precip > 50)

late_may <- daily_precip %>% 
  filter(DATE > "2012-05-10",
         DATE < "2012-06-10")

ggplot(late_may,
       aes(x = DATE,
           y = Precip)) + geom_line()


