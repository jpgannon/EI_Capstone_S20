library(ggplot2)
library(tidyverse)

#setting working directory and reading in data
setwd("D:/Capstone/data")
well_df <- read_csv("compiled_well_data.csv")

#Creating new column in new df for logic of having data or not
has_data <- well_df %>% 
  mutate(has_data = ifelse(!is.na(well_df$water_depth), TRUE, FALSE)
)


#plotting - too many dates? can't plot it/ can't save either
ggplot(data = has_data,
       mapping = aes(x = date,
                     y = Well,
                     color = has_data)) +
  geom_point()

#trying to make data smaller by grouping making multiple plots of different wells
As <- c("A5", "A6", "A7")
a_wells <- has_data %>% 
  filter(Well %in% As)

#making has_data as color so we can see more easily
#hard to tell when y is has_data and color is Well
ggplot(a_wells,
       aes(x = date,
           y = Well,
           color = has_data)) + geom_point()

#filter more wells
filter2 <- c("D1", "H4", "I3", "I6", "I7", "I8", "I9")
wells_set2 <- has_data %>% 
  filter(Well  %in% filter2)

ggplot(wells_set2,
       aes(x = date,
           y = Well,
           color = has_data)) + geom_point()

filter3 <- c("JD01", "JD02", "JD03", "JD04", "JD05", "JD06", "JD07", "JD08",
             "JD10", "JD11", "JD12", "JD13", "JD14", "JD15", "JD16", "JD17")
wells_set3 <- has_data %>% 
  filter(Well %in% filter3)

ggplot(wells_set3,
       aes(x = date,
           y = Well,
           color = has_data)) + geom_point()

filter4 <- c("JD18","JD19", "JD20", "JD21", "JD22", "JD23", "JD24", "JD25", 
             "JD26", "JD27", "JD28", "JD29", "JD30")
wells_set4 <- has_data %>% 
  filter(Well %in% filter4)

ggplot(wells_set4,
       aes(x = date,
           y = Well,
           color = has_data)) + geom_point()

filter5 <- c("K1", "K1D", "K4D", "K4M", "K4S", "K5", "K6D", "K6S", "K7D", "K7S",
             "K8", "K9", "K10", "K11", "K12")
wells_set5 <- has_data %>% 
  filter(Well %in% filter5)

k1_to_k12 <- ggplot(wells_set5,
       aes(x = date,
           y = Well,
           color = has_data)) + geom_point()

filter6 <- c("N1", "N2", "N3", "N4", "N5", "O1", "O2", "P1", "P2", "Q1", "Q2", "T1")
wells_set6 <- has_data %>% 
  filter(Well %in% filter6)

n1_to_t1 <- ggplot(wells_set6,
       aes(x = date,
           y = Well,
           color = has_data)) + 
  geom_point()


#All NAs for h4 and K12??
h4 <- well_df %>% 
  filter(Well == "H4")
k12 <- well_df %>% 
  filter(Well == "K12")

combo <- k1_to_k12 + n1_to_t1
combo
