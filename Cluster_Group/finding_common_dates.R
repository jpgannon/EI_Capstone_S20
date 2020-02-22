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
