library(ggplot2)
library(tidyverse)

#setting working directory and reading in data
setwd("D:/Capstone/data")
well_df <- read_csv("oneHourSummary.csv")

#Creating new column in new df for logic of having data or not
has_data <- well_df %>% 
  mutate(has_data = ifelse(!is.na(well_df$wtdepth), TRUE, FALSE)
)

has_data <- has_data %>% 
  arrange(desc(Well), date.)

#plotting - too many dates? can't plot it/ can't save either
#might have to just wait longer - but it will be too cluttered
ggplot(data = has_data,
       mapping = aes(x = date.,
                     y = Well,
                     color = has_data)) +
  geom_point() +
  labs(x = "Date",
       y = "Well",
       caption = "Blank spaces are where no data has been collected") +
  scale_color_discrete(name = "",
                       labels = c("NA",
                                  "Has Measured Data"))
