library(ggplot2)
library(tidyverse)

#setting working directory and reading in data
setwd("D:/Capstone/data")
well_df <- read_csv("sixHourSummary.csv")

#Creating new column in new df for logic of having data or not
has_data <- well_df %>% 
  mutate(has_data = ifelse(!is.na(well_df$water_depth), TRUE, FALSE)
)

#plotting - too many dates? can't plot it/ can't save either
#might have to just wait longer - but it will be too cluttered
ggplot(data = has_data,
       mapping = aes(x = date,
                     y = Well,
                     color = has_data)) +
  geom_point()

#trying to make data smaller by grouping making multiple plots of different wells
#making has_data as color so we can see more easily
#hard to tell when y is has_data and color is Well
filter2 <- c("A5", "A6", "A7", "D1", "H4", "I3", "I6", "I7", "I8", "I9")
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


#narrowing down dates
filter3 <- c("JD01", "JD02", "JD03", "JD04", "JD05", "JD06", "JD07", "JD08",
             "JD10", "JD11", "JD12", "JD13", "JD14", "JD15", "JD16", "JD17",
             "JD18","JD19", "JD20", "JD21", "JD22", "JD23", "JD24", "JD25", 
             "JD26", "JD27", "JD28", "JD29", "JD30")
wells_set3 <- has_data %>% 
  filter(Well %in% filter3,
         date >= "2008-01-01",
         date <= "2010-01-01")

ggplot(wells_set3,
       aes(x = date,
           y = Well,
           color = has_data)) +
  geom_point()

#narrowing down dates more
wells_set3 <- wells_set3 %>% 
  filter(date <= "2008-07-01")


filter6 <- c("N1", "N2", "N3", "N4", "N5", "O1", "O2", "P1", "P2", "Q1", "Q2", "T1")
wells_set6 <- has_data %>% 
  filter(Well %in% filter6,
         date >= "2011-01-01",
         date <= "2014-01-01")
ggplot(wells_set6_2,
       aes(x = date,
           y = Well,
           color = has_data)) +
  geom_point()

wells_set6_2 <- wells_set6 %>% 
  filter(date <= "2013-09-01")

#seeing if JD wells start at same times
jd_wells <- well_df %>% 
  filter(Well %in% filter3) %>% 
  arrange(date)

library(lubridate)
#grouping by every 6 hours
well_df$date <- ymd_hms(well_df$date)

well_df %>% group_by(Well, year(date), month(date), day(date), hour(date)) %>%
  summarize(median(level), median(water_depth)) -> hourly

colnames(hourly) <- c("Well", "year","month","day", "hour", "level","wtdepth")  

hourly$date. <- dmy_h(paste(hourly$day, hourly$month, hourly$year, hourly$hour))

hourly <- hourly %>% ungroup() %>%
  select(Well, date., level, wtdepth) 

hourly$bySixHours <- cut(hourly$date., breaks = "6 hours")

sixHourSummary <- hourly %>% 
  group_by(Well, bySixHours) %>% 
  summarise(median(level), median(wtdepth))


#trying plotting with sixHourSummary and NAs removed during calculations
sixHourSummary
colnames(sixHourSummary) <- c("Well", "date", "level", "water_depth")
write_csv(sixHourSummary, "D:/Capstone/data/sixHourSummary.csv")

well_df <- read_csv("sixHourSummary.csv")

jd_wells <- well_df %>% 
  filter(Well %in% jd_filter)

has_data <- well_df %>% 
  mutate(has_data = ifelse(!is.na(well_df$water_depth), TRUE, FALSE)
  )

has_data <- jd_wells %>% 
  mutate(has_info = ifelse(!is.na(jd_wells$water_depth), TRUE, FALSE))

ggplot(has_data,
       aes(x = date,
           y = Well,
           color = has_info)) +
  geom_point()

jd_filter <- c("JD01", "JD02", "JD03", "JD04", "JD05", "JD06", "JD07", "JD08",
               "JD10", "JD11", "JD12", "JD13", "JD14", "JD15", "JD16", "JD17",
               "JD18","JD19", "JD20", "JD21", "JD22", "JD23", "JD24", "JD25", 
               "JD26", "JD27", "JD28", "JD29", "JD30")



jd_wells <- has_data %>% 
  filter(date >= "2008-01-01",
         date <= "2008-04-15")

ggplot(jd_wells,
       aes(x = date,
           y = Well,
           color = has_info)) + geom_point()

#checking to see if JD wells start at the same times
jd_wells %>% 
  arrange(date)
