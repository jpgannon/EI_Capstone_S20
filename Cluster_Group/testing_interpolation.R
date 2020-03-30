library(tidyverse)
library(imputeTS)
library(zoo)

setwd("D:/Capstone/data")
hourly <- read_csv("oneHourSummary.csv")
hourly <- hourly %>% 
  filter(Well == "K11",
         date. <= "2013-01-01",
         date. >= "2012-01-01") %>% 
  select(date., level, wtdepth)

colnames(hourly) <- c("date", "water_level", "water_depth")

ggplot(hourly,
       aes(x = date,
           y = water_depth)) +
  geom_point()

#CREATING ARTIFICIAL GAP BETWEEN FEBRUARY AND MARCH
hourly_gaps <- hourly %>% 
  mutate(gapped_depths = ifelse(date >= "2012-02-01", 
                                ifelse(date <= "2012-03-01", NA, water_depth), water_depth)) %>% 
  select(date, gapped_depths)


ggplot(hourly_gaps,
       aes(x = date,
           y = gapped_depths)) + geom_point()

#na\_mean(hourly_gaps)
??imputeTS
interpolation_gaps <- hourly_gaps %>% 
  na_interpolation()
ggplot(interpolation_gaps,
       aes(x = date,
           y = gapped_depths)) + geom_point()

plotNA.distribution(hourly_gaps$gapped_depths)

# two smaller gaps
split_gaps <- hourly %>% 
  mutate(gapped_depths = ifelse(date >= "2012-02-01", 
                                ifelse(date <= "2012-02-08", NA, water_depth), water_depth),
         gapped_depths2 = ifelse(date >= "2012-04-01",
                                ifelse(date <= "2012-04-05", NA, gapped_depths), gapped_depths)) %>% 
  select(date, gapped_depths2) 

# plotting distribution of NA values
plotNA.distribution(split_gaps$gapped_depths2)

# interpolation of NAs to fill them
split_inter <- split_gaps %>% 
  na_interpolation()
ggplot(split_inter,
       aes(x = date,
           y = gapped_depths2)) + geom_point() +
  scale_y_reverse()

a6 <- hourly %>% 
  filter(Well == "A6",
         date. >= "2012-01-01",
         date. <= "2013-01-01")

i7 <- hourly %>% 
  filter(Well == "I7",
         date. >= "2012-01-01",
         date. <= "2013-01-01")

a6_gaps <- a6 %>% 
  mutate(wtdepth = ifelse(date. >= "2012-02-01", ifelse(date. <= "2012-02-08", NA, wtdepth), wtdepth),
                    wtdepth = ifelse(date. >= "2012-04-01",
                                            ifelse(date. <= "2012-04-05", NA, wtdepth), wtdepth))

# interpolation to fill in NAs
zc <- zoo(a6_gaps$wtdepth, a6_gaps$date.)  
zs <- zoo(i7$wtdepth, i7$date.)
z <- merge(zs, zc) #combine data
z$zc <- na.approx(z$zc, rule = 2) #interpolate NAs
Z <- z[index(zs),]
Z <- as.data.frame(Z)

?na.approx
?index

rmse <- function(x, y){
  sqrt(mean(x - y) ^ 2)
}



rmse(Z$zc[2184:2281], a6$wtdepth[2184:2281])
