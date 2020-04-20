library(tidyverse)
library(imputeTS)
library(zoo)

setwd("D:/Capstone/data")
hourly <- read_csv("oneHourSummary.csv")

#CREATING ARTIFICIAL GAP BETWEEN FEBRUARY AND MARCH
hourly_gaps <- hourly %>% 
  mutate(gapped_depths = ifelse(date >= "2012-02-01", 
                                ifelse(date <= "2012-03-01", NA, water_depth), water_depth)) %>% 
  select(date, gapped_depths)
hourly[1,] %>% 
  select(Well) %>% 
  as.character()

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
to_fill <- zoo(a6_gaps$wtdepth, a6_gaps$date.)  
filler <- zoo(i7$wtdepth, i7$date.)
combined <- merge(filler, to_fill) #combine data
combined <- combined %>% 
  transform(is_predicted = ifelse(is.na(to_fill), TRUE, FALSE))
combined$to_fill <- na.approx(combined$to_fill, na.rm = TRUE) #interpolate NAs
final_combination <- fortify.zoo(combined, name = "date.") #changes zoo object back to a data frame while keeping a date column
colnames(final_combination) <- c("date.", "Well_2", "Well_1", "is_predicted")
final_combination <- final_combination %>% 
  mutate(is_predicted = ifelse(is_predicted == 1, TRUE, FALSE))

ggplot() +
  geom_point(data = final_combination,
            mapping = aes(x = date.,
                          y = Well_1,
                          color = is_predicted)) +
  geom_line(data = final_combination,
            mapping = aes(x = date.,
                          y = Well_1)) +
  geom_line(data = final_combination,
            mapping = aes(x = date.,
                          y = Well_2),
                          color = "grey70") +
  scale_color_manual(labels = c("Original", "Predicted"), values = c("red", "blue"), name = "values") +
  scale_y_reverse()
?labs
?na.approx
?index

rmse <- function(x, y){
  sqrt(mean(x - y) ^ 2)
}


#may gap
rmse(final_combination$to_fill[2184:2281], a6$wtdepth[2184:2281])
#february gap
rmse(final_combination$to_fill[745:913], a6$wtdepth[745:913])

a6_gaps <- a6_gaps %>% 
  mutate(wtdepth = final_combination$to_fill)
final_combination <- final_combination %>% 
  mutate(date = row())

a6_gaps <- a6_gaps %>% 
  select(date., wtdepth)
i7 <- i7 %>% 
  select(date., wtdepth)

combined <- left_join(i7, a6_gaps, by = "date.")
combined <- zoo(combined)
colnames(combined) <- c("date.", "well_2", "well_1")
head(combined)
ncol(combined)
a6_year_gap <- a6 %>% 
  mutate(wtdepth = ifelse(date. >= "2012-02-01", ifelse(date. <= "2013-01-01", NA, wtdepth), wtdepth))

combined <- combined %>% 
  transform(is_predicted = ifelse(is.na(well_1), TRUE, FALSE))



to_fill <- zoo(a6_year_gap$wtdepth, a6_year_gap$date.)  
filler <- zoo(i7$wtdepth, i7$date.)
combined <- merge(filler, to_fill) #combine data
combined$to_fill <- na.approx(combined$to_fill, na.rm = TRUE) #interpolate NAs
final_combination <- combined[index(filler),]
final_combination <- fortify.zoo(final_combination, name = "date.")
?na.approx


combined <- left_join(i7, a6_gaps, by = "date.")
colnames(combined) <- c("date.", "well_2", "well_1")
combined <- combined %>% 
  mutate(is_predicted = ifelse(is.na(well_1), TRUE, FALSE))
combined$well_1 <- na.approx(combined$well_1, na.rm = FALSE)
str(combined)

yes <- data.frame()
