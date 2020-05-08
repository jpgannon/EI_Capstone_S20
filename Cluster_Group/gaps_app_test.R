#script for creating synthetic gaps in data to generate synthetic data via interpolation 
#and linear regression to test these method's ability to create accurate synthetic data


#making gaps
library(tidyverse)
setwd("D:/Capstone/data")
hourly <- read_csv("oneHourSummary.csv")

a6 <- hourly %>% 
  filter(Well == "A6")

# large gaps
a6_gaps <- a6 %>% 
  mutate(wtdepth = ifelse(date. >= "2012-02-06", ifelse(date. <= "2012-02-15", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2012-03-31", ifelse(date. <= "2012-04-12", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2011-03-18", ifelse(date. <= "2011-03-28", NA, wtdepth), wtdepth),
         Well = "A6_gaps")

i7 <- hourly %>% 
  filter(Well == "I7")
i7_gaps <- i7 %>% 
  mutate(wtdepth = ifelse(date. >= "2011-09-11", ifelse(date. <= "2011-10-02", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2011-11-15", ifelse(date. <= "2011-11-30", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2011-12-19", ifelse(date. <= "2012-01-15", NA, wtdepth), wtdepth),
         Well = "I7_gaps")

hourly_with_gaps <- rbind(hourly, a6_gaps)
hourly_with_gaps <- rbind(hourly_with_gaps, i7_gaps)

write_csv(hourly_with_gaps, path = "D:/Capstone/data/hourly_with_gaps.csv")

a6_gaps_only <- a6_gaps %>% 
  subset((date. >= "2011-03-18" & date. <= "2011-03-28") |
           (date. >= "2012-03-31" & date. <= "2012-04-12") |
           (date. >= "2012-02-06" & date. <= "2012-02-15"))

a6_gap_times <- a6 %>% 
  subset((date. >= "2011-03-18" & date. <= "2011-03-28") |
           (date. >= "2012-03-31" & date. <= "2012-04-12") |
           (date. >= "2012-02-06" & date. <= "2012-02-15"))

i7_gaps_only <- i7_gaps %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-10-02") |
           (date. >= "2011-11-15" & date. <= "2011-11-30") |
           (date. >= "2011-12-19" & date. <= "2012-01-15"))

i7_gap_times <- i7 %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-10-02") |
           (date. >= "2011-11-15" & date. <= "2011-11-30") |
           (date. >= "2011-12-19" & date. <= "2012-01-15"))

#calculating RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

#rmse(wellSample$Well1[1000:1999] - wellSample$Well1Synthetic[1000:1999])
# Two Weeks - long time frame
a6_gaps_interpolation <- read_csv("a6_predicted.csv")
a6_gaps_interpolation <- a6_gaps_interpolation %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(a6_gaps_interpolation) <- c("date.", "wtdepth")
a6_gaps_predicted_interpolation <- a6_gaps_interpolation %>% 
  subset((date. >= "2011-03-18" & date. <= "2011-03-28") |
           (date. >= "2012-03-31" & date. <= "2012-04-12") |
           (date. >= "2012-02-06" & date. <= "2012-02-15"))

a6_gaps_LR <- read_csv("a6_predicted_LR.csv")
a6_gaps_LR <- a6_gaps_LR %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(a6_gaps_LR) <- c("date.", "wtdepth")
a6_gaps_predicted_LR <- a6_gaps_LR %>% 
  subset((date. >= "2011-03-18" & date. <= "2011-03-28") |
           (date. >= "2012-03-31" & date. <= "2012-04-12") |
           (date. >= "2012-02-06" & date. <= "2012-02-15"))

a6_gaps_LR <- read_csv("a6_predicted_LR.csv")
a6_gaps_LR <- a6_gaps_LR %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(a6_gaps_LR) <- c("date.", "wtdepth")
a6_gaps_predicted_LR <- a6_gaps_LR %>% 
  subset((date. >= "2011-03-18" & date. <= "2011-03-28") |
           (date. >= "2012-03-31" & date. <= "2012-04-12") |
           (date. >= "2012-02-06" & date. <= "2012-02-15"))

a6_gap_measured <- a6 %>% 
  subset((date. >= "2011-03-18" & date. <= "2011-03-28") |
           (date. >= "2012-03-31" & date. <= "2012-04-12") |
           (date. >= "2012-02-06" & date. <= "2012-02-15"))

rmse(a6_gap_measured$wtdepth - a6_gaps_predicted_interpolation$wtdepth) # 2.331644 - Interpolation
rmse(a6_gap_measured$wtdepth - a6_gaps_predicted_LR$wtdepth) # 4.408074e-15 - Linear Regression with Well JD02


# i7 long time frame - 2 weeks
i7_inter <- read_csv("i7_predicted_inter.csv")
i7_inter <- i7_inter %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(i7_inter) <- c("date.", "wtdepth")
i7_predicted_interpolation <- i7_inter %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-10-02") |
           (date. >= "2011-11-15" & date. <= "2011-11-30") |
           (date. >= "2011-12-19" & date. <= "2012-01-15"))

i7_gaps_measured <- i7 %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-10-02") |
           (date. >= "2011-11-15" & date. <= "2011-11-30") |
           (date. >= "2011-12-19" & date. <= "2012-01-15"))

i7_LR <- read_csv("i7_predicted_LR.csv")
i7_LR <- i7_LR %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(i7_LR) <- c("date.", "wtdepth")
i7_predicted_LR <- i7_LR %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-10-02") |
           (date. >= "2011-11-15" & date. <= "2011-11-30") |
           (date. >= "2011-12-19" & date. <= "2012-01-15"))


rmse(i7_gaps_measured$wtdepth - i7_predicted_interpolation$wtdepth) # RMSE = 14.29141 Interpolation
rmse(i7_gaps_measured$wtdepth - i7_predicted_LR$wtdepth) # RMSE = 5.212442  Linear Regression
#------------------------------------------------------------------------------------------------------------------

a6_gaps_short <- a6 %>% 
  mutate(wtdepth = ifelse(date. >= "2012-02-06", ifelse(date. <= "2012-02-08", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2012-03-25", ifelse(date. <= "2012-03-27", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2011-03-18", ifelse(date. <= "2011-03-19", NA, wtdepth), wtdepth),
         Well = "A6_gaps_short")

i7_gaps_short <- i7 %>% 
  mutate(wtdepth = ifelse(date. >= "2011-09-11", ifelse(date. <= "2011-09-13", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2011-11-15", ifelse(date. <= "2011-11-17", NA, wtdepth), wtdepth),
         wtdepth = ifelse(date. >= "2011-12-19", ifelse(date. <= "2012-12-21", NA, wtdepth), wtdepth),
         Well = "I7_gaps_short")

hourly_with_gaps <- rbind(hourly, a6_gaps)
hourly_with_gaps <- rbind(hourly_with_gaps, i7_gaps)
hourly_with_gaps <- rbind(hourly_with_gaps, a6_gaps_short)
hourly_with_gaps <- rbind(hourly_with_gaps, i7_gaps_short)
write_csv(hourly_with_gaps, "D:/Capstone/data/hourly_with_gaps.csv")

#-------------------------------------------------------------------------------------------------------
# Two days - short time frame
a6_short_inter <- read_csv("a6_short_inter.csv")
a6_short_inter <- a6_short_inter %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(a6_short_inter) <- c("date.", "wtdepth")
a6_short_inter <- a6_short_inter %>% 
  subset((date. >= "2012-02-06" & date. <= "2012-02-08") |
           (date. >= "2012-03-25" & date. <= "2012-03-27") |
           (date. >= "2011-03-18" & date. <= "2011-03-19"))

a6_short_LR <- read_csv("a6_short_LR.csv")
a6_short_LR <- a6_short_LR %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(a6_short_LR) <- c("date.", "wtdepth")
a6_short_LR <- a6_gaps_LR %>% 
  subset((date. >= "2012-02-06" & date. <= "2012-02-08") |
           (date. >= "2012-03-25" & date. <= "2012-03-27") |
           (date. >= "2011-03-18" & date. <= "2011-03-19"))

a6_short_measured <- a6 %>% 
  subset((date. >= "2012-02-06" & date. <= "2012-02-08") |
           (date. >= "2012-03-25" & date. <= "2012-03-27") |
           (date. >= "2011-03-18" & date. <= "2011-03-19"))
rmse(a6_short_measured$wtdepth - a6_short_inter$wtdepth) # 2.331644 - Interpolation
rmse(a6_short_measured$wtdepth - a6_short_LR$wtdepth) # 5.064979e-15 - Linear Regression


# i7 short time frame - 2 days
i7_short_inter <- read_csv("i7_short_inter.csv")
i7_short_inter <- i7_short_inter %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(i7_short_inter) <- c("date.", "wtdepth")
i7_short_inter <- i7_short_inter %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-09-13") |
           (date. >= "2011-11-15" & date. <= "2011-11-17") |
           (date. >= "2011-12-19" & date. <= "2011-12-21"))

i7_short_measured <- i7 %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-09-13") |
           (date. >= "2011-11-15" & date. <= "2011-11-17") |
           (date. >= "2011-12-19" & date. <= "2011-12-21"))

i7_short_LR <- read_csv("i7_short_LR.csv")
i7_short_LR <- i7_short_LR %>% 
  select(Date_Time, Well_1_Water_Depth)
colnames(i7_short_LR) <- c("date.", "wtdepth")
i7_short_LR <- i7_short_LR %>% 
  subset((date. >= "2011-09-11" & date. <= "2011-09-13") |
           (date. >= "2011-11-15" & date. <= "2011-11-17") |
           (date. >= "2011-12-19" & date. <= "2011-12-21"))


rmse(i7_short_measured$wtdepth - i7_short_inter$wtdepth) # RMSE = 5.622135 Interpolation
rmse(i7_short_measured$wtdepth - i7_short_LR$wtdepth) # RMSE = 6.662009  Linear Regression
