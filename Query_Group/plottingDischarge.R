library(tidyverse)
library(scales)

discharge <- read_csv("/Users/ianbogucki/Desktop/EI Capstone/data/CleanDischargeData4") 

plot <- ggplot(discharge, mapping = aes(x = Date, y = Discharge_ls))+
  geom_line()+
  ylab("Discharge Liter/Second")+
  xlab("Date")


plot
