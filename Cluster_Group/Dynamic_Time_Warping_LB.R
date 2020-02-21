#This cluster analysis will be on Dynamic Time Warping

#The first step is setting the working directory to pull and push information to my local file

setwd("C:/Users/Lauren/Documents/Capstone/R_Files")

####
#I've already ran Robbie's master cleaning script, to create a master dataframe
#This dataframe is titled allwelldata.csv
####

#This will load in all of the packages that we should need

library(tidyverse)
library(purrr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
install.packages("dtwclust")

#Now we want to load in the well time series data

well_data <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/allwelldata.csv")

well_data #This is to inspect it and ensure it loaded the data in correctly

#Yay! Okay, so we have a master list of all of the well data currently amassed 
#This covers all of Watershed 3 for the HBEF

#To start to get a feel for the dtwclust package I'm going to choose a random sample of 5 wells
#Then create a smaller data subset containing only data from those 5 wells

num_wells <- 5 #5 wells
s_rows <- sample(1:12820210, num_wells) #This will select 5 random rows from within the 12,820,210 rows
exploratory_sample <- well_data[s_rows,] #This pulls only the 5 randomly selected rows from well_data

sample <- c(exploratory_sample$Well) #This is just to view the 5 randomly selected wells and set to a variable

sample_subset <- well_data %>% #This subsets to only include well data from the 5 random wells
  filter(Well == sample[1] | Well == sample[2] | Well == sample[3] | Well == sample[4] | Well == sample[5])

sample_subset$fCategory <- factor(sample_subset$Well) #This turns the well column into factors

ggplot(data = sample_subset, mapping = aes(x = date., y = level, colour = fCategory)) +
  geom_line()

#Okay so here is the thing, this graph looks okay but it only shows 4 wells
#So I don't know what's going wrong there, I'll come back to it with the team and ask

#My next step is I want to take this subset data and try to use the DTW process on it

sample_na_omit <- na.omit(sample_subset) #Remove all na values

well_names <- unique(sample_na_omit$Well) #This just returns all of the unique well names so I can get a count

sum(sample_na_omit$Well == well_names[1])
well_names[1] #So now I know how many instances of this particular well there are
#I can use this to create the labels for the hierarchical cluster dendrogram I create

label_list <- c(rep(well_names[1], sum(sample_na_omit$Well == well_names[1])),
                rep(well_names[2], sum(sample_na_omit$Well == well_names[2])),
                rep(well_names[3], sum(sample_na_omit$Well == well_names[3])),
                rep(well_names[4], sum(sample_na_omit$Well == well_names[4])))

distance_sample <- dist(sample_na_omit$level, method = "DTW") #This should preform the DTW

hc_sample <- hclust(distance_sample, method = "average") #This creates the hierarchical cluster

plot(hc_sample,
     labels = label_list,
     cex = 0.5)