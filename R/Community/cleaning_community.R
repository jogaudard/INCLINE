###########################################################
### Script for cleaning raw community data from INCLINE ###
###########################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)

#### Downloading data from OSF ####



#### Load data ####

comm <- read_csv2("data/Community/INCLINE_community_2018_2019_2021.csv")


#### Cleaning data ####

#Renaming columns

comm1 <- comm %>% 
  rename(block = Block, treatment = Treatment, plotID = plot, siteID = Site, measure = Measure, weather = Weather, unknown = Unknown, veg_cover = Veg_cover, veg_height_mm = Veg_height_mm, moss_depth_mm = Moss_depth_mm) %>% 
  select(!"...197")
