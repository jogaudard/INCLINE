#######################################
#  Download and clean INCLINE data  #
#######################################

# Script by Eva Lieungh

# last updated: April 2022

# Download INCLINE community data from OSF
# (requires OSF user profile and access to INCLINE OSF repository)

# remotes::install_github("Between-the-Fjords/dataDownloader") # dataDownloader package for getting data from OSF
# install.packages('osfr')
library(dataDownloader)
library(osfr) # to access private OSF repository with token
library(tidyverse) # for data cleaning

# osf_auth(token = "code") # create code on OSF webpage under user settings > personal access tokens
osf_auth(token = "secretpersonaltoken")

community <- get_file(node = "zhk3m",
                      file = "INCLINE_community_2018_2019_2021.csv",
                      path = "Data/Community", #local folder to save in
                      remote_path = "RawData/Community") #OSF folder
# https://github.com/Between-the-Fjords/dataDownloader/issues/4 
community <- read.csv('Data/Community/INCLINE_community_2018_2019_2021.csv', sep = ';') # manual workaround

# Look at the data
#------------------------------------
community[1:5,1:15]
community[1:5,180:198] # species in cols 12-182
str(community)

# Convert to presence-absence 1-0 
#------------------------------------
community <- community %>%
  mutate(across(c(12:182), ~ifelse(.=="", 0, as.character(.)))) %>% # change blanks to 0
  mutate(across(c(12:182), ~ifelse(is.na(.), 0, as.character(.)))) %>% # change NAs to 0
  mutate(across(c(12:182), ~ifelse(.!="0", 1, as.character(.)))) # change the remaining values (F,S,J etc) to 1 (present)
  # the 'else' in the functions returns the value as it was, in character format.

# Clean up some weird things
#------------------------------------
unique(community$Site)
community <- community[-which(community$Site==""),] # remove 1 row missing data
which(community$jamne!=0) # assume 'jamne' is Sel_sel and change these
community$Sel_sel[c(8944,8964)] <- 1

# Modify some data and keep only relevant columns
#------------------------------------
names(community[,12:179])
# create new column with unique subplot ID
community$subPlotID = paste(substr(community$Site,1,3),
                            community$Block,
                            community$plot,
                            community$subPlot,
                            sep = '_')

# check for duplicates and remove them 
community18 <- subset(community,year==2018) # subset because years cause replicates
rows <- which(duplicated(community18$subPlotID)==TRUE) # row numbers that contain duplicates
# In Lav block 4, plot 6 is repeated!
community18 <- community18[-rows,] # remove the duplicates
community <- community[!community$year==2018,] # remove 2018 data from whole data set
community <- rbind(community18,community) # and bind it back together (NB in a different order than before, might need some sorting)


# reorder and select columns to keep
community <- community %>%
  select(subPlotID,Site,Block,plot,subPlot,
         Treatment,year,date,Measure,
         moss,lichen,litter,soil,rock,poo,fungus,bare, 
         logger,Veg_cover,Veg_height_mm,Moss_depth_mm, 
         12:179) # all the species (excluding unknown etc)

# save clean data
#------------------------------------
write.csv(community,'Data/Community/INCLINE_community_2018_2019_2021_clean.csv')

