#######################################################
### Script for cleaning demography data for INCLINE ###
#######################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "Sib_pro_2018-2021.csv",
         path = "data/Demography",
         remote_path = "RawData/Demography")

get_file(node = "zhk3m",
         file = "Ver_alp_2018-2021.csv",
         path = "data/Demography",
         remote_path = "RawData/Demography")

get_file(node = "zhk3m",
         file = "INCLINE_metadata_LoggerDates.csv",
         path = "data",
         remote_path = "RawData")

#### Load data ####

Sib_pro <- read_delim("data/Demography/Sib_pro_2018-2021.csv", delim = ";")
Ver_alp <- read_delim("data/Demography/Ver_alp_2018-2021.csv", delim = ";")
INCLINE_metadata <- read_delim("data/INCLINE_metadata_LoggerDates.csv", delim = ";")

#### Cleaning variables names in dataset ####

# Cleaning metadata so I only have the treatment info and plotID to merge with demopgraphy data
INCLINE_metadata <- INCLINE_metadata %>% 
  select(plotID, OTC, treatment)

Sib_pro <- Sib_pro %>% 
  rename(siteID = Site, block = Block, plot = Plot, year = Year, date = Date, regitrator = Registrator, seedling = seedl) %>%  #Rename to lower capital, and correct naming convention for the INCLINE project
  mutate(plotID = paste0(siteID, "_", block, "_", plot)) %>% #creating unique plotID variable
  mutate(unique_IDS = paste0(plotID, "_", IDS)) %>% #creating unique individual ID
  left_join(INCLINE_metadata, by = "plotID") %>% #adding treatment info from INCLINE metadata file
  select(!Treat) #removing treatment column from the original dataset

Ver_alp <- Ver_alp %>% 
  rename(siteID = Site, block = Block, plot = Plot, year = Year, date = Date, regitrator = Registrator, seedling = seedl) %>%  #Rename to lower capital, and correct naming convention for the INCLINE project
  mutate(plotID = paste0(siteID, "_", block, "_", plot)) %>% #creating unique plotID variable
  mutate(unique_IDS = paste0(plotID, "_", IDS)) %>% #creating unique individual ID
  left_join(INCLINE_metadata, by = "plotID") %>% #adding treatment info from INCLINE metadata file
  select(!Treat) #removing treatment column from the original dataset
