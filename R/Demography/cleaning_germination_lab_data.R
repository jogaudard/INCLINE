################################################################
### Script for cleaning raw data from germination experiment ###
################################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "INCLINE_Germination_Seedling_Experiment_Data_Va.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "INCLINE_Germination_Seedling_Experiment_Data_SP.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "INCLINE_metadata_LoggerDates.csv",
         path = "data",
         remote_path = "RawData")

#### Load data ####

Sib_pro_germ <- read_csv("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_SP.csv")
Ver_alp_germ <- read_csv2("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_Va.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata_LoggerDates.csv", delim = ";")