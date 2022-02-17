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

#### Load data ####
Sib_pro <- read_delim("data/Demography/Sib_pro_2018-2021.csv", delim = ";")
Ver_alp <- read_delim("data/Demography/Ver_alp_2018-2021.csv", delim = ";")

