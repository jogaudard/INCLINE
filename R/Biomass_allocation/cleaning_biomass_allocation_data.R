###########################################################
### Script for cleaning biomass removal data for INCLINE ###
###########################################################

#### Libraries ####
library("tidyverse")
library("lubridate")
library("ggpubr")
library("dataDownloader")
library("osfr")

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

 # get_file(node = "zhk3m",
 #          file = "INCLINE_biomass_allocation.csv",
 #          path = "data/Biomass_allocation",
 #          remote_path = "RawData/Species_level_biomass_allocation")

#### Load data ####

biomass_allocation <- read.csv2("data/Biomass_allocation/INCLINE_biomass_allocation.csv", header = TRUE, dec = ",")

#### Clean data ####

biomass_allocation <- biomass_allocation %>% 
  rename(species= "ï..Species", siteID = Site, species_group = RS, date = Date, weather = Weather, registrator = Registrator, Ind_nr = Individualnummer, square = Square., subsquare = Subsquare., shoot_height = SH, number_of_stems = NS, number_of_leaves = NL, leaf_length = LL, leaf_width = WL, leaf_stalk_length = LSL, length_inflorescence_shoot = LIS, number_of_inflorescence_shoots = NFI, number_of_flowers = X.F, number_of_capsules = X.C, number_of_buds = X.B, number_of_witherd_flowers = X.FV, inflorescence_diameter = ID, genetID = GenetID, comments_field = Comment1, comments_lab = Comment2, aboveground_biomass = AB, belowground_biomass = BB) %>% 
  select(-c(Temperature, TempLevel, Precipitation, PrecLevel, AB1, AB2, BB1, BB2, FG)) %>% 
  mutate(genus = substr(species, 1, 3),
         sp = substr(species, 4, 6),
         species = paste0(genus, "_", sp)) %>% 
  select(-genus, -sp) %>% 
  mutate(uniqueID = paste0(species, "_", siteID, "_", square, "_", subsquare, "_", Ind_nr)) %>% 
  mutate(date = dmy(date)) %>% 
  rename(plot = square, sub_plot = subsquare, individual = Ind_nr)

biomass_allocation <- biomass_allocation %>% 
  pivot_longer(cols = c(shoot_height:inflorescence_diameter, aboveground_biomass, belowground_biomass), names_to = "trait", values_to = "value", values_drop_na = TRUE) 

### Save data to upload as cleaned on OSF

#write.csv(biomass_allocation, file = "data/cleaned_data/INCLINE_species_level_biomass_allocation.csv", row.names = FALSE)
