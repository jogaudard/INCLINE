
#########################################################
#### FLOWER PRODUCTION IN THE ALPINE PLANT COMMUNITY ####
#########################################################

#### Loading libraries ####
library(tidyverse)
library(dataDownloader)
library(lubridate)

#Flowering data
get_file(node = "zhk3m",
         file = "INCLINE_flowering_2021.csv",
         path = "Raw_data",
         remote_path = "RawData/Flowering")

#Sibbaldia procumbens demography
get_file(node = "zhk3m",
         file = "INCLINE_demography_Sib_pro.csv",
         path = "Cleaned_demography",
         remote_path = "Demography")

#Veronica alpina demography
get_file(node = "zhk3m",
         file = "INCLINE_demography_Ver_alp.csv",
         path = "Cleaned_demography",
         remote_path = "Demography")

#Flowering measurement types
get_file(node = "zhk3m",
         file = "INCLINE_flowering_measurements_2021.csv",
         path = "Raw_data",
         remote_path = "RawData/Flowering")



######################################################
#### Adding demography data to the flowering data ####
######################################################


#### Importing demography data ####

#Sibbaldia procumbens
sib_pro_demography <- read.csv("Cleaned_demography/INCLINE_demography_Sib_pro.csv", header = TRUE, sep = ",", dec = ".") #first row is headers

#Veronica alpina
ver_alp_demography <- read.csv("Cleaned_demography/INCLINE_demography_Ver_alp.csv", header = TRUE, sep = ",", dec = ".") #first row is headers



#### Defining which coordinates belong to which subplot ####

#Sibbaldia procumbens
sib_pro_demography <- sib_pro_demography %>%
  filter(year == 2021) %>%  
  mutate(subplot = case_when((X >= 0 & X < 5) & (Y >= 20 & Y <= 25) ~ 1,
                             (X >= 5 & X < 10) & (Y >= 20 & Y <= 25) ~ 2,
                             (X >= 10 & X < 15) & (Y >= 20 & Y <= 25) ~ 3,
                             (X >= 15 & X < 20) & (Y >= 20 & Y <= 25) ~ 4,
                             (X >= 20 & X < 25) & (Y >= 20 & Y <= 25) ~ 5,
                             (X >= 25 & X < 30) & (Y >= 20 & Y <= 25) ~ 6,
                             (X >= 30 & X <= 35) & (Y >= 20 & Y <= 25) ~ 7,
                             (X >= 0 & X < 5) & (Y >= 15 & Y < 20) ~ 8,
                             (X >= 5 & X < 10) & (Y >= 15 & Y < 20) ~ 9,
                             (X >= 10 & X < 15) & (Y >= 15 & Y < 20) ~ 10,
                             (X >= 15 & X < 20) & (Y >= 15 & Y < 20) ~ 11,
                             (X >= 20 & X < 25) & (Y >= 15 & Y < 20) ~ 12,
                             (X >= 25 & X < 30) & (Y >= 15 & Y < 20) ~ 13,
                             (X >= 30 & X <= 35) & (Y >= 15 & Y < 20) ~ 14,
                             (X >= 0 & X < 5) & (Y >= 10 & Y < 15) ~ 15,
                             (X >= 5 & X < 10) & (Y >= 10 & Y < 15) ~ 16,
                             (X >= 10 & X < 15) & (Y >= 10 & Y < 15) ~ 17,
                             (X >= 15 & X < 20) & (Y >= 10 & Y < 15) ~ 18,
                             (X >= 20 & X < 25) & (Y >= 10 & Y < 15) ~ 19,
                             (X >= 25 & X < 30) & (Y >= 10 & Y < 15) ~ 20,
                             (X >= 30 & X <= 35) & (Y >= 10 & Y < 15) ~ 21,
                             (X >= 0 & X < 5) & (Y >= 5 & Y < 10) ~ 22,
                             (X >= 5 & X < 10) & (Y >= 5 & Y < 10) ~ 23,
                             (X >= 10 & X < 15) & (Y >= 5 & Y < 10) ~ 24,
                             (X >= 15 & X < 20) & (Y >= 5 & Y < 10) ~ 25,
                             (X >= 20 & X < 25) & (Y >= 5 & Y < 10) ~ 26,
                             (X >= 25 & X < 30) & (Y >= 5 & Y < 10) ~ 27,
                             (X >= 30 & X <= 35) & (Y >= 5 & Y < 10) ~ 28,
                             (X >= 0 & X < 5) & (Y >= 0 & Y < 5) ~ 29,
                             (X >= 5 & X < 10) & (Y >= 0 & Y < 5) ~ 30,
                             (X >= 10 & X < 15) & (Y >= 0 & Y < 5) ~ 31,
                             (X >= 15 & X < 20) & (Y >= 0 & Y < 5) ~ 32,
                             (X >= 20 & X < 25) & (Y >= 0 & Y < 5) ~ 33,
                             (X >= 25 & X < 30) & (Y >= 0 & Y < 5) ~ 34,
                             (X >= 30 & X <= 35) & (Y >= 0 & Y < 5) ~ 35)) %>% 
  filter(!is.na(subplot))




#Veronica alpina
ver_alp_demography <- ver_alp_demography %>% 
  filter(year == 2021) %>%  #choosing only data from 2021
  mutate(subplot = case_when((X >= 0 & X < 5) & (Y >= 20 & Y <= 25) ~ 1,
                             (X >= 5 & X < 10) & (Y >= 20 & Y <= 25) ~ 2,
                             (X >= 10 & X < 15) & (Y >= 20 & Y <= 25) ~ 3,
                             (X >= 15 & X < 20) & (Y >= 20 & Y <= 25) ~ 4,
                             (X >= 20 & X < 25) & (Y >= 20 & Y <= 25) ~ 5,
                             (X >= 25 & X < 30) & (Y >= 20 & Y <= 25) ~ 6,
                             (X >= 30 & X <= 35) & (Y >= 20 & Y <= 25) ~ 7,
                             (X >= 0 & X < 5) & (Y >= 15 & Y < 20) ~ 8,
                             (X >= 5 & X < 10) & (Y >= 15 & Y < 20) ~ 9,
                             (X >= 10 & X < 15) & (Y >= 15 & Y < 20) ~ 10,
                             (X >= 15 & X < 20) & (Y >= 15 & Y < 20) ~ 11,
                             (X >= 20 & X < 25) & (Y >= 15 & Y < 20) ~ 12,
                             (X >= 25 & X < 30) & (Y >= 15 & Y < 20) ~ 13,
                             (X >= 30 & X <= 35) & (Y >= 15 & Y < 20) ~ 14,
                             (X >= 0 & X < 5) & (Y >= 10 & Y < 15) ~ 15,
                             (X >= 5 & X < 10) & (Y >= 10 & Y < 15) ~ 16,
                             (X >= 10 & X < 15) & (Y >= 10 & Y < 15) ~ 17,
                             (X >= 15 & X < 20) & (Y >= 10 & Y < 15) ~ 18,
                             (X >= 20 & X < 25) & (Y >= 10 & Y < 15) ~ 19,
                             (X >= 25 & X < 30) & (Y >= 10 & Y < 15) ~ 20,
                             (X >= 30 & X <= 35) & (Y >= 10 & Y < 15) ~ 21,
                             (X >= 0 & X < 5) & (Y >= 5 & Y < 10) ~ 22,
                             (X >= 5 & X < 10) & (Y >= 5 & Y < 10) ~ 23,
                             (X >= 10 & X < 15) & (Y >= 5 & Y < 10) ~ 24,
                             (X >= 15 & X < 20) & (Y >= 5 & Y < 10) ~ 25,
                             (X >= 20 & X < 25) & (Y >= 5 & Y < 10) ~ 26,
                             (X >= 25 & X < 30) & (Y >= 5 & Y < 10) ~ 27,
                             (X >= 30 & X <= 35) & (Y >= 5 & Y < 10) ~ 28,
                             (X >= 0 & X < 5) & (Y >= 0 & Y < 5) ~ 29,
                             (X >= 5 & X < 10) & (Y >= 0 & Y < 5) ~ 30,
                             (X >= 10 & X < 15) & (Y >= 0 & Y < 5) ~ 31,
                             (X >= 15 & X < 20) & (Y >= 0 & Y < 5) ~ 32,
                             (X >= 20 & X < 25) & (Y >= 0 & Y < 5) ~ 33,
                             (X >= 25 & X < 30) & (Y >= 0 & Y < 5) ~ 34,
                             (X >= 30 & X <= 35) & (Y >= 0 & Y < 5) ~ 35)) %>% #adding a new column where I define which coordinates belong to each of the 35 subplots
  filter(!is.na(subplot)) #filtering out data for individuals when subplot = NA (outside frame, gone or new seedling)



#### Summarising number of reproductive organs ####

#Sibbaldia procumbens
sib_pro_sum_reproduction <- sib_pro_demography %>% 
  filter(demographic_trait %in% c("number_of_capsules", "number_of_flowers", "number_of_aborted_capsules", "number_of_buds")) %>% 
  group_by(plotID, subplot) %>% #grouping data (for summing)
  mutate(Sib_pro = sum(demographic_value, na.rm = TRUE),
         Sib_pro = na_if(Sib_pro, 0)) %>%    #summarise reproductive organs and create a new column with total sum
  select(siteID, blockID, plotID, OTC, treatment, subplot, year, date, registrator, Sib_pro) %>%   #selecting which columns I want to keep from original dataset (because I will add the info to the flower-data set)
  distinct() #selecting only distinct columns


#Veronica alpina
ver_alp_sum_reproduction <- ver_alp_demography %>%
  filter(demographic_trait %in% c("number_of_capsules", "number_of_flowers", "number_of_aborted_capsules", "number_of_buds")) %>% 
  group_by(plotID, subplot) %>% 
  mutate(Ver_alp = sum(demographic_value, na.rm = TRUE),
         Ver_alp = na_if(Ver_alp, 0)) %>% 
  select(siteID, blockID, plotID, OTC, treatment, subplot, year, date, registrator, Ver_alp) %>% 
  distinct()



#### Importing community data ####


#Flowering data
reproduction_data <- read.csv("Raw_data/INCLINE_flowering_2021.csv", header = TRUE, sep = ";", dec = ",") #first row is headers


reproduction_data <- reproduction_data %>% 
  mutate(OTC = substr(Treat, 1, 1), 
         treatment = substr(Treat, 2, 2)) %>% 
  mutate(blockID = paste0(Site, "_", Block), 
         plotID = paste0(blockID, "_", Plot)) %>% 
  mutate(date = dmy(Date)) %>% 
  rename(siteID = Site, subplot = Subplot, year = Year, registrator = Registrator, writer = Writer, weather = Weather, comments = Comments) %>% 
  mutate(siteID = case_when(siteID == "Ulv" ~ "Ulvehaugen",
                            siteID == "Lav" ~ "Lavisdalen",
                            siteID == "Gud" ~ "Gudmedalen",
                            siteID == "Skj" ~ "Skjellingahaugen")) %>% 
  select(!c(Block, Plot, Treat, Date))
  
          

#### Merging the "summed" Veronica and Sibbaldia data with the "reproduction" data ####

full_reproduction_data <- reproduction_data %>% 
  left_join(sib_pro_sum_reproduction, by = c("siteID", "blockID", "plotID", "OTC", "treatment", "subplot", "year"), suffix = c("_rep", "_demo_sp")) %>% #suffix to avoid confusion between columns with same name from previous datasets
  left_join(ver_alp_sum_reproduction, by = c("siteID", "blockID", "plotID", "OTC", "treatment", "subplot", "year")) %>% 
  rename(registrator_demo_va = registrator, date_demo_va = date)


#Combining the three registrator columns from the three merged data sets into one column + renaming date columns + removing former registrator columns + adding precipitation levels
full_reproduction_data <- full_reproduction_data %>% 
  mutate(registrator = case_when(!is.na(registrator_rep) & is.na(registrator_demo_sp) & is.na(registrator_demo_va) ~ registrator_rep,
                                 is.na(registrator_rep) & !is.na(registrator_demo_sp) & is.na(registrator_demo_va) ~ registrator_demo_sp,
                                 is.na(registrator_rep) & is.na(registrator_demo_sp) & !is.na(registrator_demo_va) ~ registrator_demo_va,
                                 !is.na(registrator_rep) & !is.na(registrator_demo_sp) & is.na(registrator_demo_va) ~ paste0(registrator_rep, " & ", registrator_demo_sp),
                                 !is.na(registrator_rep) & is.na(registrator_demo_sp) & !is.na(registrator_demo_va) ~ paste0(registrator_rep, " & ", registrator_demo_va),
                                 is.na(registrator_rep) & !is.na(registrator_demo_sp) & !is.na(registrator_demo_va) ~ paste0(registrator_demo_sp, " & ", registrator_demo_va),
                                 TRUE ~ paste0(registrator_rep, " & ", registrator_demo_sp, " & ", registrator_demo_va))) %>% 
  rename(date_community = date_rep, date_Sib_pro = date_demo_sp, date_Ver_alp = date_demo_va) %>% 
  select(!c(registrator_rep, registrator_demo_sp, registrator_demo_va)) 



############################################################
#### Pivoting the reproduction data set before analysis ####
############################################################


#### Removing numbered suffix from species names ####

colnames(full_reproduction_data) <- gsub("\\_[0-99]*$", "" , colnames(full_reproduction_data))


#### Pivot data from wide to long ####

pivot_reproduction_data <- full_reproduction_data %>%
  pivot_longer(!c(siteID, blockID, plotID, subplot, OTC, treatment, year, date_community, date_Sib_pro, date_Ver_alp, registrator, writer, weather, comments), #columns I don't want to pivot
                            names_to = "species", #what to call the column where the pivoted headers (species) go
                            values_to = "reproduction_value", #what to call the column where the pivoted values go
                            values_drop_na = TRUE)  #remove NA-values



########################################################################################################
#### Summing of multiple values for several individuals of the same species within the same subplot ####
########################################################################################################

#Summing variable by group
summed_reproduction_data <- pivot_reproduction_data %>%
  group_by(siteID, plotID, subplot, species) %>% 
  mutate(flowering_value = sum(reproduction_value)) %>% 
  select(siteID, blockID, plotID, subplot, OTC, treatment, year, date_community, date_Sib_pro, date_Ver_alp, registrator, writer, weather, species, flowering_value)




####################################################################
#### Merging flowering measurement types with reproduction data ####
####################################################################

#### Importing flowering measurement data ####
flowering_measurement_types <- read.csv("Raw_data/INCLINE_flowering_measurements_2021.csv", header = TRUE, sep = ";",  dec = ",") %>% 
  rename(species = Species)


#### Merging the data sets by species ####
flowering_data <- summed_reproduction_data %>% 
  left_join(flowering_measurement_types, by = "species") %>% 
  rename(flowering_measurement_type = Measurement_type) %>% 
  select(siteID, blockID, plotID, subplot, OTC, treatment, year, date_community, date_Sib_pro, date_Ver_alp, registrator, writer, weather, species, flowering_measurement_type, flowering_value)




#write.csv(biomass_allocation, file = "data/cleaned_data/INCLINE_species_level_biomass_allocation.csv", row.names = FALSE)
