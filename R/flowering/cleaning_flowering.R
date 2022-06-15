
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




#################################################################
#### Adding demography data to my "flowering" community data ####
#################################################################


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


##################################
#### Importing community data ####
##################################

#Flowering data
reproduction_data <- read.csv("Raw_data/INCLINE_flowering_2021.csv", header = TRUE, sep = ";", dec = ",") #first row is headers


reproduction_data <- reproduction_data %>% 
  mutate(OTC = substr(Treat, 1, 1), 
         treatment = substr(Treat, 2, 2)) %>% 
  mutate(blockID = paste0(Site, "_", Block), 
         plotID = paste0(blockID, "_", Plot)) %>% 
  mutate(date = dmy(Date)) %>% 
  rename(siteID = Site, subplot = Subplot, year = Year, registrator = Registrator, writer = Writer, weather = Weather, comments = Comments) %>% 
  select(!c(Block, Plot, Treat, Date))
  
          

#### Merging the "summed" Veronica and Sibbaldia data with the "reproduction" data ####

full_reproduction_data <- reproduction_data %>% 
  left_join(sib_pro_sum_reproduction, by = c("siteID", "blockID", "plotID", "OTC", "treatment", "subplot", "year"), suffix = c("_rep", "_demo_sp")) %>% #suffix to avoid confusion between columns with same name from previous datasets
  left_join(ver_alp_sum_reproduction, by = c("siteID", "blockID", "plotID", "OTC", "treatment", "subplot", "year"))

full_reproduction_data %>% 
  case_when(!is.na(registrator_demo_sp) ~ paste0(registrator, " & ", registrator_demo_sp))



############################################################
#### Pivoting the reproduction data set before analysis ####
############################################################


#### Removing numbered suffix from species names ####

colnames(full_reproduction_data) <- gsub("\\_[0-99]*$", "" , colnames(full_reproduction_data))


#### Pivot data from wide to long ####

pivot_reproduction_data <- full_reproduction_data %>%
  pivot_longer(!c(siteID, blockID, plotID, OTC, treatment, subplot, year, date_rep, registrator_rep, writer, weather, comments, date_demo_sp, registrator_demo_sp, date, registrator), #columns I don't want to pivot
                            names_to = "species", #what to call the column where the pivoted headers (species) go
                            values_to = "reproduction_value", #what to call the column where the pivoted values go
                            values_drop_na = TRUE)  #remove NA-values



##############################################################
#### Standardising the flowering data/reproduction values ####
##############################################################



#### Summing of multiple values for several individuals of the same species within the same subplot ####

#Summing variable by group
summed_reproduction_data <- pivot_reproduction_data %>% 
  group_by(siteID, blockID, plotID, subplot, species, OTC, treatment) %>% 
  summarise(reproduction_value = sum(reproduction_value))



#### Create total means for species and standardising ####

standardised_reproduction_data <- summed_reproduction_data %>% 
  group_by(species) %>% #need to create new column called species when pivoting the data
  mutate(total_mean = mean(reproduction_value)) %>% #making a new column with mean values for all species
  mutate(reproduction_value_standard = (reproduction_value - total_mean)/total_mean) %>%
#standardizing values by removing units. When subtracting the total mean we "erase" differences between species (which we have measured differently)
  mutate(warming = case_when(OTC == "C" & treatment == "C" ~ "0",
                             OTC == "C" & treatment == "N" ~ "0",
                             OTC == "W" & treatment == "C" ~ "1",
                             TRUE ~ "1"),
         novel = case_when(OTC == "C" & treatment == "C" ~ "0",
                           OTC == "C" & treatment == "N" ~ "1",
                           OTC == "W" & treatment == "C" ~ "0",
                           TRUE ~ "1"),
         precipitation = case_when(siteID == "Skj" ~ 3402,
                                   siteID == "Gud" ~ 2130,
                                   siteID == "Lav" ~ 1561,
                                   TRUE ~ 1226))



standardised_reproduction_data <- standardised_reproduction_data %>%
   mutate(reproduction_value_standard = reproduction_value_standard + abs(min(reproduction_value_standard))) #moving up above 0




#########################################################################
#### Merging species systematics with standardised reproduction data ####
#########################################################################

#### Importing systematics data ####
species_systematics <- read.csv("species_systematics.csv", header = TRUE, sep = ";",  dec = ",")


#### Merging the data sets by species ####
systematics_reproduction_data <- standardised_reproduction_data %>% 
  left_join(species_systematics, by = "Species") %>% 
  filter(!Functional_group %in% "Fern") %>% 
  filter(!Functional_group %in% "Lycophyte") %>% 
  filter(!Functional_group %in% "Dwarf shrub") %>% 
  mutate(FG = case_when(Functional_group == "Graminoid" ~ "Agraminoid",
                        TRUE ~ "Forb")) %>% 
  mutate(SiteID = case_when(Site == "Gud" ~ "Gud",
                            Site == "Lav" ~ "Lav",
                            Site == "Ulv" ~ "Ulv",
                            TRUE ~ "ASkj")) %>% 
  mutate(Flower_count = case_when(Measurement_type == "Flower count" ~ Reproduction_value)) %>% 
                                  #Measurement_type == "Flower head count" ~ Reproduction_value,
                                  #Measurement_type == "Number of flowering individuals" ~ Reproduction_value,
                                  #Measurement_type == "Number of inflorescence units" ~ Reproduction_value)) %>% 
  mutate(Flower_head = case_when(Measurement_type == "Flower head count" ~ Reproduction_value)) %>% 
  mutate(Flower_ind = case_when(Measurement_type == "Number of flowering individuals" ~ Reproduction_value)) %>%
  mutate(Spikelets = case_when(Measurement_type == "Number of inflorescence units" ~ Reproduction_value)) %>%
  mutate(Inflorescence = case_when(Measurement_type == "Length of inflorescence" ~ Reproduction_value)) %>%
  mutate(Percent_cover = case_when(Measurement_type == "Percent cover" ~ Reproduction_value/100)) %>%
  mutate(proxy_flower_head = case_when(Measurement_type == "Flower head count" ~ 1, TRUE ~ 0)) %>% 
  mutate(proxy_flower_ind = case_when(Measurement_type == "Number of flowering individuals" ~ 1, TRUE ~ 0)) %>%
  mutate(proxy_spikelets = case_when(Measurement_type == "Number of inflorescence units" ~ 1, TRUE ~ 0)) %>%
  mutate(proxy_inflorescence = case_when(Measurement_type == "Length of inflorescence" ~ 1, TRUE ~ 0)) %>%
  mutate(proxy_percent_cover = case_when(Measurement_type == "Percent cover" ~ 1, TRUE ~ 0))



d <- as.matrix(systematics_reproduction_data[,24:29])


#Checking number of observations for each proxy
sum(systematics_reproduction_data$proxy_flower_head) #100
sum(systematics_reproduction_data$proxy_flower_ind) #244
sum(systematics_reproduction_data$proxy_spikelets) #151
sum(systematics_reproduction_data$proxy_inflorescence) #488
sum(systematics_reproduction_data$proxy_percent_cover) #29


#### Running model using 'Integrated Nested Laplace Approximation', a deterministic Bayesian method ####

inla.mod <- inla(Y ~ proxy1 + proxy2 + proxy3 + proxy4 + proxy5 + Warming + Novel + Functional_group + Warming:Novel + Warming:Functional_group + Novel:Functional_group + Warming:Novel:Functional_group + f(Block_ID, model = "iid") + f(Data_ID, model = "iid"), 
                 data = list(Y = d, 
                             proxy1 = systematics_reproduction_data$proxy_flower_head,
                             proxy2 = systematics_reproduction_data$proxy_flower_ind,
                             proxy3 = systematics_reproduction_data$proxy_spikelets,
                             proxy4 = systematics_reproduction_data$proxy_inflorescence,
                             proxy5 = systematics_reproduction_data$proxy_percent_cover,
                             Warming = systematics_reproduction_data$Warming,
                             Novel = systematics_reproduction_data$Novel,
                             Functional_group = systematics_reproduction_data$Functional_group,
                             Block_ID = systematics_reproduction_data$Block_ID,
                             Data_ID = systematics_reproduction_data$Data_ID),
                 family = c("poisson", "poisson", "poisson", "poisson", "gamma", "gamma"))

