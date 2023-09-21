##########################################################
######## PLANT COMMUNITY DATA CLEANING IN INCLINE ########
##########################################################

##### Loading libraries #####
library(tidyverse)
library(lubridate)
library(dataDownloader)
library(osfr) #To download the data from OSF we need this package to get the function osf_auth


#Use your OSF token to get excess to osf. From here you can download neccesary files
#osf_auth(token = "")#Your personal OSF token

#Community data
get_file(node = "zhk3m",
         file = "INCLINE_community_2018_2019_2021_2022_2023.csv",
         path = "data",
         remote_path = "RawData/Community")

#Meta data
get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")

#Name dictionary
get_file(node = "zhk3m",
         file = "INCLINE_community_name_dictionary.csv",
         path = "data",
         remote_path = "RawData/Community")

#Species name dictionary
get_file(node = "zhk3m",
         file = "INCLINE_species_taxonomic_name.csv",
         path = "data",
         remote_path = "RawData/Community")


##### Reading in data #####
#Community data
community_data_download <- read_delim("data\\INCLINE_community_2018_2019_2021_2022_2023.csv", col_types = cols(.default = col_character()))

#Meta data
meta_data_download <- read_delim("data\\INCLINE_metadata.csv") #Need the meta data to fill in the missing part of the treatment and OTC column for 2018.

#Name dictionary
name_dictionary <- read_delim("data\\INCLINE_community_name_dictionary.csv")

#Species name dictionary
species_dictionary <- read_delim("data\\INCLINE_species_taxonomic_name.csv")


####___________Fixing general mistakes in the dataset___________####
###### Cleaning variables in dataset ######
#For it to be easier to work with the data, all the names will be standardized after the same rules.The general rule will be capital letter on the specie name with an underscore to the surname and an underscore to cf if they have a cf at the end. All columns that have a name with space, gets a underscore instead for space. All other names that arent species gets small letters instead of capital letters. I recently learned that it is a code you can use that will fix all these problems automatic. By renaming the columns we dont need to think of their unique names when working with the data. Also fixed general mistakes like making the columns numeric, og to data after what it should be.

#community data
community_data <- community_data_download |>
  rename(Cer_sag_cf = "Cer/sag_cf", Cer_sp = "Cer _sp", Nid_seedling = "Nid seedling", block = Block, measure = Measure, site = Site, treatment = Treatment, weather = Weather, vegetation_cover = Veg_cover, vegetation_height_mm = Veg_height_mm, moss_depth_mm = Moss_depth_mm)|> #Changed wrong types and capital letters to small letters. 
  mutate(plotID = paste0(str_sub(site, 1,3), "_", block, "_", plot))|> #Making a new column called plotID
  select(-treatment,-...209)|> #Removing unnecessary columns
  mutate(block = as.numeric(block, na.rm = TRUE))|>
  mutate(plot = as.numeric(plot, na.rm = TRUE))|>
  mutate(year = as.numeric(year, na.rm = TRUE))|>
  mutate(moss = as.numeric(moss, na.rm = TRUE))|>
  mutate(lichen = as.numeric(lichen, na.rm = TRUE))|>
  mutate(litter = as.numeric(litter, na.rm = TRUE))|>
  mutate(soil = as.numeric(soil, na.rm = TRUE))|>
  mutate(rock = as.numeric(rock, na.rm = TRUE))|>
  mutate(poo = as.numeric(poo, na.rm = TRUE))|>
  mutate(fungus = as.numeric(fungus, na.rm = TRUE))|>
  mutate(bare = as.numeric(bare, na.rm = TRUE))|>
  mutate(logger = as.numeric(logger, na.rm = TRUE))|>
  mutate(vegetation_cover = as.numeric(vegetation_cover, na.rm = TRUE))|>
  mutate(vegetation_height_mm = as.numeric(vegetation_height_mm, na.rm = TRUE))|>
  mutate(moss_depth_mm = as.numeric(moss_depth_mm, na.rm = TRUE))|>
  mutate(date_comment = ifelse(date == "14.08.2019/15.08.2019", "Vegetation analysis was conducted on the 14.08.2019 and the 15.08.2019", NA)) |>
  mutate(date = ifelse(date == "14.08.2019/15.08.2019", "14.08.2019", date)) |>
  mutate(date_comment = ifelse(date == "01.08.2019/02.08.2019", "Vegetation analysis was conducted on the 01.08.2019 and the 02.08.2019", NA)) |>
  mutate(date = ifelse(date == "01.08.2019/02.08.2019", "01.08.2019", date)) |>
  mutate(date_comment = ifelse(date == "30.07.2019/31.07.2019", "Vegetation analysis was conducted on the 30.07.2019 and the 31.07.2019", NA)) |>
  mutate(date = ifelse(date == "30.07.2019/31.07.2019", "30.07.2019", date)) |>
  mutate(date_comment = ifelse(date == "31.07.2019/01.08.2019", "Vegetation analysis was conducted on the 31.07.2019 and the 01.08.2019", NA)) |>
  mutate(date = ifelse(date == "31.07.2019/01.08.2019", "31.07.2019", date)) |>
  mutate(date_comment = ifelse(date == "30.07.2021/02.08.2021", "Vegetation analysis was conducted on the 30.07.2021 and the 02.08.2021", NA)) |>
  mutate(date = ifelse(date == "30.07.2021/02.08.2021", "30.07.2021", date)) |>
  mutate(date = dmy(date))

#meta data
meta_data <- meta_data_download|>
  select(plotID, OTC, treatment) #selecting relevant variables from the meta data

#__________Combining community data and meta data and translating names__________#
# The first year the data was collected, we didn't register the treatment. Therefor by combining the community data and meta data, we will get the missing information. We are also putting in plotID as a new variable that is easier to work with than separated block and plot information. 

community_data <- community_data |>
  left_join(meta_data, by = "plotID") |>
  rename(warming = OTC) |>
  left_join(name_dictionary, by = c("recorder" = "initials"))|>
  select(-recorder) |>
  rename(recorder = name) |>
  left_join(name_dictionary, by = c("writer" = "initials"))|>
  select(-writer) |>
  rename(writer = name)


#__________ Turfmapper __________#
# To continuing cleaning the large dataset, one of our group members have made a turfmap where we can analyse the yearly changes for each specie in each plot. All the colored subplots represent the observations, and the green color changes based on the species cover for each year. The turfmapper plotting code can be found in the community_turfmapper.R script

#Making data ready for turfmapper by widening the data
community_data_longer <- community_data |>
  pivot_longer(Ach_mil:Nid_seedling, values_drop_na = TRUE)|> #Check which species is the first and the last and use these instead of Ach_mil and Nid_seedling.
  rename(species = name)|>
  unique()

#Making new variables were different signs in the dataset gives the same value based on which category it goes under. 
community_data_longer <- community_data_longer |>
  mutate(
    presence = str_detect(value, "(?i)[1234odsjf]"),
    fertile = str_detect(value, "(?i)[f]"),
    dominance = str_detect(value, "(?i)[234d]"),
    juvenile = str_detect(value, "(?i)[j]"),
    seedling = str_detect(value, "(?i)[s]")
  )|>
  mutate(dominance = case_when(dominance == "TRUE" ~ value)) |> #In theory Sibbaldia procumbens (Sib_pro) and Veronica alpina (Ver_alp) should have registered dominance in all plots at Skjellingahaugen every year. Which means that 1 = 0-25% cover in the subplot, 2 = 25-50%, 3 = 50-75% and 4 = 75-100%. So with this coding we are missing out on the 0-25% information for those species. However, the problem is that I can't be sure if people usde 1 as a dominance or a cover value. For plots where there are 2, 3 and 4s we know that 1s means dominance. Could be coded in somehow.
  mutate(presence = case_when(presence == "TRUE" ~ 1)) 

#_______ Making columns that needs to be merged in to the dataset _______#
#Making a new variable called cover
cover_column <- community_data_longer|>
  filter(measure == "cover" )|>
  select(block, plot, site, plotID, year, species, value) |>
  rename(cover = value)|>
  filter(!cover == "")|>
  filter(!cover == " ")|>
  mutate(cover = ifelse(cover == 0.1, 1, cover))|>
  mutate(cover = ifelse(cover == 0.5, 1, cover))|>
  mutate(cover = ifelse(cover == 0 & species == "Hyp_mac" & site == "Ulvehaugen" & year == 2019 & block == 7 & plot == 3, 1, cover)) |>
  mutate(cover = ifelse(cover == "5, 7", NA_real_, cover)) |> #Changing non numeric values to NA, needs to be checked when proofreading later
  mutate(cover = ifelse(cover == "?", NA_real_, cover)) |> #Changing non numeric values to NA, needs to be checked when proofreading later
  mutate(cover = ifelse(cover == "1+1*", NA_real_, cover)) |> #Changing non numeric values to NA, needs to be checked when proofreading later
  mutate(cover = as.integer(cover))

vegetation_cover_column <-community_data_longer|> 
  select(plotID, year, vegetation_cover) |> 
  filter(!is.na(vegetation_cover)) |>
  unique()

#Combining the new column "cover" and the elongated community data. Dataset with presence absence data.
community_clean <- community_data_longer |>
  left_join(cover_column, by = c("plot", "block", "site", "plotID", "year", "species"))|>
  select(-vegetation_cover)|>
  left_join(vegetation_cover_column, by = c("plotID", "year"))|>
  mutate(measure = case_when(year == 2022 & subPlot == 9 ~ "plot",
                             TRUE ~ measure)) |>
  unique()|>
  filter(subPlot != "cover")


# To get the data ready for turfmapper the wholeplot information "plot" in "measure" needs to be recoded to have the value 1 in the measure column. "plot" also needs to be filtered out in the subPlot column. This is done in seperate code.

#_______Turf mapper process happens here, see script for the code_______#



####__________Cleaning mistakes from dataset based on turfmapper evaluation__________####
#Several typing mistakes occurred when making the dataset. Therefor, we standardise again the rest of typing mistakes for the different species so its easier to work on when cleaning the data.
#General coding for all plots
community_clean <- community_clean |>
  mutate(species = ifelse(species == "Tri_eur", "Lys_eur", species))|>
  mutate(species = ifelse (species == "Antennaria_sp", "Ant_sp", species))|>
  mutate(species = ifelse (species == "Epilobium_sp", "Epi_sp", species))|>
  mutate(species = ifelse (species == "Juncus_sp", "Jun_sp", species))|>
  mutate(species = ifelse (species == "Ranunculus", "Ran_sp", species))|>
  mutate(species = ifelse(species %in% c("Eup_sp", "Eup_str"), "Eup_wet", species))|>
  mutate(species = ifelse (species == "Vio_riv", "Vio_can", species))|>
  mutate(species = ifelse(species == "Emp_her", "Emp_nig", species))|>
  mutate(species = ifelse(species == "Hup_sel", "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Gen_ana", "Gen_ama", species))|>
  mutate(species = ifelse(species == "Lyc_lyc", "Sel_sel", species))
#A couple of reason for renaming here. 1) misspellings or several names in the dataset that we combine to the name we want to use (Ant_sp, Epi_sp, Jun_sp, Ran_sp, Hyp_sel, Gen_ama), 2) species that some recorders missidentify (Eup_wet, Vio_can, Emp_nig, Sel_sel), 3) Old taxonomic names that not everyone has caught up on (Lys_eur).

#Plot and year specific changes based on evaluation from turfmapper#
#Following are changes to the dataset that can be grouped into two main ways of changing the data. 
#First merging information of two species together if we are confident that it is actually another species, or after analysing the turfmapper we can confirm that it is actually a specific species instead of just the genus that we wrote in the field (for example: Car_sp --> Car_big). 

# - mutate(species = ifelse(species == "the variable" & plotID == "the name of the plot", "the new name", species)) #The main code that rename and merges the information from two columns together in one. 

#Second this code adjusts the cover of a specific species in a specific plot in a specific year. Sometimes we needed to adjust the cover in the changes made above (when we merged species) and add up the cover of the two species that were merged. Other times this species, plot, year combination lacked a cover and this needed to be imputed by taking the average of the cover of this speices in that plot the year before and/or the year after.

# - mutate(cover = ifelse(species == "the variable" & plotID == "the name of the plot" & year == the year(s) you want the cover to apply for, the new cover, cover)) #You can also change "species" before "ifelse", and after "the new name" to change the cover for the plot. Here you need "year ==" to specify which year you want to change, if not, all covers for all year for the specific specie will be the same.

# - By putting in c("variable1", "variable2"...) beside "species ==", you can merge several columns together. You can also do the same for years, if there is several years you want the change to apply for. 

# - You can also put in "year ==" in when merging columns. Good to use when you have unknown species of cf that you want to split.

###### Skjellingahaugen ######

community_clean <- community_clean |> #Doubble check agr_mer and agr_cap in field 2023.
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_1_1" & year %in% c(2018, 2021), "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_1" & year == 2018, 4, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_1" & year == 2021, 5, cover ))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Skj_1_1" & year %in% c(2021, 2022), "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_1_1" & year == 2021, 2, cover), cover = ifelse(species == "Car_big" & plotID == "Skj_1_1" & year == 2022,6, cover ))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_1_1", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Skj_1_1", "Pin_vul", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_1_1", "Ran_acr", species)) |>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Skj_1_1", "Sib_pro", species))|>
  mutate(cover = ifelse(species == "Sib_pro" & plotID == "Skj_1_1" & year == 2018, 4, cover)) |> #Check turfmapper
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_nor") & plotID == "Skj_1_3" & year == 2018, "Car_big", species)) |> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_1_3" & year == 2018, 3, cover))|>
  mutate(species = ifelse(species %in% c("Agr_mer","Phl_alp") & plotID == "Skj_1_3", "Agr_cap", species))|>
  mutate(cover = ifelse(species =="Agr_cap" & plotID == "Skj_1_3" & year == 2022, 5, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_3" & year == 2018, 4, cover)) |>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_3" & year == 2019, 3, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_3" & year == 2021,6,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf","Agr_mer") & plotID == "Skj_1_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_4" & year == 2021, 25, cover))|>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_1_4", "Car_fla", species)) |> 
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_1_4" & year == 2021, "Leo_aut", species)) |>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_1_4" & year == 2022, "Fes_rub", species))|>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_1_5", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_5" & year == 2018, 12, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_5" & year == 2021, 13, cover))|>
  mutate(species = ifelse(species == "Pot_ere" & plotID == "Skj_1_5" & year == 2022, "Pot_cra", species))|>
  mutate(species = ifelse(species == "Car_atr" & plotID == "Skj_2_1" & year == 2018, "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_2_1" & year == 2018, 7, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_2_1" & year == 2018, "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_2_1" & year == 2018, 1, cover))|> #Was a proposal on if the Tar_sp is Hie_pil, however, non Hie_pil have been found earlier.
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_2_2", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_2_2" & year == 2021, 5, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Skj_2_2" & year == 2018, "Car_big", species)) |>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_2_2" & year == 2018, 6, cover))|>
  mutate(species = ifelse(species == "Gen_sp" & plotID == "Skj_2_2" & year == 2018, "Gen_niv", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_2_2" & year == 2022, "Ran_acr", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_2_3", "Epi_ana", species))|> 
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_2_3" & year == 2018,1,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_2_5" & year == 2022, "Alc_sp", species)) |>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_2_5" & year == 2018, "Cer_cer", species)) |>
  mutate(species = ifelse(species %in% c("Epi_ana_cf", "Epilobium_sp") & plotID == "Skj_2_5" & year %in% c(2018,2022), "Epi_ana", species)) |>
  mutate(species = ifelse(species == "Equ_sp" & plotID == "Skj_2_5" & year %in% c(2019, 2021), "Equ_arv", species)) |> 
  mutate(cover = ifelse(species == "Equ_arv" & plotID == "Skj_2_5" & year == 2021, 3, cover)) |>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Skj_2_6" & year == 2021, "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_2_6" & year == 2021, 3, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_2_6" & year == 2022, "Alc_sp", species)) |>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_2_6" & year == 2022, "Car_vag", species)) |>
  mutate(species = ifelse(species == "Poa_pra" & plotID == "Skj_2_6" & year == 2022, "Poa_alp", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_2_6" & year == 2021, "Ran_acr", species)) |> 
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Skj_2_6" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Ver_off_cf" & plotID == "Skj_2_6" & year == 2021, "Ver_off", species)) |>
  mutate(cover = ifelse(species == "Ver_off" & plotID == "Skj_2_6" & year == 2021, 2, cover)) |>
  mutate(species = ifelse(species == "Ach_mil" & plotID == "Skj_3_1", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Skj_3_1" & year == 2021, "Car_pal", species))|>
  mutate(cover = ifelse(species == "Car_pal" & plotID == "Skj_3_1" & year == 2021, 5, cover))|>
  mutate(species = ifelse(species == "Cer_cer" & plotID == "Skj_3_1" & year == 2018, "Cer_fon", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_3_1" & year == 2018, "Epi_ana", species))|> 
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_3_1" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species == "Geu_riv" & plotID == "Skj_3_1" & year == 2022, "Gen_niv", species))|>
  mutate(species = ifelse(species %in% c("Ach_mil", "Agr_cap_cf") & plotID == "Skj_3_3", "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_3_3" & year == 2018, 3, cover))|>
  mutate(species = ifelse(species == "Arc_urv" & plotID == "Skj_3_3" & year == 2022, "Bis_viv", species))|>
  mutate(cover = ifelse(species == "Bis_viv" & plotID == "Skj_3_3" & year == 2022, 10, cover))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Skj_3_3" & year == 2019, "Car_pil", species))|>
  mutate(species = ifelse(species == "Gen_cam_cf" & plotID == "Skj_3_3" & year == 2021, "Gen_niv", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Skj_3_3" & year == 2019, "Luz_mul", species))|>
  mutate(species = ifelse(species == "Sau_alp_cf" & plotID == "Skj_3_3" & year == 2022, "Sau_alp", species))|>
  mutate(cover = ifelse(species == "Sau_alp" & plotID == "Skj_3_3" & year == 2022, 10, cover))|>
  mutate(species = ifelse(species == "Bar_alp_cf" & plotID =="Skj_3_4", "Bar_alp", species))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Skj_3_4" & year == 2021, "Oma_sup", species))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_3_4" & year == 2019, "Rum_ace", species))|>
  mutate(species = ifelse(species %in% c("Nid_orchid", "Orchid") & plotID == "Skj_3_4", "Coel_vir", species))|>
  mutate(cover = ifelse(species == "Coel_vir" & plotID == "Skj_3_4" & year == 2018, 4, cover))|>
  mutate(species = ifelse(species == "Car_nor" & plotID == "Skj_3_6" & year == 2019, "Car_cap", species)) |>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Skj_3_6" & year == 2021, "Epi_ana", species)) |>
  mutate(species = ifelse(species == "Orchid" & plotID == "Skj_3_6", "Coel_vir", species)) |> 
  mutate(cover = ifelse(species == "Coel_vir" & plotID == "Skj_3_6" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_3_6" & year == 2019, "Rum_ace", species))|>
  mutate(species = ifelse(species == "Vac_myr_cf" & plotID == "Skj_3_6", "Vac_myr", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_4_1" & year == 2018, "Epi_ana", species)) |>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_4_1" & year == 2018, 1, cover))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_fla_CF") & plotID == "Skj_4_2" & year == 2018, "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_4_2" & year == 2018, 5, cover))|>
  mutate(species = ifelse(species == "Cer_fon" & plotID == "Skj_4_2" & year == 2021, "Cer_cer", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_4_2" & year == 2018, "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_4_2" & year == 2018, 3, cover))|>
  mutate(species = ifelse(species == "Vio_bif" & plotID == "Skj_4_2" & year == 2018, "Vio_pal", species))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Skj_4_2" & year == 2018, 12, cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Skj_4_3" & year == 2019, "Agr_cap", species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Skj_4_3" & year == 2019, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_4_3" & year == 2019, "Ran_acr", species))|>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_sp") & plotID == "Skj_4_4" & year %in% c(2018,2019), "Car_big", species))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_4_4" & year == 2019, "Rum_ace", species)) |>
  mutate(species= ifelse(species == "Car_big_cf" & plotID == "Skj_4_5" & year == 2018, "Car_big", species)) |> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_4_5" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Skj_4_5" & year == 2019, "Car_cap", species)) |> 
  mutate(cover = ifelse(species == "Car_cap" & plotID == "Skj_4_5" & year == 2018,1,cover))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Skj_4_5" & year == 2019, "Oma_sup", species)) |>
  mutate(cover = ifelse(species == "Oma_sup" & plotID == "Skj_4_5" & year == 2018, 3, cover))|>
  mutate(cover = ifelse(species == "Cam_rot" & plotID == "Skj_4_5" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Rum_ace" & plotID == "Skj_4_5" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Sag_sag" & plotID == "Skj_4_5" & year == 2018, 1, cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Skj_5_1" & year == 2021, "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_1" & year == 2021, 7, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_5_1" & year == 2022, "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_fla" & plotID == "Skj_5_1", "Car_big", species))|> 
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_5_1", "Fes_rub", species)) |>
  mutate(species = ifelse(species == "Hyp_mac" & plotID == "Skj_5_1" & year == 2021, "Nid_seedling", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Skj_5_1" & year == 2019, "Sib_pro", species)) |>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Skj_5_1" & year == 2021, "Ver_alp", species)) |>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Skj_5_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species %in% c("Car_fla", "Car_fla_CF") & plotID == "Skj_5_2" & year %in% c(2018,2021), "Car_vag", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Skj_5_2" & year == 2019, "Ave_fle", species))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Skj_5_2" & year == 2021, "Jun_tri", species)) |>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_5_2" & year == 2022, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_5_3" & year == 2022, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Skj_5_4" & year == 2022, "Car_pil", species))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Skj_5_4" & year == 2021, 1, cover))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Skj_5_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species %in% c("Agr_mer_CF", "Agr_mer") & plotID == "Skj_5_5" & year %in% c(2018,2021), "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_5" & year == 2018, 3, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_5" & year == 2021, 8, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Skj_5_5" & year == 2022, "Hie_pil", species))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Skj_5_5" & year == 2018, "Jun_tri", species))|>
  mutate(cover = ifelse(species == "Jun_tri" & plotID == "Skj_5_5" & year == 2018, 5, cover))|>
  mutate(species = ifelse(species %in% c("Phl_alp", "Agr_mer") & plotID == "Skj_5_6", "Agr_cap", species)) |>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_6" & year == 2018, 7, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_6" & year == 2021, 11, cover))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_6_1", "Cer_cer", species))|>
  mutate(species = ifelse(species %in% c("Hie_pil", "Hie_sp") & plotID == "Skj_6_2" & year %in% c(2018,2019,2022), "Hie_alp", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Skj_6_2" & year == 2021, "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_6_2" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_6_2" & year == 2018, "Car_vag", species))|>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_6_3" & year == 2021, "Car_big", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_6_3" & year == 2018, "Cer_cer", species)) |>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Skj_6_3", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_6_4", "Car_vag", species))|> #Sjekk om overlapp
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Skj_6_4" & year == 2021, "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_6_4" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_6_6" & year == 2018, "Car_vag", species))|> 
  mutate(species = ifelse(species == "Sel_sp" & plotID == "Skj_6_6" & year == 2019, "Sel_sel", species))|>
  mutate(cover = ifelse(species == "Sel_sel" & plotID == "Skj_6_6" & year == 2019,1,cover))|>
  mutate(cover = ifelse(species == "Leo_aut" & plotID == "Skj_6_6" & year == 2019, 3, cover))|>
  mutate(species = ifelse(species == "Car_lep" & plotID == "Skj_7_1" & year == 2021, "Car_big", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Skj_7_1" & year == 2019, "Car_cap", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_7_1" & year == 2019, "Car_vag", species))|> 
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Skj_7_1" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Skj_7_1" & year == 2019, "Cer_fon", species)) |> 
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Skj_7_1" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Jun_tri" & plotID == "Skj_7_1" & year == 2019, "Ave_fle", species))|>
  mutate(species = ifelse(species == "jamne" & plotID == "Skj_7_1" & year == 2019, "Sel_sel", species))|>
  mutate(cover = ifelse(species == "Sel_sel" & plotID == "Skj_7_1" & year == 2019,3,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap", "Agr_mer_CF") & plotID == "Skj_7_2" & year %in% c(2019,2022), "Ant_odo", species))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Skj_7_2" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Skj_7_2" & year == 2022, 4, cover))|>
  mutate(species = ifelse(species == "Car_cap" & plotID == "Skj_7_5" & year == 2021, "Car_big", species))

###### Gudmeddalen ######

community_clean <- community_clean|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_2" & year == 2019, "Car_big", species))|>
  mutate(species = ifelse(species == "Vio_pal_cf" & plotID == "Gud_1_3", "Vio_pal", species))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Gud_1_3" & year == 2019, 4 ,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_4", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Bet_sp" & plotID == "Gud_1_4", "Bet_nan", species)) |>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_5", "Agr_cap", species)) |>
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_1_5", "Car_fla", species))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_6", "Agr_cap", species))|>
  mutate(cover = ifelse (species == "Ast_alp" & plotID == "Gud_1_6" & year == 2022, 3, cover ))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Gud_1_6", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_fla" & plotID == "Gud_1_6" & year == 2019, "Car_big", species))|>
  # mutate(cover = ifelse(species == "Ave_fle" & plotID == "Gud_1_6" & year == 2021, 3,cover))|> Not sure why I converted this one
  mutate(species = ifelse(species =="Ave_fle" & plotID == "Gud_1_6" & year == 2019 & subPlot %in% c(7,14,19,20,26,28,30,21,32), "Fes_rub", species))|> 
  mutate(species = ifelse(species =="Ave_fle" & plotID == "Gud_1_6" & year == 2022 & subPlot %in% c(19,26), "Fes_rub", species))|> 
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_2_2", "Car_fla", species))|> #Sjekk
  mutate(species = ifelse(species == "Car_pil" & plotID == "Gud_2_2", "Car_pal", species))|>
  mutate(species = ifelse(species == "Rum_ace_cf" & plotID == "Gud_2_2", "Rum_ace", species))|>
  mutate(species = ifelse(species == "Vio_tri_cf" & plotID == "Gud_2_3", "Vio_tri", species))|>
  mutate(species = ifelse(species == "Ver_ser_cf" & plotID == "Gud_2_4", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Gud_2_4" & year == 2019, 5, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_3_2", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_3_2" & year %in% c(2018,2019), 5, cover))|>
  mutate(species = ifelse(species %in% c("Pyr_sp", "Pyr_sp_IKKE_rotundifolia") & plotID == "Gud_3_2", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Gud_3_2", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Gud_3_2" & year %in% c(2018,2019,2022), 2, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Gud_3_3", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Gud_3_3" & year %in% c(2018,2019), 2, cover))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Gud_3_3", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Gud_3_3", "Ran_acr", species))|>
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Gud_3_3" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Gud_3_3" & year == 2021, 3, cover))|>
  mutate(species = ifelse(species == "Vio_pal" & plotID == "Gud_3_3", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Gud_3_3" & year == 2019, 11, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Gud_3_5", "Hie_alp", species)) |>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Gud_3_5", "Unknown", species))|>
  mutate(cover = ifelse (species == "Ave_fle" & plotID == "Gud_3_5" & year == 2019,2,cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_3_5" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Gud_3_6", "Hie_alp", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_6" & year == 2019, "Car_big", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_6" & year == 2021 & subPlot %in% c(4,5,6,28,33), "Car_pil", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_6" & year == 2021 & subPlot == 29, "Car_pil", species))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_4_1", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Pot_ere_cf" & plotID == "Gud_4_1" & year == 2021, "Pot_ere", species))|>
  mutate(species = ifelse(species =="Car_big" & plotID == "Gud_4_1" & year == 2019, "Car_fla", species))|>
  mutate(cover = ifelse(species == "Car_fla" & plotID == "Gud_4_1" & year == 2019, 11, cover))|>
  mutate(species = ifelse(species =="Car_big" & plotID == "Gud_4_1" & year == 2022, "Car_vag", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Gud_4_1" & year == 2019, "Car_nor", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Gud_4_1" & year == 2021, 7, cover))|>
  mutate(cover = ifelse(species == "Ast_alp" & plotID == "Gud_4_1" & year == 2019, 1, cover))|>
  mutate(cover = ifelse(species == "Ast_alp" & plotID == "Gud_4_3", 3, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_4_3", "Car_big", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Gud_4_3","Car_nor", species ))|>
  mutate(species = ifelse(species == "Ant_odo" & plotID == "Gud_4_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Gud_4_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Alc_alp_cf" & plotID == "Gud_4_4", "Alc_alp", species))|>
  mutate(cover = ifelse(species == "Alc_alp" & plotID == "Gud_4_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Gud_4_4", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Gud_4_4" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Gud_4_6", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Gud_4_6" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Par_pal_cf" & plotID == "Gud_4_6", "Par_pal", species))|>
  mutate(species = ifelse(species == "Pot_cra" & plotID == "Gud_4_6", "Pot_ere", species))|>
  mutate(cover = ifelse(species == "Pot_ere" & plotID == "Gud_4_6" & year == 2019, 9, cover))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Tri_ces") & plotID == "Gud_5_1", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Car_atr" & plotID == "Gud_5_1", "Car_sax", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_5_2" & year == 2019, "Car_sax", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Gud_5_2", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Nid_juvenile" & plotID == "Gud_5_2", "Nid_seedling", species))|>
  mutate(species = ifelse(species %in% c("Nid_orchid", "Orchid") & plotID == "Gud_5_2", "Coel_vir", species))|>
  mutate(species = ifelse(species == "Lyc_alp" & plotID == "Gud_5_4" & year == 2018, "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Gud_5_4", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_5_5", "Car_fla", species))|>
  mutate(cover = ifelse(species == "Car_fla" & plotID == "Gud_5_5" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Gud_5_5", "Hie_pil", species))|>
  mutate(species = ifelse(species %in% c("Lyc_alp","Sel_sel") & plotID == "Gud_5_5", "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Par_pal_cf" & plotID == "Gud_5_5", "Par_pal", species))|>
  mutate(species = ifelse(species %in% c("Pot_cra", "Pot_ere_cf") & plotID == "Gud_5_5", "Pot_ere", species))|>
  mutate(cover = ifelse(species == "Pot_ere" & plotID == "Gud_5_5" & year == 2019, 9, cover))|>
  mutate(species = ifelse(species %in% c("Pyr_sp", "Pyr_sp_cf") & plotID == "Gud_5_5", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Dip_alp" & plotID == "Gud_5_5", "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Ave_fle_cf" & plotID == "Gud_5_6", "Ave_fle", species))|> 
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Gud_5_6" & year == 2018,5,cover))|>
  mutate(species = ifelse(species == "Ave_fle" & plotID == "Gud_6_1", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Hie_pil" & plotID == "Gud_6_1", "Sol_vir", species))|>
  mutate(species = ifelse(species == "Vio_bif" & plotID == "Gud_6_1", "Vio_pal", species))|>
  mutate(cover = ifelse(species =="Vio_pal" & plotID == "Gud_6_1" & year == 2022, 3,cover))|>
  mutate(species = ifelse(species == "Pyr_sp_IKKE_rotundifolia" & plotID == "Gud_6_2", "Pyr_sp",species))|>
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_6_3", "Car_fla", species))|>
  mutate(species = ifelse(species == "Vio_can_cf" & plotID == "Gud_6_3", "Vio_can", species))|>
  mutate(cover = ifelse(species == "Sil_aca" & plotID == "Gud_6_3" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Gud_6_4", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Gud_6_6", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Hie_pil_cf" & plotID == "Gud_6_6", "Hie_pil", species))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Gud_7_1", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_7_1" & year == 2018,3,cover))|>
  mutate(species = ifelse(species == "Car_pal_cf" & plotID == "Gud_7_1", "Car_pal", species ))|>
  mutate(cover = ifelse(species == "Eup_wet" & plotID == "Gud_7_1" & year == 2019, 5, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_7_2", "Car_big", species))|>
  mutate(species = ifelse(species == "Vio_can" & plotID == "Gud_7_2", "Vio_pal", species))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Gud_7_2" & year == 2019, 3, cover))|>
  mutate(species = ifelse(species == "Ver_cha_cf" & plotID == "Gud_7_2", "Ver_cha", species))|> #Sjekk!
  mutate(cover = ifelse(species =="Vac_myr" & plotID == "Gud_7_2" & year == 2022, 9,cover))|>
  mutate(species = ifelse(species == "Ast_alp_cf" & plotID == "Gud_7_3", "Ast_alp", species))|> 
  mutate(cover = ifelse(species == "Ast_alp" & plotID == "Gud_7_3" & year == 2019, 21,cover))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_3" & year == 2018, "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_7_3" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_7_4", "Car_fla", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Gud_7_4","Car_pal", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Gud_7_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Pyr_sp_IKKE_rotundifolia" & plotID == "Gud_7_4", "Pyr_sp",species))|>
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Gud_7_4" & year == 2018 & is.na(cover),0,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Gud_7_6", "Agr_cap", species))|>
  mutate(cover = ifelse(species =="Agr_cap" & plotID == "Gud_7_6" & year == 2019, 7, cover))|>
  mutate(species = ifelse(species %in% c("Fes_rub","Fes_rub_cf_kanskje_Ave_fle") & plotID == "Gud_7_6" & year %in% c(2018, 2019), "Fes_rub", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Gud_7_6", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Gud_7_6", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Gud_7_6", "Car_cap", species))|>
  mutate(cover = ifelse(species == "Car_cap" & plotID == "Gud_7_6" & year %in% c(2021,2022),1 ,cover))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Gud_7_6" & year == 2019, 6, cover))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Gud_7_6" & year == 2019, 1,cover))

###### Lavisdalen ######

community_clean <- community_clean |>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_1_1", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_1_1", "Car_sp", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_1_1", "Car_nor", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_1", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Lav_1_1" & year == 2019, 4,cover))|>
  mutate(species = ifelse(species == "Fes_ovi_cf" & plotID == "Lav_1_1", "Fes_ovi", species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_1_1" & year == 2019, 3,cover))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_1_1", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_1_1" & year == 2019, 1,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_1_2", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_2", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Lav_1_2" & year == 2018,1,cover))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Lav_1_2" & year == 2019, 4,cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_1_2", "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_1_2" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species == "Fes_ovi_cf" & plotID == "Lav_1_2", "Fes_ovi", species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_1_2" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species == "Luz_spi_cf" & plotID == "Lav_1_2", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_1_2", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_1_2" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_1_3", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_3" & year == 2019, 7,cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_3" & year == 2021, 2,cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_1_3", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_1_3" & year == 2019, 1,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_1_3", "Car_nor", species))|>
  mutate(cover = ifelse(species == "Bis_viv" & plotID == "Lav_1_3" & year == 2022, 4,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_1_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_4" & year == 2021, 2,cover))|>
  mutate(species = ifelse(species == "Ant_odo_cf" & plotID == "Lav_1_4", "Ant_odo", species))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Lav_1_4" & year == 2019, 5,cover))|>
  mutate(species = ifelse(species %in% c("Cer_alp_cf", "Cer_alp") & plotID == "Lav_1_4", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID =="Lav_1_4" & year == 2019, 3,cover))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_4", "Cer_cer", species))|>
  mutate(species = ifelse(species == "Eri_uni_cf" & plotID == "Lav_1_4", "Eri_uni", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_1_4", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_1_4" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_1_6", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_6" & year == 2021, 2,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_1_6", "Car_nor", species))|> 
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_6", "Cer_cer", species))|> 
  mutate(species = ifelse(species == "Cer_alp" & plotID =="Lav_1_6", "Cer_fon",species))|>
  mutate(species = ifelse(species == "Fes_ovi_cf" & plotID =="Lav_1_6", "Fes_ovi",species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_1_6" & year == 2019, 1,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_1", "Car_nor", species))|> 
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_2_2", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_2", "Car_nor", species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_2_2",2, cover))|>
  mutate(species = ifelse(species == "Car_sp_den_lyse" & plotID == "Lav_2_2", "Car_pil", species))|>
  mutate(cover = ifelse(species == "Car_pil" & plotID == "Lav_2_2" & year == 2019 ,3, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Lav_2_2", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_2_2",4, cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_2_3", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Alc_alp_cf" & plotID == "Lav_2_3", "Alc_alp",species))|>
  mutate(cover = ifelse(species == "Alc_alp" & plotID == "Lav_2_3" & year == 2019,13,cover))|>
  mutate(species = ifelse(species == "Ave_fle_cf" & plotID == "Lav_2_3", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Lav_2_3" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Car_pil" & plotID == "Lav_2_3", "Car_pal", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_3", "Car_nor", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_2_3", "Poa_pra", species))|>
  mutate(species = ifelse(species %in% c("Ranunculus", "Ran_acr") & plotID == "Lav_2_3", "Ran_pyg", species))|>
  mutate(species = ifelse(species %in% c("Vac_myr", "Ver_alp_cf") & plotID == "Lav_2_3", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_2_3" & year == 2019,3,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_2_4", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_4", "Car_nor", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_2_4", "Poa_pra", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_2_4", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ran_sp" & plotID == "Lav_2_4", "Ran_acr", species))|>
  mutate(species = ifelse (species == "Ver_alp_cf" & plotID == "Lav_2_4", "Ver_alp", species))|> 
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_2_4" & year == 2019,3,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_2_5" & year == 2022, "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_5" & year == 2019, "Car_nor", species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_2_5" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_2_5" & year == 2019, "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_2_5" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Poa_alp_cf" & plotID == "Lav_2_5" & year == 2019, "Poa_alp", species))|>
  mutate(cover = ifelse(species == "Poa_alp" & plotID == "Lav_2_5" & year == 2019,2,cover))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Lav_2_5" & year == 2021, 5,cover))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_2_5", "Pyr_min", species))|>
  mutate(species = ifelse (species == "Ver_alp_cf" & plotID == "Lav_2_5", "Ver_alp", species))|> 
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_2_5" & year == 2019,2,cover))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Lav_2_6", "Oma_sup", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_2_6", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Lav_2_6", "Ran_acr", species))|>
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Lav_2_6" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Vac_myr" & plotID == "Lav_2_6", "Sal_her", species))|>
  mutate(cover = ifelse(species == "Sal_her" & plotID == "Lav_2_6" & year == 2021, 5, cover ))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Lav_3_1", "Ave_fle", species))|>
  mutate(species = ifelse(species %in% c("Agr_cap","Agr_mer_CF") & plotID == "Lav_3_1", "Agr_mer", species))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_3_1" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_3_1" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_3_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_3_1", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Lav_3_1" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_3_1", "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_3_1" & year == 2019, 3, cover))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Lav_3_1", "Ran_acr", species))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Lav_3_1", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Lav_3_1" & year == 2021,2,cover))|>
  mutate(species = ifelse(species == "Car_nor" & plotID == "Lav_3_3" & year == 2021, "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_3_3" & year == 2021, 6, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_3_3", "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_3_3" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Lav_3_3", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Lav_3_3" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Oxy_dig" & plotID == "Lav_3_3" & year == 2019,2 , cover))|>
  mutate(species = ifelse(species == "Tar_sp_cf" & plotID == "Lav_3_3", "Tar_sp", species))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Lav_3_3" & year == 2019,7 , cover))|>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Lav_3_3", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_3_3" & year == 2019, 2, cover)) |>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Lav_3_3", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Lav_3_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Lav_3_4" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species %in% c("Car_sp","Car_vag", "Car_vag_CF") & plotID == "Lav_3_4","Car_big", species ))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_3_4" & year == 2019, 6, cover))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Lav_3_5", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Lav_3_5" & year == 2019, 7, cover))|>
  mutate(cover = ifelse(species == "Rum_ace" & plotID == "Lav_3_5" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_3_6", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_3_6" & year == 2021,2,cover))|>
  mutate(species = ifelse(species %in% c("Car_sp","Car_nor_cf") & plotID == "Lav_3_6", "Car_nor", species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_3_6" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_3_6" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species =="Fes_viv" & plotID == "Lav_3_6", "Fes_ovi", species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_3_6" & year == 2021, 4, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_4_1", "Car_nor", species))|> # MC% sjekkes nC8ye i 2023!!!
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Lav_4_1", "Car_vag", species))|>
  mutate(species = ifelse(species == "Gen_cam_cf" & plotID == "Lav_4_1", "Gen_cam", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Lav_4_1", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Sal_sp" & plotID == "Lav_4_1", "Sal_lan", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_4_3","Car_nor",species))|>
  mutate(species = ifelse(species =="Des_ces" & plotID == "Lav_4_2","Des_alp",species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Lav_4_4", "Car_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_4_4", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Lav_4_5", "Car_cap", species))|>
  mutate(species = ifelse(species == "Fes_ovi" & plotID == "Lav_4_5", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Lav_4_5", "Pyr_sp", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_5_2", "Car_big", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_5_2", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_5_2"& year == 2021, 10, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_5_2", "Car_nor", species))|>
  mutate(species = ifelse(species %in% c("Car_nig_cf", "Car_nor_cf") & plotID == "Lav_5_3", "Car_nor",species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_5_3" & year == 2021,2,cover))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_5_3" & year == 2022,1,cover))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_5_3", "Car_pil", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_5_5", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_5_5" & year == 2021,2,cover))|>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_nor_cf") & plotID == "Lav_5_5", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_5_5" & year == 2019,4,cover))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_5_5" & year == 2021,4,cover))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Lav_5_5", "Cer_fon", species))|>
  mutate(species = ifelse(species %in% c("Hie_alp", "Hie_sp") & plotID == "Lav_5_5", "Hie_pil",species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Lav_5_5", "Poa_pra", species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Lav_5_5", "Pyr_sp", species))|>
  mutate(species = ifelse(species %in% c("Agr_mer", "Agr_cap_cf") & plotID == "Lav_5_6", "Agr_cap", species))|>
  mutate(cover = ifelse (species == "Agr_cap" & plotID == "Lav_5_6" & year == 2021,2,cover))|>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_nor_cf")& plotID == "Lav_5_6", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_5_6" & year == 2019,5,cover))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Lav_5_6", "Cer_fon",species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Lav_5_6", "Pyr_sp", species))|>
  mutate(species = ifelse(species == "Sal_sp" & plotID == "Lav_5_5", "Sal_lan", species))|>
  mutate(species = ifelse(species == "Agr_cap" & plotID == "Lav_6_2", "Agr_mer", species))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_6_2" & year == 2018,2,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Lav_6_2", "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_cap" & plotID == "Lav_6_2", "Car_nor", species ))|>
  mutate(species = ifelse(species == "Agr_cap" & plotID == "Lav_6_3" & year == 2022, "Agr_mer", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_6_3", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_sp_den_lyse" & plotID == "Lav_6_3", "Car_sp", species))|>
  mutate(species = ifelse(species == "Car_sp_den_lyse" & plotID == "Lav_6_5", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_6_5" & year == 2022, 7,cover))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Lav_6_5", "Jun_tri", species))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Lav_6_5", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Lav_6_5" & year == 2022, 1,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_6_6", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_6_6", "Car_pil", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Lav_7_1", "Car_nor", species))|>
  mutate(species = ifelse(species %in% c("Fes_viv_cf", "Fes_ovi") & plotID == "Lav_7_1", "Fes_viv", species))|>
  mutate(cover = ifelse(species == "Fes_viv" & plotID == "Lav_7_1" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Fes_viv" & plotID == "Lav_7_1" & year == 2022, 2, cover))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_7_1", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Lav_7_1", "Ran_acr", species))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Lav_7_2", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_7_2", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_7_2", 3, cover))|>
  mutate(species = ifelse(species == "Fes_ovi" & plotID == "Lav_7_2", "Fes_viv", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Lav_7_2", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Lav_7_2", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_7_2" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Ave_fle" & plotID == "Lav_7_3", "Fes_ovi", species))|>
  mutate(species = ifelse(species == "Des_ces" & plotID == "Lav_7_3", "Des_alp", species))|>
  mutate(cover = ifelse(species =="Des_alp" & plotID == "Lav_7_3" & year == 2021, 6,cover))|>
  mutate(species = ifelse(species == "Fes_viv" & plotID == "Lav_7_3", "Fes_ovi", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Lav_7_2", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Lav_7_2", "Poa_pra", species))

###### Ulvehaugen ######

community_clean <- community_clean |>
  mutate(species = ifelse(species == "Vio_sp" & plotID == "Ulv_1_1", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_1_1" & year == 2021, 7, cover))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_1_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Ave_fle_cf" & plotID == "Ulv_1_3", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Ulv_1_3", "Car_vag", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Ulv_1_3", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Ulv_1_3", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_1_3" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Tri_sp" & plotID == "Ulv_1_3", "Tri_rep", species))|>
  mutate(cover = ifelse(species == "Tha_alp" & plotID == "Ulv_1_3" & year == 2021, 1, cover))|>
  mutate(cover = ifelse(species == "Vac_vit" & plotID == "Ulv_1_3" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_1_4", "Alc_sp", species))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Ulv_1_4" & year == 2019,4,cover))|>
  mutate(species = ifelse(species == "Cer_sag_cf" & plotID == "Ulv_1_4", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Ulv_1_4" & year == 2019,4,cover))|>
  mutate(species = ifelse(species == "Oma_nor" & plotID == "Ulv_1_4", "Oma_sup", species))|>
  mutate(species = ifelse(species == "Poa_pra" & plotID == "Ulv_1_4", "Poa_alp", species))|>
  mutate(species = ifelse(species == "Unknown" & plotID == "Ulv_1_4", "Des_ces", species))|>
  mutate(species = ifelse(species == "Cer_alp_cf" & plotID == "Ulv_1_5", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Ulv_1_5" & year == 2019, 6,cover))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_1_5", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_2_1", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Vio_pal" & plotID == "Ulv_2_1", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_2_1" & year == 2019, 6, cover))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Ulv_2_2", "Car_pil", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_2_2", "Leo_aut", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Ulv_2_2" & year == 2022,1,cover ))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Ulv_2_3", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Ulv_2_3" & year == 2019,19,cover))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Ulv_2_3" & year == 2022,2,cover))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_2_3", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Ulv_2_4", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Ulv_2_4" & year == 2022, 2,cover))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Ulv_2_4", "Oma_sup", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_2_4" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Ulv_2_4", "Sib_pro", species))|>
  mutate(cover = ifelse(species == "Leo_aut" & plotID == "Ulv_2_4" & year == 2018, 1,cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_2_4" & year == 2022,3,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_2_5", "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Ulv_2_5", "Car_cap", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_2_5", "Leo_aut", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_2_5" & year == 2022,2, cover))|>
  mutate(species = ifelse(species %in% c("Lyc_sp", "Sel_sp") & plotID == "Ulv_2_5", "Sel_sel", species))|>
  mutate(species = ifelse(species == "Ver_cha" & plotID == "Ulv_2_5", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Ulv_2_5" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Sib_pro" & plotID == "Ulv_2_5" & year == 2019, 5,cover))|>
  mutate(cover = ifelse(species =="Alc_sp" & plotID == "Ulv_2_5" & year == 2021, 30, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Ulv_3_1", "Epi_ana", species))|> 
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Ulv_3_1", "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_3_1" & year == 2018,4,cover))|>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Ulv_3_2", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Ulv_3_2" & year == 2019, 14, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_3_2", "Alc_sp", species))|>
  mutate(cover = ifelse(species == "Des_ces" & plotID == "Ulv_3_2" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Unknown" & plotID == "Ulv_3_2", "Coel_vir", species))|>
  mutate(species = ifelse(species == "Epi_nor" & plotID == "Ulv_3_2", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Ulv_3_2", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_3_2", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Epi_nor" & plotID == "Ulv_3_3", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Ver_cha" & plotID == "Ulv_3_3", "Ver_alp", species))|>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Ulv_3_3", "Ver_alp", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Ulv_3_4", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Ulv_3_4", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Epi_nor" & plotID == "Ulv_3_4", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_3_5", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Ulv_3_5", "Hie_pil",species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Ulv_3_5", "Leo_aut", species))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Ulv_4_1", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_4_1" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_4_1" & year == 2021, 4, cover))|>
  mutate(species = ifelse(species == "Car_sp_2" & plotID == "Ulv_4_1", "Car_sp", species))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Ulv_4_1", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Poa_alp" & plotID == "Ulv_4_1" & year == 2019 ,1,cover))|>
  mutate(cover = ifelse(species == "Sal_her" & plotID == "Ulv_4_1" & year == 2019 ,1,cover))|>
  mutate(species = ifelse(species == "Vio_sp" & plotID == "Ulv_4_1", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_4_1" & year == 2021, 5,cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Ulv_4_3", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_4_3" & year == 2018, 17,cover))|>
  mutate(species = ifelse(species %in% c("Gen_sp", "Gen_ana") & plotID == "Ulv_4_3", "Gen_ama", species))|>
  mutate(species = ifelse(species == "Pot_ere" & plotID == "Ulv_4_3", "Pot_cra", species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Ulv_4_3", "Poa_pra", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Ulv_4_4", "Car_pil", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Ulv_4_4", "Sib_pro", species))|>
  mutate(species = ifelse(species == "Vio_pal" & plotID == "Ulv_4_4", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_4_4" & year == 2022, 6,cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Ulv_5_1", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Ulv_5_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species %in% c("Unknown", "Eri_uni_cf") & plotID == "Ulv_5_1", "Eri_uni", species))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Ulv_5_1", "Hie_alp", species))|>
  mutate(species = ifelse(species == "Rum_ace_cf" & plotID == "Ulv_5_1", "Rum_ace", species))|>
  mutate(cover = ifelse(species == "Rum_ace" & plotID == "Ulv_5_1" & year == 2021, 3, cover))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Ulv_5_1" & year == 2021, 2, cover))|>
  mutate(cover = ifelse(species == "Cam_rot" & plotID == "Ulv_5_1" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Ulv_5_3", "Rum_ace", species))|>
  mutate(species = ifelse(species %in% c("Car_sp", "Car_big") & plotID == "Ulv_5_3", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Ulv_5_3" & year == 2018,8, cover))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Ulv_5_3", "Rum_ace", species))|>
  mutate(species = ifelse(species == "Car_big" & plotID == "Ulv_5_4", "Car_vag", species))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Ulv_5_4", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Jun_sp" & plotID == "Ulv_5_4", "Jun_tri", species))|>
  mutate(cover = ifelse(species == "Jun_tri" & plotID == "Ulv_5_4" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Ulv_5_4", "Pyr_min", species))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Ulv_5_4" & year == 2019, 1,cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_5_5" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Ulv_5_5", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Ulv_5_5", "Car_vag", species))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Ulv_5_5", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Ulv_5_5", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Ulv_5_5", "Pyr_sp", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Ulv_5_5", "Sib_pro", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_6_1", "Leo_aut", species))|>
  mutate(species = ifelse(species =="Alc_alp_cf" & plotID == "Ulv_6_1", "Alc_alp", species))|>
  mutate(cover = ifelse(species =="Alc_alp" & plotID == "Ulv_6_1" & year == 2021, 1,cover))|>
  mutate(species = ifelse(species == "Car_sp_smal" & plotID == "Ulv_6_2", "Car_cap", species))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Ulv_6_2", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_6_2" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_6_4", "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_sp_smal" & plotID == "Ulv_6_4", "Car_sp", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Ulv_6_4", "Car_nor", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_6_5", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Ulv_6_5", "Car_nor", species))|>
  mutate(species = ifelse(species == "Agr_cap" & plotID == "Ulv_6_6", "Agr_mer", species))|>
  mutate(cover = ifelse(species =="Agr_mer" & plotID == "Ulv_6_6" & year == 2022,16,cover))|>
  mutate(species = ifelse(species == "Car_sp_smal" & plotID == "Ulv_6_6", "Car_big", species))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle") & plotID == "Ulv_7_2", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Ulv_7_2", "Car_big", species))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Ulv_7_2", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Car_pil" & plotID == "Ulv_7_2" & year == 2019, 1, cover))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Ulv_7_2" & year == 2021, 3, cover))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle") & plotID == "Ulv_7_3", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Alc_sp" & plotID == "Ulv_7_3", "Alc_alp", species))|>
  mutate(species = ifelse(species == "Hyp_mac" & plotID == "Ulv_7_3", "Hyp_sp", species))|>
  mutate(cover = ifelse(species == "Ver_off" & plotID == "Ulv_7_2" & year == 2019 & is.na(cover),0, cover))|>
  mutate(cover = ifelse(species =="Vio_can" & plotID == "Ulv_7_2" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle", "Fes_ovi") & plotID == "Ulv_7_4", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Sil_aca_cf" & plotID == "Ulv_7_4", "Sil_aca", species))|>
  mutate(cover = ifelse(species == "Tha_alp" & plotID == "Ulv_7_4" & year == 2019, 1, cover))|>
  mutate(cover = ifelse(species == "Ach_mil" & plotID == "Ulv_7_4" & year == 2019 & is.na(cover),0, cover))


###### Ulv_7_3 2018 ######
#The Ulv_7_3 2018 plot lacks cover. To use the data, we have decided to give it the same cover as the year after

#Making a dataset with the 2019 coverdata for the species in Ulv_7_3
Ulv_7_3_2019 <- community_clean|>
  filter(plotID == "Ulv_7_3") |>
  filter(year == 2019) |>
  select(species, cover, plotID)|>
  mutate(year = 2018)|>
  unique()|>
  rename(imputed_cover = cover)

community_clean <- community_clean|>
  left_join(Ulv_7_3_2019, by = c("species", "plotID", "year")) |>
  mutate(cover = ifelse(plotID == "Ulv_7_3" & year == 2018, imputed_cover, cover)) |> #Using coverdata from 2019
  select(-imputed_cover) |>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Ulv_7_3" & year == 2018, 1, cover)) #Fes_ovi did not have a cover in 2019, it was only present in one subplot therefore it gets a 1 in cover.

community_clean <- community_clean|>
  mutate(functional_group = case_when(species %in% c("Ant_odo", "Eup_wet", "Sib_pro", "Alc_alp", "Alc_sp", "Oma_sup", "Ver_alp", "Vio_pal", "Cam_rot", "Sag_sag", "Leo_aut", "Sel_sel", "Pyr_sp", "Luz_mul", "Tar_sp", "Pot_cra", "Dip_alp", "Tha_alp", "Lys_eur", "Hie_alp", "Rum_ace", "Cer_cer", "Epi_ana", "Equ_arv", "Epi_sp", "Tof_pus", "Nid_seedling", "Bar_alp", "Sil_aca", "Par_pal", "Hie_alp", "Cer_fon", "Pot_ere", "Vio_bif", "Coel_vir", "Ran_acr", "Gen_niv", "Pin_vul", "Eri_sp", "Ach_mil", "Pyr_min", "Bis_viv", "Ast_alp", "Rum_acl", "Bot_lun", "Gen_ama", "Ran_sp", "Oxy_dig", "Fern", "Ger_syl", "Geu_riv", "Rhi_min", "Hie_sp", "Tri_ces", "Hyp_sel", "Sol_vir", "Vio_can", "Ort_sec", "Pru_vul", "Ver_off", "Suc_pra", "Hyp_mac", "Ran_pyg", "Dry_oct", "Luz_spi", "Tri_rep", "Hyp_sp", "Ste_gra", "Sel_sp", "Vio_tri", "Ver_cha", "Nid_juvenile", "Gen_sp", "Tri_sp", "Oma_sp", "Cer_alp", "Tri_pra", "Sil_vul", "Sag_sp", "Phe_con", "Gym_dry", "Oma_nor", "Gal_sp", "Gen_cam", "Oxa_ace", "Lot_cor", "Aco_sep", "Eri_uni", "Equ_sci", "Sau_alp", "Leu_vul", "Hie_pil", "Vio_sp", "Gal_bor", "Lyc_alp") ~ "Forbs",
                                      species %in% c( "Nar_str", "Agr_mer", "Agr_cap", "Car_big", "Car_nor", "Car_cap", "Car_pal", "Car_pil", "Poa_pra", "Car_vag", "Ave_fle", "Des_ces", "Poa_alp", "Jun_tri", "Phl_alp", "Fes_ovi", "Fes_rub", "Sau_alp", "Fes_sp" ,"Car_sp", "Ant_dio", "Fes_viv", "Des_alp", "Car_fla", "Car_sax", "Ant_sp", "Car_atr" ) ~ "Graminoids",
                                      species %in% c("Sal_her", "Vac_myr", "Vac_uli", "Sal_sp", "Bet_nan", "Bet_pub", "Sal_lan") ~"Deciduous_shrubs",
                                      species %in% c("Emp_nig", "Vac_vit", "Cal_vul", "Arc_urc") ~ "Evergreen_shrubs")) |>
  group_by(plotID, year) |>
  mutate(recorder = paste(unique(recorder), collapse = " & "),
         writer = paste(unique(writer), collapse = " & ")) |>
  ungroup() |>
  group_by(plotID, subPlot, year, species) |>
  mutate( #If any species have been duplicated (ex: changing Agr_mer to Agr_cap if there were already an Agr_cap in that plot), combine this information in value, fertile, seedling and juvenile.
    value = paste(unique(value), collapse = ""),
    fertile = any(fertile),
    seedling = any(seedling),
    juvenile = any(juvenile)) |>
  ungroup() |>
  group_by(plotID, year) |>
  mutate(
    date = min(date)) #If data collection was done over several days pick the first date.


community_clean <- community_clean|>
  mutate(bare_ground = soil) |>
  mutate(bare_ground = ifelse(!is.na(bare), bare, bare_ground)) |>
  select(-block, -plot, -bare, -soil)

total_cover <- community_clean |>
  select(plotID, subPlot, moss, year, measure, poo, lichen, litter, rock, fungus, bare_ground) |>
  filter(measure == "subPlot") |>
  unique() |>  #Gjør at koden kun tar de unike verdiene, og tar derfor vekk kopier. 
  group_by(plotID, year) |>
  mutate(total_bryophyte_cover = sum(moss, na.rm = TRUE) / 29, 
         total_litter_cover = sum(litter, na.rm = TRUE) / 29,
         total_lichen_cover = sum(lichen, na.rm = TRUE) / 29, 
         total_bare_ground_cover = sum(bare_ground, na.rm = TRUE) / 29, 
         total_poo_cover = sum(poo, na.rm = TRUE) / 29,
         total_rock_cover = sum(rock, na.rm = TRUE) / 29,
         total_fungus_cover = sum(fungus, na.rm = TRUE) / 29) |>
  mutate(total_bryophyte_cover = case_when(is.na(total_bryophyte_cover) ~ 0,
                                           total_bryophyte_cover == 0 ~ 0,
                                           total_bryophyte_cover < 1 ~ 1,
                                           total_bryophyte_cover > 0 ~ round(total_bryophyte_cover, digits = 0)),
         total_litter_cover = case_when(is.na(total_litter_cover) ~ 0,
                                        total_litter_cover == 0 ~ 0,
                                        total_litter_cover < 1 ~ 1,
                                        total_litter_cover > 0 ~ round(total_litter_cover, digits = 0)),
         total_lichen_cover = case_when( is.na(total_lichen_cover) ~ 0,
                                         total_lichen_cover == 0 ~ 0,
                                         total_lichen_cover < 1 ~ 1,
                                         total_lichen_cover > 0 ~ round(total_lichen_cover, digits = 0)), 
         total_bare_ground_cover = case_when(is.na(total_bare_ground_cover) ~ 0,
                                             total_bare_ground_cover == 0 ~ 0,
                                             total_bare_ground_cover < 1 ~ 1,
                                             total_bare_ground_cover > 0 ~ round(total_bare_ground_cover, digits = 0)), 
         total_poo_cover = case_when(is.na(total_poo_cover) ~ 0,
                                     total_poo_cover == 0 ~ 0,
                                     total_poo_cover < 1 ~ 1,
                                     total_poo_cover > 0 ~ round(total_poo_cover, digits = 0)),
         total_rock_cover = case_when(is.na(total_rock_cover) ~ 0,
                                      total_rock_cover == 0 ~ 0,
                                      total_rock_cover < 1 ~ 1,
                                      total_rock_cover > 0 ~ round(total_rock_cover, digits = 0)),
         total_fungus_cover = case_when(is.na(total_fungus_cover) ~ 0,
                                        total_fungus_cover == 0 ~ 0,
                                        total_fungus_cover < 1 ~ 1,
                                        total_fungus_cover > 0 ~ round(total_fungus_cover, digits = 0))) |>
  select( -c(moss, measure, poo, lichen, litter, rock, fungus, bare_ground))

vegetation_height_and_moss_depth_mean <- community_clean |>
  select(plotID, subPlot, year, vegetation_height_mm, moss_depth_mm)|>
  unique()|>
  group_by(plotID, year)|>
  mutate(vegetation_height_mean = mean(vegetation_height_mm, na.rm = TRUE),
         moss_depth_mean = mean(moss_depth_mm, na.rm = TRUE)) |>
  select(-c(vegetation_height_mm, moss_depth_mm, subPlot)) |>
  mutate(vegetation_height_mean = round(vegetation_height_mean, digits = 0)) |>
  mutate(moss_depth_mean = round(moss_depth_mean, digits = 0)) |>
  ungroup() |>
  unique() |>
  mutate(vegetation_height_mean = ifelse(is.na(vegetation_height_mean), NA_real_, vegetation_height_mean),
         moss_depth_mean = ifelse(is.na(moss_depth_mean), NA_real_, moss_depth_mean))

community_clean <- community_clean |>
  left_join(total_cover, by = c("subPlot","plotID", "year"))|>
  left_join(vegetation_height_and_moss_depth_mean, by = c("plotID", "year")) |>
  unique() #removing any duplicates from the renaming process (if there was already a Car_big in the subplot and we renamed Car_sp tp Car_big for example)


####______ Making the 3 final datasets that are cleaned and put out in OSF ______####
community_clean_subplot <- community_clean |>
  filter(measure == "subPlot") |>
  select("site", "plotID", "warming", "treatment", "year", "date",
         "recorder", "writer", "subPlot","moss",
         "lichen", "litter", "rock", "poo", "fungus", "bare_ground",
         "logger", "vegetation_height_mm", "moss_depth_mm", "functional_group", "species",
         "value", "presence", "fertile", "dominance", "juvenile", "seedling") |>
  unique()

community_clean_species_cover <- community_clean |>
  select("site", "plotID", "warming", "treatment", "year", "date",
         "recorder", "writer", "functional_group", "species",
         "cover") |>
  unique()

community_clean_plotlevel_info <- community_clean |>
  filter(subPlot != 9) |>
  select("site", "plotID", "warming", "treatment", "year", "date",
         "recorder", "writer", "vegetation_cover", "total_bryophyte_cover",
         "total_litter_cover", "total_lichen_cover", "total_bare_ground_cover",
         "total_poo_cover", "total_rock_cover", "total_fungus_cover", "vegetation_height_mean", "moss_depth_mean") |>
  unique() |>
  pivot_longer(cols = vegetation_cover:moss_depth_mean, names_to = "name", values_to = "value")


write.csv(community_clean_species_cover, file = "data_cleaned/INCLINE_community_species_cover.csv", row.names= FALSE)

write.csv(community_clean_subplot, file = "data_cleaned/INCLINE_community_subplot.csv", row.names= FALSE)

write.csv(community_clean_plotlevel_info, file = "data_cleaned/INCLINE_community_plotlevel_info.csv", row.names= FALSE)

### Making dataset for ITEX data paper ##
# Only controls (warmed and not warmed)
# They only wanted the cover data on the plot level, no subplot presense/absense data
# Change species name to be full names, and update/check with the taxonomy ITEX is using

ITEX_community_data <- community_clean_species_cover |> 
  bind_rows(community_clean_plotlevel_info) |> 
  filter(treatment == "C") |>
  filter(year != "2022") |> #Removing 2022 because we have seen that we don't trust the method we used that year, so we don't want to include that year
  rename(SITE = site, PLOT = plotID, YEAR = year, ABUNDANCE = cover, TREATMENT = warming) |> 
  mutate(SUBSITE = NA,
         X = NA,
         Y = NA,
         STATUS = "Live",
         TISSUE = NA,
         HIT = NA,
         ) |> 
  mutate(ABUNDANCE = ifelse(is.na(ABUNDANCE), value, ABUNDANCE)) |> 
  select(-date, -recorder, -writer, -functional_group, -treatment) |> 
  mutate(SITE = str_to_upper(str_sub(SITE, 1, 3)),
         PLOT = str_sub(PLOT, 5,7),
         TREATMENT = ifelse(TREATMENT == "W", "OTC", "CTL")) |>
  left_join(species_dictionary, by = "species") |> 
  mutate(SPECIES_NAME = ifelse(is.na(taxonomic_name), name, taxonomic_name)) |> 
  select(SITE, SUBSITE, PLOT, YEAR,  X, Y, STATUS, TISSUE, HIT, SPECIES_NAME, ABUNDANCE, TREATMENT)
  

write.csv(ITEX_community_data , file = "data_cleaned/ITEX_data_INCLINE.csv", row.names= FALSE)
