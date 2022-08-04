library("tidyverse")
library("dataDownloader")
library("osfr")

#osf_auth(token = "insert personal token") 
# Meta data ----------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data_cleaned",
         remote_path = "RawData")
INCLINE_metadata <- read_csv2("data_cleaned/INCLINE_metadata.csv")

# c-flux ------------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2020.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")
cflux <- read_csv("data_cleaned/INCLINE_c-flux_2020.csv", col_types = "cddddddfffcTfd")


# Climate -----------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_microclimate.zip",
         path = "data_cleaned",
         remote_path = "Climate")
unzip("data_cleaned/INCLINE_microclimate.zip")
file.remove("data_cleaned/INCLINE_microclimate.zip")

microclimate_air_temperature <- read_csv("data_cleaned/INCLINE_microclimate_air_temperature.csv", col_types = "Tcffffcd")
microclimate_soil_temperature <- read_csv("data_cleaned/INCLINE_microclimate_soil_temperature.csv", col_types = "Tcffffcd")
microclimate_ground_temperature <- read_csv("data_cleaned/INCLINE_microclimate_ground_temperature.csv", col_types = "Tcffffcd")
microclimate_soil_moisture <- read_csv("data_cleaned/INCLINE_microclimate_soil_moisture.csv", col_types = "Tcffffcd")


# NDVI --------------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_NDVI_2019_2020_2021.csv",
         path = "data_cleaned",
         remote_path = "NDVI")

ndvi <- read_csv("data_cleaned/INCLINE_NDVI_2019_2020_2021.csv", col_types = "fdDffffcc")


# biomass removal ---------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_biomass_removal.csv",
         path = "data_cleaned",
         remote_path = "Biomass_removal")

biomass_removal <- read_csv("data_cleaned/INCLINE_biomass_removal.csv", col_types = "dDfDfDfffcffd")
  

# Demography --------------------------------------------------------------

get_file(node = "zhk3m",
         file = "INCLINE_demography_Sib_pro.csv",
         path = "data_cleaned",
         remote_path = "Demography")

get_file(node = "zhk3m",
         file = "INCLINE_demography_Ver_alp.csv",
         path = "data_cleaned",
         remote_path = "Demography")

demography_Sib_pro <- read_csv("data_cleaned/INCLINE_demography_Sib_pro.csv", col_types = "fffdDfffcffddfffd")
demography_Ver_alp <- read_csv("data_cleaned/INCLINE_demography_Ver_alp.csv", col_types = "fffdDfffcffddfffd")
  
  

# Germination_alpine ------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_seedling_traits_alpine.csv",
         path = "data_cleaned",
         remote_path = "Germination_alpine")

get_file(node = "zhk3m",
         file = "INCLINE_seedling_data_alpine.csv",
         path = "data_cleaned",
         remote_path = "Germination_alpine")

get_file(node = "zhk3m",
         file = "INCLINE_seedling_data_subalpine.csv",
         path = "data_cleaned",
         remote_path = "Germination_subalpine")

seedling_traits_alpine <- read_csv("data_cleaned/INCLINE_seedling_traits_alpine.csv", col_types = "ffffffcfDDcfffcfd")
seedling_data_alpine <- read_csv("data_cleaned/INCLINE_seedling_data_alpine.csv", col_types = "cffffffffDfddfdddddc")
seedling_data_subalpine <- read_csv("data_cleaned/INCLINE_seedling_data_subalpine.csv", col_types = "ffffffcfdddfddDf")
  

# Seedbank ----------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_seedbank_survival.csv",
         path = "data_cleaned",
         remote_path = "Seedbank")

seedbank_survival <- read_csv("data_cleaned/INCLINE_seedbank_survival.csv", col_types = "fffffcDDfcc")
  


# Seeds_per_capsule -------------------------------------------------------

get_file(node = "zhk3m",
         file = "INCLINE_seeds_per_capsule.csv",
         path = "data_cleaned",
         remote_path = "Seeds_per_capsule")

seeds_per_capsule <- read_csv("data_cleaned/INCLINE_seeds_per_capsule.csv", col_types = "ffDccfdcd")

# Species_level_biomass_allocation ----------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_species_level_biomass_allocation.csv",
         path = "data_cleaned",
         remote_path = "Species_level_biomass_allocation")

species_level_biomass_allocation <- read_csv("data_cleaned/INCLINE_species_level_biomass_allocation.csv", col_types = "fffDcfffffcccfd")

# Flowering ----------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_flowering.csv",
         path = "data_cleaned",
         remote_path = "Flowering")

flowering <- read_csv("data_cleaned/INCLINE_flowering.csv", col_types = "fffffffDDDffcffd")


