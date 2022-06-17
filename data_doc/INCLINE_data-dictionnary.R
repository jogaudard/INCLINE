#create data dictionnary for readme file
 library(tidyverse)
 library(lubridate)
 library(dataDownloader)

 # data import -------------------------------------------------------------
 source("data_doc/INCLINE_download_cleandata.R")
 
 # data dic function -------------------------------------------------------
 
 # From Aud script
 # https://github.com/audhalbritter/Three-D/blob/master/R/Rgathering/make_data_dic.R
 
 make_data_dictionary <- function(data, description_table, table_ID, keep_table_ID = FALSE){
   
   # get range
   range <- data %>%
     as_tibble() %>%
     summarise(
       across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
       across(where(is.numeric), ~paste(round(min(., na.rm = TRUE), 3),round(max(., na.rm = TRUE), 3), sep = " - ")),
       across(where(is.Date), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
       across(where(is.POSIXct), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
     ) %>%
     pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")
   
   # get class and make it into a tibble
   class <- map_dfr(data %>% as_tibble, ~enframe(class(.x)[1], value = "Variable type"),
                    .id = "Variable name") %>%
     select(-name) %>%
     # give sensible names
     mutate(`Variable type` = case_when(`Variable type` %in% c("character", "logical") ~ "categorical",
                                        `Variable type` %in% c("integer", "numeric") ~ "numeric",
                                        `Variable type` %in% c("Date") ~ "date",
                                        `Variable type` %in% c("POSIXct") ~ "date_time")) %>%
     mutate(TableID = table_ID) %>%
     # join with range
     left_join(range, by = "Variable name")
   
   # get class table with
   dictionary <- bind_rows(
     # join general variables
     class %>%
       inner_join(description_table %>%
                    filter(is.na(TableID)) %>%
                    select(-TableID), by = "Variable name"),
     # join special variables with same name but different meaning across datasets
     class %>%
       inner_join(description_table %>%
                    filter(!is.na(TableID),
                           TableID == table_ID), by = c("Variable name", "TableID"))
   ) %>%
     select(TableID, "Variable name", Description, "Variable type", "Variable range or levels", "Unit", "How measured")
   
   if(keep_table_ID){
     return(dictionary)
   } else{
     dictionary <- dictionary %>%
       select(-TableID)
     return(dictionary)
   }
   
 }
 
 # data description ------------------------------------------------------------
 description <- read_csv("data_doc/INCLINE_data_dic.csv") %>% 
   mutate(TableID = as.character(TableID)) 
 
  # c-flux ------------------------------------------------------------------
 cflux <- read_csv("data_cleaned/INCLINE_c-flux_2020.csv")
 cflux_dic <- make_data_dictionary(data = cflux,
                                   description_table = description,
                                   table_ID = NA_character_
 )
 # Climate -----------------------------------------------------------------
 microclimate_air_temperature <- read_csv("data_cleaned/INCLINE_microclimate_air_temperature.csv", col_types = "Tfffffcfd")
 microclimate_soil_temperature <- read_csv("data_cleaned/INCLINE_microclimate_soil_temperature.csv", col_types = "Tfffffcfd")
 microclimate_ground_temperature <- read_csv("data_cleaned/INCLINE_microclimate_ground_temperature.csv", col_types = "Tfffffcfd")
 microclimate_soil_moisture <- read_csv("data_cleaned/INCLINE_microclimate_soil_moisture.csv", col_types = "Tfffffcfd")
 
 microclimate_air_temperature_dic <- make_data_dictionary(data = microclimate_air_temperature,
                                                          description_table = description,
                                                          table_ID = NA_character_
 )
 
 microclimate_soil_temperature_dic <- make_data_dictionary(data = microclimate_soil_temperature,
                                                           description_table = description,
                                                           table_ID = NA_character_
 )
 
 microclimate_ground_temperature_dic <- make_data_dictionary(data = microclimate_ground_temperature,
                                                             description_table = description,
                                                             table_ID = NA_character_
 )
 
 microclimate_soil_moisture_dic <- make_data_dictionary(data = microclimate_soil_moisture,
                                                        description_table = description,
                                                        table_ID = NA_character_
 )
 
 microclimate_dic <- full_join(microclimate_soil_moisture_dic, microclimate_ground_temperature_dic) %>% 
   full_join(microclimate_soil_temperature_dic) %>% 
   full_join(microclimate_air_temperature_dic)
 # NDVI --------------------------------------------------------------------
 ndvi <- read_csv("data_cleaned/INCLINE_NDVI_2019_2020_2021.csv", col_types = "fdDffffcc")
 
 ndvi_dic <- make_data_dictionary(data = ndvi,
                                  description_table = description,
                                  table_ID = NA_character_
 )
 # biomass removal ---------------------------------------------------------
 
 biomass_removal_dic <- read_csv("data_cleaned/INCLINE_biomass_removal.csv", col_types = "dDfDfDfffcffd") %>% 
   make_data_dictionary(description_table = description,
                        table_ID = NA_character_
 )
 # Demography --------------------------------------------------------------
 demography_Sib_pro_dic <- read_csv("data_cleaned/INCLINE_demography_Sib_pro.csv", col_types = "fffdDffffffddfffd") %>%
   make_data_dictionary(
     description_table = description,
     table_ID = NA_character_
   )
 
 demography_Ver_alp_dic <- read_csv("data_cleaned/INCLINE_demography_Ver_alp.csv", col_types = "fffdDffffffddfffd") %>%
   make_data_dictionary(
     description_table = description,
     table_ID = NA_character_
   )
 
 demography_dic <- full_join(demography_Sib_pro_dic, demography_Ver_alp_dic)
 # Germination_alpine ------------------------------------------------------
 seedling_traits_alpine_dic <- read_csv("data_cleaned/INCLINE_seedling_traits_alpine.csv", col_types = "ffffffffDDDfffffcfd") %>%
   make_data_dictionary(
     description_table = description,
     table_ID = NA_character_
   )
 # Seedbank ----------------------------------------------------------------
 seedbank_survival_dic <- read_csv("data_cleaned/INCLINE_seedbank_survival.csv", col_types = "ffffffDDdcc") %>%
   make_data_dictionary(
     description_table = description,
     table_ID = NA_character_
   )
 # Seeds_per_capsule -------------------------------------------------------
 seeds_per_capsule_dic <- read_csv("data_cleaned/INCLINE_seeds_per_capsule.csv", col_types = "ffDfcfdfd") %>%
   make_data_dictionary(
     description_table = description,
     table_ID = NA_character_
   )
 # Species_level_biomass_allocation ----------------------------------------
 species_level_biomass_allocation_dic <- read_csv("data_cleaned/INCLINE_species_level_biomass_allocation.csv", col_types = "fffDcfffffccffd") %>%
   make_data_dictionary(
     description_table = description,
     table_ID = NA_character_
   )



 
 

 
 
 

# creating data list ------------------------------------------------------

 

# making dictionnaries --------------------------------------------------------------



 
 # cflux_range <- cflux %>% 
 #   summarise(
 #     across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
 #     across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
 #   ) %>% 
 #   pivot_longer(cols = everything(), names_to = "Variable names", values_to = "Variable range")
 # 