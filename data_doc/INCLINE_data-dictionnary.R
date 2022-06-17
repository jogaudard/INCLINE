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

 

# reading data ------------------------------------------------------------
 description <- read_csv("data_doc/INCLINE_data_dic.csv") %>% 
   mutate(TableID = as.character(TableID))
 
 cflux <- read_csv("data_cleaned/INCLINE_c-flux_2020.csv")
 microclimate <- read_csv("data_cleaned/INCLINE_microclimate.csv")
 ndvi <- read_csv("data_cleaned/INCLINE_NDVI_2019_2020_2021.csv", col_types = "fdDffffcc")
 

# creating data list ------------------------------------------------------

 

# making dictionnaries --------------------------------------------------------------

cflux_dic <- make_data_dictionary(data = cflux,
                                  description_table = description,
                                  table_ID = NA_character_
                                  )
 
 ndvi_dic <- make_data_dictionary(data = ndvi,
                                  description_table = description,
                                  table_ID = NA_character_
                                  )
 
 microclimate_dic <- make_data_dictionary(data = microclimate,
                                  description_table = description,
                                  table_ID = NA_character_
 )
 
 # cflux_range <- cflux %>% 
 #   summarise(
 #     across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
 #     across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
 #   ) %>% 
 #   pivot_longer(cols = everything(), names_to = "Variable names", values_to = "Variable range")
 # 