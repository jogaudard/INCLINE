#create data dictionnary for readme file
 library(tidyverse)
 library(lubridate)
 library(dataDownloader)
 

# data import -------------------------------------------------------------



# co2 fluxes --------------------------------------------------------------

 get_file(node = "zhk3m",
          file = "INCLINE_c-flux_2020.csv",
          path = "data_cleaned",
          remote_path = "C-Flux")
 
 cflux <- read_csv("data_cleaned/INCLINE_c-flux_2020.csv")
 
 cflux_range <- cflux %>% 
   summarise(
     across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
     across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
   ) %>% 
   pivot_longer(cols = everything(), names_to = "Variable names", values_to = "Variable range")
 