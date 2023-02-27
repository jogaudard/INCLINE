library(dataDownloader)
library(tidyverse)
library(lubridate)

get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2022.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_soil-moisture_2022.csv",
         path = "data_cleaned",
         remote_path = "Climate")

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data/C-Flux/summer_2022/raw_data",
         remote_path = "RawData")

INCLINE_metadata <- read_csv2("data/C-Flux/summer_2022/raw_data/INCLINE_metadata.csv")

fluxes <- read_csv("data_cleaned/INCLINE_c-flux_2022.csv") %>% 
  left_join(INCLINE_metadata)

soil_moisture_avg <- read_csv("data_cleaned/INCLINE_soil-moisture_2022.csv")


# fluxes df compatible with ITEX ------------------------------------------


fluxes_itex <- fluxes %>% 
  filter(
    type == "ER"
    # & treatment == "C"
  ) %>% 
  left_join(soil_moisture_avg) %>% 
  mutate(
    date = date(datetime)
  ) %>% 
  select(plotID, siteID, date, treatment, OTC, temp_soil, soil_moisture, flux)



































