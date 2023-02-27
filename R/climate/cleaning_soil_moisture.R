library(dataDownloader)
library(tidyverse)
library(lubridate)

get_file(node = "zhk3m",
         file = "INCLINE_soil-moisture_2022.csv",
         path = "data/INCLINE_microclimate",
         remote_path = "RawData/Climate")


soil_moisture <- read_csv("data/INCLINE_microclimate/INCLINE_soil-moisture_2022.csv")
