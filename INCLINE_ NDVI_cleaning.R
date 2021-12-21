# to clean NVDVI data
library(tidyverse)
library(lubridate)
library(dataDownloader)

get_file(node = "zhk3m",
         file = "INCLINE_NDVI_2020.csv",
         path = "data",
         remote_path = "RawData/NDVI")

get_file(node = "zhk3m",
         file = "NDVI_2019_2021.csv",
         path = "data",
         remote_path = "RawData/NDVI")