library("dataDownloader")


# c-flux ------------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2020.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")

# Climate -----------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_microclimate.csv",
         path = "data_cleaned",
         remote_path = "Climate")

# NDVI --------------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_NDVI_2019_2020_2021.csv",
         path = "data_cleaned",
         remote_path = "NDVI")

