library("dataDownloader")


# c-flux ------------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2020.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")

# Climate -----------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_microclimate.zip",
         path = "data_cleaned",
         remote_path = "Climate")
unzip("data_cleaned/INCLINE_microclimate.zip")
file.remove("data_cleaned/INCLINE_microclimate.zip")

# NDVI --------------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_NDVI_2019_2020_2021.csv",
         path = "data_cleaned",
         remote_path = "NDVI")


# biomass removal ---------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_biomass_removal.csv",
         path = "data_cleaned",
         remote_path = "Biomass_removal")


# Demography --------------------------------------------------------------

get_file(node = "zhk3m",
         file = "INCLINE_demography_Sib_pro.csv",
         path = "data_cleaned",
         remote_path = "Demography")

get_file(node = "zhk3m",
         file = "INCLINE_demography_Ver_alp.csv",
         path = "data_cleaned",
         remote_path = "Demography")


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

# Seedbank ----------------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_seedbank_survival.csv",
         path = "data_cleaned",
         remote_path = "Seedbank")


# Seeds_per_capsule -------------------------------------------------------

get_file(node = "zhk3m",
         file = "INCLINE_seeds_per_capsule.csv",
         path = "data_cleaned",
         remote_path = "Seeds_per_capsule")


# Species_level_biomass_allocation ----------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_species_level_biomass_allocation.csv",
         path = "data_cleaned",
         remote_path = "Species_level_biomass_allocation")

# Flowering ----------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_flowering.csv",
         path = "data_cleaned",
         remote_path = "Flowering")

# Meta data ----------------------------------------
get_file(node = "zhk3m",
          file = "INCLINE_metadata.csv",
          path = "data_cleaned",
          remote_path = "RawData")

