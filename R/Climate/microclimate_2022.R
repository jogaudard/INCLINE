#### loading packages ####

my_packages <- c(
  "tidyverse",
  "dataDownloader",
  "myClim"
)

lapply(my_packages, library, character.only = TRUE)

#### importing and reading data ####

microclimate2022_og <- tomst_import_many(
  "zhk3m",
  c(
    "INCLINE_TOMST_Fall2022.zip",
    "INCLINE_TOMST_Spring2022.zip",
    "TOMST_received_from_TOMST_fall2023.zip"
  ),
  "data",
  "RawData/Climate"
)

microclimate2022 <- microclimate2022_og |>
  filter(
    year(datetime) == 2022
  )

tomst_metadata2022 <- read_csv("data/tomst_logger_metadata_2022.csv")

# slicing ####
