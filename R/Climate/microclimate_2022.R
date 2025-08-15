#### loading packages ####

my_packages <- c(
  "tidyverse",
  "dataDownloader",
  "myClim"
)

lapply(my_packages, library, character.only = TRUE)

#### importing and reading data ####
source("https://raw.githubusercontent.com/jogaudard/INCLINE/tomst2022/R/Climate/tomst_gathering.R")


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

microclimate2022_all <- microclimate2022_og |>
  filter(
    year(datetime) == 2022
  )

tomst_metadata2022_og <- read_csv("data/tomst_logger_metadata_2022.csv")


# slicing ####

tomst_metadata2022 <- tomst_metadata2022_og |>
  mutate(
    loggerID = str_remove_all(loggerID, "TMS"),
    date_in = case_when(
      season == "winter" & is.na(date_in) ~ ymd("2021-12-31"),
      .default = date_in
    ),
    date_out = case_when(
      season == "winter" & is.na(date_out) ~ ymd("2023-01-01"),
      .default = date_out
    )
  )



microclimate2022 <- microclimate2022_all |>
  left_join(
    tomst_metadata2022, by = "loggerID", relationship = "many-to-many"
  ) |>
  filter(
    datetime > date_in
    & datetime < date_out
  ) |>
  arrange(datetime)

# calculating soil moisture ####
source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/functions/soilmoisture_correction.R")

microclimate2022 <- microclimate2022 |>
  mutate( # calculate soil moisture
    soil_moisture = soil.moist(
      rawsoilmoist = RawSoilmoisture,
      soil_temp = soil_temperature,
      soilclass = "silt_loam" #it is the closest soil class, but still very wrong. The TMS calibration tool does not have any class for our soil
    ))

# plottting ####


microclimate2022 |>
  filter(
    datetime > ymd("2022-08-01")
    & datetime < ymd("2022-08-31")
    & siteID == "Gudmedalen"
  ) |>
  pivot_longer(c(soil_temperature, soil_moisture, ground_temperature, air_temperature), names_to = "sensor") |>
  ggplot(aes(datetime, value)) +
  geom_point() +
  facet_wrap(. ~ sensor, scales = "free", ncol = 1)
