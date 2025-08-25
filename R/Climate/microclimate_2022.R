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
    "TOMST_received_from_TOMST_fall2023.zip",
    "INCLINE_TOMST_Fall_2023.zip",
    "INCLINE_microclimate.zip",
    "INCLINE_TOMST_Spring_2023.zip"
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
    date(datetime) > date_in
    & date(datetime) < date_out
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

microclimate2022 <- microclimate2022 |>
  pivot_longer(c(soil_temperature, soil_moisture, ground_temperature, air_temperature), names_to = "sensor")

saveRDS(microclimate2022, "data/microclimate2022.rds")

microclimate2022 <- readRDS("data/microclimate2022.rds")

# cleaning ####

microclimate2022_clean <- microclimate2022 |>
  mutate(value = case_when(
    loggerID == 94205721 & date(datetime) == ymd("2022-02-18") ~ NA, # messed up and no record the next 3 days
    siteID %in% c("Gudmedalen", "Ulvehaugen", "Skjellingahaugen") & month(datetime) == 8 & sensor == "soil_temperature" & value > 20 ~ NA,
    siteID == "Skjellingahaugen" & month(datetime) == 9 & sensor == "soil_temperature" & value > 20 ~ NA,
    .default = value
  )
  )

# plottting ####


microclimate2022_clean |>
  filter(
    # datetime > ymd("2022-08-06")
    # & datetime < ymd("2022-08-10")
    month(datetime) == 12
    # date(datetime) == ymd("2022-02-17")
    & siteID == "Skjellingahaugen"
    # & loggerID == 94194656
  ) |>
  ggplot(aes(datetime, value)) +
  geom_point(size = 0.1) +
  theme_bw() +
  facet_wrap(. ~ sensor, scales = "free", ncol = 1)



# checking ranges ####

microclimate2022_clean |>
  reframe(
    .by = sensor,
    range = range(value, na.rm = TRUE)
  )

microclimate2022_clean |>
  filter(
    sensor == "soil_temperature"
    & value > 20
    ) |>
  View()

microclimate2022_clean |>
  filter(
    siteID == "Gudmedalen"
    & month(datetime) == 2) |>
  View()

microclimate2022_all |>
  left_join(
    tomst_metadata2022, by = "loggerID", relationship = "many-to-many"
  ) |>
  filter(
    siteID == "Gudmedalen"
    & month(datetime) == 11) |>
  View()

tomst_metadata2022 |>
  filter(
    season == "growth"
    & siteID == "Ulvehaugen"
  ) |>
  View()
