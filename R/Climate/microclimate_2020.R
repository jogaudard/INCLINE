#### loading packages ####

my_packages <- c(
  "tidyverse",
  "dataDownloader",
  "myClim"
)

lapply(my_packages, library, character.only = TRUE)

#### importing and reading data ####
source("https://raw.githubusercontent.com/jogaudard/INCLINE/master/R/Climate/tomst_gathering.R")


microclimate2020_og <- tomst_import_many(
  "zhk3m",
  c(
    "INCLINE_TOMST_Fall_2020.zip", # works
    "INCLINE_TOMST_Spring_2020.zip", # works
    "INCLINE_microclimate.zip",
    "Retrieved_data_fall_2020.zip", # working
    "INCLINE_TOMST_Spring_2021.zip" # works
  ),
  "data",
  "RawData/Climate"
)

microclimate2020_all <- microclimate2020_og |>
  filter(
    year(datetime) == 2020
  )


get_file(node = "zhk3m",
         file = "INCLINE_metadata_LoggerDates.csv",
         path = "data",
         remote_path = "RawData/Climate")


# Read in meta data
tomst_metadata2020_og <- read_csv2("data/INCLINE_metadata_LoggerDates.csv", col_types = "ffffffccccccccc") %>% 
  mutate(
    date_logger1_in = dmy(date_logger1_in), #dates in correct format
    date_logger2_in = dmy(date_logger2_in),
    date_out_fall2020 = dmy(date_out_fall2020),
    date_logger3_in = dmy(date_logger3_in),
    date_out_fall2021 = dmy(date_out_fall2021),
    tomst_logger_2 = case_when( #the first year, loggers were not taken out, which is why there is no date out 1
      is.na(tomst_logger_2) ~ tomst_logger_1,
      !is.na(tomst_logger_2) ~ tomst_logger_2
    ),
    date_logger2_in = case_when( #when the logger were not taken out, the date in is the one from the year before
      tomst_logger_2 == tomst_logger_1 ~ date_logger1_in,
      TRUE ~ date_logger2_in
    )
  ) %>% 
  select(!c(tomst_logger_1, date_logger1_in)) %>% #we don't need that info anymore, it all starts from logger 2
  pivot_longer(cols = c(tomst_logger_2, tomst_logger_3), names_to = "logger", values_to = "loggerID") %>% 
  #now we want to have a single date in colum, same for date out
  mutate(
    date_in = case_when(
      logger == "tomst_logger_2" ~ date_logger2_in,
      logger == "tomst_logger_3" ~ date_logger3_in
    ),
    date_out = case_when(
      logger == "tomst_logger_2" ~ date_out_fall2020,
      logger == "tomst_logger_3" ~ date_out_fall2021
    ),
    loggerID = substr(loggerID, 4, 12)
  ) %>% 
  select(!c(date_logger2_in, date_logger3_in, date_out_fall2020, date_out_fall2021))








# slicing ####

tomst_metadata2020 <- tomst_metadata2020_og |>
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



microclimate2020 <- microclimate2020_all |>
  left_join(
    tomst_metadata2020, by = "loggerID", relationship = "many-to-many"
  ) |>
  filter(
    date(datetime) > date_in
    & date(datetime) < date_out
  ) |>
  arrange(datetime)

  

# calculating soil moisture ####
source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/functions/soilmoisture_correction.R")

microclimate2020 <- microclimate2020 |>
  mutate( # calculate soil moisture
    soil_moisture = soil.moist(
      rawsoilmoist = RawSoilmoisture,
      soil_temp = soil_temperature,
      soilclass = "silt_loam" #it is the closest soil class, but still very wrong. The TMS calibration tool does not have any class for our soil
    ))

microclimate2020 <- microclimate2020 |>
  pivot_longer(c(soil_temperature, soil_moisture, ground_temperature, air_temperature), names_to = "sensor")

saveRDS(microclimate2020, "data/microclimate2020.rds")

microclimate2020 <- readRDS("data/microclimate2020.rds")

# cleaning ####

microclimate2020_clean <- microclimate2020 |>
  mutate(value = case_when(
    loggerID == 94205721 & date(datetime) == ymd("2020-02-18") ~ NA, # messed up and no record the next 3 days
    siteID %in% c("Gudmedalen", "Ulvehaugen", "Skjellingahaugen") & month(datetime) == 8 & sensor == "soil_temperature" & value > 20 ~ NA,
    siteID == "Skjellingahaugen" & month(datetime) == 9 & sensor == "soil_temperature" & value > 20 ~ NA,
    .default = value
  ),
  loggerID = as.double(loggerID)
  )

# plottting ####


# microclimate2020_clean |>
#   filter(
#     # datetime > ymd("2020-08-06")
#     # & datetime < ymd("2020-08-10")
#     month(datetime) == 12
#     # date(datetime) == ymd("2020-02-17")
#     & siteID == "Skjellingahaugen"
#     # & loggerID == 94194656
#   ) |>
#   ggplot(aes(datetime, value)) +
#   geom_point(size = 0.1) +
#   theme_bw() +
#   facet_wrap(. ~ sensor, scales = "free", ncol = 1)



# checking ranges ####

# microclimate2020_clean |>
#   reframe(
#     .by = sensor,
#     range = range(value, na.rm = TRUE)
#   )

# microclimate2020_clean |>
#   filter(
#     sensor == "soil_temperature"
#     & value > 20
#     ) |>
#   View()

# microclimate2020_clean |>
#   filter(
#     siteID == "Gudmedalen"
#     & month(datetime) == 2) |>
#   View()

# microclimate2020_all |>
#   left_join(
#     tomst_metadata2020, by = "loggerID", relationship = "many-to-many"
#   ) |>
#   filter(
#     siteID == "Gudmedalen"
#     & month(datetime) == 11) |>
#   View()

# tomst_metadata2020 |>
#   filter(
#     season == "growth"
#     & siteID == "Ulvehaugen"
#   ) |>
#   View()

# exporting ####

get_file(
  "zhk3m",
  "Climate",
  "INCLINE_microclimate.zip",
  "data"
)

unzip("data/INCLINE_microclimate.zip", exdir = "data/INCLINE_microclimate")

unlink("data/INCLINE_microclimate.zip")

air_temperature_og <- read_csv("data/INCLINE_microclimate/INCLINE_microclimate_air_temperature.csv")
ground_temperature_og <- read_csv("data/INCLINE_microclimate/INCLINE_microclimate_ground_temperature.csv")
soil_moisture_og <- read_csv("data/INCLINE_microclimate/INCLINE_microclimate_soil_moisture.csv")
soil_temperature_og <- read_csv("data/INCLINE_microclimate/INCLINE_microclimate_soil_temperature.csv")

unlink("data/INCLINE_microclimate", recursive = TRUE)

air_temperature_old <- air_temperature_og # |> # this was for the previous version of microclimate
  # separate_wider_delim(plotID, delim = "_", names = c(NA, "blockID", "plotID")) |>
  # select(!c(OTC, treatment)) |>
  # mutate(
  #   blockID = as.double(blockID),
  #   plotID = as.double(plotID)
  # )

air_temperature2020 <- microclimate2020_clean |>
  filter(sensor == "air_temperature") |>
  rename(air_temperature = "value") |>
  select(!c(sensor, time_zone, RawSoilmoisture, date_in, date_out))

air_temperature <- bind_rows(air_temperature2020, air_temperature_old) |>
  distinct(loggerID, datetime, .keep_all = TRUE)
air_temperature |>
  distinct(siteID)

write_csv(air_temperature, "data_cleaned/INCLINE_microclimate_air_temperature.csv")


ground_temperature_old <- ground_temperature_og #|> # this was for the previous version of microclimate
  # separate_wider_delim(plotID, delim = "_", names = c(NA, "blockID", "plotID")) |>
  # select(!c(OTC, treatment)) |>
  # mutate(
  #   blockID = as.double(blockID),
  #   plotID = as.double(plotID)
  # )

ground_temperature2020 <- microclimate2020_clean |>
  filter(sensor == "ground_temperature") |>
  rename(ground_temperature = "value") |>
  select(!c(sensor, time_zone, RawSoilmoisture, date_in, date_out))

ground_temperature <- bind_rows(ground_temperature2020, ground_temperature_old) |>
  distinct(loggerID, datetime, .keep_all = TRUE)
ground_temperature |>
  distinct(siteID)

write_csv(ground_temperature, "data_cleaned/INCLINE_microclimate_ground_temperature.csv")


soil_moisture_old <- soil_moisture_og #|> # this was for the previous version of microclimate
  # separate_wider_delim(plotID, delim = "_", names = c(NA, "blockID", "plotID")) |>
  # select(!c(OTC, treatment)) |>
  # mutate(
  #   blockID = as.double(blockID),
  #   plotID = as.double(plotID)
  # )

soil_moisture2020 <- microclimate2020_clean |>
  filter(sensor == "soil_moisture") |>
  rename(soil_moisture = "value") |>
  select(!c(sensor, time_zone, date_in, date_out))

soil_moisture <- bind_rows(soil_moisture2020, soil_moisture_old) |>
  distinct(loggerID, datetime, .keep_all = TRUE)
soil_moisture |>
  distinct(siteID)

write_csv(soil_moisture, "data_cleaned/INCLINE_microclimate_soil_moisture.csv")

soil_temperature_old <- soil_temperature_og # |> # this was for the previous version of microclimate
  # separate_wider_delim(plotID, delim = "_", names = c(NA, "blockID", "plotID")) |>
  # select(!c(OTC, treatment)) |>
  # mutate(
  #   blockID = as.double(blockID),
  #   plotID = as.double(plotID)
  # )

soil_temperature2020 <- microclimate2020_clean |>
  filter(sensor == "soil_temperature") |>
  rename(soil_temperature = "value") |>
  select(!c(sensor, time_zone, RawSoilmoisture, date_in, date_out))

soil_temperature <- bind_rows(soil_temperature2020, soil_temperature_old) |>
  distinct(loggerID, datetime, .keep_all = TRUE)
soil_temperature |>
  distinct(siteID)


write_csv(soil_temperature, "data_cleaned/INCLINE_microclimate_soil_temperature.csv")

unlink("data_cleaned/INCLINE_microclimate.zip") # just in case it is already there

zip("data_cleaned/INCLINE_microclimate.zip", c("data_cleaned/INCLINE_microclimate_air_temperature.csv",
                                               "data_cleaned/INCLINE_microclimate_soil_temperature.csv",
                                               "data_cleaned/INCLINE_microclimate_ground_temperature.csv",
                                               "data_cleaned/INCLINE_microclimate_soil_moisture.csv"),
                                               flags = '-r9Xj')
