###########################
### READ IN DATA ###
###########################
#

source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/Climate/soilmoisture_correction.R")
library(tidyverse)
library(lubridate)
library(dataDownloader)
# library(purrrlyr)
# only needed for soiltemp template
# source("R/Rgathering/ReadInPlotLevel.R")

# get_file(node = "zhk3m",
#          file = "climate_tomst.zip",
#          path = "data",
#          remote_path = "RawData/Climate")
get_file(node = "zhk3m",
         file = "INCLINE_microclimate.zip",
         path = "data",
         remote_path = "RawData/Climate")
get_file(node = "zhk3m",
         file = "logger_info.csv",
         path = "data",
         remote_path = "RawData/Climate")

unzip("data/INCLINE_microclimate.zip", exdir = "data")

#### CLIMATE DATA ####

# Read in meta data
metaTomst <- read_csv2("data/logger_info.csv", col_names = TRUE, na = c(""), col_types = "fcffffncnc") %>% 
  mutate(
    date_logger_in = dmy(date_logger_in),
    date_logger2_in = dmy(date_logger2_in)
  ) %>% 
  pivot_longer(cols = c("tomst-logger","tomst_logger_2"), names_to = "tomst", values_to = "loggerID") %>% #putting all the loggerID in one column and creating a new column specifying if it is logger 1 or 2
  mutate( #creating column date in and date out for selection of time window
    date_in = case_when(tomst == "tomst-logger" ~ date_logger_in, 
                        tomst == "tomst_logger_2" ~ date_logger2_in),
    date_out = case_when(tomst == "tomst-logger" ~ date_logger2_in), #date ou of logger 1 is date in of logger 2, I guess it makes sense
    loggerID = as.factor(loggerID)
    ) %>% 
  select(!c(date_logger_in, date_logger2_in, tomst))



### Read in files
files <- dir(path = "data/INCLINE_microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# remove empty file 
files <- files[!(files %in% c(
  "data/INCLINE_microclimate/data_94194607_2.csv",
  "data/INCLINE_microclimate/data_94194699_3.csv"
  ))]

# Function to read in data
temp <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "File")

# import cutting file fro cleaning

cutting <- read_csv("data/INCLINE_microclimate_cut.csv", na = "", col_types = "ffTT")

gc()

microclimate <- temp %>% 
  # rename column names
  rename("ID" = "X1", "datetime" = "X2", "time_zone" = "X3", "soil_temperature" = "X4", "ground_temperature" = "X5", "air_temperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>% 
  mutate(datetime = ymd_hm(datetime)) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    loggerID = substr(File, nchar(File)-13, nchar(File)-6),
    loggerID = as.factor(loggerID)
  ) %>% 
  select(!c(File, ID)) %>% 
  distinct() %>% 
  left_join(metaTomst, by = "loggerID") %>% 
  # group_by(loggerID) %>%
  mutate(
    date_out = replace_na(date_out, today("CET")) #the logger still in the field don't have a date_out (NA in the metaData), but we need a date_out to filter. Today's date can only be correct because if you are never running this script on future data ;-)
  ) %>% 
  filter(
    datetime > date_in
    & datetime <= date_out #maybe we don't have the data from the logger that broke, which is why are getting an empty df with that line
  ) %>% 
  mutate( # calculate and first cleaning of soil moisture
    soil_moisture = soil.moist(
      rawsoilmoist = RawSoilmoisture,
      soil_temp = soil_temperature,
      soilclass = "loamy_sand_A" #need to check if correct soil class
    ),
    soil_moisture = case_when(
      soil_moisture <= 0 ~ NA_real_,
      TRUE ~ soil_moisture),
    air_temperature = case_when(loggerID %in% c("94194653", "94194607", "94194609") & air_temperature < -20 ~ NA_real_,
                                loggerID %in% c("94194653", "94194607") & air_temperature > 40 ~ NA_real_,
                                TRUE ~ as.numeric(air_temperature)),

    ground_temperature = case_when(loggerID %in% c("94194653") & ground_temperature < -40 ~ NA_real_,
                                                                       loggerID %in% c("94194653") & ground_temperature > 40 ~ NA_real_,
                                                                       TRUE ~ as.numeric(ground_temperature)),

    soil_temperature = case_when(loggerID %in% c("94194610", "94194613", "94194615", "94194616", "94194619", "94194652", "94194653", "94194654", "94194657", "94194658", "94194659", "94194660", "94194691", "94194694", "94194695", "94194699", "94205702", "94205731", "94205732") & soil_temperature > 25 ~ NA_real_,
                                                                     loggerID %in% c("94194662", "94194667", "94194670", "94194693", "94194696", "94194611", "94194617", "94194626", "94194625", "94194655", "94194661", "94194681", "94194669", "94194666", "94205729", "94205727", "94205704", "94205703", "94205730", "94205731", "94205734", "94205760") & soil_temperature > 20 ~ NA_real_,
                                                                     loggerID %in% c("94194653") & soil_temperature < -40 ~ NA_real_,
                                                                     # LoggerID %in% c("", "") & Date_Time > "2020-07-03 08:00:00" ~ NA_real_,
                                                                     # LoggerID %in% c("") & ErrorFlag == 1 ~ NA_real_,
                                                                     # LoggerID %in% c("") & Date_Time > "2020-07-17 01:00:00" & Date_Time < "2020-09-16 01:00:00" ~ NA_real_,
                                                                     # LoggerID == "" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                                                     TRUE ~ as.numeric(soil_temperature))
  ) %>% 
  select(!RawSoilmoisture) %>% # we want vertical tidy data
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value")# %>% 
  # left_join(cutting, by = c("loggerID", "sensor")) %>% #not sure if this is a good idea
  # mutate(
  #   cut = case_when(
  #     datetime %in% c(start_cut, end_cut) ~ "cut",
  #     TRUE ~ "keep"
  #   )
  # )
  # 
  # 
  # 
  # # fix wrong values (first graph them to see what to fix, below is what was done for another dataset)
  # mutate(air_temperature = case_when(loggerID %in% c("94194653") & air_temperature < -40 ~ NA_real_,
  #                                   TRUE ~ as.numeric(air_temperature)),
  #        
  #        ground_temperature = case_when(loggerID %in% c("94194653") & ground_temperature < -40 ~ NA_real_,
  #                                      loggerID %in% c("94194653") & ground_temperature > 40 ~ NA_real_,
  #                                      TRUE ~ as.numeric(ground_temperature)),
  #        
  #        soil_temperature = case_when(loggerID %in% c("94194610", "94194613", "94194615", "94194616", "94194619", "94194652", "94194653", "94194654", "94194657", "94194658", "94194659", "94194660", "94194691", "94194694", "94194695", "94194699", "94205702", "94205731", "94205732") & soil_temperature > 25 ~ NA_real_,
  #                                    loggerID %in% c("94194662", "94194667", "94194670", "94194693", "94194696", "94194611", "94194617", "94194626", "94194625", "94194655", "94194661", "94194681", "94194669", "94194666", "94205729", "94205727", "94205704", "94205703", "94205730", "94205731", "94205734", "94205760") & soil_temperature > 20 ~ NA_real_,
  #                                    loggerID %in% c("94194653") & soil_temperature < -40 ~ NA_real_,
  #                                    # LoggerID %in% c("", "") & Date_Time > "2020-07-03 08:00:00" ~ NA_real_,
  #                                    # LoggerID %in% c("") & ErrorFlag == 1 ~ NA_real_,
  #                                    # LoggerID %in% c("") & Date_Time > "2020-07-17 01:00:00" & Date_Time < "2020-09-16 01:00:00" ~ NA_real_,
  #                                    # LoggerID == "" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
  #                                    TRUE ~ as.numeric(soil_temperature)),
  #        soil_moisture = soil.moist(
  #          rawsoilmoist = RawSoilmoisture,
  #          soil_temp = soil_temperature,
  #          soilclass = "loamy_sand_A" #need to check if correct soil class
  #        ),
  #        soil_moisture = case_when(
  #          soil_moisture <= 0 ~ NA_real_,
  #          TRUE ~ soil_moisture
  #          )) %>% 
  # select(!RawSoilmoisture) %>% 
  # pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value")

gc()

microclimate %>% 
  filter(
    sensor == "air_temperature" &
      site == "Ulvehaugen"
    ) %>% 
  ggplot(aes(x = datetime, y = value)) +
  geom_line(size = 0.2, aes(group = loggerID)) +
  # scale_x_datetime(date_breaks = "60 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(loggerID), ncol = 3, scales = "fixed") +
  ggsave("air_temp_microclimate.png", height = 40, width = 80, units = "cm")

#adding soil moisture
# microclimate <- microclimate %>% 
#   mutate(
#     soil_moisture = soil.moist(
#       rawsoilmoist = RawSoilmoisture,
#       soil_temp = soil_temperature,
#       soilclass = "loamy_sand_A" #need to check if correct soil class
#     )
#   )
# #cleaning soil moisture data
# microclimate <- microclimate %>% 
#   mutate(
#     soil_moisture = case_when(
#       soil_moisture <= 0 ~ NA_real_,
#       TRUE ~ soil_moisture
#     )
#   )


# Save clean file
write_csv(microclimate, "data/INCLINE_microclimate.csv")

# Checking data
# dd <- microclimate
# 
# 
# dd %>% 
#   #filter(destSiteID == "Lia") %>% 
#   # filter(LoggerID %in% c("94200493", "94200499")) %>% 
#   #filter(SoilTemperature < 20) %>% 
#   # filter(Date_Time < "2020-07-05 08:00:00") %>% 
#   ggplot(aes(x = Date_Time, y = GroundTemperature, colour = as.factor(LoggerID))) +
#   geom_line() +
#   # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
#   facet_wrap(~ LoggerID, scales = "free") +
#   theme(legend.position="none") +
#   ggsave("GroundTemperature.png", height = 50, width = 100, units = "cm")
# 
# dd %>% 
#   #filter(destSiteID == "Lia") %>% 
#   # filter(LoggerID %in% c("94200493", "94200499")) %>% 
#   #filter(SoilTemperature < 20) %>% 
#   # filter(Date_Time < "2020-07-05 08:00:00") %>% 
#   ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(LoggerID))) +
#   geom_line() +
#   # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
#   facet_wrap(~ LoggerID, scales = "free") +
#   theme(legend.position="none") +
#   ggsave("SoilTemperature.png", height = 50, width = 100, units = "cm")
# 
# dd %>% 
#   #filter(destSiteID == "Lia") %>% 
#   # filter(LoggerID %in% c("94200493", "94200499")) %>% 
#   #filter(SoilTemperature < 20) %>% 
#   # filter(Date_Time < "2020-07-05 08:00:00") %>% 
#   ggplot(aes(x = Date_Time, y = AirTemperature, colour = as.factor(LoggerID))) +
#   geom_line() +
#   # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
#   facet_wrap(~ LoggerID, scales = "free") +
#   theme(legend.position="none") +
#   ggsave("AirTemperature.png", height = 50, width = 100, units = "cm")
# 
# microclimate %>% 
#   #filter(destSiteID == "Lia") %>% 
#   # filter(LoggerID %in% c("94200493", "94200499")) %>% 
#   #filter(SoilTemperature < 20) %>% 
#   # filter(Date_Time < "2020-07-05 08:00:00") %>% 
#   ggplot(aes(x = datetime, y = soil_moisture, colour = as.factor(LoggerID))) +
#   geom_line() +
#   # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
#   facet_wrap(~ LoggerID, scales = "free") +
#   theme(legend.position="none") +
#   ggsave("soil_moisture.png", height = 50, width = 100, units = "cm")
