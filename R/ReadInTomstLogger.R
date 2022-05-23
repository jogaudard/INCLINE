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
         file = "INCLINE_metadata_LoggerDates.csv",
         path = "data",
         remote_path = "RawData/Climate")

unzip("data/INCLINE_microclimate.zip", exdir = "data")

#### CLIMATE DATA ####

# Read in meta data
metatomst <- read_csv2("data/INCLINE_metadata_LoggerDates.csv", col_types = "ffffffccccccccc") %>% 
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

# metaTomst <- read_csv2("data/logger_info.csv", col_names = TRUE, na = c(""), col_types = "fcffffncnc") %>% 
#   mutate(
#     date_logger_in = dmy(date_logger_in),
#     date_logger2_in = dmy(date_logger2_in)
#   ) %>% 
#   pivot_longer(cols = c("tomst-logger","tomst_logger_2"), names_to = "tomst", values_to = "loggerID") %>% #putting all the loggerID in one column and creating a new column specifying if it is logger 1 or 2
#   mutate( #creating column date in and date out for selection of time window
#     date_in = case_when(tomst == "tomst-logger" ~ date_logger_in, 
#                         tomst == "tomst_logger_2" ~ date_logger2_in),
#     date_out = case_when(tomst == "tomst-logger" ~ date_logger2_in), #date ou of logger 1 is date in of logger 2, I guess it makes sense
#     loggerID = as.factor(loggerID)
#     ) %>% 
#   select(!c(date_logger_in, date_logger2_in, tomst))



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

# cutting <- read_csv("data/INCLINE_microclimate_cut.csv", na = "", col_types = "ffTT")

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
  left_join(metatomst, by = "loggerID") %>% 
  # group_by(loggerID) %>%
  # mutate(
  #   date_out = replace_na(date_out, today("CET")) #the logger still in the field don't have a date_out (NA in the metaData), but we need a date_out to filter. Today's date can only be correct because if you are never running this script on future data ;-)
  # ) %>% 
  filter(
    datetime > date_in + 1
    & datetime < date_out #maybe we don't have the data from the logger that broke, which is why are getting an empty df with that line
  ) %>% 
  mutate( # calculate soil moisture
    soil_moisture = soil.moist(
      rawsoilmoist = RawSoilmoisture,
      soil_temp = soil_temperature,
      soilclass = "silt_loam" #it is the closest soil class, but still very wrong. The TMS calibration tool does not have any class for our soil
    )) %>% 
  select(!c(RawSoilmoisture, logger)) %>% # we want vertical tidy data
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value")

gc()


# cleaning ----------------------------------------------------------------


#now we can start cleaning
microclimate <- microclimate %>% 
  mutate(
    cutting = case_when(
      #air temperature cleaning
      sensor == "air_temperature"
      & loggerID == 94194607
      & datetime %in% c(ymd_hms("2019-08-20T00:00:01"):ymd_hms("2019-08-30T00:00:01"))
      & value < -5 ~ "cut",
      
      sensor == "air_temperature"
      & loggerID %in% c("94194653", "94194607", "94194609")
      & value < -40 ~ "cut",
      
      sensor == "air_temperature" 
      & loggerID %in% c("94194653", "94194607") 
      & value > 40 ~ "cut",
      
      sensor == "air_temperature" 
      & loggerID %in% c("94194664") 
      & value < -40 ~ "cut",
      
      sensor == "air_temperature"
      & loggerID %in% c("94194624","94194627","94194630","94194689","94194638","94194622","94194623","94194690")
      & value < -40 ~ "cut",
      
      sensor == "air_temperature" 
      & loggerID == 94194624
      & datetime %in% c(ymd_hms("2019-11-01T00:00:01"):ymd_hms("2019-11-10T00:00:01"))
      & value > 15 ~ "cut",
      
      #soil temperature cleaning
      sensor == "soil_temperature"
      & loggerID %in% c("94194651","94194610", "94194613", "94194615", "94194616", "94194619", "94194652", "94194653", "94194654", "94194657", "94194658", "94194659", "94194660", "94194691", "94194694", "94194695", "94194699", "94205702", "94205731", "94205732","94205703", "94205711", "94205709", "94205745", "94194607")
      & value > 25 ~ "cut",
      
      sensor == "soil_temperature"
      & loggerID %in% c("94194654","94194682","94205720","94205737", "94205734", "94194626", "94194680", "94205760", "94194611","94194617","94194620","94194670","94194696","94194661","94194618","94194662","94194666","94194669", "94194655", "94194681", "94205731")
      & value > 20 ~ "cut",
      
      sensor == "soil_temperature"
      & loggerID %in% c("94194653","94194627","94194689","94194622","94194607")
      & value < -30 ~ "cut",
      
      sensor == "soil_temperature"
      & loggerID == 94194624
      & datetime %in% c(ymd_hms("2019-11-01T00:00:01"):ymd_hms("2019-11-07T00:00:01"))
      & value > 1 ~ "cut",
      
      sensor == "soil_temperature" 
      & loggerID %in% c("94205730", "94205739") &
        datetime %in% c(ymd_hms("2020-08-10T00:00:01"):ymd_hms("2020-08-14T00:00:01")) ~ "cut",
      
      sensor == "soil_temperature"
      & loggerID == 94205735
      & datetime %in% c(ymd_hms("2021-08-15T00:00:01"):ymd_hms("2021-08-20T00:00:01"))
      & value > 15 ~ "cut",
      
      sensor == "soil_temperature"
      & loggerID %in% c("94194629", "94205758", "94205759", "94205757", "94205736")
      & datetime %in% c(ymd_hms("2021-06-15T00:00:01"):ymd_hms("2021-06-20T00:00:01")) ~ "cut",
      
      #ground temperature cleaning
      sensor == "ground_temperature"
      & loggerID %in% c("94194689", "94194607", "94194653")
      & value < -20 ~ "cut",
      
      sensor == "ground_temperature"
      & loggerID %in% c("94194653")
      & value > 50 ~ "cut",
      
     
      #soil moisture cleaning
      sensor == "soil_moisture"
      & value <= 0 ~ "cut",
      
      sensor == "soil_moisture"
      & loggerID %in% c("94194689")
      & datetime %in% c(ymd_hms("2020-07-13T00:00:01"):ymd_hms("2020-10-01T00:00:01")) ~ "cut",
      
      
      TRUE ~ "keep"
    )
  )
gc()

# graphs ------------------------------------------------------------------


#soil temperature
# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_temperature" &
#       site == "Gudmedalen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_temperature" &
#       site == "Skjellingahaugen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")


# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_temperature" &
#       site == "Lavisdalen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")



# gc()


# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_temperature" &
#       site == "Ulvehaugen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.1, aes(group = loggerID)) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

#ground temperature
# microclimate %>% #cleaned
#   filter(
#     sensor == "ground_temperature" &
#       site == "Gudmedalen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

# microclimate %>% #cleaned
#   filter(
#     sensor == "ground_temperature" &
#       site == "Skjellingahaugen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")


# microclimate %>% #clean
#   filter(
#     sensor == "ground_temperature" &
#       site == "Lavisdalen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")



# gc()

# microclimate %>% #cleaned
#   filter(
#     sensor == "ground_temperature" &
#       site == "Ulvehaugen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#         "keep" = "#1e90ff",
#         "cut" = "#ff0800"
#       )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

#air temperature
# microclimate %>% #cleaned
#   filter(
#     sensor == "air_temperature" &
#       site == "Gudmedalen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

# microclimate %>% #cleaned
#   filter(
#     sensor == "air_temperature" &
#       site == "Skjellingahaugen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")


# microclimate %>% #cleaned
#   filter(
#     sensor == "air_temperature" &
#       site == "Lavisdalen"
#   ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

  

# gc()

# microclimate %>% #cleaned
#   filter(
#     sensor == "air_temperature" &
#       site == "Ulvehaugen"
#     ) %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.1, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#         "keep" = "#1e90ff",
#         "cut" = "#ff0800"
#       )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

#soil_moisture
# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_moisture" &
#       site == "Gudmedalen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")

# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_moisture" &
#       site == "Skjellingahaugen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")


# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_moisture" &
#       site == "Lavisdalen"
#   ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")



# gc()

# microclimate %>% #cleaned
#   filter(
#     sensor == "soil_moisture" &
#       site == "Ulvehaugen"
#     ) %>%
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.1, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#         "keep" = "#1e90ff",
#         "cut" = "#ff0800"
#       )) +
#   scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free") +
#   ggsave("microclimate.png", height = 40, width = 80, units = "cm")




# microclimate %>% 
#   select(site) %>% 
#   distinct()


# make clean dataset ------------------------------------------------------

microclimate_clean <- microclimate %>% 
  filter(
    cutting == "keep"
  ) %>% 
  select(datetime, loggerID, plotID, site, block, plot, OTC, treatment, comment, sensor, value)

gc()
# Save clean file
write_csv(microclimate_clean, "data_cleaned/INCLINE_microclimate.csv")



