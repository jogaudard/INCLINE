###########################
### READ IN DATA ###
###########################

library(tidyverse)
library(lubridate)
# only needed for soiltemp template
# source("R/Rgathering/ReadInPlotLevel.R")


#### CLIMATE DATA ####

# Read in meta data
metaTomst <- read_csv2("data/metaData/Logger_info.csv", col_names = TRUE, na = c(""), col_types = "fcffffncnc") %>% 
  mutate(
    Date_logger_in = dmy(Date_logger_in),
    Date_logger2_in = dmy(Date_logger2_in)
  ) %>% 
  pivot_longer(cols = c("Tomst-logger","Tomst_logger_2"), names_to = "tomst", values_to = "loggerID") %>% #putting all the loggerID in one column and creating a new column specifying if it is logger 1 or 2
  mutate( #creating column date in and date out for selection of time window
    date_in = case_when(tomst == "Tomst-logger" ~ Date_logger_in, 
                        tomst == "Tomst_logger_2" ~ Date_logger2_in),
    date_out = case_when(tomst == "Tomst-logger" ~ Date_logger2_in) #date ou of logger 1 is date in of logger 2, I guess it makes sense
  ) %>% 
  select(!c(Date_logger_in, Date_logger2_in, tomst))



### Read in files
files <- dir(path = "data/climate_tomst", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# remove empty file
# files <- files[!(files %in% c("data/climate_tomst/.csv"))]

# Function to read in data
temp <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "File")


# These are 3 unknown loggers. Temp pattern does not fit with rest of the data. And short time period
# "94201711", "94201712", "94201713"


TomstLogger_2019_2020 <- temp %>% 
  # rename column names
  rename("ID" = "X1", "Date_Time" = "X2", "Time_zone" = "X3", "SoilTemperature" = "X4", "GroundTemperature" = "X5", "AirTemperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>% 
  mutate(Date_Time = ymd_hm(Date_Time)) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    LoggerID = substr(File, nchar(File)-13, nchar(File)-6),
    LoggerID = as.double(LoggerID)
  ) %>% 
  left_join(metaTomst, by = c("LoggerID"="loggerID")) %>% 
  
  # Data curation
  
  # Remove data before initial date time
  group_by(LoggerID) %>%
  mutate(
    date_out = replace_na(date_out, today("CET")) #the logger still in the field don't have a date_out (NA in the metaData), but we need a date_out to filter. Today's date can only be correct because if you are never running this script on future data ;-)
  ) %>% 
  filter(
    Date_Time > date_in
    & Date_Time <= date_out #maybe we don't have the data from the logger that broke, which is why are getting an empty df with that line
  ) %>% 
  
  # fix wrong values (first graph them to see what to fix, below is what was done for another dataset)
  mutate(AirTemperature = case_when(LoggerID %in% c("94195252", "94195220") & AirTemperature < -40 ~ NA_real_,
                                    LoggerID == "94195209" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                    TRUE ~ as.numeric(AirTemperature)),
         
         GroundTemperature = case_when(LoggerID %in% c("94195208", "94195252") & GroundTemperature < -40 ~ NA_real_,
                                       LoggerID == "94195209" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                       TRUE ~ as.numeric(GroundTemperature)),
         
         SoilTemperature = case_when(LoggerID %in% c("94195252", "94195236") & SoilTemperature < -40 ~ NA_real_,
                                     LoggerID %in% c("94200493", "94200499") & Date_Time < "2020-07-03 08:00:00" ~ NA_real_,
                                     LoggerID %in% c("94195208") & ErrorFlag == 1 ~ NA_real_,
                                     LoggerID %in% c("94200493") & Date_Time > "2020-07-17 01:00:00" & Date_Time < "2020-09-16 01:00:00" ~ NA_real_,
                                     LoggerID == "94195209" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                     TRUE ~ as.numeric(SoilTemperature)))


# Save clean file
write_csv(x = TomstLogger_2019_2020, path = "data_cleaned/climate/THREE-D_TomstLogger_2019_2020.csv")


# Checking data
dd <- TomstLogger_2019_2020


dd %>% 
  #filter(destSiteID == "Lia") %>% 
  # filter(LoggerID %in% c("94200493", "94200499")) %>% 
  #filter(SoilTemperature < 20) %>% 
  # filter(Date_Time < "2020-07-05 08:00:00") %>% 
  ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(LoggerID))) +
  geom_line() +
  # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
  facet_wrap(~ LoggerID, scales = "free") +
  theme(legend.position="none")

