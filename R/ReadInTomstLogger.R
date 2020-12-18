###########################
### READ IN DATA ###
###########################
#
library(tidyverse)
library(lubridate)
# library(purrrlyr)
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
files <- files[!(files %in% c("data/climate_tomst/data_94194607_2.csv"))]

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

# write csv per site
# temp <- TomstLogger_2019_2020 %>% 
#   mutate(
#     Site = as_factor(Site)
#   ) %>% 
#   nest(!c(Site)) %>% 
#   by_row(~write.csv(.$data, file = .$dataset))

# temp2 <- TomstLogger_2019_2020 %>% 
#   drop_na(Site) %>% 
#   nest(data = c(File, ID, Date_Time, Time_zone, SoilTemperature, GroundTemperature, 
#                 AirTemperature, RawSoilmoisture, Shake, ErrorFlag, LoggerID, 
#                 plotID, Block, Plot, OTC, Treatment, date_in, date_out)) %>%
#   by_row(~write.csv(.$data, file = paste(.$Site, ".csv", sep = "")))

# do the soil moisture calibration with the horrible Tomst excel sheet

# import and join the files after calibration
  
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
  mutate(AirTemperature = case_when(LoggerID %in% c("94194653") & AirTemperature < -40 ~ NA_real_,
                                    TRUE ~ as.numeric(AirTemperature)),
         
         GroundTemperature = case_when(LoggerID %in% c("94194653") & GroundTemperature < -40 ~ NA_real_,
                                       LoggerID %in% c("94194653") & GroundTemperature > 40 ~ NA_real_,
                                       TRUE ~ as.numeric(GroundTemperature)),
         
         SoilTemperature = case_when(LoggerID %in% c("94194610", "94194613", "94194615", "94194616", "94194619", "94194652", "94194653", "94194654", "94194657", "94194658", "94194659", "94194660", "94194691", "94194694", "94194695", "94194699", "94205702", "94205731", "94205732") & SoilTemperature > 25 ~ NA_real_,
                                     LoggerID %in% c("94194662", "94194667", "94194670", "94194693", "94194696", "94194611", "94194617", "94194626", "94194625", "94194655", "94194661", "94194681", "94194669", "94194666", "94205729", "94205727", "94205704", "94205703", "94205730", "94205731", "94205734", "94205760") & SoilTemperature > 20 ~ NA_real_,
                                     LoggerID %in% c("94194653") & SoilTemperature < -40 ~ NA_real_,
                                     # LoggerID %in% c("", "") & Date_Time > "2020-07-03 08:00:00" ~ NA_real_,
                                     # LoggerID %in% c("") & ErrorFlag == 1 ~ NA_real_,
                                     # LoggerID %in% c("") & Date_Time > "2020-07-17 01:00:00" & Date_Time < "2020-09-16 01:00:00" ~ NA_real_,
                                     # LoggerID == "" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                     TRUE ~ as.numeric(SoilTemperature)))


# Save clean file
write_csv(TomstLogger_2019_2020, "INCLINE_TomstLogger_2019_2020.csv")

# Checking data
dd <- TomstLogger_2019_2020


dd %>% 
  #filter(destSiteID == "Lia") %>% 
  # filter(LoggerID %in% c("94200493", "94200499")) %>% 
  #filter(SoilTemperature < 20) %>% 
  # filter(Date_Time < "2020-07-05 08:00:00") %>% 
  ggplot(aes(x = Date_Time, y = GroundTemperature, colour = as.factor(LoggerID))) +
  geom_line() +
  # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
  facet_wrap(~ LoggerID, scales = "free") +
  theme(legend.position="none") +
  ggsave("GroundTemperature.png", height = 50, width = 100, units = "cm")

dd %>% 
  #filter(destSiteID == "Lia") %>% 
  # filter(LoggerID %in% c("94200493", "94200499")) %>% 
  #filter(SoilTemperature < 20) %>% 
  # filter(Date_Time < "2020-07-05 08:00:00") %>% 
  ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(LoggerID))) +
  geom_line() +
  # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
  facet_wrap(~ LoggerID, scales = "free") +
  theme(legend.position="none") +
  ggsave("SoilTemperature.png", height = 50, width = 100, units = "cm")

dd %>% 
  #filter(destSiteID == "Lia") %>% 
  # filter(LoggerID %in% c("94200493", "94200499")) %>% 
  #filter(SoilTemperature < 20) %>% 
  # filter(Date_Time < "2020-07-05 08:00:00") %>% 
  ggplot(aes(x = Date_Time, y = AirTemperature, colour = as.factor(LoggerID))) +
  geom_line() +
  # geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
  facet_wrap(~ LoggerID, scales = "free") +
  theme(legend.position="none") +
  ggsave("AirTemperature.png", height = 50, width = 100, units = "cm")