#### loading packages ####

my_packages <- c(
  "tidyverse",
  "dataDownloader",
  "myClim"
)

lapply(my_packages, library, character.only = TRUE)

#### importing and reading data ####

get_file(node = "zhk3m",
         file = "INCLINE_microclimate.zip",
         path = "data",
         remote_path = "RawData/Climate")

unzip("data/INCLINE_microclimate.zip", exdir = "data")

### Read in files
files <- dir(path = "data/INCLINE_microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# remove empty file 


# Function to read in data
temp <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "File")

microclimate <- temp %>% 
  # rename column names
  rename("ID" = "X1", "datetime" = "X2", "time_zone" = "X3", "soil_temperature" = "X4", "ground_temperature" = "X5", "air_temperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>% 
  mutate(
    datetime = as.character(datetime),
    datetime = substr(datetime, start = 0, stop = 16), #some dates are in ymd_hms format
    datetime = ymd_hm(datetime)
    ) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    loggerID = substr(File, nchar(File)-13, nchar(File)-6),
    loggerID = as.factor(loggerID)
  ) %>% 
  select(!c(File, ID)) %>% 
  distinct()