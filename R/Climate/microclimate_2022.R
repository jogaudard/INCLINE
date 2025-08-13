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
unlink("data/INCLINE_microclimate.zip")

### Read in files
files <- dir(path = "data/INCLINE_microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# remove empty file 

test_myclim <- mc_read_files("data/INCLINE_microclimate", dataformat_name = "TOMST", recursive = F, silent = T)

# Function to read in data
# temp <- map_df(set_names(files), function(file) {
#   file %>% 
#     set_names() %>% 
#     map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
# }, .id = "File")

microclimate_all <- files |>
  map(
    read_delim,
    col_names = FALSE,
    delim = ";",
    show_col_types = FALSE,
    id = "filename",
    progress = FALSE,
    .progress = TRUE
  ) |>
  keep(function(df) {
  any(df$X1 != "File is empty")
}) |>
  list_rbind()


microclimate2022 <- microclimate_all %>% 
  # rename column names
  rename("ID" = "X1", "datetime" = "X2", "time_zone" = "X3", "soil_temperature" = "X4", "ground_temperature" = "X5", "air_temperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>% 
  mutate(
    datetime_c = as.character(datetime),
    datetime_s = substr(datetime_c, start = 0, stop = 16), #some dates are in ymd_hms format
    datetime = ymd_hm(datetime_s)
    ) %>% 
  mutate(
    loggerID = substr(filename, nchar(filename)-13, nchar(filename)-6),
    loggerID = as.factor(loggerID)
  ) %>% 
  select(!c(filename, ID)) %>% 
  distinct() |>
  mutate(
    year = year(datetime)
  ) |>
  filter(
    # year(datetime) == 2022
    ErrorFlag == 0
  )

unlink("data/INCLINE_microclimate/")