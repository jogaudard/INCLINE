#### loading packages ####

my_packages <- c(
  "tidyverse",
  "dataDownloader",
  "myClim"
)

lapply(my_packages, library, character.only = TRUE)

#### function for all #####
# those files are so massive that I will try to write a fct that
# download the zip
# unzip and delete the zip
# read the data
# delete the file


tomst_import <- function(node, file, path, remote_path) {
  get_file(node = node,
         file = file,
         path = path,
         remote_path = remote_path)

  # unzipping
  zip_loc <- paste(path, file, sep = "/")
  unzip(zip_loc, exdir = path)

  # deleting the zip (that's permanent, not in the trash!)
  unlink(zip_loc)
  message("zip file deleted!")

  # list the files
  files_loc <- str_remove(zip_loc, ".zip")
  files <- dir(path = files_loc, pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

  
  # reading the files
  microclimate_all <- files |>
    map(
    read_delim,
    col_names = FALSE,
    delim = ";",
    show_col_types = FALSE,
    col_types = cols(.default = col_character()),
    id = "filename",
    progress = FALSE,
    .progress = TRUE
  ) |>
  keep(function(df) { # removing empty files
  any(df$X1 != "File is empty")
}) |> 
  list_rbind() |>
  mutate_all(~ gsub(",", ".", ., fixed = TRUE)) |> # replace , with .
  distinct() |> # removing duplicates
  rename(
    "ID" = "X1",
    "datetime" = "X2",
    "time_zone" = "X3",
    "soil_temperature" = "X4",
    "ground_temperature" = "X5",
    "air_temperature" = "X6",
    "RawSoilmoisture" = "X7",
    "Shake" = "X8",
    "ErrorFlag" = "X9") |>
    mutate(
    datetime = substr(datetime, start = 0, stop = 16), #some dates are in ymd_hms format
    datetime = ymd_hm(datetime),
    filename = basename(filename),
    # loggerID = substr(filename, nchar(filename)-13, nchar(filename)-6),
    # loggerID = as.factor(loggerID),
    time_zone = as.numeric(time_zone),
    soil_temperature = as.numeric(soil_temperature),
    ground_temperature = as.numeric(ground_temperature),
    air_temperature = as.numeric(air_temperature),
    RawSoilmoisture = as.numeric(RawSoilmoisture),
    Shake = as.numeric(Shake),
    ErrorFlag = as.numeric(ErrorFlag)
  ) |>
  separate_wider_delim(
        filename,
        delim = "_",
        names = c(NA, "loggerID"),
        too_many = "drop"
      ) |>
    filter(ErrorFlag == 0) |>
    select(!any_of(c("ID", "Shake", "ErrorFlag", "X10")))

  # deleting files
  unlink(files_loc, recursive = TRUE)
  message("files deleted!")

  microclimate_all
}

# function to import many tomst logger data at once ####

tomst_import_many <- function(node, files, path, remote_path) {
  output <- map(
    files,
    tomst_import,
    node = node,
    path = path,
    remote_path = remote_path,
    .progress = TRUE
  ) |>
  list_rbind() |>
  distinct() # removing duplicate

  output
}

# testing functions #####

microclimate_test <- tomst_import("zhk3m", "INCLINE_TOMST_Spring2022.zip", "data", "RawData/Climate")

microclimate_all_test <- tomst_import_many("zhk3m",
                                           c("INCLINE_microclimate.zip",
                                             "INCLINE_TOMST_Spring2022.zip",
                                             "INCLINE_TOMST_Fall2022.zip"
                                           ),
                                           "data",
                                           "RawData/Climate")

#### importing and reading data ####



get_file(node = "zhk3m",
         file = "INCLINE_microclimate.zip",
         path = "data",
         remote_path = "RawData/Climate")

unzip("data/INCLINE_microclimate.zip", exdir = "data")
unlink("data/INCLINE_microclimate.zip")

# list files ####
files <- dir(path = "data/INCLINE_microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# function to import data ####

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