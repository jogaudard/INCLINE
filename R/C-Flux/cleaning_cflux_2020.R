library(fluxible)
library(tidyverse)
library(dataDownloader)
library(fs)


get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2020.csv",
         path = "data/C-Flux/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_cutting_2020.csv",
         path = "data/C-Flux/raw_data",
         remote_path = "RawData/C-Flux")

# Unzip files
zipFile <- "data/C-Flux/raw_data/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data/C-Flux/raw_data"
  unzip(zipFile, exdir = outDir)
}

file.remove(zipFile)

location <- "data/C-Flux/raw_data/rawData" #location of datafiles
#import all squirrel files and select date/time and CO2_calc columns, merge them, name it fluxes
fluxes <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename(CO2 = "CO2 (ppm)") %>%  #rename the column to get something more practical without space
  mutate(
    date = dmy(Date), #convert date in POSIXct
    datetime = as_datetime(paste(date, Time))  #paste date and time in one column
  ) %>%
  select(datetime, CO2)

#import date/time and PAR columns from PAR file

PAR <- list.files(path = location, pattern = "*PAR*", full.names = TRUE) |>
  map_dfr( # we map read_tsv on all the files
    # read_tsv is the version of read_delim for tab separated value files
    read_tsv,
    na = c("NA"),
    col_names = paste0("V", seq_len(3)),
    # creates a column with the filename, that we can use as flux ID
    id = "filename"
  ) |>
  rename(datetime = V2, PAR = V3) |>
  filter(V1 == 2) |> # anything else seems to be an error
  mutate(
    PAR = as.numeric(PAR), #removing any text from the PAR column (again, the logger...)
    datetime = ymd_hms(datetime)
  ) %>% 
  select(datetime, PAR)


#import date/time and value column from iButton file

temp_air <- dir_ls(location, regexp = "*temp*") %>% 
  map_dfr(read_csv,  na = c("#N/A"), skip = 20, col_names = c("datetime", "unit", "temp_value", "temp_dec"), col_types = "ccnn") %>%
  mutate(temp_dec = replace_na(temp_dec,0),
         temp_air = temp_value + temp_dec/1000, #because stupid iButtons use comma as delimiter AND as decimal point
         datetime = dmy_hms(datetime)
  ) %>% 
  select(datetime, temp_air)


#join the df


combined <- fluxes %>% 
  left_join(PAR, by = "datetime") %>% 
  left_join(temp_air, by = "datetime")

#import the record file
# incline <- read_csv("data/C-Flux/summer_2020/INCLINE_field-record_2020.csv", na = c(""), col_types = "cccntcnc") %>%
#   drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
#   mutate(
#     date = dmy(date),
#     start = as_datetime(paste(date, starting_time)), #converting the date as posixct, pasting date and starting time together
#     # Datetime = Start, #useful for left_join
#     end = start + measurement - endcrop, #creating column End and cropping the end of the measurement
#     start = start + startcrop #cropping the start
#   ) %>%  
#   drop_na(starting_time) %>% #need to remove rows with NA in starting time: when running out of battery we took only two measurements per plot, but the replicates stayed in the record file
#   select(plot_ID,type,replicate,starting_time,date,campaign,remarks,start,end)

incline <- read_csv("data/C-Flux/summer_2020/INCLINE_field-record_2020.csv", na = c(""), col_types = "cccntcfc") %>% 
  drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
  mutate(
    date = dmy(date),
    start = ymd_hms(paste(date, starting_time)), #converting the date as posixct, pasting date and starting time together
    end = start + measurement, #creating column End
    start_window = start + startcrop, #cropping the start
    end_window = end - endcrop #cropping the end of the measurement
  )
