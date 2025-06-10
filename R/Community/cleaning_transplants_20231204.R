#######################################################
### script for cleaning transplant data for INCLINE ###
#######################################################

#### libraries ####
library(tidyverse)
library(lubridate)

#### load data ####
transplants <- read_csv("data/Transplants_2019_2023_20231204.csv", col_types = cols(.default=col_guess())) |> 
  # have only legal names through function clean_names (only lower case and snake case)
  janitor::clean_names()

#### clean data ####
transplants <- transplants |> 
  rename(siteID = site) |> 
  # make new column "blockID" by pasting together first 3 letters of "siteID", underscore, and "block"
  # similar for new column "plotID"
  # make column "date" to dates
   mutate(blockID = paste0(substr(siteID,1,3), "_", block), 
   plotID = paste0(substr(siteID,1,3), "_", block, "_", plot),
   date = dmy(date)) |> 
  # remove columns "block" and "plot" (because info is now in "blockID" and "plotID")
   select(-block, -plot)


  # make wide format into long format
transplants_long <- transplants |> 
    pivot_longer(cols = "x1":"x35", names_to = "subplot_presence", values_to = "subplot_presence_value")
transplants_long
 
transplants_long <- transplants_long |> 
   # rename column "sub_plot"
    rename(subplot_transplant = sub_plot) |> 
   # rearrange order of all columns
    select(siteID, blockID, plotID, treatment, registrator, year, date, subplot_transplant, species  :cover_outside_sub_plots, subplot_presence, subplot_presence_value, comment_registrator, comment_transcriber) |> 
   # replace all NA in column "flowering" by "0"
    mutate(flowering = ifelse(is.na(flowering), 0, flowering)) |> 
   # remove all "x" in column "subplot_presence"
    mutate(subplot_presence = gsub("x", "",as.character(subplot_presence))) |> 
   # change category of column "subplot_presence" to integer
    mutate(subplot_presence = as.integer(subplot_presence))
   
  


                                         







