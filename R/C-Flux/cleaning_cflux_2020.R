library(fluxible)
library(tidyverse)
library(dataDownloader)
library(fs)
library(ggplot2)
library(ggforce)
library(progress)
library(stringr)
library(broom)
library(ggpmisc)


get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/raw_data",
         remote_path = "RawData/13_Three-D_raw_C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2020.csv",
         path = "data/C-Flux/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_cutting_2020.csv",
         path = "data/C-Flux/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")

metadata <- read_csv2("data/INCLINE_metadata.csv") |>
    mutate(
        plot_ID = str_to_upper(plotID)
    )


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

record <- read_csv("data/C-Flux/summer_2020/INCLINE_field-record_2020.csv", na = c(""), col_types = "cccntcfc") %>% 
  drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
  mutate(
    date = dmy(date),
    start = ymd_hms(paste(date, starting_time)) #converting the date as posixct, pasting date and starting time together
  ) |>
  select(!c(date, starting_time))

incline_record <- record |>
  filter(campaign != "LRC") # we will do the LRC separately, because there is some overlap

incline_record_lrc <- record |>
  filter(campaign == "LRC")

conc_incline <- flux_match(
    combined,
    incline_record,
    datetime,
    start_col = start,
    measurement_length = 180
)

conc_incline_lrc <- flux_match(
    combined,
    incline_record_lrc,
    datetime,
    start_col = start,
    measurement_length = 180
)

conc_incline_fit <- flux_fitting(
    conc_incline,
    f_conc = CO2,
    f_datetime = datetime,
    fit_type = "exp_zhao18",
    start_cut = 30, # there seems to be a real problem at the start of quite some fluxes
    end_cut = 40
)

conc_incline_fit_lrc <- flux_fitting(
    conc_incline_lrc,
    f_conc = CO2,
    f_datetime = datetime,
    fit_type = "exp_zhao18",
    start_cut = 30, # there seems to be a real problem at the start of quite some fluxes
    end_cut = 40
)

conc_incline_flag <- flux_quality(
    conc_incline_fit,
    f_conc = CO2,
    error = 200,
    force_lm = c(
        # 50, # small peak at the start getting the exp the wrong way
        # 101, # strange dip at the start
        # 60, # dip at the start
        # 75,
        74, # exp messed up but lm is fine
        98, # noise at the start messing up the exp
        81, # curvature because of dip at start
        105, # curvature because of dip at start
        70, # curvature because of dip at start
        97, # curvature because of dip at start
        314, # exp bad, but lm is fine
        315, # exp bad, but lm is fine
        665, # lm is fine
        696 # lm is fines
    ),
    # force_zero = c(
    #     77, # it is flat
    # ),
    force_ok = c(
        # 149, # there is curvature
        461, # start is actually fine
        393, # definitely not zero
        469 # dip at the end, but actually ok
    ),
    force_discard = c(
        324 # what is that shape?
    )
)

# min(conc_incline$CO2, na.rm = TRUE)
# max(conc_incline$CO2, na.rm = TRUE)

conc_incline_flag_lrc <- flux_quality(
    conc_incline_fit_lrc,
    f_conc = CO2,
    error = 200
)


# conc_incline_flag |>
#     filter(
#         campaign == 2
#         & replicate == 1
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign2_1",
#     f_ylim_lower = 250
# )

# conc_incline_flag_lrc |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "incline_lrc",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 2
#         & replicate == 2
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign2_2",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 2
#         & replicate == 3
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign2_3",
#     f_ylim_lower = 250
# )


# conc_incline_flag |>
#     filter(
#         campaign == 3
#         & replicate == 1
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign3_1",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 3
#         & replicate == 2
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign3_2",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 3
#         & replicate == 3
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign3_3",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 4
#         & replicate == 1
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign4_1",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 4
#         & replicate == 2
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign4_2",
#     f_ylim_lower = 250
# )

# conc_incline_flag |>
#     filter(
#         campaign == 4
#         & replicate == 3
#     ) |>
#     flux_plot(
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_plotname = "campaign4_3",
#     f_ylim_lower = 250
# )

# flux_plot(
#     conc_incline_flag,
#     f_conc = CO2,
#     f_datetime = datetime,
#     output = "pdfpages",
#     f_ylim_lower = 250
# )

# cleaning PAR

#PAR: same + NA for soilR and ER

conc_incline_flag <- conc_incline_flag |>
  mutate(
    PAR =
      case_when(
        type == "ER" & PAR < 0 ~ 0,
        type == "ER" & PAR > 10 ~ NA_real_,
        type == "NEE" & PAR < 75 ~ NA_real_,
        TRUE ~ PAR
      )
  )



plot_PAR <- function(slope_df, filter, filename, scale){
plot <- filter(slope_df, type == ((filter))) %>%
  ggplot(aes(x = datetime)) +
    geom_point(size = 0.2, aes(group = f_fluxid, y = PAR, color = f_cut)) +
    scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
    do.call(facet_wrap_paginate,
      args = c(facets = ~f_fluxid, ncol = 5, nrow = 3, scales = ((scale)))
    ) +
    scale_color_manual(values = c(
      "cut" = "#D55E00",
      "keep" = "#009E73"
    ))

    pdf(((filename)), paper = "a4r", width = 11.7, height = 8.3)


 pb <- progress_bar$new(
      format =
        "Printing plots in pdf document [:bar] :current/:total (:percent)",
      total = n_pages(plot)
    )
    pb$tick(0)
    Sys.sleep(3)
    for (i in 1:n_pages(plot)) {
      pb$tick()
      Sys.sleep(0.1)
      print(plot +
        do.call(facet_wrap_paginate,
          args = c(
            facets = ~f_fluxid,
            page = i,
            ncol = 5, nrow = 3, scales = ((scale))
          )
        ))
    }
    quietly(dev.off())

}

# passing plots as comment to save time
# plot_PAR(conc_incline_flag, "NEE", "plot_NEE_PAR.pdf", "free")
# plot_PAR(conc_incline_flag, "ER", "plot_ER_PAR.pdf", "free")
# plot_PAR(conc_incline_flag_lrc, 1, "plot_LRC1_PAR.pdf", "free")
# plot_PAR(conc_incline_flag_lrc, 2, "plot_LRC2_PAR.pdf", "free")
# plot_PAR(conc_incline_flag_lrc, 3, "plot_LRC3_PAR.pdf", "free")
# plot_PAR(conc_incline_flag_lrc, 4, "plot_LRC4_PAR.pdf", "free")
# plot_PAR(conc_incline_flag_lrc, 5, "plot_LRC5_PAR.pdf", "free")
# plot_PAR(conc_incline_flag_lrc, 6, "plot_LRC6_PAR.pdf", "free")




fluxes_incline <- flux_calc(
    slopes_df = conc_incline_flag,
    slope_col = f_slope_corr,
    f_datetime = datetime,
    temp_air_col = temp_air,
    setup_volume = 34.3,
    plot_area = 0.08575,
    cols_keep = c(
        "plot_ID",
        "treatment",
        "type",
        "replicate",
        "campaign",
        "remarks",
        "f_quality_flag"
    ),
    cols_ave = "PAR",
    flux_unit = "mmol",
    conc_unit = "ppm",
    atm_pressure = 1
)

fluxes_incline_lrc <- flux_calc(
    conc_incline_flag_lrc,
    f_slope_corr,
    datetime,
    temp_air,
    setup_volume = 34.3,
    atm_pressure = 1,
    plot_area = 0.08575,
    cols_keep = c(
        "plot_ID",
        "treatment",
        "type",
        "replicate",
        "campaign",
        "remarks",
        "f_quality_flag"
    ),
    cols_ave = "PAR",
    flux_unit = "mmol",
    conc_unit = "ppm"
)

# now we can do LRC


fluxes_incline_lrc <- fluxes_incline_lrc |>
    select(!treatment) |>
    left_join(metadata, by = join_by("plot_ID"))

ggplot(fluxes_incline_lrc, aes(x = PAR_ave, y = f_flux, color = OTC)) +
  geom_point(size = 0.9) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE)

coefficients_lrc <- fluxes_incline_lrc %>%
  group_by(OTC) %>% 
  nest %>% 
  mutate(lm = map(data, ~ lm(f_flux ~ PAR_ave + I(PAR_ave^2), data = .x)),
         table = map(lm, tidy),
         table = map(table, select, term, estimate),
         table = map(table, pivot_wider, names_from = term, values_from = estimate) 
  ) %>% 
  unnest(table) %>% 
  select(OTC, `(Intercept)`, PAR_ave, `I(PAR_ave^2)`) %>% 
  rename(
    origin = "(Intercept)",
    a = "I(PAR_ave^2)",
    b = "PAR_ave"
  )


# #origini is calculated with coefficients from the model and flux and PAR value of specific flux

fluxes_incline <- fluxes_incline |>
    select(!treatment) |>
    left_join(metadata, by = join_by("plot_ID"))

fluxes_incline |>
  summarise(
    .by = type,
    mean = median(PAR_ave, na.rm = TRUE)
    )

PARfix <- 500 #PAR value at which we want the corrected flux to be for NEE
PARnull <- 0 #PAR value for ER



flux_corrected_PAR <- fluxes_incline %>% 
  left_join(coefficients_lrc, by = "OTC") %>% 
  mutate(
    PAR_corrected_flux = 
      case_when( #we correct only the NEE
        type == "NEE" ~ f_flux + a * (PARfix^2 - PAR_ave^2) + b * (PARfix - PAR_ave),
        type == "ER" ~ f_flux + a * (PARnull^2 - PAR_ave^2) + b * (PARnull - PAR_ave),
        .default = f_flux
      )
    # delta_flux = flux - corrected_flux
  )

write_csv(flux_corrected_PAR, "data_cleaned/INCLINE_c-flux_2020.csv")

# let's compare with previous cleaning

get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2020_old.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")

flux_incline_old <- read_csv("data_cleaned/INCLINE_c-flux_2020_old.csv")

flux_incline_old <- flux_incline_old |>
  filter(type %in% c("NEE", "ER")) |>
  select(turfID, type, campaign, replicate, flux) |>
  rename(flux_old = "flux")

fluxes_incline_new <- flux_corrected_PAR |>
  filter(type %in% c("NEE", "ER")) |>
  select(plot_ID, type, campaign, replicate, f_flux, PAR_corrected_flux)

fluxes_comparison <- full_join(
  flux_incline_old,
  fluxes_incline_new,
  by = join_by("turfID" == "plot_ID", type, campaign, replicate)
) |>
pivot_longer(
  cols = c(flux_old, PAR_corrected_flux),
  names_to = "method",
  values_to = "flux"
)

fluxes_comparison |>
  ggplot(aes(f_flux, flux, color = method)) +
  geom_point(size = 0.5) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_abline(slope = 1, intercept = 0) +
  stat_poly_line() +
  stat_correlation(use_label("cor.label", "R2", "n")) +
  facet_grid(type ~ .)

fluxes_comparison |>
  filter(method == "flux_old") |>
  summarise(
    .by = type,
    mean_oldflux = mean(flux, na.rm = TRUE),
    mean_fflux = mean(f_flux, na.rm = TRUE)
  ) |>
  mutate(
    diff = (mean_oldflux - mean_fflux) / abs(mean_fflux)
  )

