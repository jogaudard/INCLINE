#####################
#    OTC effects    #
#####################

# Script by Eva Lieungh & Ragnhild Gya

# Read in microclimate data from INCLINE OSF, 
# calculate mean temperatures and soil moisture
# with and without OTC (Warm/Cold treatment),
# and find difference between Warm and Cold treatments
# at the block level (finest possible scale)

library(tidyverse)
library(lubridate) # for handling time formats
library(patchwork)

microclimate_air_temperature1 <- read.csv('data_cleaned/INCLINE_microclimate_air_temperature.csv')
microclimate_ground_temperature1 <- read.csv('data_cleaned/INCLINE_microclimate_ground_temperature.csv')
microclimate_soil_temperature1 <- read.csv('data_cleaned/INCLINE_microclimate_soil_temperature.csv') 
microclimate_soil_moisture1 <- read.csv('data_cleaned/INCLINE_microclimate_soil_moisture.csv')
#INCLINE_metadata <- read.csv2('data_cleaned/INCLINE_metadata.csv')


microclimate_air_temperature <- microclimate_air_temperature1 %>% 
  mutate(datetime = ymd_hms(datetime), # recode as POSIXct, format: "2019-06-12 00:15:00"
         blockID = substr(plotID,1,5)) %>%  # add unique block ID
  filter(!treatment == "R") %>% # remove data from removal plots
  mutate(month = month(datetime),
         date = date(datetime),
         hour = hour(datetime),
         day_night = ifelse(hour %in% 8:20, "day", "night")) %>% 
  filter(month %in% c(6:8)) # filter (roughly) times when OTCs were up

microclimate_ground_temperature <- microclimate_ground_temperature1 %>% 
  mutate(datetime = ymd_hms(datetime), # recode as POSIXct, format: "2019-06-12 00:15:00"
         blockID = substr(plotID,1,5)) %>%  # add unique block ID
  filter(!treatment == "R") %>% # remove data from removal plots
  mutate(month = month(datetime),
         date = date(datetime),
         hour = hour(datetime),
         day_night = ifelse(hour %in% 8:20, "day", "night")) %>% 
  filter(month %in% c(6:8)) # filter (roughly) times when OTCs were up

microclimate_soil_temperature <- microclimate_soil_temperature1 %>% 
  mutate(datetime = ymd_hms(datetime), # recode as POSIXct, format: "2019-06-12 00:15:00"
         blockID = substr(plotID,1,5)) %>%  # add unique block ID
  filter(!treatment == "R") %>% # remove data from removal plots
  mutate(month = month(datetime),
         date = date(datetime),
         hour = hour(datetime),
         day_night = ifelse(hour %in% 8:20, "day", "night")) %>% 
  filter(month %in% c(6:8)) # filter (roughly) times when OTCs were up

microclimate_soil_moisture <- microclimate_soil_moisture1 %>% 
  mutate(datetime = ymd_hms(datetime), # recode as POSIXct, format: "2019-06-12 00:15:00"
         blockID = substr(plotID,1,5)) %>%  # add unique block ID
  filter(!treatment == "R") %>% # remove data from removal plots
  mutate(month = month(datetime),
         date = date(datetime),
         hour = hour(datetime),
         day_night = ifelse(hour %in% 8:20, "day", "night")) %>% 
  filter(month %in% c(6:8)) # filter (roughly) times when OTCs were up

#### Make plots --------------------------------

mycolors = c('C' = 'black','W' = '#7F0E0E')

#### Air temperature 
air_temp_Ulv <- microclimate_air_temperature %>% 
  filter(siteID == "Ulvehaugen",
         date == "2020-08-15") %>% #"2020-08-15" or "2021-07-23" or "2019-08-03"
  # group_by(blockID, OTC) %>% 
  # mutate(mean_temp_block = mean(air_temperature)) %>% 
  # ungroup() %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(air_temperature)) %>% 
  ggplot(aes(x = datetime, y = air_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Air temperature") +
  xlab("Hour of the day")

air_temp_Lav <- microclimate_air_temperature %>% 
  filter(siteID == "Lavisdalen",
         date == "2020-08-16") %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(air_temperature)) %>% 
  ggplot(aes(x = datetime, y = air_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Air temperature") +
  xlab("Hour of the day")

air_temp_Gud <- microclimate_air_temperature %>% 
  filter(siteID == "Gudmedalen",
         date == "2020-08-16") %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(air_temperature)) %>% 
  ggplot(aes(x = datetime, y = air_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Air temperature") +
  xlab("Hour of the day")

air_temp_Skj <- microclimate_air_temperature %>% 
  filter(siteID == "Skjellingahaugen",
         date == "2020-08-16") %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(air_temperature)) %>% 
  ggplot(aes(x = datetime, y = air_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Air temperature") +
  xlab("Hour of the day")

#### Ground temperature
ground_temp_Ulv <- microclimate_ground_temperature %>% 
  filter(siteID == "Ulvehaugen",
         date == "2020-08-15") %>% #"2020-08-15" or "2021-07-23" or "2019-08-03"
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(ground_temperature)) %>% 
  ggplot(aes(x = datetime, y = ground_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Ground temperature") +
  xlab("Hour of the day")

ground_temp_Lav <- microclimate_ground_temperature %>% 
  filter(siteID == "Lavisdalen",
         date == "2020-08-16") %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(ground_temperature)) %>% 
  ggplot(aes(x = datetime, y = ground_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Ground temperature") +
  xlab("Hour of the day")

ground_temp_Gud <- microclimate_ground_temperature %>% 
  filter(siteID == "Gudmedalen",
         date == "2020-08-16") %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(ground_temperature)) %>% 
  ggplot(aes(x = datetime, y = ground_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Ground temperature") +
  xlab("Hour of the day")

ground_temp_Skj <- microclimate_ground_temperature %>% 
  filter(siteID == "Skjellingahaugen",
         date == "2020-08-16") %>% 
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(ground_temperature)) %>% 
  ggplot(aes(x = datetime, y = ground_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Ground temperature") +
  xlab("Hour of the day")

#### Soil temperature
soil_temp_Ulv <- microclimate_soil_temperature %>% 
  filter(siteID == "Ulvehaugen",
         date == "2020-08-15") %>% #"2020-08-15" or "2021-07-23" or "2019-08-03"
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(soil_temperature)) %>% 
  ggplot(aes(x = datetime, y = soil_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Soil temperature") +
  xlab("Hour of the day")

soil_temp_Lav <- microclimate_soil_temperature %>% 
  filter(siteID == "Lavisdalen",
         date == "2020-08-16") %>%
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(soil_temperature)) %>% 
  ggplot(aes(x = datetime, y = soil_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Soil temperature") +
  xlab("Hour of the day")

soil_temp_Gud <- microclimate_soil_temperature %>% 
  filter(siteID == "Gudmedalen",
         date == "2021-07-14") %>%
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(soil_temperature)) %>% 
  ggplot(aes(x = datetime, y = soil_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Soil temperature") +
  xlab("Hour of the day")

soil_temp_Skj <- microclimate_soil_temperature %>% 
  filter(siteID == "Skjellingahaugen",
         date == "2020-08-16") %>%
  group_by(OTC, datetime) %>% 
  mutate(mean_temp = mean(soil_temperature)) %>% 
  ggplot(aes(x = datetime, y = soil_temperature, color = OTC)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(x = datetime, y = mean_temp), size = 1) +
  ylim(4,32) +
  theme_classic() +
  scale_color_manual(values = mycolors) +
  ylab("Soil temperature") +
  xlab("Hour of the day")


microclimate_plot <- (air_temp_Ulv | air_temp_Lav | air_temp_Gud | air_temp_Skj)/
  (ground_temp_Ulv | ground_temp_Lav | ground_temp_Gud | ground_temp_Skj)/
  (soil_temp_Ulv | soil_temp_Lav | soil_temp_Gud | soil_temp_Skj) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12), legend.position = "bottom")

ggsave(microclimate_plot, filename = "microclimate_plot.pdf", width = 25, height = 19, units = "cm")




#### Old code from Eva ####


# make plots per site for a single nice day 
#-----------------------------------------------
mycolors = c('C' = 'black','W' = '#7F0E0E') # changed to black to improve visibility. original grey and red: '#AFAFAF','#7F0E0E'
# Ulvehaugen
{
  Ulv1d <- data[data$site == 'Ulvehaugen',]
  Ulv1d <- Ulv1d[Ulv1d$datetime >= as.POSIXlt('2020-08-11 00:00:00') & Ulv1d$datetime <= as.POSIXct('2020-08-11 23:59:00'),]
  Ulv1d <- Ulv1d %>%
    group_by(sensor, OTC, loggerID, date = date(datetime), hour = hour(datetime),) %>%
    summarise(value = mean(value)) %>%
    group_by(sensor, OTC, hour) %>%
    mutate(mean_value = mean(value))
  for (i in unique(Ulv1d$sensor)) {
    ggplot(Ulv1d[Ulv1d$sensor==i,],) +
      geom_line(aes(x = hour, y = value, group = interaction(OTC, loggerID), 
                    colour = OTC), alpha = 0.2) + 
      geom_line(aes(x = hour, y = mean_value, group = OTC,
                    colour = OTC), alpha = 2, size = 1.2) +
      scale_color_manual(values = mycolors, labels = c('Cold', 'Warmed')) +	theme_classic() + 
      ylab(i) +	xlab('Time')
    ggsave(paste0('plot_', i,'_ULV', '.png'), width = 2000, height = 1500, units = 'px')
  }
}

# Låvisdalen
{
  Lav1d <- data[data$site == 'Lavisdalen',]
  Lav1d <- Lav1d[Lav1d$datetime >= as.POSIXlt('2020-08-17 00:00:00') & Lav1d$datetime <= as.POSIXct('2020-08-17 23:59:00'),]
  Lav1d <- Lav1d %>%
    group_by(sensor, OTC, loggerID, date = date(datetime), hour = hour(datetime),) %>%
    summarise(value = mean(value)) %>%
    group_by(sensor, OTC, hour) %>%
    mutate(mean_value = mean(value))
  for (i in unique(Lav1d$sensor)) {
    ggplot(Lav1d[Lav1d$sensor==i,],) +
      geom_line(aes(x = hour, y = value, group = interaction(OTC, loggerID), 
                    colour = OTC), alpha = 0.2) + 
      geom_line(aes(x = hour, y = mean_value, group = OTC,
                    colour = OTC), alpha = 2, size = 1.2) +
      scale_color_manual(values = mycolors, labels = c('Cold', 'Warmed')) +	theme_classic() +
      ylab(i) +	xlab('Time')
    ggsave(paste0('plot_', i,'_LAV', '.png'), width = 2000, height = 1500, units = 'px')
  }
}

# Gudmedalen
{
  Gud1d <- data[data$site == 'Gudmedalen',] # for some reason only has data after 14.08.2020
  Gud1d <- Gud1d[Gud1d$datetime >= as.POSIXlt('2020-08-17 00:00:00') & Gud1d$datetime <= as.POSIXct('2020-08-17 23:59:00'),]
  Gud1d <- Gud1d %>%
    group_by(sensor, OTC, loggerID, date = date(datetime), hour = hour(datetime),) %>%
    summarise(value = mean(value)) %>%
    group_by(sensor, OTC, hour) %>%
    mutate(mean_value = mean(value))
  for (i in unique(Gud1d$sensor)) {
    ggplot(Gud1d[Gud1d$sensor==i,],) +
      geom_line(aes(x = hour, y = value, group = interaction(OTC, loggerID), 
                    colour = OTC), alpha = 0.2) + 
      geom_line(aes(x = hour, y = mean_value, group = OTC,
                    colour = OTC), alpha = 2, size = 1.2) +
      scale_color_manual(values = mycolors, labels = c('Cold', 'Warmed')) +	theme_classic() +
      ylab(i) +	xlab('Time')
    ggsave(paste0('plot_', i,'_GUD', '.png'), width = 2000, height = 1500, units = 'px')
  }
}

# Skjellingahaugen
{
  Skj1d <- data[data$site == 'Skjellingahaugen',] # for some reason only has data after 14.08.2020
  Skj1d <- Skj1d[Skj1d$datetime >= as.POSIXlt('2020-08-17 00:00:00') & Skj1d$datetime <= as.POSIXct('2020-08-17 23:59:00'),]
  Skj1d <- Skj1d %>%
    group_by(sensor, OTC, loggerID, date = date(datetime), hour = hour(datetime),) %>%
    summarise(value = mean(value)) %>%
    group_by(sensor, OTC, hour) %>%
    mutate(mean_value = mean(value))
  for (i in unique(Skj1d$sensor)) {
    ggplot(Skj1d[Skj1d$sensor==i,],) +
      geom_line(aes(x = hour, y = value, group = interaction(OTC, loggerID), 
                    colour = OTC), alpha = 0.2) + 
      geom_line(aes(x = hour, y = mean_value, group = OTC,
                    colour = OTC), alpha = 2, size = 1.2) +
      scale_color_manual(values = mycolors, labels = c('Cold', 'Warmed')) +	theme_classic() +
      ylab(i) +	xlab('Time')
    ggsave(paste0('plot_', i,'_SKJ', '.png'), width = 2000, height = 1500, units = 'px')
  }
}

# get warm and cold on different columns and compute difference
#----------------------------------------------------------------
data_wide <- data %>%
  select(datetime,site,blockID,OTC,value,sensor) %>%
  pivot_wider(names_from = OTC,
              values_from = value,
              values_fill = NA, # fill missing values with NA
              values_fn = mean) # if there are multiple values per block&treatment&time, take the mean
data_wide$diff <- data_wide$W-data_wide$C # calculate difference between warmed and cold per timestep

# mean differences overall (at all time steps in July+August)
meandiffs <- data_wide %>%
  group_by(sensor,site) %>%
  summarise(mean(na.omit(diff)))
write.csv(meandiffs,'mean_difference_W-C.csv')

# mean differences only during daytime
daytime <- data_wide %>% 
  filter(hour(datetime) >= 08 & hour(datetime) <= 20)
daytimediffs <- daytime %>%
  group_by(sensor,site) %>%
  summarise(mean(na.omit(diff)))
write.csv(daytimediffs,'mean_difference_W-C_daytime.csv')

# mean differences only during nighttime
night <- data_wide %>% 
  filter(hour(datetime) >= 20 & hour(datetime) >= 08)
nightdiffs <- night %>%
  group_by(sensor,site) %>%
  summarise(mean(na.omit(diff)))
write.csv(nightdiffs,'mean_difference_W-C_nighttime.csv')

# mean differences per site on the same time periods as those selected for plotting
Ulv1ddiff <- Ulv1d %>% 
  select(date,hour,OTC,value,sensor) %>%
  pivot_wider(names_from = OTC,
              values_from = value,
              values_fill = NA, # fill missing values with NA
              values_fn = mean) %>% # if there are multiple values per block&treatment&time, take the mean
  mutate(diff = W - C) %>%
  group_by(sensor) %>%
  summarise(mean(na.omit(diff)))

Lav1ddiff <- Lav1d %>% 
  select(date,hour,OTC,value,sensor) %>%
  pivot_wider(names_from = OTC,
              values_from = value,
              values_fill = NA, # fill missing values with NA
              values_fn = mean) %>% # if there are multiple values per block&treatment&time, take the mean
  mutate(diff = W - C) %>%
  group_by(sensor) %>%
  summarise(mean(na.omit(diff)))

Gud1ddiff <- Gud1d %>% 
  select(date,hour,OTC,value,sensor) %>%
  pivot_wider(names_from = OTC,
              values_from = value,
              values_fill = NA, # fill missing values with NA
              values_fn = mean) %>% # if there are multiple values per block&treatment&time, take the mean
  mutate(diff = W - C) %>%
  group_by(sensor) %>%
  summarise(mean(na.omit(diff)))

Skj1ddiff <- Skj1d %>% 
  select(date,hour,OTC,value,sensor) %>%
  pivot_wider(names_from = OTC,
              values_from = value,
              values_fill = NA, # fill missing values with NA
              values_fn = mean) %>% # if there are multiple values per block&treatment&time, take the mean
  mutate(diff = W - C) %>%
  group_by(sensor) %>%
  summarise(mean(na.omit(diff)))

write.csv(rbind(Ulv1ddiff,Lav1ddiff,Gud1ddiff,Skj1ddiff),'plot_day_differences.csv')



######################## additional unfinished code snippets ##########################

# # add weather data from seklima.met.no
# {
# weather <- read.csv('Data/climate/weather_obs_seklima.csv', sep = ';') # downloaded from seklima.met.no 12.05.2022 
# weather <- weather %>%	select(station,datetime,wind_1h,precipitation_12h)
# weather$datetime <- ymd_hms(weather$datetime)
# weather$wind_1h <- gsub(',','.',weather$wind_1h)
# weather$wind_1h <- as.numeric(weather$wind_1h)
# weather$precipitation_12h <- gsub(',','.',weather$precipitation_12h)
# weather$precipitation_12h <- as.numeric(weather$precipitation_12h)
# weather <- weather[weather$station=='SN53680',] # station SN53680 (Flåm) has the most data
# }
# 
# # join weather data into main data
# data <- left_join(data,weather) # left join keeps all rows in data but only matching from weather

# air temperature
#-----------------------------
at <- subset(data_wide, data$sensor == 'air_temperature')

{plot(at[at$datetime >= as.POSIXlt('2019-06-30 23:59:00') & at$datetime <= as.POSIXct('2019-08-31 23:59:00'),]$datetime,
      at[at$datetime >= as.POSIXlt('2019-06-30 23:59:00') & at$datetime <= as.POSIXct('2019-08-31 23:59:00'),]$diff, 
      main = 'Difference in air temperature between OTC and ambient, 2019', 
      xlab = 'time', ylab = 'difference (W - C in blocks) ', pch='.')
  abline(h=0)}

mean(na.omit(at$diff))

# plot difference over two days in July
ggplot(at[at$datetime >= as.POSIXlt('2019-07-30 00:00:00') & at$datetime <= as.POSIXct('2019-07-31 23:59:00'),],
       aes(x=datetime, y=diff, group=blockID,
           color=blockID, alpha = 1/100)) +
  geom_line() +
  ggtitle('Difference W/C air temp in 2019')


# get mean temperatures across blocks
means <- at %>% 
  group_by(datetime) %>%
  summarise(W = mean(W),
            C = mean(C))

# plot air temperature inside / outside OTC per blocks for one day
ggplot() +
  scale_color_manual(values = c('C' = 'blue',
                                'W' = 'red')) +
  geom_line(data=at[at$datetime >= as.POSIXlt('2019-08-01 00:00:00') & at$datetime <= as.POSIXct('2019-08-01 23:59:00'),],
            aes(x=datetime, y=C, group=blockID,
                color='C', alpha = 1/10)) +
  geom_line(data=at[at$datetime >= as.POSIXlt('2019-08-01 00:00:00') & at$datetime <= as.POSIXct('2019-08-01 23:59:00'),],
            aes(x=datetime, y=W, group=blockID,
                color='W', alpha = 1/10)) +
  geom_line(data = means[means$datetime >= as.POSIXlt('2019-08-01 00:00:00') & means$datetime <= as.POSIXct('2019-08-01 23:59:00'),], 
            aes(x=datetime, y=W))  +
  ggtitle('Air temperature') +
  xlab('Time') +
  ylab('Air temperature')

# does wind influence the difference? 
m0 <- lm(at$diff ~ 1)
m1 <- lm(at$diff ~ at$precipitation_12h)
m2 <- lm(at$diff ~ at$wind_1h)
summary(m2)
# no.