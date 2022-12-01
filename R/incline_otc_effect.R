#####################
#    OTC effects    #
#####################

# Script by EL

# Read in microclimate data from INCLINE OSF, 
# calculate mean temperatures and soil moisture
# with and without OTC (Warm/Cold treatment),
# and find difference between Warm and Cold treatments
# at the block level (finest possible scale)

library(tidyverse)
library(lubridate) # for handling time formats

data <- read.csv('Data/climate/INCLINE_microclimate.csv') # downloaded 12.05.2022
data$datetime <- ymd_hms(data$datetime) # recode as POSIXct, format: "2019-06-12 00:15:00"
data$blockID = paste(substr(data$site,1,3), data$block, sep = '_') # add unique block ID

# check that there are records for all blocks
table(data$blockID,data$OTC)
rmblocks = c('Gud_4','Gud_5') # blocks missing one treatment
data <- data[!data$blockID %in% rmblocks,] # remove blocks where a treatment is missing

# remove data from removal plots
data <- data[!data$treatment=='R',]

# subset (roughly) times when OTCs were up
data <- subset(data,
    datetime >= as.POSIXlt('2019-06-30 23:59:00') & datetime <= as.POSIXct('2019-08-31 23:59:00') | # 2019
    datetime >= as.POSIXct('2020-06-30 23:59:00') & datetime <= as.POSIXct('2020-08-31 23:59:00') | # 2020
    datetime >= as.POSIXct('2021-06-30 23:59:00') & datetime <= as.POSIXct('2021-08-31 23:59:00'))  # 2021

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
