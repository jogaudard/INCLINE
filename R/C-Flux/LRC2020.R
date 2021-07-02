library("dataDownloader")
library(tidyverse)
library(broom)

#download data from OSF and read it
get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "C-Flux")

flux <- read_csv("data/C-Flux/summer_2020/INCLINE_c-flux_2020.csv")

treatment <- read_csv("treatment.csv")

#adding treatment column
flux <- flux %>% 
  left_join(treatment, by = c("turfID" = "Plot_ID")) %>% 
  rename(treatment = Treatment)

lrc_flux <- filter(flux, campaign == "LRC")

#graph each light response curves
ggplot(lrc_flux, aes(x = PARavg, y = flux, color = turfID)) +
  geom_point(size = 0.1) +
  # geom_smooth(method = "lm", se = FALSE)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

#grouping per treatment instead of turfs
#!! this part needs to be modified, not the same system as Three-D!!
#need to add a treatment column

# treatment <- read_csv("treatment.csv")
# 
# lrc_flux <- lrc_flux %>% 
#   left_join(treatment, by = c("turfID" = "Plot_ID")) %>% 
#   rename(treatment = Treatment)

#plotting
ggplot(lrc_flux, aes(x = PARavg, y = flux, color = treatment)) +
  geom_point(size = 0.1) +
  # geom_smooth(method = "lm", se = FALSE)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

#extract the equation and correct all the NEE fluxes for PAR = 1000 micromol/s/m2

coefficients_lrc <- lrc_flux %>%
  group_by(treatment) %>% 
  nest %>% 
  mutate(lm = map(data, ~ lm(flux ~ PARavg + I(PARavg^2), data = .x)),
         table = map(lm, tidy),
         table = map(table, select, term, estimate),
         table = map(table, pivot_wider, names_from = term, values_from = estimate)
         
         ) %>% 
  unnest(table) %>% 
  select(treatment, `(Intercept)`, PARavg, `I(PARavg^2)`) %>% 
  rename(
    origin = "(Intercept)",
    a = "I(PARavg^2)",
    b = "PARavg"
  )


#what I want to do: predict flux at PAR = 1000, given the origin
#origini is calculated with coefficients from the model and flux and PAR value of specific flux
# corrected_flux = flux + a (1000^2 - PAR^2) + b (1000 - PAR)

PARfix <- 1000 #PAR value at which we want the corrected flux to be

flux_test <- flux %>% 
  left_join(coefficients_lrc, by = "treatment") %>% 
  mutate(
    corrected_flux = 
    case_when( #we correct only the NEE
      type == "NEE" ~ flux + a * (PARfix^2 - PARavg^2) + b * (PARfix - PARavg),
      type == "ER" ~ flux
    )
  )
