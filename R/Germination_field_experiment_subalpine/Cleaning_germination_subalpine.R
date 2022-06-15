library(lme4)
library(tidyverse)
library(dplyr)
library(broom)

## Laster inn data
SeedScoring <-read.csv("Data2021.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

### BEGYNNER UTEN T50; FORDI JEG VIL BARE HA EN LINJE FOR HVERT PLOT, IKKE TUSENVIS

# Legge til kolonne med total fr? s?dd i hver block (n)
SeedScoring <- SeedScoring %>%
  group_by(SiteID, BlockID, OTC, Vegetation, Species) %>%
  mutate(TotSpeciesBlock=n()) %>%
  ungroup()

# Legge til kolonne med om det er spirt idet hele tatt (Germination) (f?r da totalt antall spirt og total spiringsrate)
SeedScoring$Germination <- ifelse(SeedScoring$Date1==1,1, 
                                  ifelse(SeedScoring$Date2==1,1,
                                  ifelse(SeedScoring$Date3==1,1,
                                  ifelse(SeedScoring$Date4==1,1,
                                  ifelse(SeedScoring$Date5==1,1,
                                  ifelse(SeedScoring$Date6==1,1,
                                  ifelse(SeedScoring$Date7==1,1,0)))))))

# Sjekker at det er rett
sum(SeedScoring$Germination)
# 588
sum(SeedScoring$Germination)/10900
# 0.05394495

# Legge til kolonne med totalt spirt av hver art i hvert plott 
SeedScoring <- SeedScoring %>%
  group_by(SiteID, BlockID, OTC, Vegetation, Species) %>%
  mutate(TotEmerged = sum(Germination, na.rm = TRUE))%>%
  ungroup()

# Kolonne med proportionEmerged
SeedScoring <- SeedScoring %>% 
  mutate(ProportionEmerged = TotEmerged/TotSpeciesBlock)

# Legge til tall ned og sl? sammen for ? f? Species_IDnr
SeedScoring$Observation <- 1:nrow(SeedScoring)
SeedScoring$Observation <- paste(SeedScoring$Species,SeedScoring$Observation,sep="_")

# Egen kolonne for de 80 individuelle plottene
SeedScoring$Plot <- paste(SeedScoring$SiteID, SeedScoring$BlockID, SeedScoring$OTC, SeedScoring$Vegetation, sep = "_")

# Kolonne for PlotSpecies
SeedScoring$PlotSpecies <- paste(SeedScoring$Plot,SeedScoring$Species,sep="_")

# S? lager kolonne med SurvivedPlot for hver art 
SeedScoring <- SeedScoring %>%
  group_by(Plot, Species) %>%
  mutate(SurvivedPlot = sum(Date7)) %>%
  ungroup()

sum(SeedScoring$Date7)

SeedScoring <- mutate(SeedScoring, ProportionSurvived = SurvivedPlot/TotEmerged)

# Kolonne med ikke survived og ikke emerged
SeedScoring$NotSurvivedPlot <- (SeedScoring$TotEmerged - SeedScoring$SurvivedPlot)

SeedScoring$NotEmergedPlot <- (SeedScoring$TotSpeciesBlock - SeedScoring$TotEmerged)

# Fjerner alt annet slik at jeg bare har en av hver PlotSpecies
SeedDF <- SeedScoring[!duplicated(SeedScoring$PlotSpecies),]

# PRECIP 0-3 IKKE 1-4:
SeedDF$Precip[SeedDF$Precip == 1] <- 0
SeedDF$Precip[SeedDF$Precip == 2] <- 0.45
SeedDF$Precip[SeedDF$Precip == 3] <- 1.4
SeedDF$Precip[SeedDF$Precip == 4] <- 2.5


# Kolonne med proportion emerged
SeedDF$ProportionEmerged <- (SeedDF$TotEmerged/SeedDF$TotSpeciesBlock)

# Kolonne med ikke emerged
SeedDF$TotNotEmerged <- (SeedDF$TotSpeciesBlock - SeedDF$TotEmerged)
SeedDF1 <- SeedDF
SeedDF1$Vegetation <- as.factor(SeedDF$Vegetation)
SeedDF$Vegetation <- as.factor(SeedDF$Vegetation)
SeedDF1$Precip <- as.factor(SeedDF$Precip)
head(SeedDF)

## ALT I DATA CLEANING SCRIPTET ER MED. TROR NESTE BLIR T50 SCRIPTET...

SeedT50 <-read.csv("Data2021.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')

# Legge til kolonne med total fr? s?dd i hver block (n)
SeedT50 <- SeedT50 %>%
  group_by(SiteID, BlockID, OTC, Vegetation, Species) %>%
  mutate(TotSpeciesBlock=n()) %>%
  ungroup()

# Forandre navn p? Date kolonnene
colnames(SeedT50)[c(9, 10, 11, 12, 13, 14, 15)] <- c("Date13", "Date14", "Date15", "Date16", "Date17", "Date18", "Date19")

# Legge til sn?smelt (date12 - 0) og to datoer til (date10 - -14 date11 - -28)
SeedT50 <- SeedT50 %>% add_column(Date12 = 0, .after = "SpeciesID")
SeedT50 <- SeedT50 %>% add_column(Date11 = 0, .after = "SpeciesID")
SeedT50 <- SeedT50 %>% add_column(Date10 = 0, .after = "SpeciesID")


# Legge til kolonne med om det er spirt idet hele tatt (Germination) (f?r da totalt antall spirt og total spiringsrate)
SeedT50$Germination <- ifelse(SeedT50$Date10==1,1, 
                       ifelse(SeedT50$Date11==1,1,
                       ifelse(SeedT50$Date12==1,1,
                       ifelse(SeedT50$Date13==1,1,
                       ifelse(SeedT50$Date14==1,1,
                       ifelse(SeedT50$Date15==1,1,
                       ifelse(SeedT50$Date16==1,1,
                       ifelse(SeedT50$Date17==1,1,
                       ifelse(SeedT50$Date18==1,1,
                       ifelse(SeedT50$Date19==1,1,0))))))))))

sum(SeedT50$Germination)
# 588
sum(SeedT50$Germination)/10900
# 0.05394495

# Legge til kolonne med totalt spirt av hver art i hvert plott 
SeedT50 <- SeedT50 %>%
  group_by(SiteID, BlockID, OTC, Vegetation, Species) %>%
  mutate(TotEmerged = sum(Germination, na.rm = TRUE))%>%
  ungroup()


# Egen kolonne for de 80 individuelle plottene
SeedT50$Plot <- paste(SeedT50$SiteID, SeedT50$BlockID, SeedT50$OTC, SeedT50$Vegetation, sep = "_")

# Kolonne for PlotSpecies
SeedT50$PlotSpecies <- paste(SeedT50$Plot,SeedT50$Species,sep="_")

# Fjerner bare alle plot hvor totemerged er 0
SeedT50 <- filter(SeedT50, TotEmerged>0)

# Legge til tall ned og sl? sammen for ? f? Species_IDnr
SeedT50$Observation <- 1:nrow(SeedT50)
SeedT50$Observation <- paste(SeedT50$Species,SeedT50$Observation,sep="_")

####################################################################
########################## LANGT DATASETT ##########################
####################################################################

# Gj?re datasett langt (Date er kolonne med date1-7 nedover)
SeedLongT50 <- gather(SeedT50, Date, Presence, "Date10":"Date19", factor_key = TRUE)

# Lage dager kolonne
SeedLongT50$Days <- paste(SeedLongT50$SiteID,SeedLongT50$BlockID, SeedLongT50$Date,sep="_")

# DAGER
Days <-read.csv("dagt50.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM')
# view(Days)
Days <- gather(Days, Date, Day, "Date10":"Date19", factor_key = TRUE)
Days$Days <- paste(Days$SiteID, Days$BlockID, Days$Date, sep = "_")
Days <- subset(Days, select = c("Days", "Day"))

# Erstatte verdier i dataframe fra Days for ? f? inn dager
SeedLongT50 <- merge(SeedLongT50, Days, all.x = TRUE)


# KUMULATIV SUM???
SeedLongT50 <- SeedLongT50 %>%
  group_by(Plot, Species, Observation) %>%
  mutate(csum = as.factor(cumany(Presence == 1))) %>%
  mutate(csum2 = recode(csum, "TRUE" = 1, "FALSE" = 0)) %>%
  ungroup()

## M? BARE LAGE SISTE KOLONNE FOR KUMULATIV
SeedLongT50 <- SeedLongT50 %>%
  group_by(Date, Species, Plot) %>%
  mutate(CumSpeciesPlot = sum(csum2, na.rm = TRUE))%>%
  ungroup()

# KUMULATIV PROSENT 
SeedLongT50 <- mutate(SeedLongT50, CumSpecPlotPercent = SeedLongT50$CumSpeciesPlot/SeedLongT50$TotEmerged*100)

unique(SeedLongT50$CumSpecPlotPercent)

# De som er NaN (not a number fordi 0/0) erstatter jeg med 0 vel?
# SeedLongT50$CumSpecPlotPercent[is.nan(SeedLongT50$CumSpecPlotPercent)] <- 0

### HVORDAN FINNER JEG TID TIL 50%??
# Lager kolonne for PlotSpecies
# SeedLongT50$PlotSpecies <- paste(SeedLongT50$Plot,SeedLongT50$Species,sep="_")


SeedLongT50$CumNotEmerged <- (SeedLongT50$TotEmerged - SeedLongT50$CumSpeciesPlot)

# GLM for ? finne T50
glm1 <- SeedLongT50 %>% 
  group_by(PlotSpecies) %>%
  nest() %>%
  mutate(mod = map(.x = data, ~ glm(cbind(CumSpeciesPlot, CumNotEmerged) ~ Day, data = .x, family = "binomial")), 
         coef = map(.x=mod, tidy)) %>%
  unnest(coef) 

filter(glm1, PlotSpecies == "GUD_1_C_0_Ver_off") %>% 
  slice(1) %>% 
  pull(mod) %>% 
  {augment(x = .[[1]], type.predict = "response")} %>% 
  distinct(Day, .keep_all = TRUE)

glm2 <- glm1 %>% select(PlotSpecies, term, estimate)
glm2 <- pivot_wider(glm2, names_from = term, values_from = estimate)

# Bruke intercept og coefficient for ? finne day n?r cumSpeciesPlot er 50%:
glm2$T50 <- -(glm2$`(Intercept)`)/glm2$Day

# LEFTJOINER S?NN AT JEG F?R T50 I SEEDDF:
SeedDF <- merge(x = SeedDF, y = glm2, by = "PlotSpecies", all.x = TRUE)

#runder av til integer
SeedDF$T50 <- round(SeedDF$T50)

## Tok ikke med det siste fra T50, tror det bare var rot.
## SeedScoringTot fra master script var bare rot.



############# ANALYSER JEG ENDTE OPP MED ########################
#### PROPORTION EMERGED MODELL JEG ENDER OPP MED: 
ModPropEmerged <- glmer(cbind(TotEmerged, TotNotEmerged) ~ OTC + Vegetation + Strategy + Precip + (Precip)^2 + OTC:Vegetation + OTC:Strategy + OTC:Precip + Vegetation:Strategy + Vegetation:Precip + Strategy:Precip + OTC:Strategy:Precip + Vegetation:Strategy:Precip + (1|SiteID) + (1|Species) + (1|BlockID), family = binomial(link = "logit"), data = SeedDF)
summary(ModPropEmerged)

# PROPORTION SURVIVED MODELL JEG ENDER OPP MED:
ModPropSurvived <- glmer(cbind(SurvivedPlot, NotSurvivedPlot) ~ OTC + Vegetation + Strategy + Precip + OTC:Vegetation + OTC:Strategy + OTC:Precip + Vegetation:Strategy + Vegetation:Precip + Strategy:Precip + OTC:Vegetation:Strategy + OTC:Strategy:Precip + Vegetation:Strategy:Precip + (1|SiteID) + (1|Species), family = binomial(link = "logit"), data = SeedDF)
while (length(ModPropSurvived@optinfo$conv$lme4) > 0) {
  pars = getME(ModPropSurvived,c("theta","fixef"))
  ModPropSurvived <-
    update(ModPropSurvived,
           start = pars,
           control = glmerControl(optCtrl = list(maxfun = 2e5)))
}
summary(ModPropSurvived)

# T50% JEG ENDER OPP MED:
ModT50 <- glmer(T50 ~ OTC + Vegetation + Strategy + Precip^2 + OTC:Vegetation + OTC:Strategy + OTC:Precip + Vegetation:Strategy + Vegetation:Precip + Strategy:Precip + OTC:Vegetation:Strategy + OTC:Vegetation:Precip + Vegetation:Strategy:Precip + (1|SiteID) + (1|Species) + (1|BlockID), family = poisson(link = "log"), data = SeedDF)
summary(ModT50)

library(sjPlot)
tab_model(ModPropEmerged, ModT50, ModPropSurvived, show.ci=TRUE)
