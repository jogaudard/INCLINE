
#########################################################
#### FLOWER PRODUCTION IN THE ALPINE PLANT COMMUNITY ####
#########################################################

#### Loading libraries ####
library(tidyverse)
library(dataDownloader)

get_file(node = "zhk3m",
         file = "INCLINE_flowering_2021.csv",
         path = "Raw_data",
         remote_path = "RawData/Flowering")




#################################################################
#### Adding demography data to my "flowering" community data ####
#################################################################


#### Importing demography data ####

#Sibbaldia procumbens
sib_pro_demography <- read.csv("Sib_pro_2018-2021.csv", header = TRUE, sep = ";", dec = ",") #first row is headers

#Veronica alpina
ver_alp_demography <- read.csv("Ver_alp_2018-2021.csv", header = TRUE, sep = ";", dec = ",") #first row is headers



#### Defining which coordinates belong to which subplot ####

#Sibbaldia procumbens
sib_pro_demography <- sib_pro_demography %>%
  filter(Year == 2021) %>% 
  select(Site:comment_transcription) %>% 
  mutate(Subplot = case_when((X >= 0 & X < 5) & (Y >= 20 & Y <= 25) ~ 1,
                             (X >= 5 & X < 10) & (Y >= 20 & Y <= 25) ~ 2,
                             (X >= 10 & X < 15) & (Y >= 20 & Y <= 25) ~ 3,
                             (X >= 15 & X < 20) & (Y >= 20 & Y <= 25) ~ 4,
                             (X >= 20 & X < 25) & (Y >= 20 & Y <= 25) ~ 5,
                             (X >= 25 & X < 30) & (Y >= 20 & Y <= 25) ~ 6,
                             (X >= 30 & X <= 35) & (Y >= 20 & Y <= 25) ~ 7,
                             (X >= 0 & X < 5) & (Y >= 15 & Y < 20) ~ 8,
                             (X >= 5 & X < 10) & (Y >= 15 & Y < 20) ~ 9,
                             (X >= 10 & X < 15) & (Y >= 15 & Y < 20) ~ 10,
                             (X >= 15 & X < 20) & (Y >= 15 & Y < 20) ~ 11,
                             (X >= 20 & X < 25) & (Y >= 15 & Y < 20) ~ 12,
                             (X >= 25 & X < 30) & (Y >= 15 & Y < 20) ~ 13,
                             (X >= 30 & X <= 35) & (Y >= 15 & Y < 20) ~ 14,
                             (X >= 0 & X < 5) & (Y >= 10 & Y < 15) ~ 15,
                             (X >= 5 & X < 10) & (Y >= 10 & Y < 15) ~ 16,
                             (X >= 10 & X < 15) & (Y >= 10 & Y < 15) ~ 17,
                             (X >= 15 & X < 20) & (Y >= 10 & Y < 15) ~ 18,
                             (X >= 20 & X < 25) & (Y >= 10 & Y < 15) ~ 19,
                             (X >= 25 & X < 30) & (Y >= 10 & Y < 15) ~ 20,
                             (X >= 30 & X <= 35) & (Y >= 10 & Y < 15) ~ 21,
                             (X >= 0 & X < 5) & (Y >= 5 & Y < 10) ~ 22,
                             (X >= 5 & X < 10) & (Y >= 5 & Y < 10) ~ 23,
                             (X >= 10 & X < 15) & (Y >= 5 & Y < 10) ~ 24,
                             (X >= 15 & X < 20) & (Y >= 5 & Y < 10) ~ 25,
                             (X >= 20 & X < 25) & (Y >= 5 & Y < 10) ~ 26,
                             (X >= 25 & X < 30) & (Y >= 5 & Y < 10) ~ 27,
                             (X >= 30 & X <= 35) & (Y >= 5 & Y < 10) ~ 28,
                             (X >= 0 & X < 5) & (Y >= 0 & Y < 5) ~ 29,
                             (X >= 5 & X < 10) & (Y >= 0 & Y < 5) ~ 30,
                             (X >= 10 & X < 15) & (Y >= 0 & Y < 5) ~ 31,
                             (X >= 15 & X < 20) & (Y >= 0 & Y < 5) ~ 32,
                             (X >= 20 & X < 25) & (Y >= 0 & Y < 5) ~ 33,
                             (X >= 25 & X < 30) & (Y >= 0 & Y < 5) ~ 34,
                             (X >= 30 & X <= 35) & (Y >= 0 & Y < 5) ~ 35)) %>% 
  filter(!is.na(Subplot))




#Veronica alpina
ver_alp_demography <- ver_alp_demography %>% 
  filter(Year == 2021) %>%  #choosing only data from 2021
  select(Site:comment_transcription) %>%  #removing unwanted columns (X.1, X.2, ...)
  mutate(Subplot = case_when((X >= 0 & X < 5) & (Y >= 20 & Y <= 25) ~ 1,
                             (X >= 5 & X < 10) & (Y >= 20 & Y <= 25) ~ 2,
                             (X >= 10 & X < 15) & (Y >= 20 & Y <= 25) ~ 3,
                             (X >= 15 & X < 20) & (Y >= 20 & Y <= 25) ~ 4,
                             (X >= 20 & X < 25) & (Y >= 20 & Y <= 25) ~ 5,
                             (X >= 25 & X < 30) & (Y >= 20 & Y <= 25) ~ 6,
                             (X >= 30 & X <= 35) & (Y >= 20 & Y <= 25) ~ 7,
                             (X >= 0 & X < 5) & (Y >= 15 & Y < 20) ~ 8,
                             (X >= 5 & X < 10) & (Y >= 15 & Y < 20) ~ 9,
                             (X >= 10 & X < 15) & (Y >= 15 & Y < 20) ~ 10,
                             (X >= 15 & X < 20) & (Y >= 15 & Y < 20) ~ 11,
                             (X >= 20 & X < 25) & (Y >= 15 & Y < 20) ~ 12,
                             (X >= 25 & X < 30) & (Y >= 15 & Y < 20) ~ 13,
                             (X >= 30 & X <= 35) & (Y >= 15 & Y < 20) ~ 14,
                             (X >= 0 & X < 5) & (Y >= 10 & Y < 15) ~ 15,
                             (X >= 5 & X < 10) & (Y >= 10 & Y < 15) ~ 16,
                             (X >= 10 & X < 15) & (Y >= 10 & Y < 15) ~ 17,
                             (X >= 15 & X < 20) & (Y >= 10 & Y < 15) ~ 18,
                             (X >= 20 & X < 25) & (Y >= 10 & Y < 15) ~ 19,
                             (X >= 25 & X < 30) & (Y >= 10 & Y < 15) ~ 20,
                             (X >= 30 & X <= 35) & (Y >= 10 & Y < 15) ~ 21,
                             (X >= 0 & X < 5) & (Y >= 5 & Y < 10) ~ 22,
                             (X >= 5 & X < 10) & (Y >= 5 & Y < 10) ~ 23,
                             (X >= 10 & X < 15) & (Y >= 5 & Y < 10) ~ 24,
                             (X >= 15 & X < 20) & (Y >= 5 & Y < 10) ~ 25,
                             (X >= 20 & X < 25) & (Y >= 5 & Y < 10) ~ 26,
                             (X >= 25 & X < 30) & (Y >= 5 & Y < 10) ~ 27,
                             (X >= 30 & X <= 35) & (Y >= 5 & Y < 10) ~ 28,
                             (X >= 0 & X < 5) & (Y >= 0 & Y < 5) ~ 29,
                             (X >= 5 & X < 10) & (Y >= 0 & Y < 5) ~ 30,
                             (X >= 10 & X < 15) & (Y >= 0 & Y < 5) ~ 31,
                             (X >= 15 & X < 20) & (Y >= 0 & Y < 5) ~ 32,
                             (X >= 20 & X < 25) & (Y >= 0 & Y < 5) ~ 33,
                             (X >= 25 & X < 30) & (Y >= 0 & Y < 5) ~ 34,
                             (X >= 30 & X <= 35) & (Y >= 0 & Y < 5) ~ 35)) %>% #adding a new column where I define which coordinates belong to each of the 35 subplots
  filter(!is.na(Subplot)) #filtering out data for individuals when Subplot = NA (outside frame, gone or new seedling)



#### Summarising number of reproductive organs ####

#Sibbaldia procumbens
sib_pro_sum_reproduction <- sib_pro_demography %>% 
  group_by(Site, Block, Plot, Subplot) %>%     #grouping data (for summing)
  mutate(Sib_pro = sum(NFL, NFL1, NFL2, NFL3, NFL4, 
                             NB, NB1, NB2, NB3, NB4,
                             NC, NC1, NC2, NC3, NC4, NC5, NC6, NC7,
                             NAC, NAC1, NAC2, NAC3, NAC4,
                             na.rm = TRUE),
         Sib_pro = na_if(Sib_pro, 0)) %>%    #summarise reproductive organs and create a new column with total sum
  select(Site, Block, Plot, Treat, Subplot, Year, Date, Registrator, Sib_pro, comment_registrator, comment_transcription) %>%   #selecting which columns I want to keep from original dataset (because I will add the info to the flower-data set)
  distinct() #selecting only distinct columns


#Veronica alpina
ver_alp_sum_reproduction <- ver_alp_demography %>% 
  group_by(Site, Block, Plot, Subplot) %>% 
  mutate(Ver_alp = sum(NFL, NB, NC, NAC, na.rm = TRUE),
         Ver_alp = na_if(Ver_alp, 0)) %>% 
  select(Site, Block, Plot, Treat, Subplot, Year, Date, Registrator, Ver_alp, comment, comment_transcription) %>% 
  distinct()


##################################
#### Importing community data ####
##################################

#Flowering data
reproduction_data <- read.csv("INCLINE_flowering_2021.csv", header = TRUE, sep = ";", dec = ",") #first row is headers



#### Merging the "summed" Veronica and Sibbaldia data with the "reproduction" data ####

full_reproduction_data <- reproduction_data %>% 
  left_join(sib_pro_sum_reproduction, by = c("Site", "Block", "Plot", "Treat", "Subplot", "Year"), suffix = c("_rep", "_demo_sp")) %>% #suffix to avoid confusion between columns with same name from previous datasets
  left_join(ver_alp_sum_reproduction, by = c("Site", "Block", "Plot", "Treat", "Subplot", "Year"))




############################################################
#### Pivoting the reproduction data set before analysis ####
############################################################


#### Removing numbered suffix from species names ####

colnames(full_reproduction_data) <- gsub("\\_[0-99]*$", "" , colnames(full_reproduction_data))


#### Pivot data from wide to long ####

pivot_reproduction_data <- full_reproduction_data %>%
  pivot_longer(!c(Site, Block, Plot, Treat, Subplot, Year, Date_rep, Registrator_rep, Writer, Weather, Comments, Date_demo_sp, Registrator_demo_sp, comment_registrator, comment_transcription.x, Date, Registrator, comment, comment_transcription.y), #columns I don't want to pivot
                            names_to = "Species", #what to call the column where the pivoted headers (species) go
                            values_to = "Reproduction_value", #what to call the column where the pivoted values go
                            values_drop_na = TRUE)  #remove NA-values



##############################################################
#### Standardising the flowering data/reproduction values ####
##############################################################



#### Summing of multiple values for several individuals of the same species within the same subplot ####

#Summing variable by group
summed_reproduction_data <- pivot_reproduction_data %>% 
  group_by(Site, Block, Plot, Subplot, Species, Treat) %>% 
  summarise(Reproduction_value = sum(Reproduction_value))



#### Create total means for species and standardising ####

standardised_reproduction_data <- summed_reproduction_data %>% 
  group_by(Species) %>% #need to create new column called species when pivoting the data
  mutate(Total_mean = mean(Reproduction_value)) %>% #making a new column with mean values for all species
  mutate(Reproduction_value_standard = (Reproduction_value - Total_mean)/Total_mean) %>%
#standardizing values by removing units. When subtracting the total mean we "erase" differences between species (which we have measured differently)
  mutate(Block_ID = paste0(Site, "_", Block)) %>% #creating "full" block names
  mutate(Plot_ID = paste0(Site, "_", Block, "_", Plot, "_", Treat)) %>% #creating "full" plot names
  mutate(Warming = case_when(Treat == "CC" ~ "0",
                          Treat == "CN" ~ "0",
                          Treat == "WC" ~ "1",
                          TRUE ~ "1"),
         Novel = case_when(Treat == "CC" ~ "0",
                           Treat == "CN" ~ "1",
                           Treat == "WC" ~ "0",
                           TRUE ~ "1"),
         Precipitation = case_when(Site == "Skj" ~ 3402,
                                   Site == "Gud" ~ 2130,
                                   Site == "Lav" ~ 1561,
                                   TRUE ~ 1226))



standardised_reproduction_data <- standardised_reproduction_data %>%
   mutate(Reproduction_value_standard = Reproduction_value_standard + abs(min(Reproduction_value_standard))) #moving up above 0




#########################################################################
#### Merging species systematics with standardised reproduction data ####
#########################################################################

#### Importing systematics data ####
species_systematics <- read.csv("species_systematics.csv", header = TRUE, sep = ";",  dec = ",")


#### Merging the data sets by species ####
systematics_reproduction_data <- standardised_reproduction_data %>% 
  left_join(species_systematics, by = "Species") %>% 
  filter(!Functional_group %in% "Fern") %>% 
  filter(!Functional_group %in% "Lycophyte") %>% 
  filter(!Functional_group %in% "Dwarf shrub") %>% 
  mutate(FG = case_when(Functional_group == "Graminoid" ~ "Agraminoid",
                        TRUE ~ "Forb")) %>% 
  mutate(SiteID = case_when(Site == "Gud" ~ "Gud",
                            Site == "Lav" ~ "Lav",
                            Site == "Ulv" ~ "Ulv",
                            TRUE ~ "ASkj")) %>% 
  mutate(Flower_count = case_when(Measurement_type == "Flower count" ~ Reproduction_value)) %>% 
                                  #Measurement_type == "Flower head count" ~ Reproduction_value,
                                  #Measurement_type == "Number of flowering individuals" ~ Reproduction_value,
                                  #Measurement_type == "Number of inflorescence units" ~ Reproduction_value)) %>% 
  mutate(Flower_head = case_when(Measurement_type == "Flower head count" ~ Reproduction_value)) %>% 
  mutate(Flower_ind = case_when(Measurement_type == "Number of flowering individuals" ~ Reproduction_value)) %>%
  mutate(Spikelets = case_when(Measurement_type == "Number of inflorescence units" ~ Reproduction_value)) %>%
  mutate(Inflorescence = case_when(Measurement_type == "Length of inflorescence" ~ Reproduction_value)) %>%
  mutate(Percent_cover = case_when(Measurement_type == "Percent cover" ~ Reproduction_value/100)) %>%
  mutate(proxy_flower_head = case_when(Measurement_type == "Flower head count" ~ 1, TRUE ~ 0)) %>% 
  mutate(proxy_flower_ind = case_when(Measurement_type == "Number of flowering individuals" ~ 1, TRUE ~ 0)) %>%
  mutate(proxy_spikelets = case_when(Measurement_type == "Number of inflorescence units" ~ 1, TRUE ~ 0)) %>%
  mutate(proxy_inflorescence = case_when(Measurement_type == "Length of inflorescence" ~ 1, TRUE ~ 0)) %>%
  mutate(proxy_percent_cover = case_when(Measurement_type == "Percent cover" ~ 1, TRUE ~ 0))



d <- as.matrix(systematics_reproduction_data[,24:29])


#Checking number of observations for each proxy
sum(systematics_reproduction_data$proxy_flower_head) #100
sum(systematics_reproduction_data$proxy_flower_ind) #244
sum(systematics_reproduction_data$proxy_spikelets) #151
sum(systematics_reproduction_data$proxy_inflorescence) #488
sum(systematics_reproduction_data$proxy_percent_cover) #29


#### Running model using 'Integrated Nested Laplace Approximation', a deterministic Bayesian method ####

inla.mod <- inla(Y ~ proxy1 + proxy2 + proxy3 + proxy4 + proxy5 + Warming + Novel + Functional_group + Warming:Novel + Warming:Functional_group + Novel:Functional_group + Warming:Novel:Functional_group + f(Block_ID, model = "iid") + f(Data_ID, model = "iid"), 
                 data = list(Y = d, 
                             proxy1 = systematics_reproduction_data$proxy_flower_head,
                             proxy2 = systematics_reproduction_data$proxy_flower_ind,
                             proxy3 = systematics_reproduction_data$proxy_spikelets,
                             proxy4 = systematics_reproduction_data$proxy_inflorescence,
                             proxy5 = systematics_reproduction_data$proxy_percent_cover,
                             Warming = systematics_reproduction_data$Warming,
                             Novel = systematics_reproduction_data$Novel,
                             Functional_group = systematics_reproduction_data$Functional_group,
                             Block_ID = systematics_reproduction_data$Block_ID,
                             Data_ID = systematics_reproduction_data$Data_ID),
                 family = c("poisson", "poisson", "poisson", "poisson", "gamma", "gamma"))



inla.mod$summary.linpred
dim(inla.mod$summary.fitted.values)

summary(inla.mod)



systematics_reproduction_data$Residuals <- log(systematics_reproduction_data$Reproduction_value) - log(inla.mod$summary.fitted.values$mean)



#Creating a row in data set with values from 1 to 1384 (representing each measurement)
systematics_reproduction_data <- systematics_reproduction_data %>% 
  ungroup()  %>%
  mutate(Data_ID = rownames(systematics_reproduction_data))


#Plotte residuals for hver proxy (select proxy fra datasettet)
 systematics_reproduction_data %>% 
   select(proxy_flower_head = 1) %>%
  ggplot(aes(x = Treat, y = Residuals)) +
   geom_boxplot()

dispersion_check(inla.mod)

# inla.mod <- inla(Y ~ proxy1 + proxy2 + proxy3 + proxy4 + proxy5 + Warming + Novel + Warming:Novel + f(Block_ID, model = "iid"), 
#                  data = list(Y = d, 
#                              proxy1 = systematics_reproduction_data$proxy_flower_head,
#                              proxy2 = systematics_reproduction_data$proxy_flower_ind,
#                              proxy3 = systematics_reproduction_data$proxy_spikelets,
#                              proxy4 = systematics_reproduction_data$proxy_inflorescence,
#                              proxy5 = systematics_reproduction_data$proxy_percent_cover,
#                              Warming = systematics_reproduction_data$Warming,
#                              Novel = systematics_reproduction_data$Novel,
#                              Block_ID = systematics_reproduction_data$Block_ID),
#                  family = c("poisson", "poisson", "poisson", "poisson", "gamma", "beta"))



# inlamodel.mean <- data.frame(Warming = NA, Novel = NA, Value=NA) 
# inlamodel.mean[1, 1:2] <- c("0", "0")
# inlamodel.mean[1, 3] <- exp(1.170)
# inlamodel.mean[2, 1:2] <- c("1", "0")
# inlamodel.mean[2, 3] <- exp(1.170 + 0.295)
# inlamodel.mean[3, 1:2] <- c("0", "1")
# inlamodel.mean[3, 3] <- exp(1.170 + 0.232)
# inlamodel.mean[4, 1:2] <- c("1", "1")
# inlamodel.mean[4, 3] <- exp(1.170 + 0.295 + 0.232 - 0.291)


#Creating an overview of distribution percentages from the summary
per1 <- sum(inla.mod$marginals.fixed[[1]][inla.mod$marginals.fixed[[1]][,1]>0,2])/sum(inla.mod$marginals.fixed[[1]][,2])*100

per2 <- sum(inla.mod$marginals.fixed[[7]][inla.mod$marginals.fixed[[7]][,1]<0,2])/sum(inla.mod$marginals.fixed[[7]][,2])*100

per3 <- sum(inla.mod$marginals.fixed[[8]][inla.mod$marginals.fixed[[8]][,1]>0,2])/sum(inla.mod$marginals.fixed[[8]][,2])*100

per4 <- sum(inla.mod$marginals.fixed[[9]][inla.mod$marginals.fixed[[9]][,1]>0,2])/sum(inla.mod$marginals.fixed[[9]][,2])*100

per5 <- sum(inla.mod$marginals.fixed[[10]][inla.mod$marginals.fixed[[10]][,1]<0,2])/sum(inla.mod$marginals.fixed[[10]][,2])*100

per6 <- sum(inla.mod$marginals.fixed[[11]][inla.mod$marginals.fixed[[11]][,1]>0,2])/sum(inla.mod$marginals.fixed[[11]][,2])*100

per7 <- sum(inla.mod$marginals.fixed[[12]][inla.mod$marginals.fixed[[12]][,1]<0,2])/sum(inla.mod$marginals.fixed[[12]][,2])*100

per8 <- sum(inla.mod$marginals.fixed[[13]][inla.mod$marginals.fixed[[13]][,1]<0,2])/sum(inla.mod$marginals.fixed[[13]][,2])*100



variables <- c("(Intercept)", "Warming1", "Novel1", "Functional_groupGraminoid", "Warming1:Novel1", "Warming1:Functional_groupGraminoid", "Novel1:Functional_groupGraminoid", "Warming1:Novel1:Functional_groupGraminoid")

percentages <- c(per1, per2, per3, per4, per5, per6, per7, per8)

percentage.data <- as.data.frame(x = percentages, row.names = variables) %>% 
  rownames_to_column()

distr.percent <- as.data.frame(inla.mod$summary.fixed) %>% 
  rownames_to_column() %>% 
  filter(!rowname %in% c("proxy1", "proxy2", "proxy3", "proxy4", "proxy5")) %>% 
  select(rowname, mean, sd) %>% 
  left_join(percentage.data, by = c("rowname"))


proxy_data <- as.data.frame(inla.mod$summary.fixed) %>% 
  rownames_to_column() %>% 
  filter(rowname %in% c("proxy1", "proxy2", "proxy3", "proxy4", "proxy5")) %>% 
  select()

#Plotting the data
ggplot(data = systematics_reproduction_data, aes(x = Treat, y = Reproduction_value_standard))+
geom_violin(data = systematics_reproduction_data, aes(x = Treat, y = Reproduction_value_standard)) +
stat_summary(fun = "median",
              geom = "crossbar", 
              width = 0.3,
              colour = "red")+
facet_wrap(~Functional_group, nrow=2, ncol=1)+
theme_bw()+
labs(x = "Treatment", y = "Flowering-index values")
 

#Creating a data frame with predicted values from the model
inlamodel.mean <- data.frame(Warming = NA, Novel = NA, Functional_group=NA, Value.link=NA, Value.resp = NA, Diff.link.CC = NA, Diff.resp.CC = NA, Diff.link.WC = NA, Diff.resp.WC = NA, Value.heads = NA, Value.ind = NA, Value.spikelet = NA, Value.inflorescence = NA, Value.cover = NA) 
inlamodel.mean[1, 1:3] <- c("0", "0", "Forb")
inlamodel.mean[1, 4] <- inla.mod$summary.fixed$mean[1]
inlamodel.mean[2, 1:3] <- c("1", "0", "Forb") 
inlamodel.mean[2, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[7]
inlamodel.mean[3, 1:3] <- c("0", "1", "Forb")
inlamodel.mean[3, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[8]
inlamodel.mean[4, 1:3] <- c("1", "1", "Forb")
inlamodel.mean[4, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[7]+inla.mod$summary.fixed$mean[8]+inla.mod$summary.fixed$mean[10]
inlamodel.mean[5, 1:3] <- c("0", "0", "Graminoid")
inlamodel.mean[5, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[9]
inlamodel.mean[6, 1:3] <- c("1", "0", "Graminoid")
inlamodel.mean[6, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[7]+inla.mod$summary.fixed$mean[9]+inla.mod$summary.fixed$mean[11]
inlamodel.mean[7, 1:3] <- c("0", "1", "Graminoid")
inlamodel.mean[7, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[8]+inla.mod$summary.fixed$mean[9]+inla.mod$summary.fixed$mean[12]
inlamodel.mean[8, 1:3] <- c("1", "1", "Graminoid")
inlamodel.mean[8, 4] <- inla.mod$summary.fixed$mean[1]+inla.mod$summary.fixed$mean[7]+inla.mod$summary.fixed$mean[8]+inla.mod$summary.fixed$mean[9]+inla.mod$summary.fixed$mean[10]+inla.mod$summary.fixed$mean[11]+inla.mod$summary.fixed$mean[12]+inla.mod$summary.fixed$mean[13]

#Getting the actual flower values from the model values with exp
inlamodel.mean$Value.resp <- exp(inlamodel.mean$Value.link)

#Differences from CC
inlamodel.mean[2, 6] <- inlamodel.mean[2, 4] - inlamodel.mean[1, 4]
inlamodel.mean[3, 6] <- inlamodel.mean[3, 4] - inlamodel.mean[1, 4]
inlamodel.mean[4, 6] <- inlamodel.mean[4, 4] - inlamodel.mean[1, 4]

inlamodel.mean[6, 6] <- inlamodel.mean[6, 4] - inlamodel.mean[5, 4]
inlamodel.mean[7, 6] <- inlamodel.mean[7, 4] - inlamodel.mean[5, 4]
inlamodel.mean[8, 6] <- inlamodel.mean[8, 4] - inlamodel.mean[5, 4] 

#Getting the actual flower difference between treatments (compared to controls) with exp
inlamodel.mean$Diff.resp.CC <- exp(inlamodel.mean$Diff.link.CC)

inlamodel.mean[2, 6] <- inlamodel.mean[2, 4] - inlamodel.mean[1, 4] #WC-CC forbs
inlamodel.mean[7, 6] <- inlamodel.mean[7, 4] - inlamodel.mean[5, 4] #CN-CC graminoids

inlamodel.mean[2, 7] <- -exp(0.0275414123)
inlamodel.mean[7, 7] <- -exp(0.0004669099)


#Differences from WC
inlamodel.mean[4, 8] <- inlamodel.mean[4, 4] - inlamodel.mean[2, 4] #WN-WC forbs
inlamodel.mean[8, 8] <- inlamodel.mean[8, 4] - inlamodel.mean[6, 4] #WN-WC graminoids

#Getting the actual flower difference between treatments (compared to warmed controls) with exp
inlamodel.mean[4, 9] <- exp(0.10640109)
inlamodel.mean[8, 9] <- -exp(0.05185708)

#Getting the actual flower values from the model values with exp for proxies
inlamodel.mean$Value.heads <- exp((inlamodel.mean$Value.link)+(inla.mod$summary.fixed$mean[2]))
inlamodel.mean$Value.ind <- exp((inlamodel.mean$Value.link)+(inla.mod$summary.fixed$mean[3]))
inlamodel.mean$Value.spikelet <- exp((inlamodel.mean$Value.link)+(inla.mod$summary.fixed$mean[4]))
inlamodel.mean$Value.inflorescence <- exp((inlamodel.mean$Value.link)+(inla.mod$summary.fixed$mean[5]))
inlamodel.mean$Value.cover <- exp((inlamodel.mean$Value.link)+(inla.mod$summary.fixed$mean[6]))


# inlamodel.mean <- inlamodel.mean %>% 
#   mutate(Treatment = case_when(Warming == 0 & Novel == 0 ~ "CC",
#                    Warming == 1 & Novel == 0 ~ "WC",
#                    Warming == 0 & Novel == 1 ~ "CN",
#                    TRUE ~ "WN"))


inla.mod1 <- inla(Y ~ proxy1 + proxy2 + proxy3 + proxy4 + proxy5 + Warming + Novel + Precipitation + Functional_group + Warming:Novel + Warming:Precipitation + Novel:Precipitation + Warming:Functional_group + Novel:Functional_group + Warming:Novel:Precipitation + Warming:Novel:Functional_group + f(Block_ID, model = "iid") + f(Data_ID, model = "iid"), 
                  data = list(Y = d, 
                              proxy1 = systematics_reproduction_data$proxy_flower_head,
                              proxy2 = systematics_reproduction_data$proxy_flower_ind,
                              proxy3 = systematics_reproduction_data$proxy_spikelets,
                              proxy4 = systematics_reproduction_data$proxy_inflorescence,
                              proxy5 = systematics_reproduction_data$proxy_percent_cover,
                              Warming = systematics_reproduction_data$Warming,
                              Novel = systematics_reproduction_data$Novel,
                              Precipitation = systematics_reproduction_data$Precipitation,
                              Functional_group = systematics_reproduction_data$Functional_group,
                              Block_ID = systematics_reproduction_data$Block_ID,
                              Data_ID = systematics_reproduction_data$Data_ID),
                  family = c("poisson", "poisson", "poisson", "poisson", "gamma", "gamma"))
 
summary(inla.mod1)



#### Calculating percentages for model distribution ####

# #Warming, Ulvehaugen, Forbs
# per1 <- sum(inla.mod$marginals.fixed[[7]][inla.mod$marginals.fixed[[7]][,1]<0,2])/sum(inla.mod$marginals.fixed[[7]][,2])*100
# 
# #Novel, Ulvehaugen, Forbs
# per2 <- sum(inla.mod$marginals.fixed[[8]][inla.mod$marginals.fixed[[8]][,1]>0,2])/sum(inla.mod$marginals.fixed[[8]][,2])*100
# 
# #Control, Låvisdalen, Forbs
# per3 <- sum(inla.mod$marginals.fixed[[9]][inla.mod$marginals.fixed[[9]][,1]<0,2])/sum(inla.mod$marginals.fixed[[9]][,2])*100
# 
# #Control, Gudmedalen, Forbs
# per4 <- sum(inla.mod$marginals.fixed[[10]][inla.mod$marginals.fixed[[10]][,1]>0,2])/sum(inla.mod$marginals.fixed[[10]][,2])*100
# 
# #Control, Skjellingahaugen, Forbs
# per5 <- sum(inla.mod$marginals.fixed[[11]][inla.mod$marginals.fixed[[11]][,1]>0,2])/sum(inla.mod$marginals.fixed[[11]][,2])*100
# 
# #Control, Ulvehaugen, Graminoid
# per6 <- sum(inla.mod$marginals.fixed[[12]][inla.mod$marginals.fixed[[12]][,1]>0,2])/sum(inla.mod$marginals.fixed[[12]][,2])*100
# 
# #Warming, novel, Ulvehaugen, Forbs
# per7 <- sum(inla.mod$marginals.fixed[[13]][inla.mod$marginals.fixed[[13]][,1]>0,2])/sum(inla.mod$marginals.fixed[[13]][,2])*100
# 
# #Warming, Låvisdalen, Forbs
# per8 <- sum(inla.mod$marginals.fixed[[14]][inla.mod$marginals.fixed[[14]][,1]>0,2])/sum(inla.mod$marginals.fixed[[14]][,2])*100
# 
# #Warming, Gudmedalen, Forbs
# per9 <- sum(inla.mod$marginals.fixed[[15]][inla.mod$marginals.fixed[[15]][,1]>0,2])/sum(inla.mod$marginals.fixed[[15]][,2])*100
# 
# #Warming, Skjellingahaugen, Forbs
# per10 <- sum(inla.mod$marginals.fixed[[16]][inla.mod$marginals.fixed[[16]][,1]>0,2])/sum(inla.mod$marginals.fixed[[16]][,2])*100
# 
# #Novel, Låvisdalen, Forbs
# per11 <- sum(inla.mod$marginals.fixed[[17]][inla.mod$marginals.fixed[[17]][,1]>0,2])/sum(inla.mod$marginals.fixed[[17]][,2])*100
# 
# #Novel, Gudmedalen, Forbs
# per12 <- sum(inla.mod$marginals.fixed[[18]][inla.mod$marginals.fixed[[18]][,1]<0,2])/sum(inla.mod$marginals.fixed[[18]][,2])*100
# 
# #Novel, Skjellingahaugen, Forbs
# per13 <- sum(inla.mod$marginals.fixed[[19]][inla.mod$marginals.fixed[[19]][,1]>0,2])/sum(inla.mod$marginals.fixed[[19]][,2])*100
# 
# #Warming, Ulvehaugen, Graminoid
# per14 <- sum(inla.mod$marginals.fixed[[20]][inla.mod$marginals.fixed[[20]][,1]>0,2])/sum(inla.mod$marginals.fixed[[20]][,2])*100
# 
# #Novel, Ulvehaugen, Graminoid
# per15 <- sum(inla.mod$marginals.fixed[[21]][inla.mod$marginals.fixed[[21]][,1]<0,2])/sum(inla.mod$marginals.fixed[[21]][,2])*100
# 
# #Warming, novel, Låvisdalen
# per16 <- sum(inla.mod$marginals.fixed[[22]][inla.mod$marginals.fixed[[22]][,1]<0,2])/sum(inla.mod$marginals.fixed[[22]][,2])*100
# 
# #Warming, novel, Gudmedalen
# per17 <- sum(inla.mod$marginals.fixed[[23]][inla.mod$marginals.fixed[[23]][,1]<0,2])/sum(inla.mod$marginals.fixed[[23]][,2])*100
# 
# #Warming, novel, Skjellingahaugen
# per18 <- sum(inla.mod$marginals.fixed[[24]][inla.mod$marginals.fixed[[24]][,1]<0,2])/sum(inla.mod$marginals.fixed[[24]][,2])*100
# 
# #Warming, novel, Graminoid
# per19 <- sum(inla.mod$marginals.fixed[[25]][inla.mod$marginals.fixed[[25]][,1]<0,2])/sum(inla.mod$marginals.fixed[[25]][,2])*100
  


#### Making a data frame with model output and percentages ####

# variables <- c("Warming1", "Novel1", "Precipitation1561", "Precipitation2130", "Precipitation3402", "Functional_groupGraminoid", "Warming1:Novel1", "Warming1:Precipitation1561", "Warming1:Precipitation2130", "Warming1:Precipitation3402", "Novel1:Precipitation1561", "Novel1:Precipitation2130", "Novel1:Precipitation3402", "Warming1:Functional_groupGraminoid", "Novel1:Functional_groupGraminoid", "Warming1:Novel1:Precipitation1561", "Warming1:Novel1:Precipitation2130", "Warming1:Novel1:Precipitation3402", "Warming1:Novel1:Functional_groupGraminoid")
# 
# 
# percentages <- c(per1, per2, per3, per4, per5, per6, per7, per8, per9, per10, per11, per12, per13, per14, per15, per16, per17, per18, per19)
# 
# 
# percentage.data <- as.data.frame(x = percentages, row.names = variables) %>% 
#   rownames_to_column()
# 
# 
# distr.percent <- as.data.frame(inla.mod$summary.fixed) %>% 
#   rownames_to_column() %>% 
#   dplyr::filter(!rowname %in% c("(Intercept)", "proxy1", "proxy2", "proxy3", "proxy4", "proxy5")) %>% 
#   dplyr::select(rowname, mean, sd) %>% 
#   left_join(percentage.data, by = c("rowname"))





########################
#### Running models ####
########################


#### Plotting the data ####

#Site
site_plot <- ggplot(systematics_reproduction_data, aes(x = Precipitation, y = Reproduction_value_standard, fill = Treat))+
  geom_boxplot()+
  facet_wrap(~Treat)



#Treatment and functional group
treat_group_plot <- ggplot(systematics_reproduction_data, aes(x = Treat, y = Reproduction_value_standard, fill = Functional_group)) +
  geom_boxplot() +
  theme_bw(base_size = 20) +
  labs(x = "Treatment", y = "Number of flowers", fill = "Functional group")



treat_group_plot2 <- ggplot(systematics_reproduction_data, aes(x = Treat, y = Reproduction_value_standard, fill = Functional_group, colour = Functional_group)) +
  geom_violin() +
  stat_summary(fun = "median", geom = "crossbar", aes(group = Functional_group), position = position_dodge(0.9)) +
  theme_bw(base_size = 20) + 
  labs(x = "Treatment", y = "Number of flowers", fill = "Functional group")


#Treatment and precipitation:
treat_precipitation_plot <- ggplot(systematics_reproduction_data, aes(x = Treat, y = Reproduction_value_standard, fill = as.factor(Precipitation))) +
  geom_boxplot() +
  theme_bw(base_size = 20) + 
  labs(x = "Treatment", y = "Number of flowers", fill = "Precipitation")

treat_precipitation_plot2 <- ggplot(systematics_reproduction_data, aes(x = Treat, y = Reproduction_value_standard, fill = as.factor(Precipitation))) +
  geom_violin() +
  theme_bw(base_size = 20) + 
  labs(x = "Treatment", y = "Number of flowers", fill = "Precipitation")



#Histogram:
#hist(systematics_reproduction_data$Reproduction_value_standard)


#Looking at number of blocks:
#ranef(group.model) #A generic function to extract the conditional modes of the random effects from a fitted model object.

#Plotting the residuals of the model as a histogram to look at distribution
#hist(resid(group.model), breaks = 40)




#Number of measurements per group and treatment
systematics_reproduction_data %>% 
  group_by(Functional_group, Treat) %>% 
  summarise(n_per_group = n())


str(systematics_reproduction_data)


#### Running models ####

#Loading libraries
library(MASS)


#Treatment, precipitation and functional group:
group.model <- glmmPQL(Reproduction_value_standard ~ 
                      Treat +
                      Precipitation +
                      Functional_group +
                      Treat : Precipitation +
                      Treat : Functional_group,
                      random = ~ 1|Block_ID, family = poisson,
                 data = systematics_reproduction_data)

summary(group.model)

#Checking for overdispersion
rdf <- df.residual(group.model)
rp <- residuals(group.model,type="pearson")
Pearson.chisq <- sum(rp^2)
prat <- Pearson.chisq/rdf
pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)

overdisp_fun(group.model)

library(emmeans)

#Model with precipitation (site) as a factor and fixed effect
group.model2 <- glmmPQL(Reproduction_value_standard ~ 
                         Treat +
                         Site +
                         Functional_group +
                         Treat : Site +
                         Treat : Functional_group +
                          Site : Functional_group +
                        Treat:Site:Functional_group,
                       random = ~ 1|Block_ID, family = poisson,
                       data = systematics_reproduction_data)
summary(group.model2)

group.model3 <- glmmPQL(Reproduction_value_standard ~ Novel*Warming*Site*Functional_group,
                        random = ~ 1|Block_ID, family = poisson,
                        data = systematics_reproduction_data)

summary(group.model3)

emm1 <- emmeans(group.model3, specs = pairwise~Novel:Warming:Site:Functional_group)


emm1$emmeans
emm1$contrasts

#Checking for overdispersion
rdf <- df.residual(group.model3)
rp <- residuals(group.model3,type="pearson")
Pearson.chisq <- sum(rp^2)
prat <- Pearson.chisq/rdf
pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)

overdisp_fun(group.model3)
str(group.model3)

#Creating a data framw with model means for each combination of variables
model.mean <- data.frame(Treat=NA, Functional_group=NA, Site=NA, Value=NA) 
model.mean[1, 1:3] <- c("CC", "Forb", "Gud") 
model.mean[1, 4] <- exp(group.model2$coefficients$fixed[1])
model.mean[2, 1:3] <- c("CN", "Forb", "Gud") 
model.mean[2, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2])
model.mean[3, 1:3] <- c("WC", "Forb", "Gud") 
model.mean[3, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3])
model.mean[4, 1:3] <- c("WN", "Forb", "Gud") 
model.mean[4, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4])
model.mean[5, 1:3] <- c("CC", "Forb", "Lav") 
model.mean[5, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[5])
model.mean[6, 1:3] <- c("CC", "Forb", "Skj") 
model.mean[6, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[6])
model.mean[7, 1:3] <- c("CC", "Forb", "Ulv") 
model.mean[7, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[7])
model.mean[8, 1:3] <- c("CC", "Graminoid", "Gud") 
model.mean[8, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[8])
model.mean[9, 1:3] <- c("CN", "Forb", "Lav") 
model.mean[9, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[9])
model.mean[10, 1:3] <- c("WC", "Forb", "Lav") 
model.mean[10, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[10])
model.mean[11, 1:3] <- c("WN", "Forb", "Lav") 
model.mean[11, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[11])
model.mean[12, 1:3] <- c("CN", "Forb", "Skj") 
model.mean[12, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[12])
model.mean[13, 1:3] <- c("WC", "Forb", "Skj") 
model.mean[13, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[13])
model.mean[14, 1:3] <- c("WN", "Forb", "Skj") 
model.mean[14, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[14])
model.mean[15, 1:3] <- c("CN", "Forb", "Ulv") 
model.mean[15, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[15])
model.mean[16, 1:3] <- c("WC", "Forb", "Ulv") 
model.mean[16, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[16])
model.mean[17, 1:3] <- c("WN", "Forb", "Ulv") 
model.mean[17, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[17])
model.mean[18, 1:3] <- c("CN", "Graminoid", "Gud") 
model.mean[18, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[18])
model.mean[19, 1:3] <- c("WC", "Graminoid", "Gud") 
model.mean[19, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[19])
model.mean[20, 1:3] <- c("WN", "Graminoid", "Gud") 
model.mean[20, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[20])
model.mean[21, 1:3] <- c("CC", "Graminoid", "Lav") 
model.mean[21, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[21])
model.mean[22, 1:3] <- c("CC", "Graminoid", "Skj") 
model.mean[22, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[22])
model.mean[23, 1:3] <- c("CC", "Graminoid", "Ulv") 
model.mean[23, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[23])
model.mean[24, 1:3] <- c("CN", "Graminoid", "Lav") 
model.mean[24, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[9]+group.model2$coefficients$fixed[18]+group.model2$coefficients$fixed[21]+group.model2$coefficients$fixed[24])
model.mean[25, 1:3] <- c("WC", "Graminoid", "Lav") 
model.mean[25, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[10]+group.model2$coefficients$fixed[19]+group.model2$coefficients$fixed[21]+group.model2$coefficients$fixed[25])
model.mean[26, 1:3] <- c("WN", "Graminoid", "Lav") 
model.mean[26, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[5]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[11]+group.model2$coefficients$fixed[20]+group.model2$coefficients$fixed[21]+group.model2$coefficients$fixed[26])
model.mean[27, 1:3] <- c("CN", "Graminoid", "Skj") 
model.mean[27, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[12]+group.model2$coefficients$fixed[18]+group.model2$coefficients$fixed[22]+group.model2$coefficients$fixed[27])
model.mean[28, 1:3] <- c("WC", "Graminoid", "Skj") 
model.mean[28, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[13]+group.model2$coefficients$fixed[19]+group.model2$coefficients$fixed[22]+group.model2$coefficients$fixed[28])
model.mean[29, 1:3] <- c("WN", "Graminoid", "Skj") 
model.mean[29, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[6]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[14]+group.model2$coefficients$fixed[20]+group.model2$coefficients$fixed[22]+group.model2$coefficients$fixed[29])
model.mean[30, 1:3] <- c("CN", "Graminoid", "Ulv") 
model.mean[30, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[2]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[15]+group.model2$coefficients$fixed[18]+group.model2$coefficients$fixed[23]+group.model2$coefficients$fixed[30])
model.mean[31, 1:3] <- c("WC", "Graminoid", "Ulv") 
model.mean[31, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[3]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[16]+group.model2$coefficients$fixed[19]+group.model2$coefficients$fixed[23]+group.model2$coefficients$fixed[31])
model.mean[32, 1:3] <- c("WN", "Graminoid", "Ulv") 
model.mean[32, 4] <- exp(group.model2$coefficients$fixed[1]+group.model2$coefficients$fixed[4]+group.model2$coefficients$fixed[7]+group.model2$coefficients$fixed[8]+group.model2$coefficients$fixed[17]+group.model2$coefficients$fixed[20]+group.model2$coefficients$fixed[23]+group.model2$coefficients$fixed[32])

n.dat <- data.frame(Treat = rep(c("CC", "CN", "WC", "WN"), 8),
                    Site = rep(c("Ulv", "Lav", "Gud", "Skj"), each=8),
                    Functional_group = rep(c(rep("Forb", 4), rep("Graminoid", 4)), 4))

n.dat$pred <- predict(group.model2, newdata = n.dat, level = 0, type = "response")

n.dat$Reproduction_value_standard <- predict(group.model2, newdata = n.dat, level = 0, type = "response")

n.dat <- n.dat %>% 
  filter(!Treat == "WN") %>% 
  mutate(Precipitation = case_when(Site == "Ulv" ~ "1226",
                                    Site == "Lav" ~ "1561",
                                    Site == "Gud" ~ "2130",
                                    TRUE ~ "3402"))


#Plotting
x <- systematics_reproduction_data
  #filter(Treat %in% list("CC", "CN", "WC"))

ggplot(data = x, aes(x = Precipitation, y = Reproduction_value_standard, fill = Site))+
  #geom_boxplot(data = x, aes(x = Precipitation, y = Reproduction_value_standard, fill = Site))+
  geom_violin(data = x, aes(x = Precipitation, y = Reproduction_value_standard, fill = Site), draw_quantiles = c(.25, .50, .75))+
  geom_point(data = n.dat, aes(x = Precipitation, y = Reproduction_value_standard, fill = Site))+
  #geom_crossbar(data = n.dat, aes = (x = pred))+
  facet_wrap(Treat~Functional_group, nrow=4, ncol=2)+
  theme_bw()+
  labs(x = "Precipitation level", y = "Flowering-index values", fill = "Site")+
ylim(0, 3)





#Checking for singularity when running the model. If singular -> too complex model.

#Looking at Std.Dev for random effects: Variance between Block_ID is smaller than residual variance (0.1929226 < 0.8920371). This is good for using the model to predict.

#Forbs with new neighbours produce significantly more flowers than in the control treatment. Graminoids (in same treatment) are not significantly different from forbs. I.e. alpine plants produce more flowers when new lowland neighbours are introduced.

#Under warming, forbs produce significantly lower numbers of flowers than in the control treatment. Graminoids (in same treatment) are significantly different from forbs. Graminoids appear unaffected by warming.

#Warming and novel competitors is not significant.

#Precipitation is not significant for any treatments.




#### Model-checking plots ####

#Are the residuals well behaved?
residuals <- resid(group.model3)
plot(fitted(group.model3), residuals)
abline(0,0)


#Is the response variable a reasonably linear function of the fitted values?
plot(fitted(group.model3), systematics_reproduction_data$Reproduction_value_standard)


#Are the errors reasonably close to normally distributed?
qqnorm(residuals)
qqline(residuals)


####################
#### Ordination ####
####################

#Summing subplot reproduction values per plot
sum.data <- systematics_reproduction_data %>% 
  group_by(Plot_ID, Species) %>% 
  mutate(Plot_reprod = sum(Reproduction_value_standard)) %>% 
  select(Plot_ID, Treat, Precipitation, Plot_reprod) %>% 
  unique()


#Pivot data from long to wide
pivot.ord.data <- sum.data %>% 
  pivot_wider(names_from = "Species", #which column the pivoted headers (species) are taken from
              values_from = "Plot_reprod", values_fill = list(Plot_reprod = 0)) #which column the pivoted values are taken from + specifies what each value should be filled in with when missing
               
pivot.ord.data_pre <- sum.data %>%
  mutate(Plot_reprod = case_when(Plot_reprod > 0 ~ 1,
                                 TRUE ~ 0)) %>%
  pivot_wider(names_from = "Species", #which column the pivoted headers (species) are taken from
              values_from = "Plot_reprod", values_fill = list(Plot_reprod = 0)) #which column the pivoted values are taken from + specifies what each value should be filled in with when missing



#Loading libraries
library(vegan)

#Making dataset with plotID and species as row- and column names (rather than numbers)
ord.data <- pivot.ord.data %>% 
  ungroup() %>% 
  column_to_rownames("Plot_ID") %>% 
  select(Bis_viv:Tri_eur)


ord.data_pre <- pivot.ord.data_pre %>% 
  ungroup() %>% 
  column_to_rownames("Plot_ID") %>% 
  select(Bis_viv:Tri_eur) 
  


ord.nmds <- metaMDS(ord.data, k=3) #running the NMDS using the vegan package, using all the defaults

ord.nmds_pre <- metaMDS(ord.data_pre)

ord.nmds #display the results

names(ord.nmds) #get the set names of data
str(ord.nmds) #shows structure of data


#variableScores <- ord.nmds$species
#sampleScores <- ord.nmds$points

plot(ord.nmds) #red + are species and black circles are plots

plot(ord.nmds, type= "t", display = c("species"))
plot(ord.nmds, type= "t", display = c("sites"))

plot(ord.nmds, type= "n")

points(ord.nmds, display=c("sites"), choices=c(1,2), pch=3, col="red")

text(ord.nmds, display=c("species"), choices=c(1,2), col="blue", cex=0.7)
text(ord.nmds, display=c("sites"), choices=c(1,2), col="blue", cex=0.7)


#### Plotting ordination with ggplot ####

#Extracting site scores
site.scores <- as.data.frame(scores(ord.nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
head(site.scores)  #look at the data
site.scores$Site_ID <- substr(site.scores$site, 1, 3)
site.scores$Treat <- substr(site.scores$site, 9, 10)
head(site.scores)
site.scores <- site.scores %>% 
  mutate(Site_ID = factor(Site_ID, levels = c("Ulv", "Lav", "Gud", "Skj"))) %>% 
  mutate(Precipitation = case_when(Site_ID == "Ulv" ~ "1226 mm/year",
                         Site_ID == "Lav" ~ "1561 mm/year",
                         Site_ID == "Gud" ~ "2130 mm/year",
                          TRUE ~ "3402 mm/year")) %>% 
  mutate(Precipitation = factor(Precipitation, levels = c("1226 mm/year", "1561 mm/year", "2130 mm/year", "3402 mm/year")))
str(site.scores)


#Extracting site scores
site.scores_pre <- as.data.frame(scores(ord.nmds_pre))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores_pre$site <- rownames(site.scores_pre)  # create a column of site names, from the rownames of data.scores
head(site.scores_pre)  #look at the data
site.scores_pre$Site_ID <- substr(site.scores_pre$site, 1, 3)
site.scores_pre$Treat <- substr(site.scores_pre$site, 9, 10)
head(site.scores_pre)
site.scores_pre <- site.scores_pre %>% 
  mutate(Site_ID = factor(Site_ID, levels = c("Ulv", "Lav", "Gud", "Skj"))) %>% 
  mutate(Precipitation = case_when(Site_ID == "Ulv" ~ "1226 mm/year",
                                   Site_ID == "Lav" ~ "1561 mm/year",
                                   Site_ID == "Gud" ~ "2130 mm/year",
                                   TRUE ~ "3402 mm/year")) %>% 
  mutate(Precipitation = factor(Precipitation, levels = c("1226 mm/year", "1561 mm/year", "2130 mm/year", "3402 mm/year")))
str(site.scores_pre)
#control shift c


functional.groups <- systematics_reproduction_data %>% 
  select(Species, Functional_group) %>% 
  rename(species = Species) %>% 
  unique()


#Extracting species scores
species.scores <- as.data.frame(scores(ord.nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) #look at the data

species.scores <- species.scores %>% 
  left_join(functional.groups, by = "species")

#Extracting species scores
species.scores_pre <- as.data.frame(scores(ord.nmds_pre, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores_pre$species <- rownames(species.scores_pre)  # create a column of species, from the rownames of species.scores
head(species.scores_pre) #look at the data

species.scores_pre <- species.scores_pre %>% 
  left_join(functional.groups, by = "species")



#Hull with precipitation
grp.Gud <- site.scores[site.scores$Precipitation == "2130 mm/year", ][chull(site.scores[site.scores$Precipitation == "2130 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Gud
grp.Lav <- site.scores[site.scores$Precipitation == "1561 mm/year", ][chull(site.scores[site.scores$Precipitation == "1561 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Lav
grp.Skj <- site.scores[site.scores$Precipitation == "3402 mm/year", ][chull(site.scores[site.scores$Precipitation == "3402 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Skj
grp.Ulv <- site.scores[site.scores$Precipitation == "1226 mm/year", ][chull(site.scores[site.scores$Precipitation == "1226 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Ulv


#Hull with sites
grp.Gud <- site.scores[site.scores$Site_ID == "Gud", ][chull(site.scores[site.scores$Site_ID == "Gud", c("NMDS1", "NMDS2")]), ]  # hull values for grp Gud
grp.Lav <- site.scores[site.scores$Site_ID == "Lav", ][chull(site.scores[site.scores$Site_ID == "Lav", c("NMDS1", "NMDS2")]), ]  # hull values for grp Lav
grp.Skj <- site.scores[site.scores$Site_ID == "Skj", ][chull(site.scores[site.scores$Site_ID == "Skj", c("NMDS1", "NMDS2")]), ]  # hull values for grp Skj
grp.Ulv <- site.scores[site.scores$Site_ID == "Ulv", ][chull(site.scores[site.scores$Site_ID == "Ulv", c("NMDS1", "NMDS2")]), ]  # hull values for grp Ulv

hull.data <- rbind(grp.Gud, grp.Lav, grp.Skj, grp.Ulv)  #combine grp.Gud, grp.Lav, grp.Skj and grp.Ulv
#hull.data <- hull.data %>% 
  #mutate(Site_ID = factor(Site_ID, levels = c("Ulv", "Lav", "Gud", "Skj")))
  #mutate(Precipitation = factor(Precipitation, levels = c("569", "1321", "1925", "2725")))
hull.data



#Hull with precipitation
grp.Gud_pre <- site.scores_pre[site.scores_pre$Precipitation == "2130 mm/year", ][chull(site.scores_pre[site.scores_pre$Precipitation == "2130 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Gud
grp.Lav_pre <- site.scores_pre[site.scores_pre$Precipitation == "1561 mm/year", ][chull(site.scores_pre[site.scores_pre$Precipitation == "1561 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Lav
grp.Skj_pre <- site.scores_pre[site.scores_pre$Precipitation == "3402 mm/year", ][chull(site.scores_pre[site.scores_pre$Precipitation == "3402 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Skj
grp.Ulv_pre <- site.scores_pre[site.scores_pre$Precipitation == "1226 mm/year", ][chull(site.scores_pre[site.scores_pre$Precipitation == "1226 mm/year", c("NMDS1", "NMDS2")]), ]  # hull values for grp Ulv


#Hull with sites
grp.Gud_pre <- site.scores_pre[site.scores_pre$Site_ID == "Gud", ][chull(site.scores_pre[site.scores_pre$Site_ID == "Gud", c("NMDS1", "NMDS2")]), ]  # hull values for grp Gud
grp.Lav_pre <- site.scores_pre[site.scores_pre$Site_ID == "Lav", ][chull(site.scores_pre[site.scores_pre$Site_ID == "Lav", c("NMDS1", "NMDS2")]), ]  # hull values for grp Lav
grp.Skj_pre <- site.scores_pre[site.scores_pre$Site_ID == "Skj", ][chull(site.scores_pre[site.scores_pre$Site_ID == "Skj", c("NMDS1", "NMDS2")]), ]  # hull values for grp Skj
grp.Ulv_pre <- site.scores_pre[site.scores_pre$Site_ID == "Ulv", ][chull(site.scores_pre[site.scores_pre$Site_ID == "Ulv", c("NMDS1", "NMDS2")]), ]  # hull values for grp Ulv


hull.data_pre <- rbind(grp.Gud_pre, grp.Lav_pre, grp.Skj_pre, grp.Ulv_pre)  #combine grp.Gud, grp.Lav, grp.Skj and grp.Ulv
#hull.data_pre <- hull.data_pre %>% 
  #mutate(Site_ID = factor(Site_ID, levels = c("Ulv", "Lav", "Gud", "Skj")))
  #mutate(Precipitation = factor(Precipitation, levels = c("1226", "1561", "2130", "3402")))
hull.data_pre

library(ggrepel)

p1 <- ggplot() + 
  geom_polygon(data=hull.data_pre,aes(x=NMDS1,y=NMDS2,fill=Precipitation,group=Precipitation),alpha=0.8) + # add the convex hulls
  #geom_point(data=site.scores_pre,aes(x=NMDS1,y=NMDS2,colour=Precipitation),size=2) + # add the point markers
  ggrepel::geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size=3, alpha=1.0) +  # add the species labels
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2),size=1.5, alpha = 0.7)+
  scale_fill_manual(values=c("1226 mm/year" = "#C7DFFB", "1561 mm/year" = "#94C6FF", "2130 mm/year" = "#4477B6", "3402 mm/year" = "#11407A")) +
  #scale_colour_manual(values=c("1226" = "#BAD8F7", "1561" = "#89B7E1", "2130" = "#2E75B6", "3402" = "#213964")) +
  labs(fill = "Precipitation levels", title = "Species")+
  coord_equal() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    axis.title.x = element_text(size=12), # remove x-axis labels
    axis.title.y = element_text(size=12), # remove y-axis labels
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank()) +
  xlim(-1.15, 1.6)+
  ylim(-1.15, 1.6)




p2 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Precipitation,group=Precipitation),alpha=0.8) + # add the convex hulls
  #geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,colour=Precipitation),size=2) + # add the point markers
  ggrepel::geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size=3.5, alpha=1.0) +  # add the species labels
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2),size=1.5, alpha = 0.7)+
  scale_fill_manual(values=c("1226 mm/year" = "#C7DFFB", "1561 mm/year" = "#94C6FF", "2130 mm/year" = "#4477B6", "3402 mm/year" = "#11407A")) +
  #scale_colour_manual(values=c("1226" = "#BAD8F7", "1561" = "#89B7E1", "2130" = "#2E75B6", "3402" = "#213964")) +
  labs(fill = "Precipitation levels", title = "Flowering community")+
  coord_equal() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
        #axis.text.y = element_blank(), # remove y-axis text
        #axis.ticks = element_blank(),  # remove axis ticks
        axis.title = element_text(size=14), # remove x-axis labels
        axis.text = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  xlim(-1.15, 1.6)+
  ylim(-1.15, 1.6)

ggsave(plot = p2, "ordination_community.precipitation.pdf", width = 30, height = 20, units = "cm")
ggsave(plot = p2, "ordination_community.precipitation.jpg", width = 30, height = 20, units = "cm")

library(patchwork)

ord_precip <- (p1 | p2) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

ggsave(plot = ord_precip, "ordination_precipiation.pdf", width = 34, height = 20, units = "cm")
ggsave(plot = ord_precip, "ordination_precipiation.jpg", width = 34, height = 20, units = "cm")


p3 <- ggplot() + 
  #geom_polygon(data=hull.data_pre,aes(x=NMDS1,y=NMDS2,fill=Precipitation,group=Precipitation),alpha=0.95) + # add the convex hulls
  #geom_point(data=site.scores_pre,aes(x=NMDS1,y=NMDS2,colour=Precipitation),size=2) + # add the point markers
  geom_text(data=species.scores_pre,aes(x=NMDS1,y=NMDS2,label=species, colour = Functional_group),size=3, alpha=1.0) +  # add the species labels
  #scale_fill_manual(values=c("1226" = "#C7DFFB", "1561" = "#94C6FF", "2130" = "#4477B6", "3402" = "#11407A")) +
  #scale_colour_manual(values=c("1226" = "#BAD8F7", "1561" = "#89B7E1", "2130" = "#2E75B6", "3402" = "#213964")) +
  #labs(fill = "Precipitation", colour = "Precipitation")+
  labs(title = "Species", colour = "Functional group")+
  coord_equal() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    axis.title.x = element_text(size=12), # remove x-axis labels
    axis.title.y = element_text(size=12), # remove y-axis labels
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank()) +
  xlim(-1.35, 1.35)+
  ylim(-1.35, 1.35)


p4 <- ggplot() + 
  #geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Precipitation,group=Precipitation),alpha=0.95) + # add the convex hulls
  #geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,colour=Precipitation),size=2) + # add the point markers
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species, colour = Functional_group),size=3, alpha=1.0) +  # add the species labels
  #scale_fill_manual(values=c("1226" = "#C7DFFB", "1561" = "#94C6FF", "2130" = "#4477B6", "3402" = "#11407A")) +
  #scale_colour_manual(values=c("1226" = "#BAD8F7", "1561" = "#89B7E1", "2130" = "#2E75B6", "3402" = "#213964")) +
  #labs(fill = "Precipitation", colour = "Precipitation")+
  labs(title = "Species + flower-numbers", colour = "Functional group")+
  coord_equal() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    axis.title.x = element_text(size=12), # remove x-axis labels
    axis.title.y = element_text(size=12), # remove y-axis labels
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())+
  xlim(-1.35, 1.35)+
  ylim(-1.35, 1.35)



(p3 | p4) +  
  plot_layout(guides = "collect") & theme(legend.position = "right")





#Plot for checking axis lengths
ggplot() + 
  #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=Treat,colour=Treat),size=3) + # add the point markers
  #geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=Site_ID),size=3,vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("Gud" = "red", "Lav" = "blue", "Skj" = "green", "Ulv" = "orange")) +
  coord_equal() +
  theme_bw()



#Filtering data per site
data.gud <- site.scores %>%
  filter(Site_ID == "Gud")

data.lav <- site.scores %>%
  filter(Site_ID == "Lav")

data.skj <- site.scores %>%
  filter(Site_ID == "Skj")

data.ulv <- site.scores %>%
  filter(Site_ID == "Ulv")


#Creating treatment groups for polygon for Gudmedalen
gud.grp.CC <- data.gud[data.gud$Treat == "CC", ][chull(data.gud[data.gud$Treat == "CC", c("NMDS1", "NMDS2")]), ]  # hull values for grp CC
gud.grp.CN <- data.gud[data.gud$Treat == "CN", ][chull(data.gud[data.gud$Treat == "CN", c("NMDS1", "NMDS2")]), ]  # hull values for grp CN
gud.grp.WC <- data.gud[data.gud$Treat == "WC", ][chull(data.gud[data.gud$Treat == "WC", c("NMDS1", "NMDS2")]), ]  # hull values for grp WC
gud.grp.WN <- data.gud[data.gud$Treat == "WN", ][chull(data.gud[data.gud$Treat == "WN", c("NMDS1", "NMDS2")]), ]  # hull values for grp WN

hull.data.gud <- rbind(gud.grp.CC, gud.grp.CN, gud.grp.WC, gud.grp.WN)  #combine grp.CC, grp.CN, grp.WC and grp.WN for Gudmedalen
hull.data.gud


#Creating treatment groups for polygon for Låvisdalen
lav.grp.CC <- data.lav[data.lav$Treat == "CC", ][chull(data.lav[data.lav$Treat == "CC", c("NMDS1", "NMDS2")]), ]  # hull values for grp CC
lav.grp.CN <- data.lav[data.lav$Treat == "CN", ][chull(data.lav[data.lav$Treat == "CN", c("NMDS1", "NMDS2")]), ]  # hull values for grp CN
lav.grp.WC <- data.lav[data.lav$Treat == "WC", ][chull(data.lav[data.lav$Treat == "WC", c("NMDS1", "NMDS2")]), ]  # hull values for grp WC
lav.grp.WN <- data.lav[data.lav$Treat == "WN", ][chull(data.lav[data.lav$Treat == "WN", c("NMDS1", "NMDS2")]), ]  # hull values for grp WN

hull.data.lav <- rbind(lav.grp.CC, lav.grp.CN, lav.grp.WC, lav.grp.WN)  #combine grp.CC, grp.CN, grp.WC and grp.WN for Låvisdalen
hull.data.lav


#Creating treatment groups for polygon for Skjellingahaugen
skj.grp.CC <- data.skj[data.skj$Treat == "CC", ][chull(data.skj[data.skj$Treat == "CC", c("NMDS1", "NMDS2")]), ]  # hull values for grp CC
skj.grp.CN <- data.skj[data.skj$Treat == "CN", ][chull(data.skj[data.skj$Treat == "CN", c("NMDS1", "NMDS2")]), ]  # hull values for grp CN
skj.grp.WC <- data.skj[data.skj$Treat == "WC", ][chull(data.skj[data.skj$Treat == "WC", c("NMDS1", "NMDS2")]), ]  # hull values for grp WC
skj.grp.WN <- data.skj[data.skj$Treat == "WN", ][chull(data.skj[data.skj$Treat == "WN", c("NMDS1", "NMDS2")]), ]  # hull values for grp WN

hull.data.skj <- rbind(skj.grp.CC, skj.grp.CN, skj.grp.WC, skj.grp.WN)  #combine grp.CC, grp.CN, grp.WC and grp.WN for Skjellingahaugen
hull.data.skj

#Creating treatment groups for polygon for Ulvehaugen
ulv.grp.CC <- data.ulv[data.ulv$Treat == "CC", ][chull(data.ulv[data.ulv$Treat == "CC", c("NMDS1", "NMDS2")]), ]  # hull values for grp CC
ulv.grp.CN <- data.ulv[data.ulv$Treat == "CN", ][chull(data.ulv[data.ulv$Treat == "CN", c("NMDS1", "NMDS2")]), ]  # hull values for grp CN
ulv.grp.WC <- data.ulv[data.ulv$Treat == "WC", ][chull(data.ulv[data.ulv$Treat == "WC", c("NMDS1", "NMDS2")]), ]  # hull values for grp WC
ulv.grp.WN <- data.ulv[data.ulv$Treat == "WN", ][chull(data.ulv[data.ulv$Treat == "WN", c("NMDS1", "NMDS2")]), ]  # hull values for grp WN

hull.data.ulv <- rbind(ulv.grp.CC, ulv.grp.CN, ulv.grp.WC, ulv.grp.WN)  #combine grp.CC, grp.CN, grp.WC and grp.WN for Skjellingahaugen
hull.data.ulv




gud.plot <- site.scores %>%
  filter(Site_ID == "Gud") %>%
  ggplot() +
  geom_polygon(data=hull.data.gud,aes(x=NMDS1,y=NMDS2,fill=Treat,group=Treat),alpha=0.4) + # add the convex hulls
  #geom_text(data = species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=3,alpha=0.8) + # add the species labels
  geom_point(aes(x=NMDS1,y=NMDS2,shape=Treat,colour=Treat),size=3) + # add the point markers
  #geom_text(aes(x=NMDS1,y=NMDS2,label=Site_ID),size=3,vjust=0) + # add the site labels
  scale_fill_manual(values=c("CC" = "#C3DFFF", "CN" = "#0077FF", "WC" = "#FFA989", "WN" = "#FF4500")) +
  scale_shape_manual(values = c("CC" = 21, "CN" = 16, "WC" = 21, "WN" = 16))+
  scale_colour_manual(values=c("CC" = "#89B7E1", "CN" = "#89B7E1", "WC" = "coral2", "WN" = "coral2")) +
  labs(shape = "Treatment:", colour = "Treatment:", fill = "Treatment:", title = "Gudmedalen")+
  coord_equal() +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-1.0, 1.0)+
  ylim(-0.75, 1.1)


lav.plot <- site.scores %>%
  filter(Site_ID == "Lav") %>%
  ggplot() +
  geom_polygon(data=hull.data.lav,aes(x=NMDS1,y=NMDS2,fill=Treat,group=Treat),alpha=0.4) + # add the convex hulls
  #geom_text(data = species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=3,alpha=0.8) + # add the species labels
  geom_point(aes(x=NMDS1,y=NMDS2,shape=Treat,colour=Treat),size=3) + # add the point markers
  #geom_text(aes(x=NMDS1,y=NMDS2,label=Site_ID),size=3,vjust=0) + # add the site labels
  scale_fill_manual(values=c("CC" = "#C3DFFF", "CN" = "#0077FF", "WC" = "#FFA989", "WN" = "#FF4500")) +
  scale_shape_manual(values = c("CC" = 21, "CN" = 16, "WC" = 21, "WN" = 16))+
  scale_colour_manual(values=c("CC" = "#89B7E1", "CN" = "#89B7E1", "WC" = "coral2", "WN" = "coral2")) +
  labs(shape = "Treatment:", colour = "Treatment:", fill = "Treatment:", title = "Låvisdalen")+
  coord_equal() +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-1.0, 1.0)+
  ylim(-0.75, 1.1)


skj.plot <- site.scores %>%
  filter(Site_ID == "Skj") %>%
  ggplot() +
  geom_polygon(data=hull.data.skj,aes(x=NMDS1,y=NMDS2,fill=Treat,group=Treat),alpha=0.4) + # add the convex hulls
  #geom_text(data = species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=3,alpha=0.8) + # add the species labels
  geom_point(aes(x=NMDS1,y=NMDS2,shape=Treat,colour=Treat),size=3) + # add the point markers
  #geom_text(aes(x=NMDS1,y=NMDS2,label=Site_ID),size=3,vjust=0) + # add the site labels
  scale_fill_manual(values=c("CC" = "#C3DFFF", "CN" = "#0077FF", "WC" = "#FFA989", "WN" = "#FF4500")) +
  scale_shape_manual(values = c("CC" = 21, "CN" = 16, "WC" = 21, "WN" = 16))+
  scale_colour_manual(values=c("CC" = "#89B7E1", "CN" = "#89B7E1", "WC" = "coral2", "WN" = "coral2")) +
  labs(shape = "Treatment:", colour = "Treatment:", fill = "Treatment:", title = "Skjellingahaugen")+
  coord_equal() +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-1.0, 1.0)+
  ylim(-0.75, 1.1)


ulv.plot <- site.scores %>%
  filter(Site_ID == "Ulv") %>%
  ggplot() +
  geom_polygon(data=hull.data.ulv,aes(x=NMDS1,y=NMDS2,fill=Treat,group=Treat),alpha=0.4) + # add the convex hulls
  #geom_text(data = species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=3,alpha=0.8) + # add the species labels
  geom_point(aes(x=NMDS1,y=NMDS2,shape=Treat,colour=Treat),size=3) + # add the point markers
  #geom_text(aes(x=NMDS1,y=NMDS2,label=Site_ID),size=3,vjust=0) + # add the site labels
  scale_fill_manual(values=c("CC" = "#C3DFFF", "CN" = "#0077FF", "WC" = "#FFA989", "WN" = "#FF4500")) +
  scale_shape_manual(values = c("CC" = 21, "CN" = 16, "WC" = 21, "WN" = 16))+
  scale_colour_manual(values=c("CC" = "#89B7E1", "CN" = "#89B7E1", "WC" = "coral2", "WN" = "coral2")) +
  labs(shape = "Treatment:", colour = "Treatment:", fill = "Treatment:", title = "Ulvehaugen")+
  coord_equal() +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-1.0, 1.0)+
  ylim(-0.75, 1.1)



ord_treat <- (ulv.plot | lav.plot | gud.plot | skj.plot) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(plot = ord_treat, "ordination_treatment.pdf", width = 34, height = 22, units = "cm")
ggsave(plot = ord_treat, "ordination_treatment.jpg", width = 34, height = 22, units = "cm")



#Combining precipitation plot with treatment plot

p2 & theme(legend.position = "right")

ord_combi <- (p2/ord_treat) + 
  plot_layout(heights = c(3, 1), guides = "collect") +
  plot_annotation(tag_levels = "a")

ggsave(plot = ord_combi, "ordination_plot_susanne.pdf", width = 34, height = 22, units = "cm")
