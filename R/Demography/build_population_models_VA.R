########################################################################
### Script for building population models for the INCLINE experiment ###
########################################################################

#### Source files ####
source("R/Demography/cleaning_demogprahy.R")
source("R/Demography/ready_demograhy_for_IPM.R")
seed_bank1 <- read_csv2("data/Demography/Seed_bank_survival.csv") 

#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(IPMpack)
library(fields)
library(conflicted)
library(patchwork)
library(arm)

#### Select preferences for conflicts ####

conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

#### Functions ####
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")
Precip_palette_black <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964", "#000000")

plot_predictions_surv <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = surv, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      scale_color_manual(values = Precip_palette_black)+
      theme_minimal()
      
   
   
   return(plot)
}

plot_predictions_surv_precip <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          precip = c(1.226, 1.561, 2.130, 3.402),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = surv, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      scale_color_manual(values = Precip_palette) +
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_growth_precip <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          precip = c(1.226, 1.561, 2.130, 3.402),
                          transition = c("2018-2019", "2019-2020", "2020-2021"),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = sizeNext, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      geom_abline() +
      ylim(minSize, maxSize) +
      scale_color_manual(values = Precip_palette) +
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_growth <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = sizeNext, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      geom_abline() +
      ylim(minSize, maxSize) +
      scale_color_manual(values = Precip_palette_black) +
      theme_minimal()
   
   
   return(plot)
}


plot_predictions_floif_precip <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          precip = c(1.226, 1.561, 2.130, 3.402),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = flo.if, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      scale_color_manual(values = Precip_palette)+
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_floif <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = flo.if, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      scale_color_manual(values = Precip_palette_black)+
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_flono_precip <-function(model, data, minSize, maxSize, ylim) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          precip = c(1.226, 1.561, 2.130, 3.402),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = flo.no, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      ylim(0, ylim) +
      scale_color_manual(values = Precip_palette)+
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_flono <-function(model, data, minSize, maxSize, ylim) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = flo.no, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      ylim(0, ylim) +
      scale_color_manual(values = Precip_palette_black) +
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_cloif_precip <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          precip = c(1.226, 1.561, 2.130, 3.402),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = clo.if, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      scale_color_manual(values = Precip_palette) +
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_cloif <-function(model, data, minSize, maxSize) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = clo.if, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      scale_color_manual(values = Precip_palette_black) +
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_clono_precip <-function(model, data, minSize, maxSize, ylim) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          precip = c(1.226, 1.561, 2.130, 3.402),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = clo.no, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      ylim(0, ylim) +
      scale_color_manual(values = Precip_palette) +
      theme_minimal()
   
   
   return(plot)
}

plot_predictions_clono <-function(model, data, minSize, maxSize, ylim) {
   
   newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                          blockID = data$blockID)
   
   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
   
   plot <- data %>% 
      ggplot(aes(x = size, y = clo.no, color = as.factor(precip))) +
      geom_jitter(height = 0.1) +
      geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
      ggtitle(paste0("AIC =", AIC(model))) +
      ylim(0, ylim) +
      scale_color_manual(values = Precip_palette_black) +
      theme_minimal()
   
   
   return(plot)
}

contourPlot2 <- function(M,meshpts,maxSize,upper,lower, title) {
   q <- sum(meshpts<=maxSize);
   plot <- filled.contour(meshpts[1:q],meshpts[1:q],M[1:q,1:q], zlim=c(upper,lower),
                  xlab="size at time t", ylab="size at time t+1", main = title, color=heat.colors, nlevels=20, cex.lab=1.5,
                  plot.axes = { axis(1); axis(2); lines(-10:50, -10:50, lty=2)});
   return(0);
}

IPM_plot <- function(IPM_control, IPM_treatment = NULL, vital = FALSE, vital_rate_matrix_treatment, vital_rate_matrix_control, minSize, maxSize, zrange) {
   
   if(isTRUE(vital)) {
      BaseIPM <- (IPM_control + IPM_treatment) / 2
      SBaseIPM <- sens(BaseIPM)
      matrix <- vital_rate_matrix_control - vital_rate_matrix_treatment
   } else {
      matrix <- IPM_control
   }
   
long_data <- as.data.frame(matrix) %>% 
   set_names(seq(minSize, maxSize, length = ncol(matrix))) %>% 
   mutate(size = seq(minSize, maxSize, length = nrow(matrix))) %>% 
   pivot_longer(cols = -size, names_to = "sizeNext") %>% 
   mutate(sizeNext = as.numeric(sizeNext))

plot <- ggplot(long_data, aes(x = sizeNext, y = size)) + 
   geom_raster(aes(fill=value)) + 
   scale_fill_viridis_c(limits = zrange, na.value = "white") +
   labs(x="Size at  time t", y="Size at time t+1") +
   theme_bw() + 
   # theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                      # axis.text.y=element_text(size=9)) +
   scale_x_continuous(expand = c(0,0)) +
   scale_y_continuous(expand = c(0,0)) +
   geom_abline()

return(plot);
}

# IPM_plot(IPM_control = IPM_VA_CC_precip2, minSize = minSize, maxSize = maxSize, zrange = c(0, 0.03)) + ggtitle("VA CC precip 2.3 m/year")

#### Downloading data from OSF ####

#Make script for reading in ready made data from OSF

#### Load data ####

#Load data 

INCLINE_metadata <- read_delim("data/INCLINE_metadata.csv", delim = ";", locale = locale(decimal_mark = ","))

INCLINE_metadata <- INCLINE_metadata %>% 
   select(plotID, "precipitation_2009-2019") %>% 
   rename(precip = "precipitation_2009-2019") %>% 
   mutate(precip = precip/1000)

# We define a new size axis for the midpoint evaluation of the IPMs:
minSize<-min(Ver_alp_2018_2021$size, na.rm=T)-1
maxSize<-max(Ver_alp_2018_2021$sizeNext, na.rm=T)+2
x<-seq(from=round(minSize),to=round(maxSize),length=100)
x0<-data.frame(size=x,size2=x*x)


seed_bank1 <- seed_bank1 %>% 
   rename(missing = "missing/dissentegrated") %>% 
   mutate(seeds_dead_in_soil_bank = case_when(missing == "Yes" ~ 1,
                                              missing == "No" ~ 0),
          seeds_germinate = case_when(germinated == "Yes" ~ 1,
                                      TRUE ~ 0),
          seeds_alive_not_germ = case_when(embryo_in_seed == "Yes" ~ 1,
                                           TRUE ~ 0),
          seeds_dead_later = case_when(dead == "Yes" ~1,
                                       TRUE ~ 0)) %>% 
   group_by(petridish, species) %>% 
   mutate(seeds_dead_in_soil_bank = sum(seeds_dead_in_soil_bank),
          seeds_germinate = sum(seeds_germinate),
          seeds_alive_not_germ = sum(seeds_alive_not_germ),
          seeds_dead_later = sum(seeds_dead_later),
          seeds_total = max(seed_number)) %>%
   select(petridish, plotID, siteID, warming, species, seeds_dead_in_soil_bank, seeds_germinate, seeds_alive_not_germ, seeds_total, seeds_dead_later) %>%
   unique() %>% 
   mutate(seeds_alive_total = seeds_germinate + seeds_alive_not_germ,
          seeds_dead_total = seeds_dead_in_soil_bank + seeds_dead_later,
          seeds_alive_total_prop = seeds_alive_total/seeds_total,
          seeds_dead_total_prop = seeds_dead_total/seeds_total,
          seeds_germinate_prop = seeds_germinate/seeds_total,
          seeds_staySB = seeds_alive_not_germ/seeds_total) %>% 
   ungroup() %>% 
   group_by(species, warming) %>% 
   mutate(seeds_alive_total = round(mean(seeds_alive_total), digits = 0),
          seeds_alive_total_prop = mean(seeds_alive_total_prop),
          seeds_dead_total = round(mean(seeds_dead_total), digits = 0),
          seeds_dead_total_prop = mean(seeds_dead_total_prop),
          seeds_germinate_prop = mean(seeds_germinate_prop),
          seeds_staySB = mean(seeds_staySB)) %>% 
   select(species, warming, seeds_alive_total, seeds_alive_total_prop, seeds_dead_total, seeds_dead_total_prop, seeds_germinate_prop, seeds_staySB) %>% 
   unique() 

Ver_alp_2018_2021 <- Ver_alp_2018_2021 %>% 
   mutate(stage = as.factor(stage),
          stageNext = as.factor(stageNext)) %>% 
   mutate(number = 1)
   
Ver_alp_2018_2021 <- Ver_alp_2018_2021 %>% 
   left_join(INCLINE_metadata, by = c("plotID")) %>% 
   mutate(treat = paste0(OTC, treatment),
          site_trans = paste0(siteID, transition),
          block_trans = paste0(blockID, transition))

VA_CC <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "C")
VA_CR <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "R")
VA_CE <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "E")
VA_CN <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "N")
VA_WC <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "C")
VA_WR <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "R")
VA_WE <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "E")
VA_WN <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "N")

VA_C_seed_bank <- seed_bank1 %>% 
   filter(species == "Ver_alp",
          warming == "C") %>% 
   ungroup()

VA_OTC_seed_bank <- seed_bank1 %>% 
   filter(species == "Ver_alp",
          warming == "OTC") %>% 
   ungroup() 
   
   

##### Ambient temperature control #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))
# survModelComp(dataf= VA_CC, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CC)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CC))

mod_surv_VA_CC <- glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CC)
plot_surv_VA_CC <- plot_predictions_surv(model = mod_surv_VA_CC, data = VA_CC, minSize, maxSize)

plot_surv_VA_CC

so_VA_CC <- makeSurvObj(VA_CC, "surv ~ size + size2")
so_VA_CC <- coerceSurvObj(so_VA_CC, as.numeric(fixef(mod_surv_VA_CC))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the bext growth model
#growthModelComp(dataf=VA_CC, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CC))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CC))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_CC)) #We chose this model based on AIC
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_CC))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CC))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CC))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CC))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CC))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CC))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CC))


mod_growth_VA_CC <- lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_CC)

plot_growth_VA_CC <- plot_predictions_growth_precip(model = mod_growth_VA_CC, data = VA_CC, minSize, maxSize)

plot_surv_VA_CC | plot_growth_VA_CC


go_VA_CC <- makeGrowthObj(VA_CC, "sizeNext ~ size")

#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_CC_precip1 <- coerceGrowthObj(go_VA_CC, c(as.numeric(fixef(mod_growth_VA_CC)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_CC)[3]) + (1.2)^2* as.numeric(fixef(mod_growth_VA_CC)[4]),
                                              as.numeric(fixef(mod_growth_VA_CC)[2])),
                                  sigma.hat(mod_growth_VA_CC)$sigma$data)
go_VA_CC_precip2 <- coerceGrowthObj(go_VA_CC, c(as.numeric(fixef(mod_growth_VA_CC)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_CC)[3]) + (2.3)^2* as.numeric(fixef(mod_growth_VA_CC)[4]),
                                              as.numeric(fixef(mod_growth_VA_CC)[2])),
                                  sigma.hat(mod_growth_VA_CC)$sigma$data)
go_VA_CC_precip3 <- coerceGrowthObj(go_VA_CC, c(as.numeric(fixef(mod_growth_VA_CC)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_CC)[3]) + (3.4)^2* as.numeric(fixef(mod_growth_VA_CC)[4]),
                                              as.numeric(fixef(mod_growth_VA_CC)[2])),
                                  sigma.hat(mod_growth_VA_CC)$sigma$data)



# Make discrete transition object
dto_VA_CC <- makeDiscreteTrans(VA_CC, discreteTrans = matrix(
   c(VA_C_seed_bank$seeds_staySB,
     (1-VA_C_seed_bank$seeds_staySB)*seedling_est_VA_C_Veg,
     (1-VA_C_seed_bank$seeds_staySB)*(1-seedling_est_VA_C_Veg), 
     0,
     sum(VA_CC$number[VA_CC$stage=="continuous"&VA_CC$stageNext=="continuous"], na.rm=T),
     sum(VA_CC$number[VA_CC$stage=="continuous"&VA_CC$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
                                meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
                                sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_VA_CC_precip1 <- makeIPMPmatrix(survObj=so_VA_CC, growObj=go_VA_CC_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CC_precip1, survObj=so_VA_CC, growObj=go_VA_CC_precip1, dff = (VA_CC  %>% filter(siteID == "Ulv")))

Pmatrix_VA_CC_precip1 <- makeIPMPmatrix(survObj=so_VA_CC, growObj=go_VA_CC_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)
contourPlot2(t(Pmatrix_VA_CC_precip1), Pmatrix_VA_CC_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_VA_CC_precip2 <- makeIPMPmatrix(survObj=so_VA_CC, growObj=go_VA_CC_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CC_precip2, survObj=so_VA_CC, growObj=go_VA_CC_precip2, dff = VA_CC)

Pmatrix_VA_CC_precip2 <- makeIPMPmatrix(survObj=so_VA_CC, growObj=go_VA_CC_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)
contourPlot2(t(Pmatrix_VA_CC_precip2), Pmatrix_VA_CC_precip2@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_VA_CC_precip3 <- makeIPMPmatrix(survObj=so_VA_CC, growObj=go_VA_CC_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CC_precip3, survObj=so_VA_CC, growObj=go_VA_CC_precip3, dff = (VA_CC %>% filter(siteID == "Skj")))

Pmatrix_VA_CC_precip3 <- makeIPMPmatrix(survObj=so_VA_CC, growObj=go_VA_CC_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)
contourPlot2(t(Pmatrix_VA_CC_precip3), Pmatrix_VA_CC_precip3@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC)) #Choosing this model
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CC))

floweringChosenModel_VA_CC <- flo.if ~ size #Making a mock model, will fill in from actual model later

mod_flo_if_VA_CC <- glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC) 

par(mfrow=c(1,1))

plot_VA_CC_floif <- plot_predictions_floif_precip(model = mod_flo_if_VA_CC, data = VA_CC, minSize, maxSize)

plot_VA_CC_floif 



# Choosing the best model for estimating the number of flowers, if an individual flowers

VA_CC <- VA_CC %>% 
   filter(!unique_IDS == "Ulv_6_2_4")

summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = VA_CC))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = VA_CC))
summary(glmer(flo.no ~ size + precip +I(precip^2) + (1|block_trans), family = 'poisson', data = VA_CC)) 
AIC(glmer(flo.no ~ size + precip+I(size^2) + (1|block_trans), family = 'poisson', data = VA_CC))
summary(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = VA_CC))
AIC(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = VA_CC))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = VA_CC)) #Choosing this model
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = VA_CC)) 
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = VA_CC))
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = VA_CC))

#AIC(glmer(flo.no~size+I(size^2)+I(size^3) + (1|BlockID), family = 'poisson', data = VA_CC))

flowerNumberChosenModel_VA_CC <- flo.no ~ size  #Chosen based on biology by looking at the data

mod_flo_no_VA_CC <- glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = VA_CC)

plot_flo_no_VA_CC <-plot_predictions_flono(model = mod_flo_no_VA_CC, data = VA_CC, minSize, maxSize, ylim = 15) 

plot_flo_no_VA_CC

# Make fecundity object
fo_VA_CC <-makeFecObj(VA_CC, 
                      Formula= c(floweringChosenModel_VA_CC, flowerNumberChosenModel_VA_CC),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_Veg), continuous=(1-(VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_VA_CC@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_CC)[1]) + 1.2*as.numeric(fixef(mod_flo_if_VA_CC)[3]) + (1.2)^2* as.numeric(fixef(mod_flo_if_VA_CC)[4]),
                                       as.numeric(fixef(mod_flo_if_VA_CC)[2]))
fo_VA_CC@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_VA_CC))
fo_VA_CC_precip1 <- fo_VA_CC

fo_VA_CC@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_CC)[1]) + 2.3*as.numeric(fixef(mod_flo_if_VA_CC)[3]) + (2.3)^2* as.numeric(fixef(mod_flo_if_VA_CC)[4]),
                                       as.numeric(fixef(mod_flo_if_VA_CC)[2]))
fo_VA_CC@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_VA_CC))
fo_VA_CC_precip2 <- fo_VA_CC

fo_VA_CC@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_CC)[1]) + 3.4*as.numeric(fixef(mod_flo_if_VA_CC)[3]) + (3.4)^2* as.numeric(fixef(mod_flo_if_VA_CC)[4]),
                                       as.numeric(fixef(mod_flo_if_VA_CC)[2]))
fo_VA_CC@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_VA_CC))
fo_VA_CC_precip3 <- fo_VA_CC

Fmatrix_VA_CC_precip1 <- makeIPMFmatrix(fecObj=fo_VA_CC_precip1, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_CC_precip2 <- makeIPMFmatrix(fecObj=fo_VA_CC_precip2, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_CC_precip3 <- makeIPMFmatrix(fecObj=fo_VA_CC_precip3, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:


contourPlot2(t(Fmatrix_VA_CC_precip1), Fmatrix_VA_CC_precip1@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

contourPlot2(t(Fmatrix_VA_CC_precip2), Fmatrix_VA_CC_precip2@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings")

contourPlot2(t(Fmatrix_VA_CC_precip3), Fmatrix_VA_CC_precip3@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_CC@meshpoints,
#            Fmatrix_VA_CC@meshpoints,
#            t(Fmatrix_VA_CC),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_CC))

#### C matrix ####

VA_CC_clones <- VA_CC %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CC)) #Choosing thins model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CC))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CC))
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CC))


mod_clo_VA_CC <- glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CC)
CloneChosenModel_VA_CC <- clo.if ~ size + size2 


plot_clo_if_VA_CC <- plot_predictions_cloif(model = mod_clo_VA_CC, data = VA_CC, minSize, maxSize)
plot_clo_if_VA_CC

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear mixed effects models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CC))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CC))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_CC))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_CC))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = VA_CC))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = VA_CC))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_CC))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_CC))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_CC))
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_CC))

mod_clo_no_VA_CC <- glm(clo.no ~ 1, family = 'poisson', data = VA_CC)
CloneNumberChosenModel_VA_CC <- clo.no ~ 1


plot_clo_no_VA_CC <- plot_predictions_clono(model = mod_clo_no_VA_CC, data = VA_CC, minSize, maxSize, ylim = 6)
plot_clo_no_VA_CC

# Clonal size depending on mother size
# x11()
# par(mfrow=c(1,1))
# growthModelComp(dataf=VA_CC_clones, makePlot=TRUE, legendPos="topright", mainTitle="Growth")
# CloneSizeVariable_VA_CC <- "1"  #Chosen based on AIC
# 
# go_clone_VA_CC <- makeGrowthObj(VA_CC_clones, sizeNext ~ 1)


# summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CC_clones))
# AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CC_clones))
# summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_CC_clones)) #This is the best model AIC based
# AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_CC_clones))
# summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CC_clones))
# AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CC_clones))
# summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CC_clones))
# AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CC_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CC_clones)) #Using this model because it doesn't make biological sense that the clones would become smaller as the parents increase in size
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CC_clones))


mod_clone_growth_VA_CC <- lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CC_clones)
CloneSizeVariable_VA_CC <- "1"

plot_clone_growth_VA_CC <- plot_predictions_growth(model = mod_clone_growth_VA_CC, data = VA_CC_clones, minSize, maxSize)
plot_clone_growth_VA_CC

#Make clonal object
co_VA_CC <- makeClonalObj(VA_CC, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_CC_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_CC, Formula = c(CloneChosenModel_VA_CC, CloneNumberChosenModel_VA_CC),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_VA_CC@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_VA_CC))
co_VA_CC@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_CC)) #not really needed since this is a linear model
co_VA_CC@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_VA_CC))
co_VA_CC@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_CC)$sigma$data
co_VA_CC <- co_VA_CC


Cmatrix_VA_CC <- makeIPMCmatrix(clonalObj = co_VA_CC, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_CC), Cmatrix_VA_CC@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_CC_precip1 <- Pmatrix_VA_CC_precip1 + Fmatrix_VA_CC_precip1 + Cmatrix_VA_CC
#contourPlot2(t( M = IPM_VA_CC_precip1, meshpts = Pmatrix_VA_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_VA_CC_precip1, minSize = minSize, maxSize = maxSize, zrange = c(-0.03, 0.04)) + ggtitle("Veronica alpina CC precip 1.2 m/year")
persp(IPM_VA_CC_precip1)
as.numeric(eigen(IPM_VA_CC_precip1)$value[1])


IPM_VA_CC_precip2 <- Pmatrix_VA_CC_precip2 + Fmatrix_VA_CC_precip2 + Cmatrix_VA_CC
image(t(IPM_VA_CC_precip2))
IPM_plot(IPM_control = IPM_VA_CC_precip2, minSize = minSize, maxSize = maxSize, zrange = c(-0.03, 0.04)) + ggtitle("Veronica alpina CC precip 2.3 m/year")
persp(IPM_VA_CC_precip2)
as.numeric(eigen(IPM_VA_CC_precip2)$value[1])


IPM_VA_CC_precip3 <- Pmatrix_VA_CC_precip3 + Fmatrix_VA_CC_precip3 + Cmatrix_VA_CC
image(t(IPM_VA_CC_precip3))
IPM_plot(IPM_control = IPM_VA_CC_precip3, minSize = minSize, maxSize = maxSize, zrange = c(-0.03, 0.04)) + ggtitle("Veronica alpina CC precip 3.4 m/year")
persp(IPM_VA_CC_precip3)
as.numeric(eigen(IPM_VA_CC_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_CC_precip1), Pmatrix_VA_CC_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina CC precip 1.2 m/year")
contourPlot2(t(IPM_VA_CC_precip2), Pmatrix_VA_CC_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina CC precip 2.3 m/year")
contourPlot2(t(IPM_VA_CC_precip3), Pmatrix_VA_CC_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina CC precip 3.4 m/year")


##### Ambient temperature removal #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))
# survModelComp(dataf= VA_CC, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR)) #We chose this model 
AIC(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CR))

# summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CR))
# AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CR))
# summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR))
# AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR))


mod_surv_VA_CR <- glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR)
plot_surv_VA_CR <- plot_predictions_surv_precip(model = mod_surv_VA_CR, data = VA_CR, minSize, maxSize)

x11()
plot_surv_VA_CC | plot_surv_VA_CR

so_VA_CR <- makeSurvObj(VA_CR, "surv ~ size")
so_VA_CR_precip1 <- coerceSurvObj(so_VA_CR, c(as.numeric(fixef(mod_surv_VA_CR)[1]) + 1.2*as.numeric(fixef(mod_surv_VA_CR)[3]) + (1.2)^2* as.numeric(fixef(mod_surv_VA_CR)[4]),
                                              as.numeric(fixef(mod_surv_VA_CR)[2]))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj
so_VA_CR_precip2 <- coerceSurvObj(so_VA_CR, c(as.numeric(fixef(mod_surv_VA_CR)[1]) + 2.3*as.numeric(fixef(mod_surv_VA_CR)[3]) + (2.3)^2* as.numeric(fixef(mod_surv_VA_CR)[4]),
                                              as.numeric(fixef(mod_surv_VA_CR)[2])))
so_VA_CR_precip3 <- coerceSurvObj(so_VA_CR, c(as.numeric(fixef(mod_surv_VA_CR)[1]) + 3.4*as.numeric(fixef(mod_surv_VA_CR)[3]) + (3.4)^2* as.numeric(fixef(mod_surv_VA_CR)[4]),
                                              as.numeric(fixef(mod_surv_VA_CR)[2])))


# choosing the best growth model
#growthModelComp(dataf=VA_CC, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CR)) #We chose this model based on AIC
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CR))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CR)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CR))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = VA_CR))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = VA_CR))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CR))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CR))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CR))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CR))


mod_growth_VA_CR <- lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CR)

plot_growth_VA_CR <- plot_predictions_growth_precip(model = mod_growth_VA_CR, data = VA_CR, minSize, maxSize)

plot_surv_VA_CR | plot_growth_VA_CR


go_VA_CR <- makeGrowthObj(VA_CR, "sizeNext ~ size + size2")

#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_CR_precip1 <- coerceGrowthObj(go_VA_CR, c(as.numeric(fixef(mod_growth_VA_CR)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_CR)[4]) + (1.2)^2* as.numeric(fixef(mod_growth_VA_CR)[5]),
                                              as.numeric(fixef(mod_growth_VA_CR)[2]),
                                              as.numeric(fixef(mod_growth_VA_CR)[3])),
                                  sigma.hat(mod_growth_VA_CR)$sigma$data)

go_VA_CR_precip2 <- coerceGrowthObj(go_VA_CR, c(as.numeric(fixef(mod_growth_VA_CR)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_CR)[4]) + (2.3)^2* as.numeric(fixef(mod_growth_VA_CR)[5]),
                                              as.numeric(fixef(mod_growth_VA_CR)[2]),
                                              as.numeric(fixef(mod_growth_VA_CR)[3])),
                                  sigma.hat(mod_growth_VA_CR)$sigma$data)
go_VA_CR_precip3 <- coerceGrowthObj(go_VA_CR, c(as.numeric(fixef(mod_growth_VA_CR)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_CR)[4]) + (3.4)^2* as.numeric(fixef(mod_growth_VA_CR)[5]),
                                              as.numeric(fixef(mod_growth_VA_CR)[2]),
                                              as.numeric(fixef(mod_growth_VA_CR)[3])),
                                  sigma.hat(mod_growth_VA_CR)$sigma$data)



# Make discrete transition object
dto_VA_CR <- makeDiscreteTrans(VA_CR, discreteTrans = matrix(
   c(VA_C_seed_bank$seeds_staySB,
     (1-VA_C_seed_bank$seeds_staySB)*seedling_est_VA_C_NoVeg,
     (1-VA_C_seed_bank$seeds_staySB)*(1-seedling_est_VA_C_NoVeg), 
     0,
     sum(VA_CR$number[VA_CR$stage=="continuous"&VA_CR$stageNext=="continuous"], na.rm=T),
     sum(VA_CR$number[VA_CR$stage=="continuous"&VA_CR$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_VA_CR_precip1 <- makeIPMPmatrix(survObj=so_VA_CR_precip1, growObj=go_VA_CR_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CR_precip1, survObj=so_VA_CR_precip1, growObj=go_VA_CR_precip1, dff = VA_CR)

Pmatrix_VA_CR_precip1 <- makeIPMPmatrix(survObj=so_VA_CR_precip1, growObj=go_VA_CR_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CR, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_VA_CR_precip1), Pmatrix_VA_CR_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_VA_CR_precip2 <- makeIPMPmatrix(survObj=so_VA_CR_precip2, growObj=go_VA_CR_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CR_precip2, survObj=so_VA_CR_precip2, growObj=go_VA_CR_precip2, dff = VA_CR)

Pmatrix_VA_CR_precip2 <- makeIPMPmatrix(survObj=so_VA_CR_precip2, growObj=go_VA_CR_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CR, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_VA_CR_precip2), Pmatrix_VA_CR_precip2@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_VA_CR_precip3 <- makeIPMPmatrix(survObj=so_VA_CR_precip3, growObj=go_VA_CR_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CR_precip3, survObj=so_VA_CR_precip3, growObj=go_VA_CR_precip3, dff = VA_CR)

Pmatrix_VA_CR_precip3 <- makeIPMPmatrix(survObj=so_VA_CR_precip3, growObj=go_VA_CR_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CR, correction = "constant", nBigMatrix = 100)
contourPlot2(t(Pmatrix_VA_CR_precip3), Pmatrix_VA_CR_precip3@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 

#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR)) #Choosing this model based on AIC
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CR))

floweringChosenModel_VA_CR <- flo.if ~ size + size2 #Making a mock model, will fill in from actual model later

mod_flo_if_VA_CR <- glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR) 

plot_VA_CR_floif <- plot_predictions_floif(model = mod_flo_if_VA_CR, data = VA_CR, minSize, maxSize)

plot_VA_CR_floif 



# Choosing the best model for estimating the number of flowers, if an individual flowers
#Using a different random effect set up here because of singularity fit. I have tried block_trans, site_trans, blockID + transition before this. 

summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
AIC(glmer(flo.no ~ size+I(size^2) + precip + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
summary(glmer(flo.no ~ size+I(size^2) + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
AIC(glmer(flo.no ~ size+I(size^2) + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
summary(glmer(flo.no ~ size + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR)) #Choosing this model based on AIC
AIC(glmer(flo.no ~ size + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
summary(glmer(flo.no ~  1 + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))
AIC(glmer(flo.no ~ 1 + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR))

flowerNumberChosenModel_VA_CR <- flo.no ~ size  

mod_flo_no_VA_CR <- glmer(flo.no ~ size + (1|siteID) + (1|transition), family = 'poisson', data = VA_CR) 

plot_flo_no_VA_CR <-plot_predictions_flono(model = mod_flo_no_VA_CR, data = VA_CR, minSize, maxSize, ylim = 15) 

plot_flo_no_VA_CR

# Make fecundity object
fo_VA_CR <-makeFecObj(VA_CR, 
                      Formula= c(floweringChosenModel_VA_CR, flowerNumberChosenModel_VA_CR),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_NoVeg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_NoVeg), continuous=(1-(VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_NoVeg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_VA_CR@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_VA_CR))
fo_VA_CR@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_VA_CR))
fo_VA_CR <- fo_VA_CR


Fmatrix_VA_CR <- makeIPMFmatrix(fecObj=fo_VA_CR, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

contourPlot2(t(Fmatrix_VA_CR), Fmatrix_VA_CR@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_CR_clones <- VA_CR %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR)) #Choosing this model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CR))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CR))
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CR))


mod_clo_VA_CR <- glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CR)
CloneChosenModel_VA_CR <- clo.if ~ size + size2 


plot_clo_if_VA_CR <- plot_predictions_cloif(model = mod_clo_VA_CR, data = VA_CR, minSize, maxSize)
plot_clo_if_VA_CR

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CR))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CR))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_CR))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_CR))
summary(glm(clo.no ~ size+I(size^2), family = 'poisson', data = VA_CR))
AIC(glm(clo.no ~ size+I(size^2), family = 'poisson', data = VA_CR))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_CR))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_CR))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_CR)) #choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_CR))

mod_clo_no_VA_CR <- glm(clo.no ~ 1, family = 'poisson', data = VA_CR)
CloneNumberChosenModel_VA_CR <- clo.no ~ 1


plot_clo_no_VA_CR <- plot_predictions_clono(model = mod_clo_no_VA_CR, data = VA_CR, minSize, maxSize, ylim = 6)
plot_clo_no_VA_CR

# Clonal size depending on mother size
# x11()
# par(mfrow=c(1,1))
# growthModelComp(dataf=VA_CC_clones, makePlot=TRUE, legendPos="topright", mainTitle="Growth")
# CloneSizeVariable_VA_CC <- "1"  #Chosen based on AIC
# 
# go_clone_VA_CC <- makeGrowthObj(VA_CC_clones, sizeNext ~ 1)


summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CR_clones))

summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CR_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CR_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = VA_CR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = VA_CR_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CR_clones)) #choosing this model based on AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CR_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CR_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CR_clones))


mod_clone_growth_VA_CR <- lmer(sizeNext ~ size + (1|block_trans), data = VA_CR_clones)
CloneSizeVariable_VA_CR <- "size"

plot_clone_growth_VA_CR <- plot_predictions_growth(model = mod_clone_growth_VA_CR, data = VA_CR_clones, minSize, maxSize)
plot_clone_growth_VA_CR


#Make clonal object
co_VA_CR <- makeClonalObj(VA_CR, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_CR_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_CR, Formula = c(CloneChosenModel_VA_CR, CloneNumberChosenModel_VA_CR),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_CR@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_VA_CR))
co_VA_CR@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_CR)) #No need to change this since we used a linear model
co_VA_CR@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_VA_CR))
co_VA_CR@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_CR)$sigma$data
co_VA_CR <- co_VA_CR


Cmatrix_VA_CR <- makeIPMCmatrix(clonalObj = co_VA_CR, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_CR), Cmatrix_VA_CR@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_CR_precip1 <- Pmatrix_VA_CR_precip1 + Fmatrix_VA_CR + Cmatrix_VA_CR
contourPlot2(t(IPM_VA_CR_precip1, Pmatrix_VA_CR_precip1@meshpoints, maxSize, 0.03, 0))
persp(IPM_VA_CR_precip1)
as.numeric(eigen(IPM_VA_CR_precip1)$value[1])


IPM_VA_CR_precip2 <- Pmatrix_VA_CR_precip2 + Fmatrix_VA_CR + Cmatrix_VA_CR
contourPlot2(t(IPM_VA_CR_precip2, Pmatrix_VA_CR_precip2@meshpoints, maxSize, 0.03, 0))
persp(IPM_VA_CR_precip2)
as.numeric(eigen(IPM_VA_CR_precip2)$value[1])


IPM_VA_CR_precip3 <- Pmatrix_VA_CR_precip3 + Fmatrix_VA_CR + Cmatrix_VA_CR
contourPlot2(t(IPM_VA_CR_precip3, Pmatrix_VA_CR_precip3@meshpoints, maxSize, 0.03, 0))
persp(IPM_VA_CR_precip3)
as.numeric(eigen(IPM_VA_CR_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_CR_precip1), Pmatrix_VA_CR_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CR - 1.2 m/year")
contourPlot2(t(IPM_VA_CR_precip2), Pmatrix_VA_CR_precip2@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CR - 2.3 m/year")
contourPlot2(t(IPM_VA_CR_precip3), Pmatrix_VA_CR_precip3@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CR - 3.4 m/year")


##### Ambient temperature extant #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CE))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CE))
summary(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CE)) #choosing this model for AIC
AIC(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CE))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = VA_CE))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = VA_CE))
summary(glmer(surv ~ size  + (1|block_trans), family = 'binomial', data = VA_CE))
AIC(glmer(surv ~ size  + (1|block_trans), family = 'binomial', data = VA_CE))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CE))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CE))

mod_surv_VA_CE <- glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CE)
plot_surv_VA_CE <- plot_predictions_surv_precip(model = mod_surv_VA_CE, data = VA_CE, minSize, maxSize)
plot_surv_VA_CE

so_VA_CE <- makeSurvObj(VA_CE, "surv ~ size")
#so_VA_CE <- coerceSurvObj(so_VA_CE, as.numeric(fixef(mod_surv_VA_CE))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

so_VA_CE_precip1 <- coerceSurvObj(so_VA_CE, c(as.numeric(fixef(mod_surv_VA_CE)[1]) + 1.2*as.numeric(fixef(mod_surv_VA_CE)[3]) + (1.2)^2* as.numeric(fixef(mod_surv_VA_CE)[4]),
                                              as.numeric(fixef(mod_surv_VA_CE)[2]))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj
so_VA_CE_precip2 <- coerceSurvObj(so_VA_CE, c(as.numeric(fixef(mod_surv_VA_CE)[1]) + 2.3*as.numeric(fixef(mod_surv_VA_CE)[3]) + (2.3)^2* as.numeric(fixef(mod_surv_VA_CE)[4]),
                                              as.numeric(fixef(mod_surv_VA_CE)[2])))
so_VA_CE_precip3 <- coerceSurvObj(so_VA_CE, c(as.numeric(fixef(mod_surv_VA_CE)[1]) + 3.4*as.numeric(fixef(mod_surv_VA_CE)[3]) + (3.4)^2* as.numeric(fixef(mod_surv_VA_CE)[4]),
                                              as.numeric(fixef(mod_surv_VA_CE)[2])))


# Choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CE))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CE))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CE)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CE))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CE)) #We chose this model based on AIC
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CE))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CE))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CE)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CE))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CE))

mod_growth_VA_CE <- lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CE)

plot_growth_VA_CE <- plot_predictions_growth_precip(model = mod_growth_VA_CE, data = VA_CE, minSize, maxSize)

plot_surv_VA_CE | plot_growth_VA_CE

go_VA_CE <- makeGrowthObj(VA_CE, "sizeNext ~ size")

#Adding coefficients and standard deviations from mixed effect model and not from the linear model as is default in makeSurvObj
#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_CE_precip1 <- coerceGrowthObj(go_VA_CE, c(as.numeric(fixef(mod_growth_VA_CE)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_CE)[3]),
                                              as.numeric(fixef(mod_growth_VA_CE)[2])),
                                  sigma.hat(mod_growth_VA_CE)$sigma$data)
go_VA_CE_precip2 <- coerceGrowthObj(go_VA_CE, c(as.numeric(fixef(mod_growth_VA_CE)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_CE)[3]),
                                              as.numeric(fixef(mod_growth_VA_CE)[2])),
                                  sigma.hat(mod_growth_VA_CE)$sigma$data)
go_VA_CE_precip3 <- coerceGrowthObj(go_VA_CE, c(as.numeric(fixef(mod_growth_VA_CE)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_CE)[3]),
                                              as.numeric(fixef(mod_growth_VA_CE)[2])),
                                  sigma.hat(mod_growth_VA_CE)$sigma$data)

# Make discrete transition object
dto_VA_CE <- makeDiscreteTrans(VA_CE, discreteTrans = matrix(
   c(VA_C_seed_bank$seeds_staySB,
     (1-VA_C_seed_bank$seeds_staySB)*seedling_est_VA_C_Veg,
     (1-VA_C_seed_bank$seeds_staySB)*(1-seedling_est_VA_C_Veg), 
     0,
     sum(VA_CE$number[VA_CE$stage=="continuous"&VA_CE$stageNext=="continuous"], na.rm=T),
     sum(VA_CE$number[VA_CE$stage=="continuous"&VA_CE$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix. First step without the discrete transitions to check the fit of the matrix - then with discrete transitions to build the actual final matrix.
Pmatrix_VA_CE_precip1 <- makeIPMPmatrix(survObj=so_VA_CE_precip1, growObj=go_VA_CE_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CE_precip2 <- makeIPMPmatrix(survObj=so_VA_CE_precip2, growObj=go_VA_CE_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CE_precip3 <- makeIPMPmatrix(survObj=so_VA_CE_precip3, growObj=go_VA_CE_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CE_precip1, survObj=so_VA_CE_precip1, growObj=go_VA_CE_precip1, dff = VA_CE)
diagnosticsPmatrix(Pmatrix_VA_CE_precip2, survObj=so_VA_CE_precip2, growObj=go_VA_CE_precip2, dff = VA_CE)
diagnosticsPmatrix(Pmatrix_VA_CE_precip3, survObj=so_VA_CE_precip3, growObj=go_VA_CE_precip3, dff = VA_CE)

Pmatrix_VA_CE_precip1 <- makeIPMPmatrix(survObj=so_VA_CE_precip1, growObj=go_VA_CE_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CE, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CE_precip2 <- makeIPMPmatrix(survObj=so_VA_CE_precip2, growObj=go_VA_CE_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CE, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CE_precip3 <- makeIPMPmatrix(survObj=so_VA_CE_precip3, growObj=go_VA_CE_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CE, correction = "constant", nBigMatrix = 100)

x11()
contourPlot2(t(Pmatrix_VA_CE_precip1), Pmatrix_VA_CE_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_CE_precip2), Pmatrix_VA_CE_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_CE_precip3), Pmatrix_VA_CE_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
#Using only blockID as random effect because of singularity fit with other higher prioritized random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID. siteID does also work, but we encapsule more of the spatial autocorrelation when we also use blockID, so going for that.
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), family = 'binomial', data = VA_CE)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), family = 'binomial', data = VA_CE)) 
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|blockID), family = 'binomial', data = VA_CE))
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|blockID), family = 'binomial', data = VA_CE))
summary(glmer(flo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_CE)) #Choosing this model based on AIC
AIC(glmer(flo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_CE)) 
summary(glmer(flo.if ~ size + (1|blockID), family = 'binomial', data = VA_CE))
AIC(glmer(flo.if ~ size + (1|blockID), family = 'binomial', data = VA_CE)) 
summary(glmer(flo.if ~ 1 + (1|blockID), family = 'binomial', data = VA_CE))
AIC(glmer(flo.if ~ 1 + (1|blockID), family = 'binomial', data = VA_CE)) 

glmer(flo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_CE, verbose = TRUE) #Checking for conversion - model seems very stable - conversion OK

# Choosing models
floweringChosenModel_VA_CE <- flo.if ~ size + size2
mod_flo_if_VA_CE <- glmer(flo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_CE)

plot_VA_CE_floif <- plot_predictions_floif(model = mod_flo_if_VA_CE, data = VA_CE, minSize, maxSize)
plot_VA_CE_floif

# Choosing the best model for estimating the number of flowers, if an individual flowers
#Using only blockID as random effect because of singularity fit with other higher prioritized random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID. 
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), family = 'poisson', data = VA_CE))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), family = 'poisson', data = VA_CE))
summary(glmer(flo.no ~ size + precip+I(precip^2) + (1|blockID), family = 'poisson', data = VA_CE))
AIC(glmer(flo.no ~ size + precip+I(precip^2) + (1|blockID), family = 'poisson', data = VA_CE))
summary(glmer(flo.no ~ size + precip + (1|blockID), family = 'poisson', data = VA_CE))
AIC(glmer(flo.no ~ size + precip + (1|blockID), family = 'poisson', data = VA_CE))
summary(glmer(flo.no ~ size  + (1|blockID), family = 'poisson', data = VA_CE)) #Choosing this model based of AIC
AIC(glmer(flo.no ~ size + (1|blockID), family = 'poisson', data = VA_CE))
summary(glmer(flo.no ~ 1  + (1|blockID), family = 'poisson', data = VA_CE))
AIC(glmer(flo.no ~ 1 + (1|blockID), family = 'poisson', data = VA_CE))


flowerNumberChosenModel_VA_CE <- flo.no ~ size
mod_flo_no_VA_CE <- glmer(flo.no ~ size  + (1|blockID), family = 'poisson', data = VA_CE)

plot_flo_no_VA_CE <-plot_predictions_flono(model = mod_flo_no_VA_CE, data = VA_CE, minSize, maxSize, ylim = 15) 
plot_flo_no_VA_CE

# Make fecundity object
fo_VA_CE <-makeFecObj(VA_CE, 
                      Formula= c(floweringChosenModel_VA_CE, flowerNumberChosenModel_VA_CE),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_Veg), continuous=(1-(VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

#Replace with coefficients form mixed effects models and make different ones for three precipitation levels
fo_VA_CE@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_VA_CE))
fo_VA_CE@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_VA_CE))

#Make F matrix
Fmatrix_VA_CE <- makeIPMFmatrix(fecObj=fo_VA_CE, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# plotting the F matrix
contourPlot2(t(Fmatrix_VA_CE), Fmatrix_VA_CE@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_CE_clones <- VA_CE %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Using only transition as random effect because of singularity fit with other higher prioritized random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID. Only transition came back without singularity
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|transition), family = 'binomial', data = VA_CE))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|transition), family = 'binomial', data = VA_CE))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|transition), family = 'binomial', data = VA_CE))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|transition), family = 'binomial', data = VA_CE))
summary(glmer(clo.if ~ size+I(size^2) + (1|transition), family = 'binomial', data = VA_CE)) #Choosing this model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|transition), family = 'binomial', data = VA_CE))
summary(glmer(clo.if ~ size + (1|transition), family = 'binomial', data = VA_CE))
AIC(glmer(clo.if ~ size + (1|transition), family = 'binomial', data = VA_CE))
summary(glmer(clo.if ~ 1 + (1|transition), family = 'binomial', data = VA_CE))
AIC(glmer(clo.if ~ 1 + (1|transition), family = 'binomial', data = VA_CE))

#Chosen model
mod_clo_VA_CE <- glmer(clo.if ~ size+I(size^2) + (1|transition), family = 'binomial', data = VA_CE)
CloneChosenModel_VA_CE <- clo.if ~ size + size2 

#Plot for visual checking
plot_clo_if_VA_CE <- plot_predictions_cloif(model = mod_clo_VA_CE, data = VA_CE, minSize, maxSize)
plot_clo_if_VA_CE

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CE))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CE))
summary(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CE))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CE))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = VA_CE))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = VA_CE))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_CE))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_CE))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_CE)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_CE))

mod_clo_no_VA_CE <- glm(clo.no ~ 1, family = 'poisson', data = VA_CE)
CloneNumberChosenModel_VA_CE <- clo.no ~ 1

plot_clo_no_VA_CE <- plot_predictions_clono(model = mod_clo_no_VA_CE, data = VA_CE, minSize, maxSize, ylim = 6)
plot_clo_no_VA_CE

#Does size of the clone depend on size of parent.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CE_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CE_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CE_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CE_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = VA_CE_clones))
AIC(lmer(sizeNext ~ size+I(size^2)+ (1|block_trans), data = VA_CE_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CE_clones)) #choosing this model based of AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CE_clones)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CE_clones))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CE_clones))

mod_clone_growth_VA_CE <- lmer(sizeNext ~ size + (1|block_trans), data = VA_CE_clones)
CloneSizeVariable_VA_CE <- "size"

plot_clone_growth_VA_CE <- plot_predictions_growth(model = mod_clone_growth_VA_CE, data = VA_CE_clones, minSize, maxSize)
plot_clone_growth_VA_CE

#Make clonal object
co_VA_CE <- makeClonalObj(VA_CE, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_CE_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_CE, Formula = c(CloneChosenModel_VA_CE, CloneNumberChosenModel_VA_CE),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_CE@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_VA_CE))
co_VA_CE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_CE)) #not needed as this is a linear model
co_VA_CE@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_CE)$sigma$data
co_VA_CE@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_VA_CE))


Cmatrix_VA_CE <- makeIPMCmatrix(clonalObj = co_VA_CE, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_CE), Cmatrix_VA_CE@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_CE_precip1 <- Pmatrix_VA_CE_precip1 + Fmatrix_VA_CE + Cmatrix_VA_CE
as.numeric(eigen(IPM_VA_CE_precip1)$value[1])

IPM_VA_CE_precip2 <- Pmatrix_VA_CE_precip2 + Fmatrix_VA_CE + Cmatrix_VA_CE
as.numeric(eigen(IPM_VA_CE_precip2)$value[1])

IPM_VA_CE_precip3 <- Pmatrix_VA_CE_precip3 + Fmatrix_VA_CE + Cmatrix_VA_CE
as.numeric(eigen(IPM_VA_CE_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_CE_precip1), Pmatrix_VA_CE_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CE - 1.2 m/year")
contourPlot2(t(IPM_VA_CE_precip2), Pmatrix_VA_CE_precip2@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CE - 2.3 m/year")
contourPlot2(t(IPM_VA_CE_precip3), Pmatrix_VA_CE_precip3@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CE - 3.4 m/year")


##### Ambient temperature novel #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CN)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_CN)) 
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_CN))

glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CN, verbose = TRUE) #Checking for conversion - model seems very stable - conversion OK

mod_surv_VA_CN <- glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CN)
plot_surv_VA_CN <- plot_predictions_surv(model = mod_surv_VA_CN, data = VA_CN, minSize, maxSize)

plot_surv_VA_CN

so_VA_CN <- makeSurvObj(VA_CN, "surv ~ size + size2")
so_VA_CN <- coerceSurvObj(so_VA_CN, as.numeric(fixef(mod_surv_VA_CN))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj


# Choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CN)) #We chose this model based on AIC
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CN))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CN)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_CN))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CN)) 
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_CN))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_CN))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_CN)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CN))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_CN))


mod_growth_VA_CN <- lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_CN)

plot_growth_VA_CN <- plot_predictions_growth_precip(model = mod_growth_VA_CN, data = VA_CN, minSize, maxSize)

plot_surv_VA_CN | plot_growth_VA_CN


go_VA_CN <- makeGrowthObj(VA_CN, "sizeNext ~ size + size2")

#Adding coefficients and standard deviations from mixed effect model and not from the linear model as is default in makeSurvObj
#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_CN_precip1 <- coerceGrowthObj(go_VA_CN, c(as.numeric(fixef(mod_growth_VA_CN)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_CN)[4]) + (1.2)^2* as.numeric(fixef(mod_growth_VA_CN)[5]),
                                              as.numeric(fixef(mod_growth_VA_CN)[2]),
                                              as.numeric(fixef(mod_growth_VA_CN)[3])),
                                  sigma.hat(mod_growth_VA_CN)$sigma$data)
go_VA_CN_precip2 <- coerceGrowthObj(go_VA_CN, c(as.numeric(fixef(mod_growth_VA_CN)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_CN)[4]) + (2.3)^2* as.numeric(fixef(mod_growth_VA_CN)[5]),
                                              as.numeric(fixef(mod_growth_VA_CN)[2]),
                                              as.numeric(fixef(mod_growth_VA_CN)[3])),
                                  sigma.hat(mod_growth_VA_CN)$sigma$data)
go_VA_CN_precip3 <- coerceGrowthObj(go_VA_CN, c(as.numeric(fixef(mod_growth_VA_CN)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_CN)[4]) + (3.4)^2* as.numeric(fixef(mod_growth_VA_CN)[5]),
                                              as.numeric(fixef(mod_growth_VA_CN)[2]),
                                              as.numeric(fixef(mod_growth_VA_CN)[3])),
                                  sigma.hat(mod_growth_VA_CN)$sigma$data)

# Make discrete transition object
dto_VA_CN <- makeDiscreteTrans(VA_CN, discreteTrans = matrix(
   c(VA_C_seed_bank$seeds_staySB,
     (1-VA_C_seed_bank$seeds_staySB)*seedling_est_VA_C_Veg,
     (1-VA_C_seed_bank$seeds_staySB)*(1-seedling_est_VA_C_Veg), 
     0,
     sum(VA_CN$number[VA_CN$stage=="continuous"&VA_CN$stageNext=="continuous"], na.rm=T),
     sum(VA_CN$number[VA_CN$stage=="continuous"&VA_CN$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix. First step without the discrete transitions to check the fit of the matrix - then with discrete transitions to build the actual final matrix.
Pmatrix_VA_CN_precip1 <- makeIPMPmatrix(survObj=so_VA_CN, growObj=go_VA_CN_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CN_precip2 <- makeIPMPmatrix(survObj=so_VA_CN, growObj=go_VA_CN_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CN_precip3 <- makeIPMPmatrix(survObj=so_VA_CN, growObj=go_VA_CN_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_CN_precip1, survObj=so_VA_CN, growObj=go_VA_CN_precip1, dff = VA_CN)
diagnosticsPmatrix(Pmatrix_VA_CN_precip2, survObj=so_VA_CN, growObj=go_VA_CN_precip2, dff = VA_CN)
diagnosticsPmatrix(Pmatrix_VA_CN_precip3, survObj=so_VA_CN, growObj=go_VA_CN_precip3, dff = VA_CN)

Pmatrix_VA_CN_precip1 <- makeIPMPmatrix(survObj=so_VA_CN, growObj=go_VA_CN_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CN, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CN_precip2 <- makeIPMPmatrix(survObj=so_VA_CN, growObj=go_VA_CN_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CN, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_CN_precip3 <- makeIPMPmatrix(survObj=so_VA_CN, growObj=go_VA_CN_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CN, correction = "constant", nBigMatrix = 100)


x11()
contourPlot2(t(Pmatrix_VA_CN_precip1), Pmatrix_VA_CN_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_CN_precip2), Pmatrix_VA_CN_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_CN_precip3), Pmatrix_VA_CN_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN)) #choosing this model based of AIC
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_CN))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CN))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_CN))

#Have not decided on a model yet because the size2 does not make a lot of sense to me.
  glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN, verbose = TRUE) #Checking for conversion - model seems very stable - conversion OK
  
  floweringChosenModel_VA_CN <- flo.if ~ size + size2
  
 mod_flo_if_VA_CN <- glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN)
 
 #Also testing other models to see if we agree with AIC. It doesn't make to much sense that is should go so much down again as we get the largest individuals as it does for the model above. But none of the other models are a better fit in our opinion. Going for the size+size2+precip+precip2 model for now.
 
 # mod_flo_if_VA_CN1 <- glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + size:precip + I(size^2):I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN)
 # mod_flo_if_VA_CN2 <- glmer(flo.if ~ size + precip+I(precip^2) + size:precip +  (1|block_trans), family = 'binomial', data = VA_CN)
 # mod_flo_if_VA_CN3 <- glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_CN)
 
 plot_VA_CN_floif <- plot_predictions_floif_precip(model = mod_flo_if_VA_CN, data = VA_CN, minSize, maxSize)
 # plot_VA_CN_floif1 <- plot_predictions_floif_precip(model = mod_flo_if_VA_CN1, data = VA_CN, minSize, maxSize)
 # plot_VA_CN_floif2 <- plot_predictions_floif_precip(model = mod_flo_if_VA_CN2, data = VA_CN, minSize, maxSize)
 # plot_VA_CN_floif3 <- plot_predictions_floif_precip(model = mod_flo_if_VA_CN3, data = VA_CN, minSize, maxSize)
 
 plot_VA_CN_floif
 
 # (plot_VA_CN_floif | plot_VA_CN_floif1) /
 #    (plot_VA_CN_floif3 | plot_VA_CN_floif2)


# Choosing the best model for estimating the number of flowers, if an individual flowers
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CN))
AIC(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CN))
summary(glm(flo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CN))
AIC(glm(flo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CN)) #Choosing this model because of AIC
summary(glm(flo.no ~ size + precip, family = 'poisson', data = VA_CN))
AIC(glm(flo.no ~ size + precip, family = 'poisson', data = VA_CN))
summary(glm(flo.no ~ size, family = 'poisson', data = VA_CN))
AIC(glm(flo.no ~ size , family = 'poisson', data = VA_CN))
summary(glm(flo.no ~ 1, family = 'poisson', data = VA_CN))
AIC(glm(flo.no ~ 1, family = 'poisson', data = VA_CN))

flowerNumberChosenModel_VA_CN <- flo.no ~ size
mod_flo_no_VA_CN <- glm(flo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CN)

plot_flo_no_VA_CN <-plot_predictions_flono_precip(model = mod_flo_no_VA_CN, data = VA_CN, minSize, maxSize, ylim = 15) 
plot_flo_no_VA_CN

# Make fecundity object
fo_VA_CN <-makeFecObj(VA_CN, 
                      Formula= c(floweringChosenModel_VA_CN, flowerNumberChosenModel_VA_CN),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_Veg), continuous=(1-(VA_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_C_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

#Replace with coefficients form mixed effects models and make different ones for three precipitation levels
fo_VA_CN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_CN)[1]) + 1.2*as.numeric(fixef(mod_flo_if_VA_CN)[4]) + (1.2)^2* as.numeric(fixef(mod_flo_if_VA_CN)[5]),
                                       as.numeric(fixef(mod_flo_if_VA_CN)[2]),
                                       as.numeric(fixef(mod_flo_if_VA_CN)[3]))
fo_VA_CN@fitFec[[2]]$coefficients <- c(as.numeric(coef(mod_flo_no_VA_CN)[1]) + 1.2*as.numeric(coef(mod_flo_no_VA_CN)[3]) + (1.2)^2* as.numeric(coef(mod_flo_no_VA_CN)[4]),
                                       as.numeric(coef(mod_flo_no_VA_CN)[2]))
fo_VA_CN_precip1 <- fo_VA_CN

fo_VA_CN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_CN)[1]) + 2.3*as.numeric(fixef(mod_flo_if_VA_CN)[4]) + (2.3)^2* as.numeric(fixef(mod_flo_if_VA_CN)[5]),
                                       as.numeric(fixef(mod_flo_if_VA_CN)[2]),
                                       as.numeric(fixef(mod_flo_if_VA_CN)[3]))
fo_VA_CN@fitFec[[2]]$coefficients <- c(as.numeric(coef(mod_flo_no_VA_CN)[1]) + 2.3*as.numeric(coef(mod_flo_no_VA_CN)[3]) + (2.3)^2* as.numeric(coef(mod_flo_no_VA_CN)[4]),
                                       as.numeric(coef(mod_flo_no_VA_CN)[2]))
fo_VA_CN_precip2 <- fo_VA_CN

fo_VA_CN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_CN)[1]) + 3.4*as.numeric(fixef(mod_flo_if_VA_CN)[4]) + (3.4)^2* as.numeric(fixef(mod_flo_if_VA_CN)[5]),
                                       as.numeric(fixef(mod_flo_if_VA_CN)[2]),
                                       as.numeric(fixef(mod_flo_if_VA_CN)[3]))
fo_VA_CN@fitFec[[2]]$coefficients <- c(as.numeric(coef(mod_flo_no_VA_CN)[1]) + 3.4*as.numeric(coef(mod_flo_no_VA_CN)[3]) + (3.4)^2* as.numeric(coef(mod_flo_no_VA_CN)[4]),
                                       as.numeric(coef(mod_flo_no_VA_CN)[2]))
fo_VA_CN_precip3 <- fo_VA_CN

#Make F matrix
Fmatrix_VA_CN_precip1 <- makeIPMFmatrix(fecObj=fo_VA_CN_precip1, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_CN_precip2 <- makeIPMFmatrix(fecObj=fo_VA_CN_precip2, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_CN_precip3 <- makeIPMFmatrix(fecObj=fo_VA_CN_precip3, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# plotting the F matrix
contourPlot2(t(Fmatrix_VA_CN_precip1), Fmatrix_VA_CN_precip1@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 
contourPlot2(t(Fmatrix_VA_CN_precip2), Fmatrix_VA_CN_precip2@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 
contourPlot2(t(Fmatrix_VA_CN_precip3), Fmatrix_VA_CN_precip3@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_CN_clones <- VA_CN %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = VA_CN))
AIC(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = VA_CN))
summary(glm(clo.if ~ size+I(size^2) + precip, family = 'binomial', data = VA_CN))
AIC(glm(clo.if ~ size+I(size^2) + precip, family = 'binomial', data = VA_CN))
summary(glm(clo.if ~ size+I(size^2) , family = 'binomial', data = VA_CN)) #choosing this model based of AIC
AIC(glm(clo.if ~ size+I(size^2), family = 'binomial', data = VA_CN))
summary(glm(clo.if ~ size , family = 'binomial', data = VA_CN))
AIC(glm(clo.if ~ size, family = 'binomial', data = VA_CN))
summary(glm(clo.if ~ 1 , family = 'binomial', data = VA_CN))
AIC(glm(clo.if ~ 1, family = 'binomial', data = VA_CN))

#Chosen model
mod_clo_VA_CN <- glm(clo.if ~ size+I(size^2) , family = 'binomial', data = VA_CN)
CloneChosenModel_VA_CN <- clo.if ~ size + size2 

#Plot for visual checking
plot_clo_if_VA_CN <- plot_predictions_cloif(model = mod_clo_VA_CN, data = VA_CN, minSize, maxSize)
plot_clo_if_VA_CN

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CN))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_CN))
summary(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CN))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_CN))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = VA_CN))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = VA_CN))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_CN))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_CN))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_CN)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_CN))

mod_clo_no_VA_CN <- glm(clo.no ~ 1, family = 'poisson', data = VA_CN)
CloneNumberChosenModel_VA_CN <- clo.no ~ 1

plot_clo_no_VA_CN <- plot_predictions_clono(model = mod_clo_no_VA_CN, data = VA_CN, minSize = minSize, maxSize = maxSize, ylim = 6)
plot_clo_no_VA_CN

#Does size of the clone depend on size of parent.
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(lm(sizeNext ~ size+I(size^2) + precip+I(precip^2), data = VA_CN_clones))
AIC(lm(sizeNext ~ size+I(size^2) + precip+I(precip^2), data = VA_CN_clones))
summary(lm(sizeNext ~ size + precip+I(precip^2), data = VA_CN_clones))
AIC(lm(sizeNext ~ size + precip+I(precip^2), data = VA_CN_clones))
summary(lm(sizeNext ~ size + precip, data = VA_CN_clones)) # choosing this model base of AIC
AIC(lm(sizeNext ~ size + precip, data = VA_CN_clones))
summary(lm(sizeNext ~ size, data = VA_CN_clones))
AIC(lm(sizeNext ~ size, data = VA_CN_clones))
summary(lm(sizeNext ~ 1, data = VA_CN_clones))
AIC(lm(sizeNext ~ 1, data = VA_CN_clones))


mod_clone_growth_VA_CN <- lm(sizeNext ~ size + precip, data = VA_CN_clones)
CloneSizeVariable_VA_CN <- "size"

plot_clone_growth_VA_CN <- plot_predictions_growth_precip(model = mod_clone_growth_VA_CN, data = VA_CN_clones, minSize, maxSize)
plot_clone_growth_VA_CN

#Make clonal object
co_VA_CN <- makeClonalObj(VA_CN, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_CN_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_CN, Formula = c(CloneChosenModel_VA_CN, CloneNumberChosenModel_VA_CN),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_CN@fitFec[[1]]$coefficients <- as.numeric(coef(mod_clo_VA_CN))#not needed as this is a linear model
co_VA_CN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_CN)) #not needed as this is a linear model
co_VA_CN@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_CN)$sigma$data
co_VA_CN@offspringRel$coefficients <- c(as.numeric(coef(mod_clone_growth_VA_CN)[1]) + 1.2*as.numeric(coef(mod_clone_growth_VA_CN)[3]),
                                        as.numeric(coef(mod_clone_growth_VA_CN)[2]))
co_VA_CN_precip1 <- co_VA_CN

co_VA_CN@offspringRel$coefficients <- c(as.numeric(coef(mod_clone_growth_VA_CN)[1]) + 2.3*as.numeric(coef(mod_clone_growth_VA_CN)[3]),
                                        as.numeric(coef(mod_clone_growth_VA_CN)[2]))
co_VA_CN_precip2 <- co_VA_CN

co_VA_CN@offspringRel$coefficients <- c(as.numeric(coef(mod_clone_growth_VA_CN)[1]) + 3.4*as.numeric(coef(mod_clone_growth_VA_CN)[3]),
                                        as.numeric(coef(mod_clone_growth_VA_CN)[2]))
co_VA_CN_precip3 <- co_VA_CN


Cmatrix_VA_CN_precip1 <- makeIPMCmatrix(clonalObj = co_VA_CN_precip1, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")
Cmatrix_VA_CN_precip2 <- makeIPMCmatrix(clonalObj = co_VA_CN_precip2, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")
Cmatrix_VA_CN_precip3 <- makeIPMCmatrix(clonalObj = co_VA_CN_precip3, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_CN_precip1), Cmatrix_VA_CN_precip1@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 
contourPlot2(t(Cmatrix_VA_CN_precip2), Cmatrix_VA_CN_precip2@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 
contourPlot2(t(Cmatrix_VA_CN_precip3), Cmatrix_VA_CN_precip3@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_CN_precip1 <- Pmatrix_VA_CN_precip1 + Fmatrix_VA_CN_precip1 + Cmatrix_VA_CN_precip1
as.numeric(eigen(IPM_VA_CN_precip1)$value[1])

IPM_VA_CN_precip2 <- Pmatrix_VA_CN_precip2 + Fmatrix_VA_CN_precip2 + Cmatrix_VA_CN_precip2
as.numeric(eigen(IPM_VA_CN_precip2)$value[1])

IPM_VA_CN_precip3 <- Pmatrix_VA_CN_precip3 + Fmatrix_VA_CN_precip3 + Cmatrix_VA_CN_precip3
as.numeric(eigen(IPM_VA_CN_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_CN_precip1), Pmatrix_VA_CN_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CN - 1.2 m/year")
contourPlot2(t(IPM_VA_CN_precip2), Pmatrix_VA_CN_precip2@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CN - 2.3 m/year")
contourPlot2(t(IPM_VA_CN_precip3), Pmatrix_VA_CN_precip3@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - CN - 3.4 m/year")

##### Warming control #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WC)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_WC))

mod_surv_VA_WC <- glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WC)
plot_surv_VA_WC <- plot_predictions_surv(model = mod_surv_VA_WC, data = VA_WC, minSize, maxSize)

plot_surv_VA_WC

so_VA_WC <- makeSurvObj(VA_WC, "surv ~ size + size2")
so_VA_WC <- coerceSurvObj(so_VA_WC, as.numeric(fixef(mod_surv_VA_WC))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# Choosing the best growth model

summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WC))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WC))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WC)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WC))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WC)) 
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WC))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WC))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WC))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WC))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WC)) #We chose this model based on AIC
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WC))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WC))


mod_growth_VA_WC <- lmer(sizeNext ~ size  + (1|block_trans), data = VA_WC)

plot_growth_VA_WC <- plot_predictions_growth(model = mod_growth_VA_WC, data = VA_WC, minSize, maxSize)

plot_surv_VA_WC | plot_growth_VA_WC


go_VA_WC <- makeGrowthObj(VA_WC, "sizeNext ~ size")
go_VA_WC <- coerceGrowthObj(go_VA_WC, as.numeric(fixef(mod_growth_VA_WC)), sigma.hat(mod_growth_VA_WC)$sigma$data) #Adding coefficients and standard deviations from mixed effect model and not from the linear model as is default in makeSurvObj


# Make discrete transition object
dto_VA_WC <- makeDiscreteTrans(VA_WC, discreteTrans = matrix(
   c(VA_OTC_seed_bank$seeds_staySB,
     (1-VA_OTC_seed_bank$seeds_staySB)*seedling_est_VA_OTC_Veg,
     (1-VA_OTC_seed_bank$seeds_staySB)*(1-seedling_est_VA_OTC_Veg), 
     0,
     sum(VA_WC$number[VA_WC$stage=="continuous"&VA_WC$stageNext=="continuous"], na.rm=T),
     sum(VA_WC$number[VA_WC$stage=="continuous"&VA_WC$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix. First step without the discrete transitions to check the fit of the matrix - then with discrete transitions to build the actual final matrix.
Pmatrix_VA_WC <- makeIPMPmatrix(survObj=so_VA_WC, growObj=go_VA_WC, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_WC, survObj=so_VA_WC, growObj=go_VA_WC, dff = VA_WC)

Pmatrix_VA_WC <- makeIPMPmatrix(survObj=so_VA_WC, growObj=go_VA_WC, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WC, correction = "constant", nBigMatrix = 100)
contourPlot2(t(Pmatrix_VA_WC), Pmatrix_VA_WC@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WC))  #Choosing this model based on AIC
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WC))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WC))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WC))

floweringChosenModel_VA_WC <- flo.if ~ size

mod_flo_if_VA_WC <- glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WC) 

plot_VA_WC_floif <- plot_predictions_floif(model = mod_flo_if_VA_WC, data = VA_WC, minSize, maxSize)
plot_VA_WC_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WC))
AIC(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WC))
summary(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WC))
AIC(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WC))
summary(glm(flo.no ~ size+I(size^2), family = 'poisson', data = VA_WC))
AIC(glm(flo.no ~ size+I(size^2), family = 'poisson', data = VA_WC))
summary(glm(flo.no ~ size, family = 'poisson', data = VA_WC)) #Choosing this model based on AIC
AIC(glm(flo.no ~ size, family = 'poisson', data = VA_WC))
summary(glm(flo.no ~ 1, family = 'poisson', data = VA_WC))
AIC(glm(flo.no ~ 1, family = 'poisson', data = VA_WC))

flowerNumberChosenModel_VA_WC <- flo.no ~ size

mod_flo_no_VA_WC <- glm(flo.no ~ size, family = 'poisson', data = VA_WC)

plot_flo_no_VA_WC <-plot_predictions_flono(model = mod_flo_no_VA_WC, data = VA_WC, minSize, maxSize, ylim = 15) 

plot_flo_no_VA_WC

# Make fecundity object
fo_VA_WC <-makeFecObj(VA_WC, 
                      Formula= c(floweringChosenModel_VA_WC, flowerNumberChosenModel_VA_WC),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_Veg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_Veg), continuous=(1-(VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

#Replace with coefficients form mixed effects models
fo_VA_WC@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_VA_WC))
fo_VA_WC@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_VA_WC)) #Not needed because both are linear models

#Make F matrix
Fmatrix_VA_WC<- makeIPMFmatrix(fecObj=fo_VA_WC, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# plotting the F matrix
contourPlot2(t(Fmatrix_VA_WC), Fmatrix_VA_WC@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_WC_clones <- VA_WC %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
# Using site_trans as random effect because block_trans did not work (singularity)
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = VA_WC))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = VA_WC))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = VA_WC))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = VA_WC))
summary(glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = VA_WC)) #Choosing this model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = VA_WC))
summary(glmer(clo.if ~ size + (1|site_trans), family = 'binomial', data = VA_WC))
AIC(glmer(clo.if ~ size + (1|site_trans), family = 'binomial', data = VA_WC))
summary(glmer(clo.if ~ 1 + (1|site_trans), family = 'binomial', data = VA_WC))
AIC(glmer(clo.if ~ 1 + (1|site_trans), family = 'binomial', data = VA_WC))

#Checking convergence - looks ok
glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = VA_WC, verbose = TRUE)

#Chosen model
mod_clo_VA_WC <- glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = VA_WC)
CloneChosenModel_VA_WC <- clo.if ~ size + size2 

#Plot for visual checking
plot_clo_if_VA_WC <- plot_predictions_cloif(model = mod_clo_VA_WC, data = VA_WC, minSize, maxSize)
plot_clo_if_VA_WC

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WC))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WC))
summary(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_WC))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_WC))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = VA_WC))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = VA_WC))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_WC))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_WC))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_WC)) #Choosig this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_WC))

mod_clo_no_VA_WC <- glm(clo.no ~ 1, family = 'poisson', data = VA_WC)
CloneNumberChosenModel_VA_WC <- clo.no ~ 1

plot_clo_no_VA_WC <- plot_predictions_clono(model = mod_clo_no_VA_WC, data = VA_WC, minSize, maxSize, ylim = 6)
plot_clo_no_VA_WC

#Does size of the clone depend on size of parent.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WC_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WC_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WC_clones)) #Choose this model based of AIC
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WC_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WC_clones))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WC_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WC_clones))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WC_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WC_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WC_clones))


mod_clone_growth_VA_WC <- lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WC_clones)
CloneSizeVariable_VA_WC <- "size"

plot_clone_growth_VA_WC <- plot_predictions_growth_precip(model = mod_clone_growth_VA_WC, data = VA_WC_clones, minSize, maxSize)
plot_clone_growth_VA_WC

#Make clonal object
co_VA_WC <- makeClonalObj(VA_WC, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_WC_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_WC, Formula = c(CloneChosenModel_VA_WC, CloneNumberChosenModel_VA_WC),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_WC@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_VA_WC))
co_VA_WC@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_WC)) #not needed as this is a linear model
co_VA_WC@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_WC)$sigma$data
co_VA_WC@offspringRel$coefficients <- c(as.numeric(fixef(mod_clone_growth_VA_WC)[1]) + 1.2*as.numeric(fixef(mod_clone_growth_VA_WC)[3]) + (1.2)^2* as.numeric(fixef(mod_clone_growth_VA_WC)[4]),
                                        as.numeric(fixef(mod_clone_growth_VA_WC)[2]))
co_VA_WC_precip1 <- co_VA_WC

co_VA_WC@offspringRel$coefficients <- c(as.numeric(fixef(mod_clone_growth_VA_WC)[1]) + 2.3*as.numeric(fixef(mod_clone_growth_VA_WC)[3]) + (2.3)^2* as.numeric(fixef(mod_clone_growth_VA_WC)[4]),
                                        as.numeric(fixef(mod_clone_growth_VA_WC)[2]))
co_VA_WC_precip2 <- co_VA_WC

co_VA_WC@offspringRel$coefficients <- c(as.numeric(fixef(mod_clone_growth_VA_WC)[1]) + 3.4*as.numeric(fixef(mod_clone_growth_VA_WC)[3]) + (3.4)^2* as.numeric(fixef(mod_clone_growth_VA_WC)[4]),
                                        as.numeric(fixef(mod_clone_growth_VA_WC)[2]))
co_VA_WC_precip3 <- co_VA_WC


Cmatrix_VA_WC_precip1 <- makeIPMCmatrix(clonalObj = co_VA_WC_precip1, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")
Cmatrix_VA_WC_precip2 <- makeIPMCmatrix(clonalObj = co_VA_WC_precip2, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")
Cmatrix_VA_WC_precip3 <- makeIPMCmatrix(clonalObj = co_VA_WC_precip3, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_WC_precip1), Cmatrix_VA_WC_precip1@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 
contourPlot2(t(Cmatrix_VA_WC_precip2), Cmatrix_VA_WC_precip2@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 
contourPlot2(t(Cmatrix_VA_WC_precip3), Cmatrix_VA_WC_precip3@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_WC_precip1 <- Pmatrix_VA_WC + Fmatrix_VA_WC + Cmatrix_VA_WC_precip1
as.numeric(eigen(IPM_VA_WC_precip1)$value[1])


IPM_VA_WC_precip2 <- Pmatrix_VA_WC + Fmatrix_VA_WC + Cmatrix_VA_WC_precip2
as.numeric(eigen(IPM_VA_WC_precip2)$value[1])


IPM_VA_WC_precip3 <- Pmatrix_VA_WC + Fmatrix_VA_WC + Cmatrix_VA_WC_precip3
as.numeric(eigen(IPM_VA_WC_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_WC_precip1), Pmatrix_VA_WC@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WC - 1.2 m/year")
contourPlot2(t(IPM_VA_WC_precip2), Pmatrix_VA_WC@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WC - 2.3 m/year")
contourPlot2(t(IPM_VA_WC_precip3), Pmatrix_VA_WC@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WC - 3.4 m/year")


##### Warming and removal #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WR)) 
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WR)) #We chose this model based on AIC
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_WR))

mod_surv_VA_WR <- glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WR)
plot_surv_VA_WR <- plot_predictions_surv(model = mod_surv_VA_WR, data = VA_WR, minSize, maxSize)

plot_surv_VA_WR

so_VA_WR <- makeSurvObj(VA_WR, "surv ~ size")
so_VA_WR <- coerceSurvObj(so_VA_WR, as.numeric(fixef(mod_surv_VA_WR))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# Choosing the best growth model

summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WR))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WR))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WR)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WR))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WR))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WR)) #We chose this model based on AIC
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WR))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WR)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WR))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WR))


mod_growth_VA_WR <- lmer(sizeNext ~ size  + precip + (1|block_trans), data = VA_WR)

plot_growth_VA_WR <- plot_predictions_growth_precip(model = mod_growth_VA_WR, data = VA_WR, minSize, maxSize)

plot_surv_VA_WR | plot_growth_VA_WR


go_VA_WR <- makeGrowthObj(VA_WR, "sizeNext ~ size")

#Adding coefficients and standard deviations from mixed effect model and not from the linear model as is default in makeSurvObj
#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_WR_precip1 <- coerceGrowthObj(go_VA_WR, c(as.numeric(fixef(mod_growth_VA_WR)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_WR)[3]),
                                                as.numeric(fixef(mod_growth_VA_WR)[2])),
                                    sigma.hat(mod_growth_VA_WR)$sigma$data)
go_VA_WR_precip2 <- coerceGrowthObj(go_VA_WR, c(as.numeric(fixef(mod_growth_VA_WR)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_WR)[3]),
                                                as.numeric(fixef(mod_growth_VA_WR)[2])),
                                    sigma.hat(mod_growth_VA_WR)$sigma$data)
go_VA_WR_precip3 <- coerceGrowthObj(go_VA_WR, c(as.numeric(fixef(mod_growth_VA_WR)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_WR)[3]),
                                                as.numeric(fixef(mod_growth_VA_WR)[2])),
                                    sigma.hat(mod_growth_VA_WR)$sigma$data)


# Make discrete transition object
dto_VA_WR <- makeDiscreteTrans(VA_WR, discreteTrans = matrix(
   c(VA_OTC_seed_bank$seeds_staySB,
     (1-VA_OTC_seed_bank$seeds_staySB)*seedling_est_VA_OTC_NoVeg,
     (1-VA_OTC_seed_bank$seeds_staySB)*(1-seedling_est_VA_OTC_NoVeg), 
     0,
     sum(VA_WR$number[VA_WR$stage=="continuous"&VA_WR$stageNext=="continuous"], na.rm=T),
     sum(VA_WR$number[VA_WR$stage=="continuous"&VA_WR$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix. First step without the discrete transitions to check the fit of the matrix - then with discrete transitions to build the actual final matrix.
Pmatrix_VA_WR_precip1 <- makeIPMPmatrix(survObj=so_VA_WR, growObj=go_VA_WR_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WR_precip2 <- makeIPMPmatrix(survObj=so_VA_WR, growObj=go_VA_WR_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WR_precip3 <- makeIPMPmatrix(survObj=so_VA_WR, growObj=go_VA_WR_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_WR_precip1, survObj=so_VA_WR, growObj=go_VA_WR_precip1, dff = VA_WR)
diagnosticsPmatrix(Pmatrix_VA_WR_precip2, survObj=so_VA_WR, growObj=go_VA_WR_precip2, dff = VA_WR)
diagnosticsPmatrix(Pmatrix_VA_WR_precip3, survObj=so_VA_WR, growObj=go_VA_WR_precip3, dff = VA_WR)

Pmatrix_VA_WR_precip1 <- makeIPMPmatrix(survObj=so_VA_WR, growObj=go_VA_WR_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WR, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WR_precip2 <- makeIPMPmatrix(survObj=so_VA_WR, growObj=go_VA_WR_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WR, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WR_precip3 <- makeIPMPmatrix(survObj=so_VA_WR, growObj=go_VA_WR_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WR, correction = "constant", nBigMatrix = 100)


contourPlot2(t(Pmatrix_VA_WR_precip1), Pmatrix_VA_WR_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 
contourPlot2(t(Pmatrix_VA_WR_precip2), Pmatrix_VA_WR_precip2@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_WR_precip3), Pmatrix_VA_WR_precip3@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR)) #Choosing this model based on AIC
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WR))  
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WR))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WR))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WR))

floweringChosenModel_VA_WR <- flo.if ~ size

mod_flo_if_VA_WR <- glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WR) 

plot_VA_WR_floif <- plot_predictions_floif_precip(model = mod_flo_if_VA_WR, data = VA_WR, minSize, maxSize)
plot_VA_WR_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WR))
AIC(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WR))
summary(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WR))
AIC(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WR))
summary(glm(flo.no ~ size+I(size^2), family = 'poisson', data = VA_WR))
AIC(glm(flo.no ~ size+I(size^2), family = 'poisson', data = VA_WR))
summary(glm(flo.no ~ size, family = 'poisson', data = VA_WR)) #Choosing this model based on AIC
AIC(glm(flo.no ~ size, family = 'poisson', data = VA_WR))
summary(glm(flo.no ~ 1, family = 'poisson', data = VA_WR))
AIC(glm(flo.no ~ 1, family = 'poisson', data = VA_WR))

flowerNumberChosenModel_VA_WR <- flo.no ~ size

mod_flo_no_VA_WR <- glm(flo.no ~ size, family = 'poisson', data = VA_WR)

plot_flo_no_VA_WR <-plot_predictions_flono(model = mod_flo_no_VA_WR, data = VA_WR, minSize, maxSize, ylim = 15) 
plot_flo_no_VA_WR

# Make fecundity object
fo_VA_WR <-makeFecObj(VA_WR, 
                      Formula= c(floweringChosenModel_VA_WR, flowerNumberChosenModel_VA_WR),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_NoVeg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_NoVeg), continuous=(1-(VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_NoVeg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

#Replace with coefficients form mixed effects models and make different ones for three precipitation levels
fo_VA_WR@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_WR)[1]) + 1.2*as.numeric(fixef(mod_flo_if_VA_WR)[3]) + (1.2)^2* as.numeric(fixef(mod_flo_if_VA_WR)[4]),
                                       as.numeric(fixef(mod_flo_if_VA_WR)[2]))
fo_VA_WR@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_VA_WR)) #Not needed because both are linear models
fo_VA_WR_precip1 <- fo_VA_WR

fo_VA_WR@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_WR)[1]) + 2.3*as.numeric(fixef(mod_flo_if_VA_WR)[3]) + (2.3)^2* as.numeric(fixef(mod_flo_if_VA_WR)[4]),
                                       as.numeric(fixef(mod_flo_if_VA_WR)[2]))
fo_VA_WR_precip2 <- fo_VA_WR

fo_VA_WR@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_WR)[1]) + 3.4*as.numeric(fixef(mod_flo_if_VA_WR)[3]) + (3.4)^2* as.numeric(fixef(mod_flo_if_VA_WR)[4]),
                                       as.numeric(fixef(mod_flo_if_VA_WR)[2]))
fo_VA_WR_precip3 <- fo_VA_WR

#Make F matrix
Fmatrix_VA_WR_precip1 <- makeIPMFmatrix(fecObj=fo_VA_WR_precip1, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_WR_precip2 <- makeIPMFmatrix(fecObj=fo_VA_WR_precip2, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_WR_precip3 <- makeIPMFmatrix(fecObj=fo_VA_WR_precip3, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# plotting the F matrix
contourPlot2(t(Fmatrix_VA_WR_precip1), Fmatrix_VA_WR_precip1@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 
contourPlot2(t(Fmatrix_VA_WR_precip2), Fmatrix_VA_WR_precip2@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 
contourPlot2(t(Fmatrix_VA_WR_precip3), Fmatrix_VA_WR_precip3@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_WR_clones <- VA_WR %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = VA_WR))
AIC(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = VA_WR))
summary(glm(clo.if ~ size+I(size^2) + precip, family = 'binomial', data = VA_WR))
AIC(glm(clo.if ~ size+I(size^2) + precip, family = 'binomial', data = VA_WR))
summary(glm(clo.if ~ size+I(size^2), family = 'binomial', data = VA_WR)) #Choosing this model based of AIC
AIC(glm(clo.if ~ size+I(size^2), family = 'binomial', data = VA_WR))
summary(glm(clo.if ~ size, family = 'binomial', data = VA_WR))
AIC(glm(clo.if ~ size, family = 'binomial', data = VA_WR))
summary(glm(clo.if ~ 1, family = 'binomial', data = VA_WR))
AIC(glm(clo.if ~ 1, family = 'binomial', data = VA_WR))

#Chosen model
mod_clo_VA_WR <- glm(clo.if ~ size+I(size^2), family = 'binomial', data = VA_WR)
CloneChosenModel_VA_WR <- clo.if ~ size + size2 

#Plot for visual checking
plot_clo_if_VA_WR <- plot_predictions_cloif(model = mod_clo_VA_WR, data = VA_WR, minSize, maxSize)
plot_clo_if_VA_WR

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WR))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WR))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WR))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WR))
summary(glm(clo.no ~ size+I(size^2), family = 'poisson', data = VA_WR))
AIC(glm(clo.no ~ size+I(size^2), family = 'poisson', data = VA_WR))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_WR))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_WR))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_WR)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_WR))


mod_clo_no_VA_WR <- glm(clo.no ~ 1, family = 'poisson', data = VA_WR)
CloneNumberChosenModel_VA_WR <- clo.no ~ 1

plot_clo_no_VA_WR <- plot_predictions_clono(model = mod_clo_no_VA_WR, data = VA_WR, minSize, maxSize, ylim = 6)
plot_clo_no_VA_WR

#Does size of the clone depend on size of parent.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WR_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WR_clones)) #This model is best AIC wise
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WR_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WR_clones))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WR_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WR_clones))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WR_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WR_clones)) #Using this model because it doesn't make biological sense that the clones would become smaller as the parents increase in size
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WR_clones))


mod_clone_growth_VA_WR <- lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WR_clones)
CloneSizeVariable_VA_WR <- "1"

plot_clone_growth_VA_WR <- plot_predictions_growth(model = mod_clone_growth_VA_WR, data = VA_WR_clones, minSize, maxSize)
plot_clone_growth_VA_WR

#Make clonal object
co_VA_WR <- makeClonalObj(VA_WR, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_WR_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_WR, Formula = c(CloneChosenModel_VA_WR, CloneNumberChosenModel_VA_WR),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_WR@fitFec[[1]]$coefficients <- as.numeric(coef(mod_clo_VA_WR)) #not really needed as this is a linear model
co_VA_WR@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_WR)) #not really needed as this is a linear model
co_VA_WR@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_WR)$sigma$data
co_VA_WR@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_VA_WR)) 



Cmatrix_VA_WR <- makeIPMCmatrix(clonalObj = co_VA_WR, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_WR), Cmatrix_VA_WR@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_WR_precip1 <- Pmatrix_VA_WR_precip1 + Fmatrix_VA_WR_precip1 + Cmatrix_VA_WR
as.numeric(eigen(IPM_VA_WR_precip1)$value[1])

IPM_VA_WR_precip2 <- Pmatrix_VA_WR_precip2 + Fmatrix_VA_WR_precip2 + Cmatrix_VA_WR
as.numeric(eigen(IPM_VA_WR_precip2)$value[1])

IPM_VA_WR_precip3 <- Pmatrix_VA_WR_precip3 + Fmatrix_VA_WR_precip3 + Cmatrix_VA_WR
as.numeric(eigen(IPM_VA_WR_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_WR_precip1), Pmatrix_VA_WR_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WR - 1.2 m/year")
contourPlot2(t(IPM_VA_WR_precip2), Pmatrix_VA_WR_precip2@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WR - 2.3 m/year")
contourPlot2(t(IPM_VA_WR_precip3), Pmatrix_VA_WR_precip3@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WR - 3.4 m/year")

##### Warming and extant #####


#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE)) 
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WE)) #We chose this model based on AIC
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = VA_WE))

mod_surv_VA_WE <- glmer(surv ~ size + (1|block_trans), family = 'binomial', data = VA_WE)
plot_surv_VA_WE <- plot_predictions_surv(model = mod_surv_VA_WE, data = VA_WE, minSize, maxSize)

plot_surv_VA_WE

so_VA_WE <- makeSurvObj(VA_WE, "surv ~ size")
so_VA_WE <- coerceSurvObj(so_VA_WE, as.numeric(fixef(mod_surv_VA_WE))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# Choosing the best growth model

summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WE))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WE))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WE)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WE))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WE))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WE)) #We chose this model based on AIC
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WE))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WE)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WE))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WE))

mod_growth_VA_WE <- lmer(sizeNext ~ size  + precip + (1|block_trans), data = VA_WE)

plot_growth_VA_WE <- plot_predictions_growth_precip(model = mod_growth_VA_WE, data = VA_WE, minSize, maxSize)
plot_surv_VA_WE | plot_growth_VA_WE


go_VA_WE <- makeGrowthObj(VA_WE, "sizeNext ~ size")

#Adding coefficients and standard deviations from mixed effect model and not from the linear model as is default in makeSurvObj
#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_WE_precip1 <- coerceGrowthObj(go_VA_WE, c(as.numeric(fixef(mod_growth_VA_WE)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_WE)[3]),
                                                as.numeric(fixef(mod_growth_VA_WE)[2])),
                                    sigma.hat(mod_growth_VA_WE)$sigma$data)
go_VA_WE_precip2 <- coerceGrowthObj(go_VA_WE, c(as.numeric(fixef(mod_growth_VA_WE)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_WE)[3]),
                                                as.numeric(fixef(mod_growth_VA_WE)[2])),
                                    sigma.hat(mod_growth_VA_WE)$sigma$data)
go_VA_WE_precip3 <- coerceGrowthObj(go_VA_WE, c(as.numeric(fixef(mod_growth_VA_WE)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_WE)[3]),
                                                as.numeric(fixef(mod_growth_VA_WE)[2])),
                                    sigma.hat(mod_growth_VA_WE)$sigma$data)


# Make discrete transition object
dto_VA_WE <- makeDiscreteTrans(VA_WE, discreteTrans = matrix(
   c(VA_OTC_seed_bank$seeds_staySB,
     (1-VA_OTC_seed_bank$seeds_staySB)*seedling_est_VA_OTC_Veg,
     (1-VA_OTC_seed_bank$seeds_staySB)*(1-seedling_est_VA_OTC_Veg), 
     0,
     sum(VA_WE$number[VA_WE$stage=="continuous"&VA_WE$stageNext=="continuous"], na.rm=T),
     sum(VA_WE$number[VA_WE$stage=="continuous"&VA_WE$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix. First step without the discrete transitions to check the fit of the matrix - then with discrete transitions to build the actual final matrix.
Pmatrix_VA_WE_precip1 <- makeIPMPmatrix(survObj=so_VA_WE, growObj=go_VA_WE_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WE_precip2 <- makeIPMPmatrix(survObj=so_VA_WE, growObj=go_VA_WE_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WE_precip3 <- makeIPMPmatrix(survObj=so_VA_WE, growObj=go_VA_WE_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_WE_precip1, survObj=so_VA_WE, growObj=go_VA_WE_precip1, dff = VA_WE)
diagnosticsPmatrix(Pmatrix_VA_WE_precip2, survObj=so_VA_WE, growObj=go_VA_WE_precip2, dff = VA_WE)
diagnosticsPmatrix(Pmatrix_VA_WE_precip3, survObj=so_VA_WE, growObj=go_VA_WE_precip3, dff = (VA_WE %>% filter(siteID == "Skj")))

Pmatrix_VA_WE_precip1 <- makeIPMPmatrix(survObj=so_VA_WE, growObj=go_VA_WE_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WE, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WE_precip2 <- makeIPMPmatrix(survObj=so_VA_WE, growObj=go_VA_WE_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WE, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WE_precip3 <- makeIPMPmatrix(survObj=so_VA_WE, growObj=go_VA_WE_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WE, correction = "constant", nBigMatrix = 100)

x11()
contourPlot2(t(Pmatrix_VA_WE_precip1), Pmatrix_VA_WE_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 
contourPlot2(t(Pmatrix_VA_WE_precip2), Pmatrix_VA_WE_precip2@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_WE_precip3), Pmatrix_VA_WE_precip3@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WE)) #Choosing this model based on AIC
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WE))

floweringChosenModel_VA_WE <- flo.if ~ size

mod_flo_if_VA_WE <- glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WE) 

plot_VA_WE_floif <- plot_predictions_floif(model = mod_flo_if_VA_WE, data = VA_WE, minSize, maxSize)
plot_VA_WE_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = VA_WE))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = VA_WE))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = VA_WE))
AIC(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = VA_WE))
summary(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = VA_WE))
AIC(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = VA_WE))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = VA_WE))  #Choosing this model based on AIC
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = VA_WE))
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = VA_WE))
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = VA_WE))

flowerNumberChosenModel_VA_WE <- flo.no ~ size

mod_flo_no_VA_WE <- glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = VA_WE)

plot_flo_no_VA_WE <-plot_predictions_flono(model = mod_flo_no_VA_WE, data = VA_WE, minSize, maxSize, ylim = 15) 
plot_flo_no_VA_WE

# Make fecundity object
fo_VA_WE <-makeFecObj(VA_WE, 
                      Formula= c(floweringChosenModel_VA_WE, flowerNumberChosenModel_VA_WE),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_Veg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_Veg), continuous=(1-(VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

#Replace with coefficients form mixed effects models and make different ones for three precipitation levels
fo_VA_WE@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_VA_WE))
fo_VA_WE@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_VA_WE))

#Make F matrix
Fmatrix_VA_WE<- makeIPMFmatrix(fecObj=fo_VA_WE, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# plotting the F matrix
contourPlot2(t(Fmatrix_VA_WE), Fmatrix_VA_WE@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_WE_clones <- VA_WE %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WE)) 
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE)) #Choosing this model based of AIC
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WE))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WE))
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WE))

#Chosen model
mod_clo_VA_WE <- glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = VA_WE)
CloneChosenModel_VA_WE <- clo.if ~ size + size2 

#Plot for visual checking
plot_clo_if_VA_WE <- plot_predictions_cloif(model = mod_clo_VA_WE, data = VA_WE, minSize, maxSize)
plot_clo_if_VA_WE

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WE))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WE))
summary(glm(clo.no ~ size+ precip+I(precip^2), family = 'poisson', data = VA_WE))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_WE))
summary(glm(clo.no ~ size+ precip, family = 'poisson', data = VA_WE))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = VA_WE))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_WE))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_WE))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_WE)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_WE))

mod_clo_no_VA_WE <- glm(clo.no ~ 1, family = 'poisson', data = VA_WE)
CloneNumberChosenModel_VA_WE <- clo.no ~ 1

plot_clo_no_VA_WE <- plot_predictions_clono(model = mod_clo_no_VA_WE, data = VA_WE, minSize, maxSize, ylim = 6)
plot_clo_no_VA_WE

#Does size of the clone depend on size of parent.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WE_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WE_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WE_clones)) #This model is best AIC wise
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = VA_WE_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WE_clones))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WE_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WE_clones))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WE_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WE_clones)) #Using this model because it doesn't make biological sense that the clones would become smaller as the parents increase in size
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WE_clones))


mod_clone_growth_VA_WE <- lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WE_clones)
CloneSizeVariable_VA_WE <- "1"

plot_clone_growth_VA_WE <- plot_predictions_growth(model = mod_clone_growth_VA_WE, data = VA_WE_clones, minSize, maxSize)
plot_clone_growth_VA_WE

#Make clonal object
co_VA_WE <- makeClonalObj(VA_WE, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_WE_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_WE, Formula = c(CloneChosenModel_VA_WE, CloneNumberChosenModel_VA_WE),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_WE@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_VA_WE)) 
co_VA_WE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_WE)) #not really needed as this is a linear model
co_VA_WE@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_WE)$sigma$data
co_VA_WE@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_VA_WE)) 



Cmatrix_VA_WE <- makeIPMCmatrix(clonalObj = co_VA_WE, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_VA_WE), Cmatrix_VA_WE@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_WE_precip1 <- Pmatrix_VA_WE_precip1 + Fmatrix_VA_WE + Cmatrix_VA_WE
as.numeric(eigen(IPM_VA_WE_precip1)$value[1])

IPM_VA_WE_precip2 <- Pmatrix_VA_WE_precip2 + Fmatrix_VA_WE + Cmatrix_VA_WE
as.numeric(eigen(IPM_VA_WE_precip2)$value[1])

IPM_VA_WE_precip3 <- Pmatrix_VA_WE_precip3 + Fmatrix_VA_WE + Cmatrix_VA_WE
as.numeric(eigen(IPM_VA_WE_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_WE_precip1), Pmatrix_VA_WE_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WE - 1.2 m/year")
contourPlot2(t(IPM_VA_WE_precip2), Pmatrix_VA_WE_precip2@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WE - 2.3 m/year")
contourPlot2(t(IPM_VA_WE_precip3), Pmatrix_VA_WE_precip3@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WE - 3.4 m/year")


##### Warming and novel #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))

#Using site_trans as random effect because we get a singularity warning with block_trans.
summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = VA_WN))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = VA_WN))
summary(glmer(surv ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = VA_WN))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = VA_WN))
summary(glmer(surv ~ size + precip + (1|site_trans), family = 'binomial', data = VA_WN))
AIC(glmer(surv ~ size + precip + (1|site_trans), family = 'binomial', data = VA_WN))
summary(glmer(surv ~ size + (1|site_trans), family = 'binomial', data = VA_WN)) #We chose this model based on AIC
AIC(glmer(surv ~ size + (1|site_trans), family = 'binomial', data = VA_WN))
summary(glmer(surv ~ 1 + (1|site_trans), family = 'binomial', data = VA_WN))
AIC(glmer(surv ~ 1 + (1|site_trans), family = 'binomial', data = VA_WN))

mod_surv_VA_WN <- glmer(surv ~ size + (1|site_trans), family = 'binomial', data = VA_WN)
plot_surv_VA_WN <- plot_predictions_surv(model = mod_surv_VA_WN, data = VA_WN, minSize, maxSize)

plot_surv_VA_WN

so_VA_WN <- makeSurvObj(VA_WN, "surv ~ size")
so_VA_WN <- coerceSurvObj(so_VA_WN, as.numeric(fixef(mod_surv_VA_WN))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# Choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WN))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WN))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WN)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = VA_WN))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WN)) #We chose this model based on AIC
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WN)) 
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WN))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WN)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WN))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WN))

mod_growth_VA_WN <- lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WN)

plot_growth_VA_WN <- plot_predictions_growth_precip(model = mod_growth_VA_WN, data = VA_WN, minSize, maxSize)
plot_surv_VA_WN | plot_growth_VA_WN


go_VA_WN <- makeGrowthObj(VA_WN, "sizeNext ~ size")

#Adding coefficients and standard deviations from mixed effect model and not from the linear model as is default in makeSurvObj
#Precipitation 1.2, 2.3 and 3.4 m/year
go_VA_WN_precip1 <- coerceGrowthObj(go_VA_WN, c(as.numeric(fixef(mod_growth_VA_WN)[1]) + 1.2*as.numeric(fixef(mod_growth_VA_WN)[3]),
                                                as.numeric(fixef(mod_growth_VA_WN)[2])),
                                    sigma.hat(mod_growth_VA_WN)$sigma$data)

go_VA_WN_precip2 <- coerceGrowthObj(go_VA_WN, c(as.numeric(fixef(mod_growth_VA_WN)[1]) + 2.3*as.numeric(fixef(mod_growth_VA_WN)[3]),
                                                as.numeric(fixef(mod_growth_VA_WN)[2])),
                                    sigma.hat(mod_growth_VA_WN)$sigma$data)

go_VA_WN_precip3 <- coerceGrowthObj(go_VA_WN, c(as.numeric(fixef(mod_growth_VA_WN)[1]) + 3.4*as.numeric(fixef(mod_growth_VA_WN)[3]),
                                                as.numeric(fixef(mod_growth_VA_WN)[2])),
                                    sigma.hat(mod_growth_VA_WN)$sigma$data)


# Make discrete transition object
dto_VA_WN <- makeDiscreteTrans(VA_WN, discreteTrans = matrix(
   c(VA_OTC_seed_bank$seeds_staySB,
     (1-VA_OTC_seed_bank$seeds_staySB)*seedling_est_VA_OTC_Veg,
     (1-VA_OTC_seed_bank$seeds_staySB)*(1-seedling_est_VA_OTC_Veg), 
     0,
     sum(VA_WN$number[VA_WN$stage=="continuous"&VA_WN$stageNext=="continuous"], na.rm=T),
     sum(VA_WN$number[VA_WN$stage=="continuous"&VA_WN$stageNext=="dead"], na.rm=T)),
   ncol = 2,
   nrow = 3, 
   dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
   meanToCont = matrix(Seedling_info_VA_mean, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
   sdToCont = matrix(sd_VA, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix. First step without the discrete transitions to check the fit of the matrix - then with discrete transitions to build the actual final matrix.
Pmatrix_VA_WN_precip1 <- makeIPMPmatrix(survObj=so_VA_WN, growObj=go_VA_WN_precip1, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WN_precip2 <- makeIPMPmatrix(survObj=so_VA_WN, growObj=go_VA_WN_precip2, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WN_precip3 <- makeIPMPmatrix(survObj=so_VA_WN, growObj=go_VA_WN_precip3, minSize=minSize, maxSize=maxSize, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_VA_WN_precip1, survObj=so_VA_WN, growObj=go_VA_WN_precip1, dff = VA_WN)
diagnosticsPmatrix(Pmatrix_VA_WN_precip2, survObj=so_VA_WN, growObj=go_VA_WN_precip2, dff = VA_WN)
diagnosticsPmatrix(Pmatrix_VA_WN_precip3, survObj=so_VA_WN, growObj=go_VA_WN_precip3, dff = VA_WN)

Pmatrix_VA_WN_precip1 <- makeIPMPmatrix(survObj=so_VA_WN, growObj=go_VA_WN_precip1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WN, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WN_precip2 <- makeIPMPmatrix(survObj=so_VA_WN, growObj=go_VA_WN_precip2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WN, correction = "constant", nBigMatrix = 100)
Pmatrix_VA_WN_precip3 <- makeIPMPmatrix(survObj=so_VA_WN, growObj=go_VA_WN_precip3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_WN, correction = "constant", nBigMatrix = 100)

x11()
contourPlot2(t(Pmatrix_VA_WN_precip1), Pmatrix_VA_WN_precip1@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 
contourPlot2(t(Pmatrix_VA_WN_precip2), Pmatrix_VA_WN_precip2@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth")
contourPlot2(t(Pmatrix_VA_WN_precip3), Pmatrix_VA_WN_precip3@meshpoints, maxSize, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WN))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = VA_WN))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WN))
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = VA_WN))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WN)) #Choosing this model based on AIC
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WN))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WN)) 
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = VA_WN))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WN))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = VA_WN))

floweringChosenModel_VA_WN <- flo.if ~ size

mod_flo_if_VA_WN <- glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = VA_WN)

plot_VA_WN_floif <- plot_predictions_floif_precip(model = mod_flo_if_VA_WN, data = VA_WN, minSize, maxSize)
plot_VA_WN_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WN))
AIC(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WN))
summary(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WN))
AIC(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = VA_WN))
summary(glm(flo.no ~ size+I(size^2), family = 'poisson', data = VA_WN))
AIC(glm(flo.no ~ size+I(size^2), family = 'poisson', data = VA_WN))
summary(glm(flo.no ~ size, family = 'poisson', data = VA_WN)) #Choosing this model based on AIC
AIC(glm(flo.no ~ size, family = 'poisson', data = VA_WN))
summary(glm(flo.no ~ 1, family = 'poisson', data = VA_WN))
AIC(glm(flo.no ~ 1, family = 'poisson', data = VA_WN))

flowerNumberChosenModel_VA_WN <- flo.no ~ size

mod_flo_no_VA_WN <- glm(flo.no ~ size, family = 'poisson', data = VA_WN)

plot_flo_no_VA_WN <-plot_predictions_flono(model = mod_flo_no_VA_WN, data = VA_WN, minSize, maxSize, ylim = 15) 
plot_flo_no_VA_WN

# Make fecundity object
fo_VA_WN <-makeFecObj(VA_WN, 
                      Formula= c(floweringChosenModel_VA_WN, flowerNumberChosenModel_VA_WN),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_Veg), 
                      meanOffspringSize = Seedling_info_VA_mean,
                      sdOffspringSize = sd_VA,
                      offspringSplitter = data.frame(seedbank=VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_Veg), continuous=(1-(VA_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_VA_OTC_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

#Replace with coefficients form mixed effects models and make different ones for three precipitation levels
fo_VA_WN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_WN)[1]) + 1.2*as.numeric(fixef(mod_flo_if_VA_WN)[3]),
                                       as.numeric(fixef(mod_flo_if_VA_WN)[2]))
fo_VA_WN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_VA_WN)) #not really needed as this is a linear model
fo_VA_WN_precip1 <- fo_VA_WN

fo_VA_WN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_WN)[1]) + 2.3*as.numeric(fixef(mod_flo_if_VA_WN)[3]),
                                       as.numeric(fixef(mod_flo_if_VA_WN)[2]))
fo_VA_WN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_VA_WN)) #not really needed as this is a linear model
fo_VA_WN_precip2 <- fo_VA_WN

fo_VA_WN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_VA_WN)[1]) + 3.4*as.numeric(fixef(mod_flo_if_VA_WN)[3]),
                                       as.numeric(fixef(mod_flo_if_VA_WN)[2]))
fo_VA_WN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_VA_WN)) #not really needed as this is a linear model
fo_VA_WN_precip3 <- fo_VA_WN

#Make F matrix
Fmatrix_VA_WN_precip1 <- makeIPMFmatrix(fecObj=fo_VA_WN_precip1, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_WN_precip2 <- makeIPMFmatrix(fecObj=fo_VA_WN_precip2, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)
Fmatrix_VA_WN_precip3 <- makeIPMFmatrix(fecObj=fo_VA_WN_precip3, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# plotting the F matrix
contourPlot2(t(Fmatrix_VA_WN), Fmatrix_VA_WN@meshpoints, maxSize, 0.03, 0, title = "Fmatrix: flower and seedlings") 

#### C matrix ####

VA_WN_clones <- VA_WN %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Using blockID as a random effect because of singularity fit with other more desirable options.  I have tried block_trans, site_trans, blockID + transition (which worked with the first model, but not the rest), siteID + transition, and transitio before landing on doing it with blockID as random effect.
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), family = 'binomial', data = VA_WN))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), family = 'binomial', data = VA_WN))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|blockID), family = 'binomial', data = VA_WN))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|blockID), family = 'binomial', data = VA_WN))
summary(glmer(clo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_WN)) #Choosing this model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_WN))
summary(glmer(clo.if ~ size + (1|blockID), family = 'binomial', data = VA_WN))
AIC(glmer(clo.if ~ size + (1|blockID), family = 'binomial', data = VA_WN))
summary(glmer(clo.if ~ 1 + (1|blockID), family = 'binomial', data = VA_WN))
AIC(glmer(clo.if ~ 1 + (1|blockID), family = 'binomial', data = VA_WN))

#Checking convergence - looks OK even if the model complains.
glmer(clo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_WN, verbose = TRUE)

#Chosen model
mod_clo_VA_WN <- glmer(clo.if ~ size+I(size^2) + (1|blockID), family = 'binomial', data = VA_WN)
CloneChosenModel_VA_WN <- clo.if ~ size + size2 

#Plot for visual checking
plot_clo_if_VA_WN <- plot_predictions_cloif(model = mod_clo_VA_WN, data = VA_WN, minSize, maxSize)
plot_clo_if_VA_WN

#If you produce clones, does how many clones you make change with size of the mother 
#Using a linear model because of singularity fit with all different combinations of random effects. I have tried block_trans, site_trans, blockID + transition, siteID + transition, transition, siteID, blockID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WN))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = VA_WN))
summary(glm(clo.no ~ size+ precip+I(precip^2), family = 'poisson', data = VA_WN))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = VA_WN))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = VA_WN))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = VA_WN))
summary(glm(clo.no ~ size, family = 'poisson', data = VA_WN))
AIC(glm(clo.no ~ size, family = 'poisson', data = VA_WN))
summary(glm(clo.no ~ 1, family = 'poisson', data = VA_WN)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = VA_WN))

mod_clo_no_VA_WN <- glm(clo.no ~ 1, family = 'poisson', data = VA_WN)
CloneNumberChosenModel_VA_WN <- clo.no ~ 1

plot_clo_no_VA_WN <- plot_predictions_clono(model = mod_clo_no_VA_WN, data = VA_WN, minSize, maxSize, ylim = 6)
plot_clo_no_VA_WN

#Does size of the clone depend on size of parent.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WN_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = VA_WN_clones))
summary(lmer(sizeNext ~ size + I(size^2) + precip + (1|block_trans), data = VA_WN_clones)) 
AIC(lmer(sizeNext ~ size + I(size^2) + precip  + (1|block_trans), data = VA_WN_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WN_clones)) #This model is best AIC wise
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WN_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = VA_WN_clones))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = VA_WN_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WN_clones)) #Using this model because it doesn't make biological sense that the clones would become smaller as the parents increase in size
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = VA_WN_clones))


mod_clone_growth_VA_WN <- lmer(sizeNext ~ size + precip + (1|block_trans), data = VA_WN_clones)
CloneSizeVariable_VA_WN <- "size"

plot_clone_growth_VA_WN <- plot_predictions_growth_precip(model = mod_clone_growth_VA_WN, data = VA_WN_clones, minSize, maxSize)
plot_clone_growth_VA_WN

#Make clonal object
co_VA_WN <- makeClonalObj(VA_WN, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_WN_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_WN, Formula = c(CloneChosenModel_VA_WN, CloneNumberChosenModel_VA_WN),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))


co_VA_WN@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_VA_WN)) 
co_VA_WN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_VA_WN)) #not really needed as this is a linear model
co_VA_WN@sdOffspringSize <- sigma.hat(mod_clone_growth_VA_WN)$sigma$data

co_VA_WN@offspringRel$coefficients <- c(as.numeric(fixef(mod_clone_growth_VA_WN)[1]) + 1.2*as.numeric(fixef(mod_clone_growth_VA_WN)[3]),
                                        as.numeric(fixef(mod_clone_growth_VA_WN)[2]))
co_VA_WN_precip1 <- co_VA_WN

co_VA_WN@offspringRel$coefficients <- c(as.numeric(fixef(mod_clone_growth_VA_WN)[1]) + 2.3*as.numeric(fixef(mod_clone_growth_VA_WN)[3]),
                                        as.numeric(fixef(mod_clone_growth_VA_WN)[2]))
co_VA_WN_precip2 <- co_VA_WN

co_VA_WN@offspringRel$coefficients <- c(as.numeric(fixef(mod_clone_growth_VA_WN)[1]) + 3.4*as.numeric(fixef(mod_clone_growth_VA_WN)[3]),
                                        as.numeric(fixef(mod_clone_growth_VA_WN)[2]))
co_VA_WN_precip3 <- co_VA_WN



Cmatrix_VA_WN_precip1 <- makeIPMCmatrix(clonalObj = co_VA_WN_precip1, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")
Cmatrix_VA_WN_precip2 <- makeIPMCmatrix(clonalObj = co_VA_WN_precip2, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")
Cmatrix_VA_WN_precip3 <- makeIPMCmatrix(clonalObj = co_VA_WN_precip3, minSize=minSize, maxSize=maxSize, nBigMatrix = 100, correction = "constant")


contourPlot2(t(Cmatrix_VA_WN_precip1), Cmatrix_VA_WN_precip1@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones")
contourPlot2(t(Cmatrix_VA_WN_precip2), Cmatrix_VA_WN_precip2@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 
contourPlot2(t(Cmatrix_VA_WN_precip3), Cmatrix_VA_WN_precip3@meshpoints, maxSize, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_VA_WN_precip1 <- Pmatrix_VA_WN_precip1 + Fmatrix_VA_WN_precip1 + Cmatrix_VA_WN_precip1
as.numeric(eigen(IPM_VA_WN_precip1)$value[1])

IPM_VA_WN_precip2 <- Pmatrix_VA_WN_precip2 + Fmatrix_VA_WN_precip2 + Cmatrix_VA_WN_precip2
as.numeric(eigen(IPM_VA_WN_precip2)$value[1])

IPM_VA_WN_precip3 <- Pmatrix_VA_WN_precip3 + Fmatrix_VA_WN_precip3 + Cmatrix_VA_WN_precip3
as.numeric(eigen(IPM_VA_WN_precip3)$value[1])

x11()
contourPlot2(t(IPM_VA_WN_precip1), Pmatrix_VA_WN_precip1@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WN - 1.2 m/year")
contourPlot2(t(IPM_VA_WN_precip2), Pmatrix_VA_WN_precip2@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WN - 2.3 m/year")
contourPlot2(t(IPM_VA_WN_precip3), Pmatrix_VA_WN_precip3@meshpoints, maxSize, 0.03, 0, title = "Veronica alpina - WN - 3.4 m/year")
