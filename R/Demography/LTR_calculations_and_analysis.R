### LTREs
library(reshape2)

#### Color palettes ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_with_white <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571")
palette <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")

# palette with values = c(
#   "-5" = "#543005",
#   "-4" = "#8C510A",
#   "-3" = "#BF812D",
#   "-2" = "#DFC27D",
#   "-1" = "#F6E8C3",
#   "0" = "#F5F5F5",
#   "1" = "#C7EAE5",
#   "2" = "#80CDC1",
#   "3" = "#35978F",
#   "4" = "#01665E",
#   "5" = "#003C30",
#   "Not Sure" = "gainsboro",
#   "Data deficient" = "light grey")) +

#### Making functions ####

### Making function to make growth matrix
growth_matrix <-function(Pmatrix, survival_object, growth_object, minSize, maxSize, discrete_trans) {
  
  growth_only <- coerceSurvObj(survival_object, c(100,0,0,0))
  Pmatrix_growth_only <- makeIPMPmatrix(survObj=growth_only, growObj=growth_object, minSize=minSize, maxSize=maxSize, discreteTrans = discrete_trans, correction = "constant", nBigMatrix = 100)
  
  return(Pmatrix_growth_only)
}


###Making general function for calculating LTREs
LTRE_calcultations <-function(IPM1, IPM2, Fmatrix1, Fmatrix2, Pmatrix1, Pmatrix2, Cmatrix1, Cmatrix2, survival_object1, survival_object2, growth_object1, growth_object2, minSize, maxSize, discrete_trans1, discrete_trans2) {

  #Making a matrix with the different between the two matrixes  
  BaseIPM <- (IPM1 + IPM2) / 2
  SBaseIPM <- sens(BaseIPM)
  
  #Calculating difference in fecundity
  Difference_fec <- Fmatrix1 - Fmatrix2
  Contributions_fec <- Difference_fec*SBaseIPM
  con_fec <- sum(Contributions_fec)
   
  #Calculating difference in clonality
  Difference_clone <- Cmatrix1 - Cmatrix2
  Contributions_clone = Difference_clone*SBaseIPM
  con_clone <- sum(Contributions_clone)
   
  #Calculating difference in survival and growth
  Difference_pm <- Pmatrix1 - Pmatrix2
  Contributions_pm = Difference_pm * SBaseIPM
  con_pm <- sum(Contributions_pm)
  
  #Calculating contribution of growth alone
  growth_only1 <- coerceSurvObj(survival_object1, c(100,0,0,0))
  Pmatrix_growth_only1 <- makeIPMPmatrix(survObj=growth_only1, growObj=growth_object1, minSize=minSize, maxSize=maxSize, discreteTrans = discrete_trans1, correction = "constant", nBigMatrix = 100)
  
  growth_only2 <- coerceSurvObj(survival_object2, c(100,0,0,0))
  Pmatrix_growth_only2 <- makeIPMPmatrix(survObj=growth_only2, growObj=growth_object2, minSize=minSize, maxSize=maxSize, discreteTrans = discrete_trans2, correction = "constant", nBigMatrix = 100)
  
  Difference_growth <- Pmatrix_growth_only1 - Pmatrix_growth_only2
  
  Contributions_growth = Difference_growth*SBaseIPM
  con_growth <- sum(Contributions_growth)
  
  # Calculating contribution of survival alone
  Contributions_surv <- Contributions_pm - Contributions_growth
  con_surv <- sum(Contributions_surv)
  
  # Summing up all vital rates
  total_con <- sum(con_fec, con_clone, con_growth, con_surv)
  
  #Difference in lambda
  lamda_diff <- as.numeric(eigen(IPM1)$value[1])-as.numeric(eigen(IPM2)$value[1])
  
  contributions <- c(con_fec, con_clone, con_growth, con_surv)
  vital_rates <- c("fecundity", "clonality", "growth", "surv")
  
  vital_rate_contributions <- as.data.frame(contributions, vital_rates) %>% 
    rownames_to_column(var = "vital_rates") %>%
    mutate(total_contribution = total_con,
           lamda_difference = lamda_diff)
  
  
  return(vital_rate_contributions)
}


###Making general function for calculating LTREs - using growth Matrix and not growth object
LTRE_calcultations_Growth <-function(IPM1, IPM2, Fmatrix1, Fmatrix2, Pmatrix1, Pmatrix2, Cmatrix1, Cmatrix2, Growth_matrix1, Growth_matrix2) {
  
  #Making a matrix with the different between the two matrixes  
  BaseIPM <- (IPM1 + IPM2) / 2
  SBaseIPM <- sens(BaseIPM)
  
  #Calculating difference in fecundity
  Difference_fec <- Fmatrix1 - Fmatrix2
  Contributions_fec <- Difference_fec*SBaseIPM
  con_fec <- sum(Contributions_fec)
  
  #Calculating difference in clonality
  Difference_clone <- Cmatrix1 - Cmatrix2
  Contributions_clone = Difference_clone*SBaseIPM
  con_clone <- sum(Contributions_clone)
  
  #Calculating difference in survival and growth
  Difference_pm <- Pmatrix1 - Pmatrix2
  Contributions_pm = Difference_pm * SBaseIPM
  con_pm <- sum(Contributions_pm)
  
  #Calculating contribution of growth alone
  Difference_growth <- Growth_matrix1 - Growth_matrix2
  Contributions_growth = Difference_growth*SBaseIPM
  con_growth <- sum(Contributions_growth)
  
  # Calculating contribution of survival alone
  Contributions_surv <- Contributions_pm - Contributions_growth
  con_surv <- sum(Contributions_surv)
  
  # Summing up all vital rates
  total_con <- sum(con_fec, con_clone, con_growth, con_surv)
  
  #Difference in lambda
  lamda_diff <- as.numeric(eigen(IPM1)$value[1])-as.numeric(eigen(IPM2)$value[1])
  
  contributions <- c(con_fec, con_clone, con_growth, con_surv)
  vital_rates <- c("fecundity", "clonality", "growth", "surv")
  
  vital_rate_contributions <- as.data.frame(contributions, vital_rates) %>% 
    rownames_to_column(var = "vital_rates") %>%
    mutate(total_contribution = total_con,
           lamda_difference = lamda_diff)
  
  
  return(vital_rate_contributions)
}


#### Treatments ####

##### Veronica alpina #####

#Making LTRE comparisons between different treatments

LTRE_VA_CC_CR_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_CR_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_CR, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_CR_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_CR, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CR_precip1, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CR_precip1, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CR, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CR - CC",
         precipitation = "1")

LTRE_VA_CC_CR_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_CR_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CR, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_CR_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CR, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CR_precip2, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CR_precip2, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CR, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CR - CC",
         precipitation = "2")


LTRE_VA_CC_CR_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_CR_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_CR, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_CR_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_CR, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CR_precip3, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CR_precip3, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CR, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CR - CC",
         precipitation = "3")

LTRE_VA_CC_CN_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_CN_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_CN_precip1, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_CN_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_CN, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CN, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CN_precip1, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CN, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CN - CC",
         precipitation = "1")

LTRE_VA_CC_CN_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_CN_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CN_precip2, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_CN_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CN, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CN, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CN_precip2, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CN, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CN - CC",
         precipitation = "2")

LTRE_VA_CC_CN_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_CN_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_CN_precip3, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_CN_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_CN, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CN, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CN_precip3, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CN, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CN - CC",
         precipitation = "3")

LTRE_VA_CC_CE_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_CE_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_CE, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_CE_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_CE, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CE_precip1, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CE_precip1, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CE, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CE - CC",
         precipitation = "1")

LTRE_VA_CC_CE_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_CE_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CE, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_CE_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CE, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CE_precip2, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CE_precip2, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CE, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CE - CC",
         precipitation = "2")

LTRE_VA_CC_CE_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_CE_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_CE, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_CE_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_CE, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CE_precip3, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CE_precip3, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CE, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CE - CC",
         precipitation = "3")

LTRE_VA_CC_WC_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WC_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WC, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_WC, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_WC_precip1, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WC, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WC, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WC, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WC - CC",
         precipitation = "1")

LTRE_VA_CC_WC_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WC_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WC, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_WC, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_WC_precip2, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WC, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WC, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WC, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WC - CC",
         precipitation = "2")

LTRE_VA_CC_WC_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WC_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WC, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_WC, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_WC_precip3, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WC, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WC, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WC, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WC - CC",
         precipitation = "3")

LTRE_VA_CC_WC_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WC_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WC, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_WC, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_WC_precip3, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WC, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WC, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WC, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WC - CC",
         precipitation = "3")

LTRE_VA_CC_WE_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WE_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WE, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_WE_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_WE_precip1, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WE, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WE, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WE, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WE - CC",
         precipitation = "1")

LTRE_VA_CC_WE_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WE_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WE, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_WE_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_WE_precip2, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WE, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WE, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WE, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WE - CC",
         precipitation = "2")

LTRE_VA_CC_WE_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WE_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WE, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_WE_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_WE_precip3, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WE, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WE, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WE, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WE - CC",
         precipitation = "3")

LTRE_VA_CC_WN_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WN_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WN_precip1, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_WN_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_WN, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WN, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WN_precip1, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WN, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WN - CC",
         precipitation = "1")

LTRE_VA_CC_WN_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WN_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WN_precip2, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_WN_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_WN, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WN, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WN_precip2, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WN, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WN - CC",
         precipitation = "2")

LTRE_VA_CC_WN_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WN_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WN_precip3, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_WN_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_WN, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WN, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WN_precip3, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WN, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WN - CC",
         precipitation = "3")

LTRE_VA_CC_WR_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip1, 
                                             IPM2 = IPM_VA_CC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip1, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip1, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip1, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip1, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WR_precip1, 
                                             growth_object2 = go_VA_CC_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - CC",
         precipitation = "1")

LTRE_VA_CC_WR_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip2, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip2, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip2, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WR_precip2, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - CC",
         precipitation = "2")

LTRE_VA_CC_WR_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip3, 
                                             IPM2 = IPM_VA_CC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip3, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip3, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip3, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip3, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_WR_precip3, 
                                             growth_object2 = go_VA_CC_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - CC",
         precipitation = "3")

LTRE_VA_WC_WR_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip1, 
                                             IPM2 = IPM_VA_WC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip1, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip1, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip1, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip1, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WR_precip1, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - WC",
         precipitation = "1")

LTRE_VA_WC_WR_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip2, 
                                             IPM2 = IPM_VA_WC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip2, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip2, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip2, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip2, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WR_precip2, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - WC",
         precipitation = "2")

LTRE_VA_WC_WR_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip3, 
                                             IPM2 = IPM_VA_WC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip3, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip3, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip3, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip3, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WR_precip3, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - WC",
         precipitation = "3")

LTRE_VA_WC_WE_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WE_precip1, 
                                             IPM2 = IPM_VA_WC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WE, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WE_precip1, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WE_precip1, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip1, 
                                             survival_object1 = so_VA_WE, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WE_precip1, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WE, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WE - WC",
         precipitation = "1")

LTRE_VA_WC_WE_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WE_precip2, 
                                             IPM2 = IPM_VA_WC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WE, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WE_precip2, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WE_precip2, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip2, 
                                             survival_object1 = so_VA_WE, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WE_precip2, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WE, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WE - WC",
         precipitation = "2")

LTRE_VA_WC_WE_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WE_precip3, 
                                             IPM2 = IPM_VA_WC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WE, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WE_precip3, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WE_precip3, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip3, 
                                             survival_object1 = so_VA_WE, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WE_precip3, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WE, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WE - WC",
         precipitation = "3")

LTRE_VA_WC_WN_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WN_precip1, 
                                             IPM2 = IPM_VA_WC_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WN_precip1, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WN_precip1, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WN, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip1, 
                                             survival_object1 = so_VA_WN, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WN_precip1, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WN, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WN - WC",
         precipitation = "1")


LTRE_VA_WC_WN_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WN_precip2, 
                                             IPM2 = IPM_VA_WC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WN_precip2, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WN_precip2, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WN, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip2, 
                                             survival_object1 = so_VA_WN, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WN_precip2, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WN, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WN - WC",
         precipitation = "2")

LTRE_VA_WC_WN_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WN_precip3, 
                                             IPM2 = IPM_VA_WC_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WN_precip3, 
                                             Fmatrix2 = Fmatrix_VA_WC, 
                                             Pmatrix1 = Pmatrix_VA_WN_precip3, 
                                             Pmatrix2 = Pmatrix_VA_WC, 
                                             Cmatrix1 = Cmatrix_VA_WN, 
                                             Cmatrix2 = Cmatrix_VA_WC_precip3, 
                                             survival_object1 = so_VA_WN, 
                                             survival_object2 = so_VA_WC, 
                                             growth_object1 = go_VA_WN_precip3, 
                                             growth_object2 = go_VA_WC, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WN, 
                                             discrete_trans2 = dto_VA_WC) %>% 
  mutate(species = "Ver_alp",
         treatment = "WN - WC",
         precipitation = "3")

LTRE_VA_CR_WR_precip_1 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip1, 
                                             IPM2 = IPM_VA_CR_precip1, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip1, 
                                             Fmatrix2 = Fmatrix_VA_CR, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CR_precip1, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip1, 
                                             Cmatrix2 = Cmatrix_VA_CR, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_CR_precip1, 
                                             growth_object1 = go_VA_WR_precip1, 
                                             growth_object2 = go_VA_CR_precip1, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_CR) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - CR",
         precipitation = "1")

LTRE_VA_CR_WR_precip_2 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip2, 
                                             IPM2 = IPM_VA_CR_precip2, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip2, 
                                             Fmatrix2 = Fmatrix_VA_CR, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip2, 
                                             Pmatrix2 = Pmatrix_VA_CR_precip2, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip2, 
                                             Cmatrix2 = Cmatrix_VA_CR, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_CR_precip2, 
                                             growth_object1 = go_VA_WR_precip2, 
                                             growth_object2 = go_VA_CR_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_CR) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - CR",
         precipitation = "2")

LTRE_VA_CR_WR_precip_3 <- LTRE_calcultations(IPM1 = IPM_VA_WR_precip3, 
                                             IPM2 = IPM_VA_CR_precip3, 
                                             Fmatrix1 = Fmatrix_VA_WR_precip3, 
                                             Fmatrix2 = Fmatrix_VA_CR, 
                                             Pmatrix1 = Pmatrix_VA_WR_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CR_precip3, 
                                             Cmatrix1 = Cmatrix_VA_WR_precip3, 
                                             Cmatrix2 = Cmatrix_VA_CR, 
                                             survival_object1 = so_VA_WR, 
                                             survival_object2 = so_VA_CR_precip3, 
                                             growth_object1 = go_VA_WR_precip3, 
                                             growth_object2 = go_VA_CR_precip3, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_WR, 
                                             discrete_trans2 = dto_VA_CR) %>% 
  mutate(species = "Ver_alp",
         treatment = "WR - CR",
         precipitation = "3")

##### Sibbaldia #####

LTRE_SP_CC_CR <- LTRE_calcultations(IPM1 = IPM_SP_CR, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CR, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CR, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CR, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CR, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CR, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CR, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CR - CC",
         precipitation = "2")

LTRE_SP_CC_CN_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_CN_precip1, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CN_precip1, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CN_precip1, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CN, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CN_precip1, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CN, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CN, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CN - CC",
         precipitation = "1")

LTRE_SP_CC_CN_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_CN_precip2, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CN_precip2, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CN_precip2, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CN, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CN_precip2, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CN, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CN, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CN - CC",
         precipitation = "2")

LTRE_SP_CC_CN_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_CN_precip3, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CN_precip3, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CN_precip3, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CN, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CN_precip3, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CN, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CN, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CN - CC",
         precipitation = "3")


LTRE_SP_CC_CE_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_CE_precip1, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CE_precip1, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CE_precip1, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CE, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CE_precip1, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CE, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CE, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CE - CC",
         precipitation = "1")

LTRE_SP_CC_CE_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_CE_precip2, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CE_precip2, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CE_precip2, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CE, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CE_precip2, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CE, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CE, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CE - CC",
         precipitation = "2")

LTRE_SP_CC_CE_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_CE_precip3, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_CE_precip3, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_CE_precip3, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_CE, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_CE_precip3, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_CE, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_CE, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CE - CC",
         precipitation = "3")

LTRE_SP_CC_WC_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WC_precip1, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WC_precip1, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WC, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WC, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WC, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WC, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WC, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WC - CC",
         precipitation = "1")

LTRE_SP_CC_WC_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WC_precip2, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WC_precip2, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WC, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WC, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WC, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WC, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WC, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WC - CC",
         precipitation = "2")

LTRE_SP_CC_WC_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WC_precip3, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WC_precip3, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WC, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WC, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WC, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WC, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WC, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WC - CC",
         precipitation = "3")

LTRE_SP_CC_WE_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WE_precip1, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WE_precip1, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WE, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WE_precip1, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WE, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WE, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WE, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WE - CC",
         precipitation = "1")

LTRE_SP_CC_WE_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WE_precip2, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WE_precip2, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WE, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WE_precip2, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WE, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WE, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WE, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WE - CC",
         precipitation = "2")

LTRE_SP_CC_WE_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WE_precip3, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WE_precip3, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WE, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WE_precip3, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WE, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WE, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WE, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WE - CC",
         precipitation = "3")

LTRE_SP_CC_WN <- LTRE_calcultations(IPM1 = IPM_SP_WN, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WN, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WN, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WN, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WN, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WN, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WN, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WN - CC",
         precipitation = "2")

LTRE_SP_CC_WR_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip1, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip1, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - CC",
         precipitation = "1")

LTRE_SP_CC_WR_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip2, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip2, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - CC",
         precipitation = "2")

LTRE_SP_CC_WR_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip3, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip3, 
                                             Fmatrix2 = Fmatrix_SP_CC, 
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_CC, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_CC, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_CC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - CC",
         precipitation = "3")

LTRE_SP_WC_WR_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip1, 
                                             IPM2 = IPM_SP_WC_precip1, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip1, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip1, 
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - WC",
         precipitation = "1")

LTRE_SP_WC_WR_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip2, 
                                             IPM2 = IPM_SP_WC_precip2, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip2, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip2, 
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - WC",
         precipitation = "2")

LTRE_SP_WC_WR_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip3, 
                                             IPM2 = IPM_SP_WC_precip3, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip3, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip3, 
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - WC",
         precipitation = "3")

LTRE_SP_WC_WE_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WE_precip1, 
                                             IPM2 = IPM_SP_WC_precip1, 
                                             Fmatrix1 = Fmatrix_SP_WE_precip1, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip1, 
                                             Pmatrix1 = Pmatrix_SP_WE, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WE_precip1, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WE, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WE, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WE, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WE - WC",
         precipitation = "1")

LTRE_SP_WC_WE_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WE_precip2, 
                                             IPM2 = IPM_SP_WC_precip2, 
                                             Fmatrix1 = Fmatrix_SP_WE_precip2, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip2, 
                                             Pmatrix1 = Pmatrix_SP_WE, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WE_precip2, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WE, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WE, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WE, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WE - WC",
         precipitation = "2")

LTRE_SP_WC_WE_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WE_precip3, 
                                             IPM2 = IPM_SP_WC_precip3, 
                                             Fmatrix1 = Fmatrix_SP_WE_precip3, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip3, 
                                             Pmatrix1 = Pmatrix_SP_WE, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WE_precip3, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WE, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WE, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WE, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WE - WC",
         precipitation = "3")

LTRE_SP_WC_WN_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WN, 
                                             IPM2 = IPM_SP_WC_precip1, 
                                             Fmatrix1 = Fmatrix_SP_WN, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip1, 
                                             Pmatrix1 = Pmatrix_SP_WN, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WN, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WN, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WN, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WN, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WN - WC",
         precipitation = "1")

LTRE_SP_WC_WN_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WN, 
                                             IPM2 = IPM_SP_WC_precip2, 
                                             Fmatrix1 = Fmatrix_SP_WN, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip2, 
                                             Pmatrix1 = Pmatrix_SP_WN, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WN, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WN, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WN, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WN, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WN - WC",
         precipitation = "2")

LTRE_SP_WC_WN_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WN, 
                                             IPM2 = IPM_SP_WC_precip3, 
                                             Fmatrix1 = Fmatrix_SP_WN, 
                                             Fmatrix2 = Fmatrix_SP_WC_precip3, 
                                             Pmatrix1 = Pmatrix_SP_WN, 
                                             Pmatrix2 = Pmatrix_SP_WC, 
                                             Cmatrix1 = Cmatrix_SP_WN, 
                                             Cmatrix2 = Cmatrix_SP_WC, 
                                             survival_object1 = so_SP_WN, 
                                             survival_object2 = so_SP_WC, 
                                             growth_object1 = go_SP_WN, 
                                             growth_object2 = go_SP_WC, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WN, 
                                             discrete_trans2 = dto_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WN - WC",
         precipitation = "3")

LTRE_SP_CR_WR_precip_1 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip1, 
                                             IPM2 = IPM_SP_CR, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip1, 
                                             Fmatrix2 = Fmatrix_SP_CR,
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_CR, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_CR, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_CR, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_CR, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_CR) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - CR",
         precipitation = "1")

LTRE_SP_CR_WR_precip_2 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip2, 
                                             IPM2 = IPM_SP_CR, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip2, 
                                             Fmatrix2 = Fmatrix_SP_CR,
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_CR, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_CR, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_CR, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_CR, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_CR) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - CR",
         precipitation = "2")


LTRE_SP_CR_WR_precip_3 <- LTRE_calcultations(IPM1 = IPM_SP_WR_precip3, 
                                             IPM2 = IPM_SP_CR, 
                                             Fmatrix1 = Fmatrix_SP_WR_precip3, 
                                             Fmatrix2 = Fmatrix_SP_CR,
                                             Pmatrix1 = Pmatrix_SP_WR, 
                                             Pmatrix2 = Pmatrix_SP_CR, 
                                             Cmatrix1 = Cmatrix_SP_WR, 
                                             Cmatrix2 = Cmatrix_SP_CR, 
                                             survival_object1 = so_SP_WR, 
                                             survival_object2 = so_SP_CR, 
                                             growth_object1 = go_SP_WR, 
                                             growth_object2 = go_SP_CR, 
                                             minSize = minSize_SP, 
                                             maxSize = maxSize_SP, 
                                             discrete_trans1 = dto_SP_WR, 
                                             discrete_trans2 = dto_SP_CR) %>% 
  mutate(species = "Sib_pro",
         treatment = "WR - CR",
         precipitation = "3")


##### Making LTRE data phrame #####

LTRE_treatments <- LTRE_VA_CC_CR_precip_1 %>% 
  bind_rows(LTRE_VA_CC_CR_precip_2) %>% 
  bind_rows(LTRE_VA_CC_CR_precip_3) %>% 
  bind_rows(LTRE_VA_CC_CN_precip_1) %>% 
  bind_rows(LTRE_VA_CC_CN_precip_2) %>% 
  bind_rows(LTRE_VA_CC_CN_precip_3) %>% 
  bind_rows(LTRE_VA_CC_CE_precip_1) %>% 
  bind_rows(LTRE_VA_CC_CE_precip_2) %>% 
  bind_rows(LTRE_VA_CC_CE_precip_3) %>% 
  bind_rows(LTRE_VA_CC_WC_precip_1) %>% 
  bind_rows(LTRE_VA_CC_WC_precip_2) %>% 
  bind_rows(LTRE_VA_CC_WC_precip_3) %>%
  bind_rows(LTRE_VA_CC_WE_precip_1) %>% 
  bind_rows(LTRE_VA_CC_WE_precip_2) %>% 
  bind_rows(LTRE_VA_CC_WE_precip_3) %>%
  bind_rows(LTRE_VA_CC_WN_precip_1) %>% 
  bind_rows(LTRE_VA_CC_WN_precip_2) %>%
  bind_rows(LTRE_VA_CC_WN_precip_3) %>% 
  bind_rows(LTRE_VA_CC_WR_precip_1) %>%
  bind_rows(LTRE_VA_CC_WR_precip_2) %>%
  bind_rows(LTRE_VA_CC_WR_precip_3) %>% 
  bind_rows(LTRE_VA_WC_WR_precip_1) %>%
  bind_rows(LTRE_VA_WC_WR_precip_2) %>%
  bind_rows(LTRE_VA_WC_WR_precip_3) %>% 
  bind_rows(LTRE_VA_WC_WE_precip_1) %>%
  bind_rows(LTRE_VA_WC_WE_precip_2) %>%
  bind_rows(LTRE_VA_WC_WE_precip_3) %>% 
  bind_rows(LTRE_VA_WC_WN_precip_1) %>%
  bind_rows(LTRE_VA_WC_WN_precip_2) %>%
  bind_rows(LTRE_VA_WC_WN_precip_3) %>% 
  bind_rows(LTRE_VA_CR_WR_precip_1) %>% 
  bind_rows(LTRE_VA_CR_WR_precip_2) %>% 
  bind_rows(LTRE_VA_CR_WR_precip_3) %>% 
  bind_rows(LTRE_SP_CC_CR) %>% 
  bind_rows(LTRE_SP_CC_CN_precip_1) %>% 
  bind_rows(LTRE_SP_CC_CN_precip_2) %>% 
  bind_rows(LTRE_SP_CC_CN_precip_3) %>% 
  bind_rows(LTRE_SP_CC_CE_precip_1) %>% 
  bind_rows(LTRE_SP_CC_CE_precip_2) %>% 
  bind_rows(LTRE_SP_CC_CE_precip_3) %>% 
  bind_rows(LTRE_SP_CC_WC_precip_1) %>% 
  bind_rows(LTRE_SP_CC_WC_precip_2) %>% 
  bind_rows(LTRE_SP_CC_WC_precip_3) %>%
  bind_rows(LTRE_SP_CC_WE_precip_1) %>% 
  bind_rows(LTRE_SP_CC_WE_precip_2) %>% 
  bind_rows(LTRE_SP_CC_WE_precip_3) %>%
  bind_rows(LTRE_SP_CC_WN) %>% 
  bind_rows(LTRE_SP_CC_WR_precip_1) %>%
  bind_rows(LTRE_SP_CC_WR_precip_2) %>%
  bind_rows(LTRE_SP_CC_WR_precip_3) %>% 
  bind_rows(LTRE_SP_WC_WR_precip_1) %>%
  bind_rows(LTRE_SP_WC_WR_precip_2) %>%
  bind_rows(LTRE_SP_WC_WR_precip_3) %>% 
  bind_rows(LTRE_SP_WC_WE_precip_1) %>%
  bind_rows(LTRE_SP_WC_WE_precip_2) %>%
  bind_rows(LTRE_SP_WC_WE_precip_3) %>% 
  bind_rows(LTRE_SP_WC_WN_precip_1) %>%
  bind_rows(LTRE_SP_WC_WN_precip_2) %>%
  bind_rows(LTRE_SP_WC_WN_precip_3) %>% 
  bind_rows(LTRE_SP_CR_WR_precip_1) %>% 
  bind_rows(LTRE_SP_CR_WR_precip_2) %>% 
  bind_rows(LTRE_SP_CR_WR_precip_3)


##### Making plots #####

# Prediction specific plots
Ver_alp_LTRE_predictions <- LTRE_treatments %>% 
  filter(species == "Ver_alp") %>% 
  filter(treatment %in% c("WR - CR", "CR - CC", "WC - CC", "WR - WC", "CE - CC", "CN - CC", "WE - WC", "WN - WC")) %>% 
  mutate(treatment = factor(treatment, levels = c("WC - CC", "WR - CR", "CR - CC",  "WR - WC", "CE - CC", "CN - CC", "WE - WC", "WN - WC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference, color = "lambda difference")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 4) +
  theme_bw() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = "#000000") +
  #scale_fill_manual(values = c("#DEDEDE", "#A1A1A1", "#6E6E6E", "#000000")) +
  #scale_color_manual(values = "#B50E00") +
  ggtitle("Vital rate contribution to lambda difference in Veronica alpina") +
  ylab("Change in population growth rate (λ)") +
  xlab("Annual precipitation (m/year)")

Sib_pro_LTRE_predictions <- LTRE_treatments %>% 
  filter(species == "Sib_pro") %>% 
  filter(treatment %in% c("WR - CR", "CR - CC", "WC - CC", "WR - WC", "CE - CC", "CN - CC", "WE - WC", "WN - WC")) %>% 
  mutate(treatment = factor(treatment, levels = c("WC - CC", "WR - CR", "CR - CC",  "WR - WC", "CE - CC", "CN - CC", "WE - WC", "WN - WC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference, color = "lambda difference")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 4) +
  theme_bw() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = "#000000") +
  #scale_fill_manual(values = c("#DEDEDE", "#A1A1A1", "#6E6E6E", "#000000")) +
  #scale_color_manual(values = "#B50E00") +
  ggtitle("Vital rate contribution to lambda difference in Sibbaldia procumbens") +
  ylab("Change in population growth rate (λ)") +
  xlab("Annual precipitation (m/year)")


Ver_alp_LTRE_all_treatments <- LTRE_treatments %>% 
  filter(species == "Ver_alp") %>% 
  filter(treatment != "WR - CR") %>% 
  mutate(treatment = factor(treatment, levels = c("CR - CC", "CE - CC", "CN - CC", "WR - CC", "WE - CC", "WN - CC", "WR - WC", "WE - WC", "WN - WC", "WC - CC"))) %>% 
ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference, color = "lambda difference")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 3) +
  theme_bw() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = "#000000") +
  #scale_fill_manual(values = c("#DEDEDE", "#A1A1A1", "#6E6E6E", "#000000")) +
  #scale_color_manual(values = "#B50E00") +
  ggtitle("Vital rate contribution to lambda difference in Veronica alpina") +
  ylab("Change in population growth rate (λ)") +
  xlab("Annual precipitation (m/year)")

Ver_alp_LTRE_CC_comparison <- LTRE_treatments %>% 
  filter(species == "Ver_alp") %>% 
  filter(treatment != "WR - CR") %>% 
  filter(treatment %in% c("CR - CC", "CE - CC", "CN - CC", "WR - CC", "WE - CC", "WN - CC", "WC - CC")) %>% 
  mutate(treatment = factor(treatment, levels = c("CR - CC", "CE - CC", "CN - CC", "WC - CC","WR - CC", "WE - CC", "WN - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference, color = "lambda difference")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 4) +
  theme_bw() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = "#000000") +
  #scale_fill_manual(values = c("#DEDEDE", "#A1A1A1", "#6E6E6E", "#000000")) +
  #scale_color_manual(values = "#B50E00") +
  ggtitle("Vital rate contribution to lambda difference in Veronica alpina")+
  ylab("Change in population growth rate (λ)") +
  xlab("Annual precipitation (m/year)")

Sib_pro_LTRE_all_treatments <- LTRE_treatments %>% 
  filter(species == "Sib_pro") %>% 
  filter(treatment != "WR - CR") %>% 
  mutate(treatment = factor(treatment, levels = c("CR - CC", "CE - CC", "CN - CC", "WR - CC", "WE - CC", "WN - CC","WR - WC", "WE - WC", "WN - WC", "WC - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference, color = "lambda difference")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 3) +
  theme_bw() +
  scale_fill_manual(values = palette) +
  #scale_fill_viridis_d()+
  scale_color_manual(values = "#000000") +
  #scale_fill_manual(values = c("#DEDEDE", "#A1A1A1", "#6E6E6E", "#000000")) +
  #scale_color_manual(values = "#B50E00") +
  ggtitle("Vital rate contribution to lambda difference in Sibbaldia procumbens") +
  ylab("Change in population growth rate (λ)") +
  xlab("Annual precipitation (m/year)")

Sib_pro_LTRE_CC_comparisons <- LTRE_treatments %>% 
  filter(species == "Sib_pro") %>% 
  filter(treatment != "WR - CR") %>% 
  filter(treatment %in% c("CR - CC", "CE - CC", "CN - CC", "WR - CC", "WE - CC", "WN - CC", "WC - CC")) %>% 
  mutate(treatment = factor(treatment, levels = c("CR - CC", "CE - CC", "CN - CC", "WC - CC","WR - CC", "WE - CC", "WN - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference, color = "lambda difference")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 4) +
  theme_bw() +
  scale_fill_manual(values = palette) +
  #scale_fill_viridis_d()+
  scale_color_manual(values = "#000000") +
  #scale_fill_manual(values = c("#DEDEDE", "#A1A1A1", "#6E6E6E", "#000000")) +
  #scale_color_manual(values = "#B50E00") +
  ggtitle("Vital rate contribution to lambda difference in Sibbaldia procumbens") +
  ylab("Change in population growth rate (λ)") +
  xlab("Annual precipitation (m/year)")


Susanne_plot_ver_alp <- LTRE_treatments %>% 
  filter(species == "Ver_alp") %>% 
  mutate(treatment = factor(treatment, levels = c("CE - CC", "CN - CC", "CR - CC", "WE - CC", "WN - CC", "WR - CC", "WE - WC", "WN - WC", "WR - WC", "WC - CC"))) %>% 
  filter(treatment %in% c("WC - CC", "CN - CC", "WN - CC", "WN - WC")) %>% 
  mutate(treatment = factor(treatment, levels = c("WC - CC", "CN - CC", "WN - WC", "WN - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 4) +
  theme_bw() +
  scale_fill_manual(values = c("#DEDEDE","#6D64E4", "#A1A1A1", "#6E6E6E"))

#ggsave(Susanne_plot_ver_alp, filename = "Ver_alp_fecundity.pdf", width = 24, height = 10, units = "cm")

Susanne_plot_sib_pro <- LTRE_treatments %>% 
  filter(species == "Sib_pro") %>% 
  mutate(treatment = factor(treatment, levels = c("CE - CC", "CN - CC", "CR - CC", "WE - CC", "WN - CC", "WR - CC", "WE - WC", "WN - WC", "WR - WC", "WC - CC"))) %>% 
  filter(treatment %in% c("CN - CC", "WC - CC", "WN - CC", "WN - WC")) %>% 
  mutate(treatment = factor(treatment, levels = c("WC - CC", "CN - CC", "WN - WC", "WN - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 4) +
  theme_bw() +
  scale_fill_manual(values = c("#DEDEDE","#FFC300", "#A1A1A1", "#6E6E6E"))

#ggsave(Susanne_plot_sib_pro, filename = "Sib_pro_fecundity.pdf", width = 24, height = 10, units = "cm")

#### Precipitation ####
#Making LTRE comparisons between precipitation levels within treatments

LTRE_VA_CC_precip_1_2 <- LTRE_calcultations (IPM1 = IPM_VA_CC_precip1, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CC_precip1, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_CC_precip1, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CC, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CC, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CC_precip1, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CC, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CC",
         precipitation = "2-1")


LTRE_VA_CC_precip_2_3 <- LTRE_calcultations (IPM1 = IPM_VA_CC_precip3, 
                                             IPM2 = IPM_VA_CC_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CC_precip3, 
                                             Fmatrix2 = Fmatrix_VA_CC_precip2, 
                                             Pmatrix1 = Pmatrix_VA_CC_precip3, 
                                             Pmatrix2 = Pmatrix_VA_CC_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CC, 
                                             Cmatrix2 = Cmatrix_VA_CC, 
                                             survival_object1 = so_VA_CC, 
                                             survival_object2 = so_VA_CC, 
                                             growth_object1 = go_VA_CC_precip3, 
                                             growth_object2 = go_VA_CC_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CC, 
                                             discrete_trans2 = dto_VA_CC) %>% 
  mutate(species = "Ver_alp",
         treatment = "CC",
         precipitation = "2-3")

LTRE_VA_CE_precip_1_2 <- LTRE_calcultations (IPM1 = IPM_VA_CE_precip1, 
                                             IPM2 = IPM_VA_CE_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CE, 
                                             Fmatrix2 = Fmatrix_VA_CE,
                                             Pmatrix1 = Pmatrix_VA_CE_precip1,
                                             Pmatrix2 = Pmatrix_VA_CE_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CE, 
                                             Cmatrix2 = Cmatrix_VA_CE, 
                                             survival_object1 = so_VA_CE_precip1, 
                                             survival_object2 = so_VA_CE_precip2, 
                                             growth_object1 = go_VA_CE_precip1, 
                                             growth_object2 = go_VA_CE_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CE, 
                                             discrete_trans2 = dto_VA_CE) %>% 
  mutate(species = "Ver_alp",
         treatment = "CE",
         precipitation = "2-1")

LTRE_VA_CE_precip_2_3 <- LTRE_calcultations (IPM1 = IPM_VA_CE_precip3, 
                                             IPM2 = IPM_VA_CE_precip2, 
                                             Fmatrix1 = Fmatrix_VA_CE, 
                                             Fmatrix2 = Fmatrix_VA_CE,
                                             Pmatrix1 = Pmatrix_VA_CE_precip3,
                                             Pmatrix2 = Pmatrix_VA_CE_precip2, 
                                             Cmatrix1 = Cmatrix_VA_CE, 
                                             Cmatrix2 = Cmatrix_VA_CE, 
                                             survival_object1 = so_VA_CE_precip3, 
                                             survival_object2 = so_VA_CE_precip2, 
                                             growth_object1 = go_VA_CE_precip3, 
                                             growth_object2 = go_VA_CE_precip2, 
                                             minSize = minSize, 
                                             maxSize = maxSize, 
                                             discrete_trans1 = dto_VA_CE, 
                                             discrete_trans2 = dto_VA_CE) %>% 
  mutate(species = "Ver_alp",
         treatment = "CE",
         precipitation = "2-3")

LTRE_precip <- LTRE_VA_CC_precip_1_2 %>% 
  bind_rows(LTRE_VA_CC_precip_2_3) %>% 
  bind_rows(LTRE_VA_CE_precip_1_2) %>% 
  bind_rows(LTRE_VA_CE_precip_2_3)

LTRE_precip %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 3) +
  theme_bw()

#### Susanne sine gjennomsnittsplot ####

growth_VA_CC_precip1 <- growth_matrix(Pmatrix = Pmatrix_VA_CC_precip1,
                                      survival_object = so_VA_CC,
                                      growth_object = go_VA_CC_precip1,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_CC)

growth_VA_CC_precip2 <- growth_matrix(Pmatrix = Pmatrix_VA_CC_precip2,
                                      survival_object = so_VA_CC,
                                      growth_object = go_VA_CC_precip2,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_CC)

growth_VA_CC_precip3 <- growth_matrix(Pmatrix = Pmatrix_VA_CC_precip3,
                                      survival_object = so_VA_CC,
                                      growth_object = go_VA_CC_precip3,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_CC)
# CN treatment

growth_VA_CN_precip1 <- growth_matrix(Pmatrix = Pmatrix_VA_CN_precip1,
                                      survival_object = so_VA_CN,
                                      growth_object = go_VA_CN_precip1,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_CN)

growth_VA_CN_precip2 <- growth_matrix(Pmatrix = Pmatrix_VA_CN_precip2,
                                      survival_object = so_VA_CN,
                                      growth_object = go_VA_CN_precip2,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_CN)

growth_VA_CN_precip3 <- growth_matrix(Pmatrix = Pmatrix_VA_CN_precip3,
                                      survival_object = so_VA_CN,
                                      growth_object = go_VA_CN_precip3,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_CN)

## WC treatment 
growth_VA_WC <- growth_matrix(Pmatrix = Pmatrix_VA_WC,
                                      survival_object = so_VA_WC,
                                      growth_object = go_VA_WC,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_WC)

# WN treatment

growth_VA_WN_precip1 <- growth_matrix(Pmatrix = Pmatrix_VA_WN_precip1,
                                      survival_object = so_VA_WN,
                                      growth_object = go_VA_WN_precip1,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_WN)

growth_VA_WN_precip2 <- growth_matrix(Pmatrix = Pmatrix_VA_WN_precip2,
                                      survival_object = so_VA_WN,
                                      growth_object = go_VA_WN_precip2,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_WN)

growth_VA_WN_precip3 <- growth_matrix(Pmatrix = Pmatrix_VA_WN_precip3,
                                      survival_object = so_VA_WN,
                                      growth_object = go_VA_WN_precip3,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_VA_WN)

## Sib pro
## CC treament

growth_SP_CC <- growth_matrix(Pmatrix = Pmatrix_SP_CC,
                                      survival_object = so_SP_CC,
                                      growth_object = go_SP_CC,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_SP_CC)
# CN treatment

growth_SP_CN_precip1 <- growth_matrix(Pmatrix = Pmatrix_SP_CN_precip1,
                                      survival_object = so_SP_CN,
                                      growth_object = go_SP_CN,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_SP_CN)

growth_SP_CN_precip2 <- growth_matrix(Pmatrix = Pmatrix_SP_CN_precip2,
                                      survival_object = so_SP_CN,
                                      growth_object = go_SP_CN,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_SP_CN)

growth_SP_CN_precip3 <- growth_matrix(Pmatrix = Pmatrix_SP_CN_precip3,
                                      survival_object = so_SP_CN,
                                      growth_object = go_SP_CN,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_SP_CN)

## WC treatment 
growth_SP_WC <- growth_matrix(Pmatrix = Pmatrix_SP_WC,
                              survival_object = so_SP_WC,
                              growth_object = go_SP_WC,
                              minSize = minSize,
                              maxSize = maxSize,
                              discrete_trans = dto_SP_WC)

# WN treatment

growth_SP_WN<- growth_matrix(Pmatrix = Pmatrix_SP_WN,
                                      survival_object = so_SP_WN,
                                      growth_object = go_SP_WN,
                                      minSize = minSize,
                                      maxSize = maxSize,
                                      discrete_trans = dto_SP_WN)


LTRE_SP_CC_CN_mean_precip <- LTRE_calcultations_Growth(IPM1 = (IPM_SP_CN_precip1 + IPM_SP_CN_precip2 + IPM_SP_CN_precip3)/3, 
                                             IPM2 = IPM_SP_CC, 
                                             Fmatrix1 = (Fmatrix_SP_CN_precip1 + Fmatrix_SP_CN_precip2 + Fmatrix_SP_CN_precip3)/3, 
                                             Fmatrix2 = Fmatrix_SP_CC,
                                             Pmatrix1 = (Pmatrix_SP_CN_precip1 + Pmatrix_SP_CN_precip2 + Pmatrix_SP_CN_precip3)/3, 
                                             Pmatrix2 = Pmatrix_SP_CC,
                                             Cmatrix1 = Cmatrix_SP_CN, 
                                             Cmatrix2 = Cmatrix_SP_CC, 
                                             Growth_matrix1 = (growth_SP_CN_precip1 + growth_SP_CN_precip2 + growth_SP_CN_precip3)/3,
                                             Growth_matrix2 = growth_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "CN - CC")


LTRE_SP_CC_WC_mean_precip <- LTRE_calcultations_Growth(IPM1 = (IPM_SP_WC_precip1 + IPM_SP_WC_precip2 + IPM_SP_WC_precip3)/3, 
                                                       IPM2 = IPM_SP_CC, 
                                                       Fmatrix1 = (Fmatrix_SP_WC_precip1 + Fmatrix_SP_WC_precip2 + Fmatrix_SP_WC_precip3)/3, 
                                                       Fmatrix2 = Fmatrix_SP_CC,
                                                       Pmatrix1 = Pmatrix_SP_WC, 
                                                       Pmatrix2 = Pmatrix_SP_CC,
                                                       Cmatrix1 = Cmatrix_SP_WC, 
                                                       Cmatrix2 = Cmatrix_SP_CC, 
                                                       Growth_matrix1 = growth_SP_WC,
                                                       Growth_matrix2 = growth_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WC - CC")

LTRE_SP_CC_WN_mean_precip <- LTRE_calcultations_Growth(IPM1 = IPM_SP_WN, 
                                                       IPM2 = IPM_SP_CC, 
                                                       Fmatrix1 = Fmatrix_SP_WN, 
                                                       Fmatrix2 = Fmatrix_SP_CC,
                                                       Pmatrix1 = Pmatrix_SP_WN, 
                                                       Pmatrix2 = Pmatrix_SP_CC,
                                                       Cmatrix1 = Cmatrix_SP_WN, 
                                                       Cmatrix2 = Cmatrix_SP_CC, 
                                                       Growth_matrix1 = growth_SP_WN,
                                                       Growth_matrix2 = growth_SP_CC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WN - CC")

LTRE_SP_WC_WN_mean_precip <- LTRE_calcultations_Growth(IPM1 = IPM_SP_WN, 
                                                       IPM2 = (IPM_SP_WC_precip1 + IPM_SP_WC_precip2 + IPM_SP_WC_precip3)/3, 
                                                       Fmatrix1 = Fmatrix_SP_WN, 
                                                       Fmatrix2 = (Fmatrix_SP_WC_precip1 + Fmatrix_SP_WC_precip2 + Fmatrix_SP_WC_precip3)/3,
                                                       Pmatrix1 = Pmatrix_SP_WN, 
                                                       Pmatrix2 = Pmatrix_SP_WC,
                                                       Cmatrix1 = Cmatrix_SP_WN, 
                                                       Cmatrix2 = Cmatrix_SP_WC, 
                                                       Growth_matrix1 = growth_SP_WN,
                                                       Growth_matrix2 = growth_SP_WC) %>% 
  mutate(species = "Sib_pro",
         treatment = "WN - WC")




LTRE_treatments_mean_precip <- LTRE_VA_WC_WN_mean_precip  %>% 
  bind_rows(LTRE_VA_CC_WN_mean_precip) %>% 
  bind_rows(LTRE_VA_CC_WC_mean_precip) %>% 
  bind_rows(LTRE_VA_CC_CN_mean_precip) %>% 
  bind_rows(LTRE_SP_CC_CN_mean_precip) %>% 
  bind_rows(LTRE_SP_CC_WN_mean_precip) %>% 
  bind_rows(LTRE_SP_CC_WC_mean_precip) %>% 
  bind_rows(LTRE_SP_WC_WN_mean_precip) 
  
Susanne_plot_ver_alp_mean <- LTRE_treatments_mean_precip %>% 
  filter(species == "Ver_alp") %>% 
  mutate(treatment = factor(treatment, levels = c("CN - CC", "WN - CC", "WN - WC", "WC - CC"))) %>% 
  filter(treatment %in% c("WC - CC", "CN - CC", "WN - CC", "WN - WC")) %>% 
  mutate(treatment = factor(treatment, levels = c("WC - CC", "CN - CC", "WN - WC", "WN - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=treatment)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  scale_fill_manual(values = c("#DEDEDE","#6D64E4", "#A1A1A1", "#6E6E6E"))

Susanne_plot_Sib_pro_mean <- LTRE_treatments_mean_precip %>% 
  filter(species == "Sib_pro") %>% 
  mutate(treatment = factor(treatment, levels = c("CN - CC", "WN - CC", "WN - WC", "WC - CC"))) %>% 
  filter(treatment %in% c("WC - CC", "CN - CC", "WN - CC", "WN - WC")) %>% 
  mutate(treatment = factor(treatment, levels = c("WC - CC", "CN - CC", "WN - WC", "WN - CC"))) %>% 
  ggplot(aes(fill=vital_rates, y=contributions, x=treatment)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  scale_fill_manual(values = c("#DEDEDE","#FFC300", "#A1A1A1", "#6E6E6E"))


plot_ver_alp_mean <- (Susanne_plot_ver_alp_mean /
  Susanne_plot_ver_alp) +
  plot_layout(heights = c(2, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 15))

plot_sib_pro_mean <- (Susanne_plot_Sib_pro_mean /
    Susanne_plot_sib_pro) +
  plot_layout(heights = c(2, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 15))

#ggsave(plot_ver_alp_mean, filename = "Ver_alp_fecundity_mean.pdf", width = 20, height = 24, units = "cm")
#ggsave(plot_sib_pro_mean, filename = "Sib_pro_fecundity_mean.pdf", width = 20, height = 24, units = "cm")