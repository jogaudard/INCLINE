### LTREs
library(reshape2)

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
  growth_only1 <- coerceSurvObj(survival_object1, c(100,0))
  Pmatrix_growth_only1 <- makeIPMPmatrix(survObj=growth_only1, growObj=growth_object1, minSize=minSize, maxSize=maxSize, discreteTrans = discrete_trans1, correction = "constant", nBigMatrix = 100)
  
  growth_only2 <- coerceSurvObj(survival_object2, c(100,0))
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


#### Treatments ####
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
                                             Cmatrix1 = Cmatrix_VA_CN_precip1, 
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
                                             Cmatrix1 = Cmatrix_VA_CN_precip2, 
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
                                             Cmatrix1 = Cmatrix_VA_CN_precip3, 
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
                                             Cmatrix1 = Cmatrix_VA_WE, 
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
                                             Cmatrix1 = Cmatrix_VA_WE, 
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
                                             Cmatrix1 = Cmatrix_VA_WE, 
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
                                             Cmatrix1 = Cmatrix_VA_WN_precip1, 
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
                                             Cmatrix1 = Cmatrix_VA_WN_precip2, 
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
                                             Cmatrix1 = Cmatrix_VA_WN_precip3, 
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
                                             Cmatrix1 = Cmatrix_VA_WR, 
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
                                             Cmatrix1 = Cmatrix_VA_WR, 
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
                                             Cmatrix1 = Cmatrix_VA_WR, 
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
                                             Cmatrix1 = Cmatrix_VA_WR, 
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
                                             Cmatrix1 = Cmatrix_VA_WR, 
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
                                             Cmatrix1 = Cmatrix_VA_WR, 
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
                                             Cmatrix1 = Cmatrix_VA_WE, 
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
                                             Cmatrix1 = Cmatrix_VA_WE, 
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
                                             Cmatrix1 = Cmatrix_VA_WE, 
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
                                             Cmatrix1 = Cmatrix_VA_WN_precip1, 
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
                                             Cmatrix1 = Cmatrix_VA_WN_precip2, 
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
                                             Cmatrix1 = Cmatrix_VA_WN_precip3, 
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
  bind_rows(LTRE_VA_WC_WN_precip_3)

LTRE_treatments %>% 
  mutate(treatment = factor(treatment, levels = c("CE - CC", "CN - CC", "CR - CC", "WE - CC", "WN - CC", "WR - CC", "WE - WC", "WN - WC", "WR - WC", "WC - CC"))) %>% 
ggplot(aes(fill=vital_rates, y=contributions, x=precipitation)) + 
  geom_bar(position="stack", stat="identity") +
  geom_point(aes(y = lamda_difference)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~treatment, ncol = 3) +
  theme_bw()

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
