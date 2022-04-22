### LTREs
library(reshape2)

A <- matrix(c(2,5,2,1,0,0,0,0,1,0,0,0,0,1,3,5,6,0,0,1,0,0,0,2,0,0,1,2,7,2,4,6,2,5,1,0,0,1,0,0,0,1,0,0,3,5,4,0,0,1,0,0,1,0,0,2,0,3,5,7,3,1,4,0,1,0,0,0,0,2,0,0,0,1,3,4,6,0,0,1), byrow=T, nrow=8, ncol=10)
colnames(A) <- letters[1:10]
rownames(A) <- LETTERS[1:8]
print(A)
longData<-melt(A)

###Making general function for calculating LTREs
LTRE_calcultations <-function(IPM1, IPM2, Fmatrix1, Fmatrix2, Pmatrix1, Pmatrix2, Cmatrix1, Cmatrix2) {

  x <- list()
  #Making a matrix with the different between the two matrixes  
  BaseIPM <- (IPM1 + IPM2) / 2
  SBaseIPM <- sens(BaseIPM)
  
  #Calculating difference in fecundity
  Difference_fec <- Fmatrix2 - Fmatrix1
  Contributions_fec <- Difference_fec*SBaseIPM
  con_fec <- sum(Contributions_fec)
  #plotting difference in fecundity
  long_data <- melt(Contributions_fec)
  ggplot(long_data, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="grey90", high="red") +
    labs(x="size at  time t", y="size at time t+1", title="fecundity") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  
   contourPlot2(t(Contributions_fec), c(1:dim(Difference_fec)[1]), maxSize, 0.0005, -0.0005, title = "fecundity")
  
  
  #Making general plots
  contourPlot2(t(sens(IPM_VA_CC_precip2)), c(1:dim(sens(IPM_VA_CC_precip2))[1]), maxSize, 0.03, 0, title = "Sensitivity Veronica alpina CC")

  x <- list("con_fec" = con_fec, "plot" = plot_fec)
  
  return(x)
}

### CC - difference between precipitation levels ###




#Calculating difference in fecundity
Difference_fec <- Fmatrix_VA_CC_prec2 - Fmatrix_VA_CC_prec1
Contributions_fec <- Difference_fec*SBaseIPM
sum(Contributions_fec)

contourPlot2(t(Contributions_fec), c(1:dim(Difference_fec)[1]), maxSize, 0, -0.0005, title = "LTRE fecundity Veronica alpina CC 2.3 - 1.2 m/year")

contourPlot2(t(sens(IPM_VA_CC_precip2)), c(1:dim(sens(IPM_VA_CC_precip2))[1]), maxSize, 0.03, 0, title = "Sensitivity Veronica alpina CC")

#Calculating difference in clonality
Difference_clone <- Cmatrix_VA_CC_prec2 - Cmatrix_CC_prec1
Contributions_clone = Difference_clone*SBaseIPM
sum(Contributions_clone)

contourPlot2(t(Contributions_clone), c(1:dim(Difference_clone)[1]), maxSize, 0, -0.0001, title = "LTRE clone Veronica alpina CC 2.3 - 1.2 m/year")

#Calculating difference in P matrix
Difference_pm <- Pmatrix_VA_CC_precip2 - Pmatrix_VA_CC_precip1
Contributions_pm = Difference_pm*SBaseIPM
sum(Contributions_pm)
x11()
contourPlot2(t(Contributions_pm), c(1:dim(Difference_pm)[1]), maxSize, 0.0002, 0, title = "LTRE growth and survival VA CC 2.3 - 1.2 m/year")
contourPlot2(t(Contributions_pm), c(1:dim(Difference_pm)[1]), maxSize, 0, -0.0002, title = "LTRE growth and survival VA CC 2.3 - 1.2 m/year")
image(t(Contributions_pm), c(1:dim(Difference_pm)[1]))

#Adding together all the differences in vital rates to give a general difference between matrixes
sum(Contributions_fec)+sum(Contributions_clone)+sum(Contributions_pm)

#Find the difference in lamba between populations_
as.numeric(eigen(IPM_VA_CC_precip2)$value[1])-as.numeric(eigen(IPM_VA_CC_precip1)$value[1])

#Calculating contribution of growth alone
growth_only_VA_CC_precip1 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_only_VA_CC_precip1 <- makeIPMPmatrix(survObj=growth_only_VA_CC_precip1, growObj=go_VA_CC_prec1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)


growth_only_VA_CC_precip2 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_VA_only_CC_precip2 <- makeIPMPmatrix(survObj=growth_only_VA_CC_precip2, growObj=go_VA_CC_prec2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)

Difference_growth_VA_CC_precip_1_2 <- Pmatrix_growth_VA_only_CC_precip2 - Pmatrix_growth_VA_only_CC_precip1

Contributions_growth_VA_CC_precip_1_2 = Difference_growth*SBaseIPM
sum(Contributions_growth_VA_CC_precip_1_2)

contourPlot2(t(Contributions_growth), c(1:dim(Contributions_growth)[1]), maxSize, 0, -0.0003, "LTRE growth and survival Veronica alpina CC precip 1:precip2")

# Calculating contribution of survival alone
Contributions_surv <- Contributions_pm - Contributions_growth 
contourPlot2(t(Contributions_surv), c(1:dim(Contributions_surv)[1]), maxSize, 0, -0.0003, "LTRE growth and survival Veronica alpina CC precip 1:precip2")


sum(Contributions_fec)
sum(Contributions_clone)
sum(Contributions_growth)
sum(Contributions_surv)

sum(sum(Contributions_fec),
    sum(Contributions_clone),
    sum(Contributions_growth),
    sum(Contributions_surv))


as.numeric(eigen(IPM_VA_CC_precip2)$value[1])-as.numeric(eigen(IPM_VA_CC_precip1)$value[1])

### LTREs

### CC - difference between precipitation levels ###

#Making a matrix with the different between the two matrixes
BaseIPM <- (IPM_VA_CC_precip1 + IPM_VA_CC_precip2) / 2
SBaseIPM <- sens(BaseIPM)

#Calculating difference in fecundity
Difference_fec <- Fmatrix_VA_CC_prec2 - Fmatrix_VA_CC_prec1
Contributions_fec <- Difference_fec*SBaseIPM
sum(Contributions_fec)

contourPlot2(t(Contributions_fec), c(1:dim(Difference_fec)[1]), maxSize, 0, -0.0005, title = "LTRE fecundity Veronica alpina CC 2.3 - 1.2 m/year")

contourPlot2(t(sens(IPM_VA_CC_precip2)), c(1:dim(sens(IPM_VA_CC_precip2))[1]), maxSize, 0.03, 0, title = "Sensitivity Veronica alpina CC")

#Calculating difference in clonality
Difference_clone <- Cmatrix_VA_CC_prec2 - Cmatrix_CC_prec1
Contributions_clone = Difference_clone*SBaseIPM
sum(Contributions_clone)

contourPlot2(t(Contributions_clone), c(1:dim(Difference_clone)[1]), maxSize, 0, -0.0001, title = "LTRE clone Veronica alpina CC 2.3 - 1.2 m/year")

#Calculating difference in P matrix
Difference_pm <- Pmatrix_VA_CC_precip2 - Pmatrix_VA_CC_precip1
Contributions_pm = Difference_pm*SBaseIPM
sum(Contributions_pm)
x11()
contourPlot2(t(Contributions_pm), c(1:dim(Difference_pm)[1]), maxSize, 0.0002, 0, title = "LTRE growth and survival VA CC 2.3 - 1.2 m/year")
contourPlot2(t(Contributions_pm), c(1:dim(Difference_pm)[1]), maxSize, 0, -0.0002, title = "LTRE growth and survival VA CC 2.3 - 1.2 m/year")
image(t(Contributions_pm), c(1:dim(Difference_pm)[1]))

#Adding together all the differences in vital rates to give a general difference between matrixes
sum(Contributions_fec)+sum(Contributions_clone)+sum(Contributions_pm)

#Find the difference in lamba between populations_
as.numeric(eigen(IPM_VA_CC_precip2)$value[1])-as.numeric(eigen(IPM_VA_CC_precip1)$value[1])

#Calculating contribution of growth alone
growth_only_VA_CC_precip1 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_only_VA_CC_precip1 <- makeIPMPmatrix(survObj=growth_only_VA_CC_precip1, growObj=go_VA_CC_prec1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)


growth_only_VA_CC_precip2 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_VA_only_CC_precip2 <- makeIPMPmatrix(survObj=growth_only_VA_CC_precip2, growObj=go_VA_CC_prec2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)

Difference_growth_VA_CC_precip_1_2 <- Pmatrix_growth_VA_only_CC_precip2 - Pmatrix_growth_VA_only_CC_precip1

Contributions_growth_VA_CC_precip_1_2 = Difference_growth*SBaseIPM
sum(Contributions_growth_VA_CC_precip_1_2)

contourPlot2(t(Contributions_growth), c(1:dim(Contributions_growth)[1]), maxSize, 0, -0.0003, "LTRE growth and survival Veronica alpina CC precip 1:precip2")

# Calculating contribution of survival alone
Contributions_surv <- Contributions_pm - Contributions_growth 
contourPlot2(t(Contributions_surv), c(1:dim(Contributions_surv)[1]), maxSize, 0, -0.0003, "LTRE growth and survival Veronica alpina CC precip 1:precip2")


sum(Contributions_fec)
sum(Contributions_clone)
sum(Contributions_growth)
sum(Contributions_surv)

sum(sum(Contributions_fec),
    sum(Contributions_clone),
    sum(Contributions_growth),
    sum(Contributions_surv))


as.numeric(eigen(IPM_VA_CC_precip2)$value[1])-as.numeric(eigen(IPM_VA_CC_precip1)$value[1])

### Comparing precip level 2 and 3 ###
#Making a matrix with the different between the two matrixes
BaseIPM_VA_CC_precip_2_3 <- (IPM_VA_CC_precip2 + IPM_VA_CC_precip3) / 2
SBaseIPM_VA_CC_precip_2_3 <- sens(BaseIPM_VA_CC_precip_2_3)

#Calculating difference in fecundity
Difference_fec_VA_CC_precip_2_3 <- Fmatrix_VA_CC_prec2 - Fmatrix_VA_CC_prec3
Contributions_fec_VA_CC_precip_2_3 <- Difference_fec_VA_CC_precip_2_3*SBaseIPM_VA_CC_precip_2_3
sum(Contributions_fec_VA_CC_precip_2_3)

contourPlot2(t(Contributions_fec_VA_CC_precip_2_3), c(1:dim(Contributions_fec_VA_CC_precip_2_3)[1]), maxSize,  0, -0.0005, title = "LTRE fecundity Veronica alpina CC  2.3 - 3.4 m/year")

#contourPlot2(t(sens(IPM_VA_CC_precip2)), c(1:dim(sens(IPM_VA_CC_precip2))[1]), maxSize, 0.03, 0, title = "Sensitivity Veronica alpina CC")

#Calculating difference in clonality
Difference_clone_VA_CC_precip_2_3 <- Cmatrix_VA_CC_prec2 - Cmatrix_CC_prec3
Contributions_clone_VA_CC_precip_2_3 = Difference_clone_VA_CC_precip_2_3*SBaseIPM_VA_CC_precip_2_3
sum(Contributions_clone_VA_CC_precip_2_3)

contourPlot2(t(Contributions_clone_VA_CC_precip_2_3), c(1:dim(Difference_clone_VA_CC_precip_2_3)[1]), maxSize, 0, -0.0001, title = "LTRE clone Veronica alpina CC 2.3 - 3.4 m/year")

#Calculating difference in P matrix
Difference_pm_VA_CC_precip_2_3 <- Pmatrix_VA_CC_precip2 - Pmatrix_VA_CC_precip3
Contributions_pm_VA_CC_precip_2_3 = Difference_pm_VA_CC_precip_2_3*SBaseIPM_VA_CC_precip_2_3
sum(Contributions_pm_VA_CC_precip_2_3)

contourPlot2(t(Contributions_pm_VA_CC_precip_2_3), c(1:dim(Difference_pm_VA_CC_precip_2_3)[1]), maxSize, 0.02, -0.02, title = "LTRE growth and survival VA CC 2.3 - 3.4 m/year")

#Adding together all the differences in vital rates to give a general difference between matrixes
sum(Contributions_fec_VA_CC_precip_2_3)+sum(Contributions_clone_VA_CC_precip_2_3)+sum(Contributions_pm_VA_CC_precip_2_3)

#Find the difference in lamba between populations_
as.numeric(eigen(IPM_VA_CC_precip2)$value[1])-as.numeric(eigen(IPM_VA_CC_precip3)$value[1])

#Calculating contribution of growth alone
growth_only_VA_CC_precip3 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_only_VA_CC_precip3 <- makeIPMPmatrix(survObj=growth_only_VA_CC_precip3, growObj=go_VA_CC_prec3, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)


growth_only_VA_CC_precip2 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_only_VA_CC_precip2 <- makeIPMPmatrix(survObj=growth_only_VA_CC_precip2, growObj=go_VA_CC_prec2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)

Difference_growth_VA_CC_precip_2_3 <- Pmatrix_growth_only_VA_CC_precip2 - Pmatrix_growth_only_VA_CC_precip1

Contributions_growth_VA_CC_precip_2_3 = Difference_growth_VA_CC_precip_2_3*SBaseIPM_VA_CC_precip_2_3
sum(Contributions_growth_VA_CC_precip_2_3)

contourPlot2(t(Contributions_growth_VA_CC_precip_2_3), c(1:dim(Contributions_growth_VA_CC_precip_2_3)[1]), maxSize, 0, -0.0003, "LTRE growth and survival Veronica alpina CC precip 1:precip2")

# Calculating contribution of survival alone
Contributions_surv_VA_CC_precip_2_3 <- Contributions_pm_VA_CC_precip_2_3 - Contributions_growth_VA_CC_precip_2_3
contourPlot2(t(Contributions_surv_VA_CC_precip_2_3), c(1:dim(Contributions_surv_VA_CC_precip_2_3)[1]), maxSize, 0, -0.0003, "LTRE growth and survival Veronica alpina CC precip 1:precip2")


sum(Contributions_fec_VA_CC_precip_2_3)
sum(Contributions_clone_VA_CC_precip_2_3)
sum(Contributions_growth_VA_CC_precip_2_3)
sum(Contributions_surv_VA_CC_precip_2_3)

sum(sum(Contributions_fec_VA_CC_precip_2_3),
    sum(Contributions_clone_VA_CC_precip_2_3),
    sum(Contributions_growth_VA_CC_precip_2_3),
    sum(Contributions_surv_VA_CC_precip_2_3))


as.numeric(eigen(IPM_VA_CC_precip2)$value[1])-as.numeric(eigen(IPM_VA_CC_precip3)$value[1])



