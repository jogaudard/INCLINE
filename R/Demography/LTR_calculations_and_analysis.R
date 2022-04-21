### LTREs

### CC - difference between precipitation levels ###

#Making a matrix with the different between the two matrixes
BaseIPM <- (IPM_VA_CC_precip1 + IPM_VA_CC_precip2) / 2
SBaseIPM <- sens(BaseIPM)

#Calculating difference in fecundity
Difference_fec <- Fmatrix_VA_CC_prec2 - Fmatrix_VA_CC_prec1
Contributions_fec <- Difference_fec*SBaseIPM
sum(Contributions_fec)

contourPlot2(t(Contributions_fec), c(1:dim(Difference_fec)[1]), maxSize, 0, -0.0005, title = "LTRE fecundity Veronica alpina CC precip 1:precip2")

#Calculating difference in clonality
Difference_clone <- Cmatrix_VA_CC_prec2 - Cmatrix_CC_prec1
Contributions_clone = Difference_clone*SBaseIPM
sum(Contributions_clone)

contourPlot2(t(Contributions_clone), c(1:dim(Difference_clone)[1]), maxSize, 0, -0.0001, title = "LTRE clone Veronica alpina CC precip 1:precip2")

#Calculating difference in P matrix
Difference_pm <- Pmatrix_VA_CC_precip2 - Pmatrix_VA_CC_precip1
Contributions_pm = Difference_pm*SBaseIPM
sum(Contributions_pm)

contourPlot2(t(Contributions_pm), c(1:dim(Difference_pm)[1]), maxSize, 0, -0.0002, title = "LTRE growth and survival VA CC precip 1:precip2")

#Adding together all the differences in vital rates to give a general difference between matrixes
sum(Contributions_fec)+sum(Contributions_clone)+sum(Contributions_pm)

#Find the difference in lamba between populations_
as_numeric(eigen(IPM_VA_CC_precip2)$value[1])-as_numeric(eigen(IPM_VA_CC_precip1)$value[1])

#Calculating contribution of growth alone
growth_only_CC_precip1 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_only_CC_precip1 <- makeIPMPmatrix(survObj=growth_only_CC_precip1, growObj=go_VA_CC_prec1, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)


growth_only_CC_precip2 <- coerceSurvObj(so_VA_CC, c(100,0))
Pmatrix_growth_only_CC_precip2 <- makeIPMPmatrix(survObj=growth_only_CC_precip2, growObj=go_VA_CC_prec2, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)

Difference_growth <- Pmatrix_growth_only_CC_precip2 - Pmatrix_growth_only_CC_precip1

Contributions_growth = Difference_growth*SBaseIPM
sum(Contributions_growth)

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
