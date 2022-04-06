### LTREs
# TT2 vs TT1sTT2
BaseIPM <- (first_IPM+second_IPM)/2
SBaseIPM <- sens(BaseIPM)
Difference.fec <- Fmatrix_VA_CR-Fmatrix_VA_CC
Contributions.fec = Difference.fec*SBaseIPM
sum(Contributions.fec)

contourPlot(t(VO.TT2.Contributions.fec), c(1:dim(VO.TT2.Difference.fec)[1]), maxSize.VO.TT2, 0.01, -0.01)
title("LTRE, TT1sTT2 vs. TT2, fec")

Difference.clone <- Cmatrix_CR-Cmatrix_CC
Contributions.clone = Difference.clone*SBaseIPM
sum(Contributions.clone)

contourPlot(t(VO.TT2.Contributions.clone), c(1:dim(VO.TT2.Difference.clone)[1]), maxSize.VO.TT2, 0.01, -0.01)
title("LTRE, TT1sTT2 vs. TT2, clone")

Difference.pm <- Pmatrix_CR-Pmatrix_CC
Contributions.pm = Difference.pm*SBaseIPM
sum(Contributions.pm)

contourPlot(t(VO.TT2.Contributions.pm), c(1:dim(VO.TT2.Difference.pm)[1]), maxSize.VO.TT2, 0.01, -0.01)
title("LTRE, TT1sTT2 vs. TT2, Pmatrix")

sum(Contributions.fec)+sum(Contributions.clone)+sum(Contributions.pm)

as.numeric(eigen(IPM.VO.TT2.all)$value[1])-as.numeric(eigen(IPM.VO.TT1sTT2.all)$value[1])

growth.only <- coerceSurvObj(so_CR, c(100,0))

Pmatrix.growth.only <- makeIPMPmatrix(survObj=growth.only, growObj=go_CR, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CR, correction = "constant", nBigMatrix = 100)


growth.only_CC <- coerceSurvObj(so_CC, c(100,0))
Pmatrix.growth.only_CC <- makeIPMPmatrix(survObj=growth.only_CC, growObj=go_CC, minSize=minSize, maxSize=maxSize, discreteTrans = dto_VA_CC, correction = "constant", nBigMatrix = 100)

Difference.growth <- Pmatrix.growth.only-Pmatrix.growth.only_CC

Contributions.growth = Difference.growth*SBaseIPM

sum(Contributions.growth)
contourPlot(t(VO.TT2.Contributions.growth), c(1:dim(VO.TT2.Difference.growth)[1]), maxSize.VO.TT2, 0.006, -0.006)
title("LTRE, TT1sTT2 vs. TT2, growth")
# contributions survival

Contributions.surv <- Contributions.pm-Contributions.growth       # or sum(VO.TT2.Contributions.pm)-sum(VO.TT2.Contributions.growth)

sum(Contributions.fec)
sum(Contributions.clone)
sum(Contributions.growth)
sum(Contributions.surv)

sum(sum(Contributions.fec),
    sum(Contributions.clone),
    sum(Contributions.growth),
    sum(Contributions.surv))
