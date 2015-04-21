## ----echo=FALSE, results='hide', message=FALSE---------------------------
library(RJafroc)

## ------------------------------------------------------------------------
str(rocData)

## ------------------------------------------------------------------------
str(frocData)

## ------------------------------------------------------------------------
str(roiData)

## ------------------------------------------------------------------------
rocXlsx <- "http://www.devchakraborty.com/RocData/rocData.xlsx"
rocLrc <- "http://www.devchakraborty.com/RocData/rocData.lrc"
rocCsv <- "http://www.devchakraborty.com/RocData/rocData.csv"
rocImrmc <- "http://www.devchakraborty.com/RocData/rocData.imrmc"
frocXlsx <- "http://www.devchakraborty.com/FrocData/frocData.xlsx"
roiXlsx <- "http://www.devchakraborty.com/RoiData/roiData.xlsx"

fullName <- rocXlsx
download.file(url = fullName, basename(fullName), mode = "wb")
RocDataXlsx<- ReadDataFile(fileName = basename(fullName))

fullName <- rocLrc
download.file(url = fullName, basename(fullName))
RocDataLrc<- ReadDataFile(fileName = basename(fullName), format = "MRMC")

fullName <- rocCsv
download.file(url = fullName, basename(fullName))
RocDataCsv<- ReadDataFile(fileName = basename(fullName), format = "MRMC")

fullName <- rocImrmc
download.file(url = fullName, basename(fullName))
RocDataImrmc<- ReadDataFile(fileName = basename(fullName), format = "iMRMC")

fullName <- frocXlsx
download.file(url = fullName, basename(fullName), mode = "wb")
FrocDataXlsx<- ReadDataFile(fileName = basename(fullName))

fullName <- roiXlsx
download.file(url = fullName, basename(fullName), mode = "wb")
RoiDataXlsx<- ReadDataFile(fileName = basename(fullName))


## ----results='hide'------------------------------------------------------
# ROC example
retDbmRoc  <- DBMHAnalysis(rocData, fom = "Wilcoxon") 
print(retDbmRoc)


## ----results='hide'------------------------------------------------------
retORRoc  <- ORHAnalysis(rocData, fom = "Wilcoxon") 
print(retORRoc)
CovOR <- retORRoc$varComp
cov1 <- CovOR$varCov[3]
cov2 <- CovOR$varCov[4]
cov3 <- CovOR$varCov[5]
varEps <- CovOR$varCov[6]
msTR <- retORRoc$msTR
msT <- retORRoc$msT

## ------------------------------------------------------------------------
CovOR

## ------------------------------------------------------------------------
retDbm  <- DBMHAnalysis(rocData, fom = "Wilcoxon") 
effectSize <- retDbm$ciDiffTrtRRRC$Estimate
varYTR <- retDbm$varComp$varComp[3]
varYTC <- retDbm$varComp$varComp[4]
varYEps <- retDbm$varComp$varComp[6]

## ------------------------------------------------------------------------
for (J in 6:10) {
  ret <- SampleSizeGivenJ(J, varYTR, varYTC, varYEps, 
                          effectSize = effectSize) 
  message("# of readers = ", J, ", estimated # of cases = ", ret$K, "\n",
      "predicted power = ", signif(ret$power, 4), "\n")
}

## ------------------------------------------------------------------------
retOR  <- ORHAnalysis(rocData, fom = "Wilcoxon") 
effectSize <- retDbm$ciDiffTrtRRRC$Estimate
CovOR <- retOR$varComp
cov1 <- CovOR$varCov[3]
cov2 <- CovOR$varCov[4]
cov3 <- CovOR$varCov[5]
varErrOR <- CovOR$varCov[6]
msTR <- retOR$msTR
KStar <- length(rocData$NL[1,1,,1])
for (J in 6:10) {
  ret <- SampleSizeGivenJ(J, cov1 = cov1, cov2 = cov2, cov3 = cov3, 
                          varEps = varErrOR, msTR = msTR, KStar = KStar,
                          effectSize = effectSize) 
  message("# of readers = ", J, ", estimated # of cases = ", ret$K, "\n",
      "predicted power = ", signif(ret$power, 4), "\n")
}


## ----results='hide',eval=FALSE-------------------------------------------
#  ## default JAFROC analysis, wJAFROC FOM is assumed
#  retDbmwJafroc  <- DBMHAnalysis(frocData)
#  print(retDbmwJafroc)

## ----results='hide', eval=FALSE------------------------------------------
#  ## wJAFROC1 FOM (use only if there are no non-diseased cases)
#  retDbmwJafroc1  <- DBMHAnalysis(frocData, fom = "wJAFROC1")
#  print(retDbmwJafroc1)
#  
#  retDbmJafroc  <- DBMHAnalysis(frocData, fom = "JAFROC")
#  print(retDbmJafroc)
#  
#  ## JAFROC1 FOM (use only if there are no non-diseased cases)
#  retDbmJafroc1  <- DBMHAnalysis(frocData, fom = "JAFROC1")
#  print(retDbmJafroc1)

## ----results='hide',eval=FALSE-------------------------------------------
#  # following three examples are for ROC data inferred from FROC data using different methods
#  retDbmHrAuc  <- DBMHAnalysis(frocData, fom = "HrAuc")
#  # highest rating inferred ROC
#  
#  retDbmSongA1  <- DBMHAnalysis(frocData, fom = "SongA1")
#  retDbmSongA2  <- DBMHAnalysis(frocData, fom = "SongA2")

## ----results='hide',eval=FALSE-------------------------------------------
#  # ROI example
#  retDbmRoi  <- DBMHAnalysis(roiData, fom = "ROI")
#  

## ----message=FALSE,eval=FALSE--------------------------------------------
#  OutputReport(dataset = rocData, method = "DBMH", fom = "Wilcoxon",
#               dataDscrpt = "MyROCData",  showWarnings = FALSE)
#  OutputReport(dataset = rocData, method = "DBMH", fom = "Wilcoxon",
#               reportFile = "MyROCDataAnalysis.txt",  showWarnings = FALSE)
#  OutputReport(dataset = rocData, method = "ORH", fom = "Wilcoxon",
#               showWarnings = FALSE)
#  OutputReport(dataset = frocData, method = "DBMH", fom = "Wilcoxon",
#               showWarnings = FALSE) # ERROR!
#  OutputReport(dataset = frocData, method = "ORH",
#               showWarnings = FALSE) # default fom is wJAFROC
#  OutputReport(dataset = frocData, method = "DBMH", fom = "HrAuc",
#               showWarnings = FALSE)
#  OutputReport(dataset = roiData, method = "ORH", fom = "ROI",
#               showWarnings = FALSE)

## ----message=FALSE,eval=FALSE--------------------------------------------
#  OutputReport("rocData.xlsx", format = "JAFROC", method = "DBMH",
#               fom = "Wilcoxon", dataDscrpt = "MyROC2Data",
#               showWarnings = FALSE)

## ------------------------------------------------------------------------
plotM <- c(1:2)
plotR <- c(1:5)
plotROC <- EmpiricalOpCharac(data = rocData, trts = plotM, 
                             rdrs = plotR, opChType = "ROC")

## ------------------------------------------------------------------------
plotMAvg <- list(1, 2)
plotRAvg <- list(c(1:5),c(1:5))
plotRocAvg <- EmpiricalOpCharac(dataset = rocData, trts = plotMAvg, 
                                rdrs = plotRAvg, opChType = "ROC")

## ----out.width= '0.49\\linewidth', fig.show='hold', echo=FALSE-----------
print(plotROC$ROCPlot)

## ----out.width= '0.49\\linewidth', fig.show='hold', echo=FALSE-----------
print(plotRocAvg$ROCPlot)

## ------------------------------------------------------------------------
plotM <- c(1:2)
plotR <- c(1:4)
plotROC <- EmpiricalOpCharac(data = frocData, trts = plotM, 
                             rdrs = plotR, opChType = "ROC")

plotMAvg <- list(1, 2)
plotRAvg <- list(c(1:4),c(1:4))
plotRocAvg <- EmpiricalOpCharac(data = frocData, trts = plotMAvg, 
                                rdrs = plotRAvg, opChType = "ROC")

plotMAvg <- list(1, 2)
plotRAvg <- list(c(1:4),c(1:4))
plotAFROC <- EmpiricalOpCharac(data = frocData, trts = plotMAvg, 
                               rdrs = plotRAvg, opChType = "AFROC")

plotMAvg <- list(1, 2)
plotRAvg <- list(c(1:4),c(1:4))
plotFROC <- EmpiricalOpCharac(data = frocData, trts = plotMAvg, 
                              rdrs = plotRAvg, opChType = "FROC")


## ----out.width= '0.49\\linewidth', fig.show='hold', echo=FALSE-----------
print(plotROC$ROCPlot)

## ----out.width= '0.49\\linewidth', fig.show='hold', echo=FALSE-----------
print(plotRocAvg$ROCPlot)

## ----out.width= '0.49\\linewidth', fig.show='hold', echo=FALSE-----------
print(plotAFROC$AFROCPlot)

## ----out.width= '0.49\\linewidth', fig.show='hold', echo=FALSE-----------
print(plotFROC$FROCPlot)

