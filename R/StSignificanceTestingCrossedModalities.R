#' Perform significance testing using crossed treatments analysis
#' 
#' @description  Performs ORH analysis for specified crossed treatments dataset 
#'    averaged over specified treatment factor
#' 
#' 
#' @param ds The crossed treatments dataset
#' @param avgIndx The index of the treatment to be averaged over
#' @param FOM See \code{\link{StSignificanceTesting}}
#' @param alpha See \code{\link{StSignificanceTesting}}
#' @param analysisOption See \code{\link{StSignificanceTesting}}
#' 
#' @return The return list contains the same items with \code{\link{StSignificanceTesting}}.
#' 
#' @examples
#' \donttest{ 
#' ## read the built in dataset
#' retCrossed2 <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
#' }
#' 
#' @export
StSignificanceTestingCrossedModalities <- function(ds, avgIndx, FOM = "wAFROC", 
                                                   alpha = 0.05, analysisOption = "ALL"){
  
  if (ds$descriptions$design != "FCTRL-X-MOD") stop("Dataset is not factorial crossed modality")
  options(stringsAsFactors = FALSE)
  NL <- ds$ratings$NL
  LL <- ds$ratings$LL
  perCase <- ds$lesions$perCase
  IDs <- ds$lesions$IDs
  weights <- ds$lesions$weights
  maxNL <- dim(NL)[5]
  maxLL <- dim(LL)[5]
  dataType <- ds$dataType
  if(avgIndx == 1){
    modalityID <- ds$descriptions$modalityID2
  }else{
    modalityID <- ds$descriptions$modalityID1
  }
  readerID <- ds$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[4]
  K2 <- dim(LL)[4]
  K1 <- K - K2
  
  if (!analysisOption %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not an available analysisOption.", analysisOption)
    stop(errMsg)
  }    
  
  if (I < 2) {
    stop("The analysis requires at least 2 treatments")
  }
  
  ret <- EstimateVarCovCrossed(NL, LL, perCase, IDs, weights, maxNL, maxLL, FOM, avgIndx)
  Var <- ret$Var
  Cov1 <- ret$Cov1
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  fomArray <- ret$fomArray  # sic! 4/29/20
  trMeans <- rowMeans(fomArray)
  fomMean <- mean(fomArray)
  
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(fomArray[i, ]) - fomMean)^2
  }
  msT <- J * msT/(I - 1)
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(fomArray[, j]) - fomMean)^2
  }
  msR <- I * msR/(J - 1)
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (fomArray[i, j] - mean(fomArray[i, ]) - mean(fomArray[, j]) + fomMean)^2
    }
  }
  msTR <- msTR/((J - 1) * (I - 1))
  
  # TBA need citations here
  varTR <- msTR - Var + Cov1 + max(Cov2 - Cov3, 0)
  varR <- (msR - Var - (I - 1) * Cov1 + Cov2 + (I - 1) * Cov3 - varTR)/I
  varCovArray <- c(varR, varTR, Cov1, Cov2, Cov3, Var)
  nameArray <- c("Var(R)", "Var(T*R)", "COV1", "COV2", "COV3", "Var(Error)")
  varComp <- data.frame(varCov = varCovArray, 
                        row.names = nameArray, 
                        stringsAsFactors = FALSE)
  
  varSingle <- vector(length = I)
  cov2Single <- vector(length = I)
  for (i in 1:I) {
    if (avgIndx == 1){
      nl <- NL[ , i, , , ]
      ll <- LL[ , i, , , ]
      dim(nl) <- c(length(ds$descriptions$modalityID1), 1, J, K, maxNL)
      dim(ll) <- c(length(ds$descriptions$modalityID1), 1, J, K2, max(perCase))
    }else{
      nl <- NL[ i, , , , ]
      ll <- LL[ i, , , , ]
      dim(nl) <- c(1, length(ds$descriptions$modalityID2), J, K, maxNL)
      dim(ll) <- c(1, length(ds$descriptions$modalityID2), J, K2, max(perCase))
    }
    
    
    ret <- EstimateVarCovCrossed(nl, ll, perCase, IDs, weights, maxNL, maxLL, FOM, avgIndx)
    varSingle[i] <- ret$Var
    if (J > 1) {
      cov2Single[i] <- ret$Cov2
    } else {
      cov2Single[i] <- 0
    }
  }
  
  varEchRder <- vector(length = J)
  cov1EchRder <- vector(length = J)
  for (j in 1:J) {
    nl <- NL[ , , j, , ]
    ll <- LL[ , , j, , ]
    dim(nl) <- c(length(ds$descriptions$modalityID1), length(ds$descriptions$modalityID2), 1, K, maxNL)
    dim(ll) <- c(length(ds$descriptions$modalityID1), length(ds$descriptions$modalityID2), 1, K2, max(perCase))
    ret <- EstimateVarCovCrossed(nl, ll, perCase, IDs, weights, maxNL, maxLL, FOM, avgIndx)
    varEchRder[j] <- ret$Var
    cov1EchRder[j] <- ret$Cov1
  }
  
  msRSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    msRSingle[i] <- sum((fomArray[i, ] - trMeans[i])^2)/(J - 1)
  }
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trMeans[i] - trMeans[ip]
      diffTRName[ii] <- paste(modalityID[i], modalityID[ip], sep = "-")
      ii <- ii + 1
    }
  }
  
  msNum <- msT
  
  # ************ RRRC ****************
  if (analysisOption %in% c("RRRC", "ALL")) {
    if (J > 1) {
      msDenRRRC <- msTR + max(J * (Cov2 - Cov3), 0)
      fRRRC <- msNum/msDenRRRC
      ddfRRRC <- msDenRRRC^2/(msTR^2/((I - 1) * (J - 1)))
      pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
      stdErrRRRC <- sqrt(2 * msDenRRRC/J)
      tStat <- vector()
      tPr <- vector()
      CIRRRC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRRC
        tPr[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE) # critical correction, noted by user Lucy D'Agostino McGowan
        ci <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
        if (length(ci) == 0){
          CIRRRC[i, ] <- c(NA, NA)
        }else{
          CIRRRC[i, ] <- ci
        }
      }
      # This code was failing on oldrelease R v3.5.3 under test context("StSignificanceTestingCrossedModalities")
      # The outputs for ciDiffTrtRRRC were not equal for goodvalues and currentvalues
      # forcing original order to be kept
      
      for (i in (1:length(diffTRName))) {
        diffTRName[i] <- paste0(paste0("Row",i,"_"),diffTRName[i])
      }
      
      attributes(diffTRName) <- NULL #statement #1a
      attributes(diffTRMeans) <- NULL #statement #1b
      
      ciDiffTrtRRRC <- data.frame(Treatment = diffTRName, 
                                  Estimate = diffTRMeans, 
                                  StdErr = rep(stdErrRRRC, choose(I, 2)), 
                                  DF = rep(ddfRRRC, choose(I, 2)), 
                                  t = tStat, 
                                  PrGTt = tPr, 
                                  CILower = CIRRRC[,1],
                                  CIUpper = CIRRRC[,2],
                                  stringsAsFactors = FALSE)
      
      # print(attributes(ciDiffTrtRRRC))
      # print(attributes(ciDiffTrtRRRC$Treatment))
      # print(attributes(ciDiffTrtRRRC$Estimate))
      # print(class(ciDiffTrtRRRC$Treatment))
      ################################################################################
      # this whole issue could be a git problem of not updating a regenerated new good value
      # I had to do two commits followed by push: one with file deleted and one with file regenerated.
      # Then all travis releases worked;
      # if statements #1,ab IS commented
      # $names
      # [1] "Treatment" "Estimate"  "StdErr"    "DF"        "t"         "PrGTt"     "CILower"   "CIUpper"  
      # 
      # $class
      # [1] "data.frame"
      # 
      # $row.names
      # [1] 1 2 3 4 5 6
      # 
      # NULL
      # NULL
      # [1] "character"
      
      # if statements #1,ab is NOT commented
      # $names
      # [1] "Treatment" "Estimate"  "StdErr"    "DF"        "t"         "PrGTt"     "CILower"   "CIUpper"  
      # 
      # $class
      # [1] "data.frame"
      # 
      # $row.names
      # [1] 1 2 3 4 5 6
      # 
      # $levels
      # [1] "Row1_20-40" "Row2_20-60" "Row3_20-80" "Row4_40-60" "Row5_40-80" "Row6_60-80"
      # 
      # $class
      # [1] "factor"
      # 
      # NULL
      # [1] "factor"    
      ################################################################################
      
      dfSingleRRRC <- array(dim = I)
      msDenSingleRRRC <- array(dim = I)
      stdErrSingleRRRC <- array(dim = I)
      CISingleRRRC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRRC[i] <- msRSingle[i] + max(J * cov2Single[i], 0)
        dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
        stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J)
        ci <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
        if (length(ci) == 0){
          CISingleRRRC[i, ] <- c(NA, NA)
        }else{
          CISingleRRRC[i, ] <- ci
        }
        
      }
      ciAvgRdrEachTrtRRRC <- data.frame(Treatment = modalityID, 
                                        Area = trMeans, 
                                        StdErr = as.vector(stdErrSingleRRRC), 
                                        DF = as.vector(dfSingleRRRC), 
                                        CILower = CISingleRRRC[,1], 
                                        CIUpper = CISingleRRRC[,2], 
                                        stringsAsFactors = FALSE)
      
    } else {
      fRRRC <- NA
      ddfRRRC <- NA
      pRRRC <- NA
      ciDiffTrtRRRC <- NA
      ciAvgRdrEachTrtRRRC <- NA
    }
    if (analysisOption == "RRRC"){
      return(list(fomArray = fomArray, 
                  msT = msT, 
                  msTR = msTR, 
                  varComp = varComp, 
                  fRRRC = fRRRC, 
                  ddfRRRC = ddfRRRC, 
                  pRRRC = pRRRC, 
                  ciDiffTrtRRRC = ciDiffTrtRRRC, 
                  ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC)
      )
    }
  }
  
  # ************ FRRC ****************
  if (analysisOption %in% c("FRRC", "ALL")) {
    if (J > 1) {
      msDenFRRC <- Var - Cov1 + (J - 1) * (Cov2 - Cov3)
    } else {
      msDenFRRC <- Var - Cov1
    }
    fFRRC <- msNum/msDenFRRC
    ddfFRRC <- Inf
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J)
    tStat <- vector()
    tPr <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      tPr[i] <- 2 * pt(abs(tStat[i]), ddfFRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    # for (i in (1:length(diffTRName))) {
    #   diffTRName[i] <- paste0(paste0("Row",i,"-"),diffTRName[i])
    # }
    ciDiffTrtFRRC <- data.frame(Treatment = diffTRName, 
                                Estimate = diffTRMeans, 
                                StdErr = rep(stdErrFRRC, choose(I, 2)), 
                                DF = rep(ddfFRRC, choose(I, 2)), 
                                t = tStat, 
                                PrGTt = tPr, 
                                CILower = CIFRRC[,1],
                                CIUpper = CIFRRC[,2],
                                stringsAsFactors = FALSE)
    
    # colnames(ciDiffTrtFRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- varSingle[i] + (J - 1) * cov2Single[i]
      dfSingleFRRC[i] <- Inf
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J)
      CISingleFRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    ciAvgRdrEachTrtFRRC <- data.frame(Treatment = modalityID, 
                                      Area = trMeans, 
                                      StdErr = as.vector(stdErrSingleFRRC), 
                                      DF = as.vector(dfSingleFRRC), 
                                      CILower = CISingleFRRC[,1], 
                                      CIUpper = CISingleFRRC[,2], 
                                      row.names = NULL,
                                      stringsAsFactors = FALSE)
    
    #colnames(ciAvgRdrEachTrtFRRC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    
    diffTRMeansFRRC <- array(dim = c(J, choose(I, 2)))
    for (j in 1:J) {
      ii <- 1
      for (i in 1:I) {
        if (i == I) 
          break
        for (ip in (i + 1):I) {
          diffTRMeansFRRC[j, ii] <- fomArray[i, j] - fomArray[ip, j]
          ii <- ii + 1
        }
      }
    }
    
    diffTRMeansFRRC <- as.vector(t(diffTRMeansFRRC))
    stdErrFRRC <- sqrt(2 * (varEchRder - cov1EchRder))
    stdErrFRRC <- rep(stdErrFRRC, choose(I, 2))
    dim(stdErrFRRC) <- c(J, choose(I, 2))
    stdErrFRRC <- as.vector(t(stdErrFRRC))
    readerNames <- rep(readerID, choose(I, 2))
    dim(readerNames) <- c(J, choose(I, 2))
    readerNames <- as.vector(t(readerNames))
    trNames <- rep(diffTRName, J)
    dfReaderFRRC <- rep(Inf, length(stdErrFRRC))
    CIReaderFRRC <- array(dim = c(length(stdErrFRRC), 2))
    tStat <- vector()
    tPr <- vector()
    for (n in 1:length(stdErrFRRC)) {
      tStat[n] <- diffTRMeansFRRC[n]/stdErrFRRC[n]
      tPr[n] <- 2 * pt(abs(tStat[n]), dfReaderFRRC[n], lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    ciDiffTrtEachRdr <- data.frame(Reader = readerNames, 
                                   Treatment = trNames, 
                                   Estimate = diffTRMeansFRRC, 
                                   StdErr = as.vector(stdErrFRRC), 
                                   DF = as.vector(dfReaderFRRC), 
                                   t = tStat, 
                                   PrGTt = tPr, 
                                   CILower = CIReaderFRRC[,1],
                                   CIUpper = CIReaderFRRC[,2],
                                   stringsAsFactors = FALSE)
    # 5/4/20 removing all this as I better understand data.frame()
    
    #colnames(ciDiffTrtEachRdr) <- c("Reader", "Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
    
    varCovEachRdr <- data.frame(readerID, 
                                varEchRder, 
                                cov1EchRder,
                                stringsAsFactors = FALSE)
    colnames(varCovEachRdr) <- c("Reader", "Var", "Cov1")
    if (analysisOption == "FRRC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr, varCovEachRdr = varCovEachRdr
      ))
    }
  }
  
  # ************ RRFC ****************
  if (analysisOption %in% c("RRFC", "ALL")) {
    if (J > 1) {
      msDenRRFC <- msTR
      fRRFC <- msNum/msDenRRFC
      ddfRRFC <- ((I - 1) * (J - 1))
      pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
      stdErrRRFC <- sqrt(2 * msDenRRFC/J)
      tStat <- vector()
      tPr <- vector()
      CIRRFC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRFC
        tPr[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
        CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
      }
      
      # for (i in (1:length(diffTRName))) {
      #   diffTRName[i] <- paste0(paste0("Row",i,"-"),diffTRName[i])
      # }
      ciDiffTrtRRFC <- data.frame(Treatment = diffTRName, 
                                  Estimate = diffTRMeans, 
                                  StdErr = rep(stdErrRRFC, choose(I, 2)), 
                                  DF = rep(ddfRRFC, choose(I, 2)), 
                                  t = tStat, 
                                  PrGTt = tPr, 
                                  CILower = CIRRFC[,1],
                                  CIUpper = CIRRFC[,2],
                                  stringsAsFactors = FALSE)
      # 5/4/20 removing all this as I better understand data.frame()
      
      #colnames(ciDiffTrtRRFC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
      
      dfSingleRRFC <- array(dim = I)
      msDenSingleRRFC <- array(dim = I)
      stdErrSingleRRFC <- array(dim = I)
      CISingleRRFC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRFC[i] <- msRSingle[i]
        dfSingleRRFC[i] <- (J - 1)
        stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J)
        CISingleRRFC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
      }
      ciAvgRdrEachTrtRRFC <- data.frame(Treatment = modalityID, 
                                        Area = trMeans, 
                                        StdErr = as.vector(stdErrSingleRRFC), 
                                        DF = as.vector(dfSingleRRFC), 
                                        CILower = CISingleRRFC[,1], 
                                        CIUpper = CISingleRRFC[,2], 
                                        row.names = NULL,
                                        stringsAsFactors = FALSE)
      
      #colnames(ciAvgRdrEachTrtRRFC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    } else {
      fRRFC <- NA
      ddfRRFC <- NA
      pRRFC <- NA
      ciDiffTrtRRFC <- NA
      ciAvgRdrEachTrtRRFC <- NA
    }
    if (analysisOption == "RRFC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
    }
  }
  
  return(list(
    fomArray = fomArray, 
    msT = msT, 
    msTR = msTR, 
    varComp = varComp, 
    fRRRC = fRRRC, 
    ddfRRRC = ddfRRRC, 
    pRRRC = pRRRC, 
    ciDiffTrtRRRC = ciDiffTrtRRRC, 
    ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC, 
    fFRRC = fFRRC, 
    ddfFRRC = ddfFRRC, 
    pFRRC = pFRRC, 
    ciDiffTrtFRRC = ciDiffTrtFRRC, 
    ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
    ciDiffTrtEachRdr = ciDiffTrtEachRdr, 
    varCovEachRdr = varCovEachRdr, 
    fRRFC = fRRFC, 
    ddfRRFC = ddfRRFC, 
    pRRFC = pRRFC, 
    ciDiffTrtRRFC = ciDiffTrtRRFC, 
    ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
}


#' @importFrom stats cov
#' 
EstimateVarCovCrossed <- function(NL, LL, perCase, IDs, weights, maxNL, maxLL, FOM, avgIndx) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I1 <- dim(NL)[1]
  I2 <- dim(NL)[2]
  J <- dim(NL)[3]
  K <- dim(NL)[4]
  K2 <- dim(LL)[4]
  
  K1 <- K - K2
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    jkFOMArray <- array(dim = c(I1, I2, J, K1))
    for (i1 in 1:I1) {
      for (i2 in 1:I2) {
        for (j in 1:J) {
          for (k in 1:K1) {
            nl <- NL[i1, i2, j, -k, ]
            ll <- LL[i1, i2, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(perCase))
            jkFOMArray[i1, i2, j, k] <- MyFom_ij(nl, ll, perCase, IDs, weights, maxNL, maxLL, K1 - 1, K2, FOM)
          }
        }
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    jkFOMArray <- array(dim = c(I1, I2, J, K2))
    for (i1 in 1:I1) {
      for (i2 in 1:I2) {
        for (j in 1:J) {
          for (k in 1:K2) {
            nl <- NL[i1, i2, j, -(k + K1), ]
            ll <- LL[i1, i2, j, -k, ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(perCase))
            lesionIDJk <- IDs[-k, ]
            dim(lesionIDJk) <- c(K2 -1, max(perCase))
            lesionWeightJk <- weights[-k, ]
            dim(lesionWeightJk) <- c(K2 -1, max(perCase))
            jkFOMArray[i1, i2, j, k] <- MyFom_ij(nl, ll, perCase[-k], lesionIDJk, lesionWeightJk, maxNL, maxLL, K1, K2 - 1, FOM)
          }
        }
      }
    }
  } else {
    jkFOMArray <- array(dim = c(I1, I2, J, K))
    for (i1 in 1:I1) {
      for (i2 in 1:I2) {
        for (j in 1:J) {
          for (k in 1:K) {
            if (k <= K1) {
              nl <- NL[i1, i2, j, -k, ]
              ll <- LL[i1, i2, j, , ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2, max(perCase))
              jkFOMArray[i1, i2, j, k] <- MyFom_ij(nl, ll, perCase, IDs, weights, maxNL, maxLL, K1 - 1, K2, FOM)
            } else {
              nl <- NL[i1, i2, j, -k, ]
              ll <- LL[i1, i2, j, -(k - K1), ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2 - 1, max(perCase))
              lesionIDJk <- IDs[-(k - K1), ]
              dim(lesionIDJk) <- c(K2 -1, max(perCase))
              lesionWeightJk <- weights[-(k - K1), ]
              dim(lesionWeightJk) <- c(K2 -1, max(perCase))
              jkFOMArray[i1, i2, j, k] <- MyFom_ij(nl, ll, perCase[-(k - K1)], lesionIDJk, lesionWeightJk, maxNL, maxLL, K1, K2 - 1, FOM)
            }
          }
        }
      }
    }
  }
  
  K <- length(jkFOMArray[1, 1, 1, ])
  if (avgIndx == 1){
    jkFOMArray <- apply(jkFOMArray, c(2, 3, 4), mean)
    fomArray <- apply(jkFOMArray, c(1, 2), mean)
  }else{
    jkFOMArray <- apply(jkFOMArray, c(1, 3, 4), mean)
    fomArray <- apply(jkFOMArray, c(1, 2), mean)
  }
  
  Cov <- FOMijk2VarCov(jkFOMArray, varInflFactor = TRUE)
  Var <- Cov$Var
  Cov1 <- Cov$Cov1
  Cov2 <- Cov$Cov2
  Cov3 <- Cov$Cov3
  
  return(list(
    Var = Var, 
    Cov1 = Cov1, 
    Cov2 = Cov2, 
    Cov3 = Cov3, 
    fomArray = fomArray
  ))
}
