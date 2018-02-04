#' Calculate mean squares
#' 
#' Calculates the mean squares used in the DBMH and ORH methods
#' 
#' @param dataset The dataset to be analyzed, see \link{RJafroc-package}.
#' @param FOM The figure of merit to be used in the calculation. The default 
#'    is \code{"wJAFROC"}. See \link{UtilFigureOfMerit}.
#' @param method The method, in which the mean squares are calculated. The two 
#'    valid options are \code{"DBMH"} (default) and \code{"ORH"}. 
#' 
#' @return A list contating all possible mean squares
#' 
#' @details 
#' For \code{DBMH} method, \code{msT, msTR, msTC, msTRC} will not be available 
#'    if the dataset contains only one modality. Similarly, 
#'    \code{msR, msTR, msRC, msTRC} will not be returned for single reader dataset. 
#'    For \code{ORH} method, \code{msT, msR, msTR} will be returned for multiple 
#'    reader multiple modality dataset. \code{msT} is not available for sinlge 
#'    modality dataset, and \code{msR} is not available for single reader dataset.
#' 
#' @examples
#' UtilMeanSquares(dataset02, FOM = "Wilcoxon")
#' 
#' UtilMeanSquares(dataset05, method = "ORH")
#' 
#' @export

UtilMeanSquares <- function(dataset, FOM = "wJAFROC", method = "DBMH"){
  NL <- dataset$NL
  LL <- dataset$LL
  lesionNum <- dataset$lesionNum
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  if (method == "DBMH"){
    pseudoValues <- UtilPseudoValues(dataset, FOM)
    
    if (I != 1 ){
      msT <- 0
      for (i in 1:I) {
        msT <- msT + (mean(pseudoValues[i, , ]) - mean(pseudoValues))^2
      }
      msT <- msT * K * J/(I - 1)
      
      msTC <- 0
      for (i in 1:I) {
        for (k in 1:K) {
          msTC <- msTC + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
        }
      }
      msTC <- msTC * J/((I - 1) * (K - 1))
      
      msCSingleT <- rep(0, I)
      for (i in 1:I){
        for (k in 1:K) {
          msCSingleT[i] <- msCSingleT[i] + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]))^2
        }
        msCSingleT[i] <- msCSingleT[i] * J/(K - 1)
      }
    }
    
    if (J != 1){
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(pseudoValues[, j, ]) - mean(pseudoValues))^2
      }
      msR <- msR * K * I/(J - 1)
      
      msRC <- 0
      for (j in 1:J) {
        for (k in 1:K) {
          msRC <- msRC + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
        }
      }
      msRC <- msRC * I/((J - 1) * (K - 1))
      
      msCSingleR <- rep(0, J)
      for (j in 1:J){
        for (k in 1:K) {
          msCSingleR[j] <- msCSingleR[j] + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]))^2
        }
        msCSingleR[j] <- msCSingleR[j] * I/(K - 1)
      }
    }
    
    msC <- 0
    for (k in 1:K) {
      msC <- msC + (mean(pseudoValues[, , k]) - mean(pseudoValues))^2
    }
    msC <- msC * I * J/(K - 1)
    
    if (I != 1 && J != 1){
      msTR <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          msTR <- msTR + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, j, ]) + mean(pseudoValues))^2
        }
      }
      msTR <- msTR * K/((I - 1) * (J - 1))
      
      msTRC <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K) {
            msTRC <- msTRC + (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
                                mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
          }
        }
      }
      msTRC <- msTRC/((I - 1) * (J - 1) * (K - 1))
    }
    
    if (I == 1 && J == 1){
      return(list(
        msC = msC
      ))
    }else if (I == 1){
      return(list(
        msR = msR,
        msC = msC,
        msRC = msRC,
        msCSingleR = msCSingleR
      ))
    }else if (J == 1){
      return(list(
        msT = msT,
        msC = msC,
        msTC = msTC, 
        msCSingleT = msCSingleT
      ))
    }else{
      return(list(
        msT = msT,
        msR = msR,
        msC = msC,
        msTR = msTR,
        msTC = msTC,
        msRC = msRC,
        msTRC = msTRC, 
        msCSingleT = msCSingleT,
        msCSingleR = msCSingleR
        
      ))
    }
  }else if (method == "ORH"){
    if (I == 1 && J == 1){
      errMsg <- "The mean squares cannot be calculated for single reader single modality dataset."
      stop(errMsg)
    }
    fomArray <- UtilFigureOfMerit(dataset, FOM)
    fomMean <- mean(fomArray)
    
    if (I != 1){
      msT <- 0
      for (i in 1:I) {
        msT <- msT + (mean(fomArray[i, ]) - fomMean)^2
      }
      msT <- J * msT/(I - 1)
    }
    
    if (J != 1){
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(fomArray[, j]) - fomMean)^2
      }
      msR <- I * msR/(J - 1)
    }
    
    if (I != 1 && J != 1){
      msTR <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          msTR <- msTR + (fomArray[i, j] - mean(fomArray[i, ]) - mean(fomArray[, j]) + fomMean)^2
        }
      }
      msTR <- msTR/((J - 1) * (I - 1))
      return(list(
        msT = msT,
        msR = msR,
        msTR = msTR
      ))
    }else if (I == 1){
      return(list(
        msR = msR
      ))
    }else if (J == 1){
      return(list(
        msT = msT
      ))
    }
    
  }else{
    errMsg <- sprintf("%s is not a valid method.", method)
    stop(errMsg)
  }
  
}