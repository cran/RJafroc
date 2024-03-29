#' Performs DBM or OR significance testing for factorial or split-plot A,C datasets
#' 
#' @description  Performs Dorfman-Berbaum-Metz (DBM) or Obuchowski-Rockette (OR) 
#'    significance testing, for specified dataset; 
#'    significance testing refers to analysis designed to assign a P-value, 
#'    and other statistics, for 
#'    rejecting the null hypothesis (NH) that the reader-averaged 
#'    figure of merit (FOM) differences between treatments is zero. The results of 
#'    the analysis are best visualized in the text or  
#'    Excel-formatted files produced by \code{\link{UtilOutputReport}}. 
#'
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}. 
#'     Must have two or more treatments and two or more readers. The dataset design
#'     can be "FCTRL", "SPLIT-PLOT-A" or "SPLIT-PLOT-C". 
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' @param alpha The significance level of the test of the null hypothesis that all 
#'    treatment effects are zero; the default is 0.05
#' @param method The significance testing method to be used:  
#'    \code{"DBM"} (the default), representing the 
#'    Dorfman-Berbaum-Metz method or \code{"OR"}, representing the 
#'    Obuchowski-Rockette method.
#'    and the Obuchowski-Rockette significance testing methods, respectively. 
#' @param covEstMethod The covariance matrix estimation method
#'    in \code{ORH} analysis (for \code{method = "DBM"} the jackknife is always used).
#'    \itemize{ 
#'    \item \code{"Jackknife"}, the default, 
#'    \item \code{"Bootstrap"}, in which case \code{nBoots} (above) is relevant, 
#'    \item \code{"DeLong"}; requires \code{FOM = "Wilcoxon" or "ROI" or "HrAuc"}, 
#'    otherwise an error results.
#' }   
#' @param nBoots The number of bootstraps (defaults to 200), relevant only if 
#'    \code{covEstMethod = "bootstrap"} and \code{method = "OR"} 
#' @param analysisOption Determines which factors are regarded as random vs. fixed:
#' \itemize{ 
#'    \item \code{"RRRC"} = random-reader random case, 
#'    \item \code{"FRRC"} = fixed-reader random case, 
#'    \item \code{"RRFC"} = random-reader fixed case, 
#'    \item \code{"ALL"} =  outputs results of \code{"RRRC"}, \code{"FRRC"} 
#'    and \code{"RRFC"} analyses - this is the default.
#' }    
#' @param tempOrgCode, default FALSE; if TRUE, then code from version 0.0.1 of RJafroc
#'    is used (see RJafroc_0.0.1.tar). This is intended to check against errors 
#'    that crept in subsequent to the original version as I attempted to improve 
#'    the organization of the code and the output. As implicit in the name of this 
#'    temporary flag, it will eventually be removed. 
#' 
#' @return \strong{For \code{method = "DBM"} the returned list contains 4 dataframes:}
#' @return \item{FOMs}{Contains \code{foms}, \code{trtMeans} and \code{trtMeanDiffs}: 
#'    see return of \code{\link{UtilFigureOfMerit}}}
#' @return \item{ANOVA}{Contains \code{TRCAnova}, \code{VarCom}, \code{IndividualTrt} 
#'    and \code{IndividualRdr} ANOVA tables of pseudovalues}
#' @return \item{RRRC}{Contains results of \code{"RRRC"} analyses: \code{FTests}, 
#'    \code{ciDiffTrt}, \code{ciAvgRdrEachTrt}}
#' @return \item{FRRC}{Contains results of \code{"FRRC"} analyses: \code{FTests}, 
#'    \code{ciDiffTrt}, \code{ciAvgRdrEachTrt}, \code{ciDiffTrtEachRdr}}
#' @return \item{RRFC}{Contains results of \code{"RRFC"} analyses: \code{FTests}, 
#'    \code{ciDiffTrt}, \code{ciAvgRdrEachTrt}}
#' 
#' @return \strong{For \code{method = "OR"} the return list contains 4 dataframes:}
#' @return \item{FOMs}{Contains \code{foms}, \code{trtMeans} and \code{trtMeanDiffs}: 
#'    \code{\link{UtilFigureOfMerit}}}
#' @return \item{ANOVA}{Contains \code{TRAnova}, \code{VarCom}, \code{IndividualTrt} 
#'    and \code{IndividualRdr} ANOVA tables of FOM values}
#' @return \item{RRRC}{Contains results of \code{"RRRC"} analyses - same 
#'    organization as DBM, see above}
#' @return \item{FRRC}{Contains results of \code{"FRRC"} analyses - ditto}
#' @return \item{RRFC}{Contains results of \code{"RRFC"} analyses- ditto}
#' 
#' 
#' @examples
#' StSignificanceTesting(dataset02,FOM = "Wilcoxon", method = "DBM") 
#' StSignificanceTesting(dataset02,FOM = "Wilcoxon", method = "OR")
#' ## following is split-plot-c analysis using a simulated split-plot-c dataset
#' StSignificanceTesting(datasetFROCSpC, FOM = "wAFROC", method = "OR")
#' 
#' \donttest{
#' StSignificanceTesting(dataset05, FOM = "wAFROC")
#' StSignificanceTesting(dataset05, FOM = "HrAuc", method = "DBM") 
#' StSignificanceTesting(dataset05, FOM = "SongA1", method = "DBM") 
#' StSignificanceTesting(dataset05, FOM = "SongA2", method = "DBM") 
#' StSignificanceTesting(dataset05, FOM = "wAFROC1", method = "DBM")
#' StSignificanceTesting(dataset05, FOM = "AFROC1", method = "DBM")
#' StSignificanceTesting(dataset05, FOM = "AFROC", method = "DBM")
#' } 
#'
#' 
#' @references
#' Dorfman DD, Berbaum KS, Metz CE (1992) ROC characteristic rating analysis: 
#' Generalization to the Population of Readers and Patients with the Jackknife method, Invest. Radiol. 27, 723-731.
#' 
#' Obuchowski NA, Rockette HE (1995) Hypothesis Testing of the Diagnostic Accuracy for Multiple Diagnostic Tests:  
#' An ANOVA Approach with Dependent Observations, Communications in Statistics: Simulation and Computation 24, 285-308.
#' 
#' Hillis SL (2014) A marginal-mean ANOVA approach for analyzing multireader multicase radiological imaging data, 
#' Statistics in medicine 33, 330-360.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#'
#'
#' @importFrom stats pf pt qt
#' @importFrom Rcpp evalCpp
#' @useDynLib RJafroc
#'
#'      
#' @export
StSignificanceTesting <- function(dataset, FOM, FPFValue = 0.2, alpha = 0.05, method = "DBM", 
                                  covEstMethod = "jackknife", nBoots = 200, analysisOption = "ALL", tempOrgCode = FALSE)
{
  
  options(stringsAsFactors = FALSE, "digits" = 8)
  
  if (dataset$descriptions$design != "FCTRL") {
    method <- "OR"
    covEstMethod <- "jackknife"
  }
  
  I <- length(dataset$descriptions$modalityID)
  J <- length(dataset$descriptions$readerID)
  
  if (J == 1) analysisOption <- "FRRC" else if (I == 1) analysisOption <- "RRFC"
  
  if (dataset$descriptions$type == "ROI") {
    method <- "OR"
    covEstMethod <- "DeLong" 
    FOM <- "ROI"
    cat("ROI dataset: forcing method = `ORH`, covEstMethod = `DeLong` and FOM = `ROI`.\n")
  }
  
  if (!analysisOption %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not a valid analysis Option.", analysisOption)
    stop(errMsg)
  }    
  
  if (length(dataset$descriptions$modalityID) < 2) {
    analysisOption <- "FRRC"
    ErrMsg <- paste0("This analysis requires at least 2 treatments")
    stop(ErrMsg)
  }
  
  if ((length(dataset$ratings$NL[1,,1,1]) < 2) && (analysisOption != "FRRC")) {
    ErrMsg <- paste0("Must use analysisOption FRRC with 1-reader dataset")
    stop(ErrMsg)
  }
  
  if (method == "DBM"){
    if (covEstMethod != "jackknife") 
      stop("For DBM method `covEstMethod` must be jackknife")
  } else if (method == "OR") {
    if (!covEstMethod %in% c("jackknife", "bootstrap", "DeLong")) {
      errMsg <- paste0(covEstMethod, " is not an allowed covariance estimation method for ORH analysis.")
      stop(errMsg)
    }
  } else stop("Incorrect `method` argument: must be `DBM` or `OR`")
  
  if (((dataset$descriptions$design == "SPLIT-PLOT-C") || (dataset$descriptions$design == "SPLIT-PLOT-A")) && method == "DBM") 
    stop("Must use method = ORH for SPLIT-PLOT-A or SPLIT-PLOT-C dataset")
  
  if (((dataset$descriptions$design == "SPLIT-PLOT-C") || (dataset$descriptions$design == "SPLIT-PLOT-A")) && method == "OR" && covEstMethod != "jackknife") 
    stop("Must use covEstMethod = jackknife for SPLIT-PLOT-A or SPLIT-PLOT-C dataset")
  
  if (!tempOrgCode) {
    # new code
    if (method == "DBM"){
      
      return(StDBMHAnalysis(dataset, FOM, FPFValue, alpha, analysisOption))
      
    } else if (method == "OR") {
      
      if (dataset$descriptions$design == "FCTRL") {
        
        return(ORAnalysisFactorial(dataset, FOM, FPFValue, alpha, covEstMethod, nBoots, analysisOption))
        
      } else if (dataset$descriptions$design == "SPLIT-PLOT-A") {
        
        return(ORAnalysisSplitPlotA(dataset, FOM, FPFValue, alpha, analysisOption))
        
      } else if (dataset$descriptions$design == "SPLIT-PLOT-C") {
        
        return(ORAnalysisSplitPlotC(dataset, FOM, FPFValue, alpha, analysisOption))
        
      } else stop("Invalid study design: must be FCTRL, SPLIT-PLOT-A or SPLIT-PLOT-C")
    } else {
      errMsg <- sprintf("%s is not a valid analysis method.", method)
      stop(errMsg)
    }
  } else {
    # old code
    if (dataset$descriptions$design != "FCTRL") stop("old code requires FCTRL study design")
    if (method == "DBM"){
      return(DBMHAnalysis(dataset, FOM, alpha, analysisOption)) # original code: StOldCode.R
    } else if (method == "OR"){
      return(ORHAnalysis(dataset, FOM, alpha, covEstMethod, nBoots, analysisOption)) # original code: StOldCode.R
    } else {
      errMsg <- sprintf("%s is not a valid analysis method.", method)
      stop(errMsg)
    }
  }
}

