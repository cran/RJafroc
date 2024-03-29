#' Simulates an MRMC uncorrelated FROC dataset using the RSM
#' 
#' @description  Simulates an uncorrelated MRMC FROC dataset for specified numbers of
#'    readers and treatments 
#' 
#' @param mu     The mu parameter of the RSM
#' @param lambda The RSM lambda parameter
#' @param nu     The RSM nu parameter
#' @param zeta1  The lowest reporting threshold
#' @param I      The number of treatments
#' @param J      The number of readers
#' @param K1     The number of non-diseased cases
#' @param K2     The number of diseased cases
#' @param perCase    A K2 length array containing the numbers of lesions per diseased case
#' @param seed  The initial seed for the random number generator, the default 
#'     is \code{NULL}, as if no seed has been specified. 
#' 
#' @return The return value is an FROC dataset.
#' 
#' @details See book chapters on the Radiological Search Model (RSM) for details. 
#'    In this code correlations between ratings on the same case are assumed to be zero.
#' 
#' @examples
#' set.seed(1) 
#' K1 <- 5;K2 <- 7;
#' maxLL <- 2;perCase <- floor(runif(K2, 1, maxLL + 1))
#' mu <- 1;lambda <- 1;nu <- 0.99 ;zeta1 <- -1
#' I <- 2; J <- 5
#' 
#' frocDataRaw <- SimulateFrocDataset(
#'   mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
#'   I = I, J = J, K1 = K1, K2 = K2, perCase = perCase )
#'   
#' ## plot the data
#' ret <- PlotEmpiricalOperatingCharacteristics(frocDataRaw, opChType = "FROC")
#' ## print(ret$Plot)
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @importFrom stats rpois rnorm rbinom
#' 
#' @export

SimulateFrocDataset <- function(mu, lambda, nu, zeta1, I, J, K1, K2, perCase, seed = NULL){
  
  if (length(perCase) != K2) stop("SimulateFrocDataset: error in specification of number of lesions perCase vector.")
  if (!is.null(seed)) set.seed(seed)
  nNL <- rpois(I * J * (K1 + K2), lambda)
  dim(nNL) <- c(I,J,K1+K2)
  maxNL <- max(nNL)
  NL <- array(-Inf, dim = c(I, J, K1 + K2, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {  
      for (k in 1:(K1 + K2)) {
        nl <- rnorm(nNL[i,j,k])
        nl <- nl[order(nl, decreasing = TRUE)]
        nl[nl < zeta1] <- -Inf
        NL[i,j,k, ] <- c(nl, rep(-Inf, maxNL - nNL[i,j,k]))
      }
    }
  }
  
  maxLL <- max(perCase)
  LL <- array(-Inf, dim = c(I,J,K2, maxLL))
  
  for (i in 1:I) {
    for (j in 1:J) {  
      for (k in 1:K2){
        nLL <- rbinom(1, perCase[k], nu)
        ll <- rnorm(nLL, mu)
        ll <- ll[order(ll, decreasing = TRUE)]
        ll[ll < zeta1] <- -Inf
        LL[i,j,k, ] <- c(ll, rep(-Inf, maxLL - nLL))
      }
      
      IDs <- array(dim = c(K2, maxLL))
      weights <- array(dim = c(K2, maxLL))
      for (k in 1:K2){
        IDs[k, ] <- c(1:perCase[k], rep(-Inf, maxLL - perCase[k]))
        weights[k, ] <- c(rep(1 / perCase[k], perCase[k]), rep(-Inf, maxLL - perCase[k]))
      }
    }
  }  
  modalityID <- as.character(seq(1:I))
  readerID <- as.character(seq(1:J))
  fileName <- "NA"
  name <- NA
  design <- "FCTRL"
  truthTableStr <- NA
  type <- "FROC"
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
}


