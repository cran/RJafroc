% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UtilMeanSquares.R
\name{UtilMeanSquares}
\alias{UtilMeanSquares}
\title{Calculate mean squares}
\usage{
UtilMeanSquares(dataset, FOM = "wJAFROC", method = "DBMH")
}
\arguments{
\item{dataset}{The dataset to be analyzed, see \link{RJafroc-package}.}

\item{FOM}{The figure of merit to be used in the calculation. The default 
is \code{"wJAFROC"}. See \link{UtilFigureOfMerit}.}

\item{method}{The method, in which the mean squares are calculated. The two 
valid options are \code{"DBMH"} (default) and \code{"ORH"}.}
}
\value{
A list contating all possible mean squares
}
\description{
Calculates the mean squares used in the DBMH and ORH methods
}
\details{
For \code{DBMH} method, \code{msT, msTR, msTC, msTRC} will not be available 
   if the dataset contains only one modality. Similarly, 
   \code{msR, msTR, msRC, msTRC} will not be returned for single reader dataset. 
   For \code{ORH} method, \code{msT, msR, msTR} will be returned for multiple 
   reader multiple modality dataset. \code{msT} is not available for sinlge 
   modality dataset, and \code{msR} is not available for single reader dataset.
}
\examples{
UtilMeanSquares(dataset02, FOM = "Wilcoxon")

UtilMeanSquares(dataset05, method = "ORH")

}