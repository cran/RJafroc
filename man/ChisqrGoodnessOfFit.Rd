% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ChisqrGoodnessOfFit.R
\name{ChisqrGoodnessOfFit}
\alias{ChisqrGoodnessOfFit}
\title{Compute the chisquare goodness of fit statistic for ROC fitting model}
\usage{
ChisqrGoodnessOfFit(fpCounts, tpCounts, parameters, model, lesDistr)
}
\arguments{
\item{fpCounts}{The FP counts table}

\item{tpCounts}{The TP counts table}

\item{parameters}{The parameters of the model including cutoffs, see details}

\item{model}{The fitting model: "BINORMAL", "CBM" or "RSM}

\item{lesDistr}{The lesion distribution matrix; not needed for "BINORMAL" or "CBM" models}
}
\value{
The return value is a list with the following elements:

\item{chisq}{The chi-square statistic}

\item{pVal}{The p-value of the fit}

\item{df}{The degrees of freedom}
}
\description{
Compute the chisquare goodness of fit statistic for specified ROC data fitting model
}
\details{
For model = "BINORMAL" the parameters are c(a,b,zetas).
For model = "CBM" the parameters are c(mu,alpha,zetas).
For model = "RSM" the parameters are c(mu,lambdaP,nuP,zetas).
}