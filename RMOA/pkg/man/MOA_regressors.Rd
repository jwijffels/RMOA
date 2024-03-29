% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Regression.R
\name{MOA_regressors}
\alias{MOA_regressors}
\alias{TargetMean}
\alias{FadingTargetMean}
\alias{Perceptron}
\alias{AMRulesRegressor}
\alias{FIMTDD}
\alias{ORTO}
\title{MOA regressors}
\usage{
TargetMean(control = NULL, ...)

FadingTargetMean(control = NULL, ...)

Perceptron(control = NULL, ...)

AMRulesRegressor(control = NULL, ...)

FIMTDD(control = NULL, ...)

ORTO(control = NULL, ...)
}
\arguments{
\item{control}{an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}}

\item{...}{options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
Ignored if \code{control} is supplied}
}
\value{
An object of class \code{MOA_classifier} which sets up an untrained MOA model,
which can be trained using \code{\link{trainMOA}}
}
\description{
MOA regressors
}
\examples{
ctrl <- MOAoptions(model = "FIMTDD", DoNotDetectChanges = TRUE, noAnomalyDetection=FALSE,
   univariateAnomalyprobabilityThreshold = 0.5, verbosity = 5)
mymodel <- FIMTDD(control=ctrl)
mymodel
mymodel <- FIMTDD(ctrlDoNotDetectChanges = FALSE)
mymodel
}
\seealso{
\code{\link{MOAoptions}}, \code{\link{trainMOA}}
}
