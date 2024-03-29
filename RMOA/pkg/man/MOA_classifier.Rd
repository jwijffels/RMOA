% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classification.R
\name{MOA_classifier}
\alias{MOA_classifier}
\title{Create a MOA classifier}
\usage{
MOA_classifier(model, control = NULL, ...)
}
\arguments{
\item{model}{character string with a model.
E.g. HoeffdingTree, DecisionStump, NaiveBayes, HoeffdingOptionTree, ...
The list of known models can be obtained by typing RMOA:::.moaknownmodels. 
See the examples and \code{\link{MOAoptions}}.}

\item{control}{an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}}

\item{...}{options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
Ignored if \code{control} is supplied}
}
\value{
An object of class \code{MOA_classifier}
}
\description{
Create a MOA classifier
}
\examples{
RMOA:::.moaknownmodels
ctrl <- MOAoptions(model = "HoeffdingTree", leafprediction = "MC", 
   removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)
hdt <- MOA_classifier(model = "HoeffdingTree", control=ctrl)
hdt
hdt <- MOA_classifier(
 model = "HoeffdingTree", 
 numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
}
\seealso{
\code{\link{MOAoptions}}
}
