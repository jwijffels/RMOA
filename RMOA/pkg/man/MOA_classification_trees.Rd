\name{MOA_classification_trees}
\alias{HoeffdingTree}
\alias{MOA_classification_trees}
\alias{NaiveBayes}
\title{MOA classification trees}
\usage{
HoeffdingTree(control = NULL, ...)

NaiveBayes(control = NULL, ...)
}
\arguments{
  \item{control}{an object of class \code{MOAmodelOptions}
  as obtained by calling \code{\link{MOAoptions}}}

  \item{...}{options of parameters passed on to
  \code{\link{MOAoptions}}, in case \code{control} is left
  to NULL. Ignored if \code{control} is supplied}
}
\value{
An object of class \code{MOA_classifier}
}
\description{
MOA classification trees
}
\examples{
ctrl <- MOAoptions(model = "HoeffdingTree", leafprediction = "MC",
   removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)
hdt <- HoeffdingTree(control=ctrl)
hdt
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
}
\seealso{
\code{\link{MOAoptions}}
}

