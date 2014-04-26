\name{summary.MOA_classifier}
\alias{summary.MOA_classifier}
\title{Summary statistics of a MOA classifier}
\usage{
\method{summary}{MOA_classifier}(object, ...)
}
\arguments{
  \item{object}{an object of class \code{MOA_classifier}}

  \item{...}{other arguments, currently not used yet}
}
\value{
the form of the return value depends on the type of MOA
model
}
\description{
Summary statistics of a MOA classifier
}
\examples{
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
data(iris)
iris <- factorise(iris)
trainMOA(data=iris[sample(nrow(iris), size=round(nrow(iris)/2), replace=TRUE), ],
         model=hdt, class="Species")
summary(hdt)
}

