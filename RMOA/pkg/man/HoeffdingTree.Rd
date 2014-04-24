\name{HoeffdingTree}
\alias{HoeffdingTree}
\title{Create a HoeffdingTree}
\usage{
HoeffdingTree(...)
}
\arguments{
  \item{...}{options of parameters passed on to
  \code{MOAoptions}}
}
\value{
An object of class \code{HoeffdingTree}
}
\description{
Create a HoeffdingTree
}
\examples{
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
}

