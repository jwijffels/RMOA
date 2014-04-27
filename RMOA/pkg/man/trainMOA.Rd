\name{trainMOA}
\alias{trainMOA}
\title{Train a MOA classifier, like e.g. a HoeffdingTree}
\usage{
trainMOA(data, model, class, reset = TRUE, trace = FALSE)
}
\arguments{
  \item{data}{a data.frame}

  \item{model}{an object of class \code{MOA_classifier}}

  \item{class}{a character string with a column name in
  \code{data}}

  \item{reset}{logical indicating to reset the
  \code{MOA_classifier}. Defaults to TRUE.}

  \item{trace}{logical, indicating to show trace
  information on how many rows are already processed or a
  positive integer number showing progress when the
  specified number of instances have been processed.}
}
\value{
An object of class \code{MOA_classifier}
}
\description{
Train a MOA classifier, like e.g. a HoeffdingTree
}
\examples{
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
data(iris)
iris <- factorise(iris)
trainMOA(data=iris[sample(nrow(iris), size=10, replace=TRUE), ], model=hdt, class="Species")
hdt
}

