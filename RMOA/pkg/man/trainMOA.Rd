\name{trainMOA}
\alias{trainMOA}
\title{Train a MOA classifier, like e.g. a HoeffdingTree}
\usage{
trainMOA(data, model, class, reset = TRUE, ...)
}
\arguments{
  \item{data}{a data.frame}

  \item{model}{an object of class \code{MOA_classifier}}

  \item{class}{a character string with a column name in
  \code{data}}

  \item{reset}{logical indicating to reset the
  \code{MOA_classifier}. Defaults to TRUE.}

  \item{...}{other arguments, currently not used yet}
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
iris <- iris[c("Species","Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
iris <- factorise(iris)
trainMOA(data=iris[sample(nrow(iris), size=10, replace=TRUE), ], model=hdt, class="Species")
hdt
}

