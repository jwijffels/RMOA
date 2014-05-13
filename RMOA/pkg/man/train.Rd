\name{train}
\alias{train}
\title{Train a MOA classifier (e.g. a HoeffdingTree) on a datastream}
\usage{
train(model, formula, data, subset, na.action, transFUN = identity,
  chunksize = 1000, reset = TRUE)
}
\arguments{
  \item{model}{an object of class \code{MOA_model}, as
  returned by \code{\link{MOA_classifier}}, e.g. a
  \code{\link{HoeffdingTree}}}

  \item{formula}{a symbolic description of the model to be
  fit.}

  \item{data}{an object of class \code{\link{datastream}}
  set up e.g. with \code{\link{datastream_file}},
  \code{\link{datastream_dataframe}},
  \code{\link{datastream_matrix}} and
  \code{\link{datastream_ffdf}}}

  \item{subset}{an optional vector specifying a subset of
  observations to be used in the fitting process.}

  \item{na.action}{a function which indicates what should
  happen when the data contain \code{NA}s. See
  \code{\link{model.frame}} for details.}

  \item{transFUN}{a function which is used after obtaining
  \code{chunksize} number of rows from the \code{data}
  datastream before applying \code{\link{model.frame}}.
  Defaults to \code{\link{identity}}}

  \item{chunksize}{the number of rows to obtain from the
  \code{data} datastream in one chunk of model processing}

  \item{reset}{logical indicating to reset the
  \code{MOA_classifier}. Defaults to TRUE.}
}
\value{
An object of class \code{MOA_classifier}
}
\description{
Train a MOA classifier (e.g. a HoeffdingTree) on a
datastream
}
\examples{
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
data(iris)
iris <- factorise(iris)
irisdatastream <- datastream_dataframe(data=iris)
train(model = hdt, Species ~ Sepal.Length + Sepal.Width + Petal.Length,
 data = irisdatastream, chunksize = 10)
hdt
train(model = hdt, Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Length^2,
 data = irisdatastream, chunksize = 10, reset=TRUE)
hdt
}

