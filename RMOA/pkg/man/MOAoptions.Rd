\name{MOAoptions}
\alias{MOAoptions}
\title{Get and set options for models build with MOA.}
\usage{
MOAoptions(model, ...)
}
\arguments{
  \item{model}{character string with a model or an object
  of class \code{MOA_model}}

  \item{...}{other parameters}
}
\value{
An object of class \code{MOAmodelOptions}
}
\description{
Get and set options for models build with MOA.
}
\examples{
control <- MOAoptions(model = "HoeffdingTree")
control
control <- MOAoptions(model = "NaiveBayes")
control
}

