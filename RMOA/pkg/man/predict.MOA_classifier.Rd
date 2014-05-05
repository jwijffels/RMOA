\name{predict.MOA_classifier}
\alias{predict.MOA_classifier}
\title{Predict using a MOA classifier}
\usage{
\method{predict}{MOA_classifier}(object, newdata, type = "response", ...)
}
\arguments{
  \item{object}{an object of class \code{MOA_classifier}}

  \item{newdata}{a data.frame with the same structure and
  the same levels as used in \code{trainMOA}}

  \item{type}{a character string, either 'response' or
  'votes'}

  \item{...}{other arguments, currently not used yet}
}
\value{
A matrix of votes or a vector with the predicted class
}
\description{
Predict using a MOA classifier
}
\examples{
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt
data(iris)
iris <- factorise(iris)
trainMOA(data=iris[sample(nrow(iris), size=round(nrow(iris)/2), replace=TRUE), ],
         model=hdt, response="Species")
hdt
scores <- predict(hdt, newdata= iris, type="response")
str(scores)
table(scores, iris$Species)
scores <- predict(hdt, newdata= iris, type="votes")
head(scores)
}

