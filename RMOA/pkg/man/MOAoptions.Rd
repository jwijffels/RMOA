\name{MOAoptions}
\alias{MOAoptions}
\title{Get and set options for models build with MOA.}
\usage{
MOAoptions(model, ...)
}
\arguments{
  \item{model}{character string with a model or an object
  of class \code{MOA_model}. E.g. HoeffdingTree,
  DecisionStump, NaiveBayes, HoeffdingOptionTree, ... The
  list of known models can be obtained by typing
  RMOA:::.moaknownmodels. See the examples.}

  \item{...}{other parameters specifying the MOA modelling
  options of each model. See the examples.}
}
\value{
An object of class \code{MOAmodelOptions}.\cr This is a
list with elements: \enumerate{ \item model: The name of
the model \item moamodelname: The purpose of the model
known by MOA (getPurposeString) \item javaObj: a java
reference of MOA options \item options: a list with options
of the MOA model. Each list element contains the
\code{Name} of the option, the \code{Purpose} of the option
and the current \code{Value} } See the examples.
}
\description{
Get and set options for models build with MOA.
}
\examples{
control <- MOAoptions(model = "HoeffdingTree")
control
MOAoptions(model = "HoeffdingTree", leafprediction = "MC",
   removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)

## Other models known by RMOA
RMOA:::.moaknownmodels
MOAoptions(model = "AdaHoeffdingOptionTree")
MOAoptions(model = "ASHoeffdingTree")
MOAoptions(model = "DecisionStump")
MOAoptions(model = "HoeffdingAdaptiveTree")
MOAoptions(model = "HoeffdingOptionTree")
MOAoptions(model = "HoeffdingTree")
MOAoptions(model = "LimAttHoeffdingTree")
MOAoptions(model = "RandomHoeffdingTree")
MOAoptions(model = "NaiveBayes")
MOAoptions(model = "NaiveBayesMultinomial")
MOAoptions(model = "ActiveClassifier")
}

