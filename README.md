RMOA
=========

RMOA allows to interface R with MOA (http://moa.cms.waikato.ac.nz/).

RMOA interfaces with MOA version 2014.04.


Installation
-----------
The package is currently not available at CRAN.

To install the latest version from github
```
library(devtools)
install.packages("ff")
install_github("jwijffels/RMOA", subdir="RMOAjars/pkg")
install_github("jwijffels/RMOA", subdir="RMOA/pkg")
```

Examples
=========

Currently an initial stab is made to allow building a MOA classification model. 
Examples are elaborated to build a HoeffdingTree.

```S
require(RMOA)

## Create a HoeffdingTree
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt

## Define a stream - e.g. a stream based on a data.frame
data(iris)
iris <- factorise(iris)
irisdatastream <- datastream_dataframe(data=iris)

## Train the HoeffdingTree on the iris dataset
mymodel <- trainMOA(model = hdt, 
  formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length, 
  data = irisdatastream)

## Predict using the HoeffdingTree on the iris dataset
scores <- predict(mymodel, newdata=iris, type="response")
str(scores)
table(scores, iris$Species)
scores <- predict(mymodel, newdata=iris, type="votes")
head(scores)
```

Streams
=========

Data streams are implemented for classic data in R (data.frame, matrix), data in files (csv, delimited, flat table)
as well as out-of memory data in an ffdf (ff package).


Models
=========

Currently RMOA focusses on classification models (as the stream package in R already allows clustering).
Classification models which are possible through RMOA are:

Trees: AdaHoeffdingOptionTree, ASHoeffdingTree, DecisionStump, HoeffdingAdaptiveTree, HoeffdingOptionTree, HoeffdingTree, LimAttHoeffdingTree, RandomHoeffdingTree, NaiveBayes, NaiveBayesMultinomial

