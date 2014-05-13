RMOA
=========

RMOA allows to interface R with MOA (http://moa.cms.waikato.ac.nz/).


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
mymodel <- trainMOA(model = hdt, Species ~ Sepal.Length + Sepal.Width + Petal.Length, data = irisdatastream)

## Predict using the HoeffdingTree on the iris dataset
scores <- predict(mymodel, newdata=iris, type="response")
str(scores)
table(scores, iris$Species)
scores <- predict(mymodel, newdata=iris, type="votes")
head(scores)
```


