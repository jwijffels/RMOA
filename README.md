RMOA
=========

RMOA allows to interface R with MOA (http://moa.cms.waikato.ac.nz/).


Installation
-----------
The package is currently not available at CRAN.

To install the latest version from github
```
library(devtools)
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

## Train the HoeffdingTree on the iris dataset
data(iris)
iris <- factorise(iris)
trainMOA(data=iris[sample(nrow(iris), size=round(nrow(iris)/2), replace=TRUE), ], 
         model=hdt, class="Species")
hdt

## Predict the HoeffdingTree on the iris dataset
scores <- predict(hdt, newdata= iris, type="response")
str(scores)
table(scores, iris$Species)
scores <- predict(hdt, newdata= iris, type="votes")
head(scores)
```


