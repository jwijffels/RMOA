RMOA
=========

RMOA allows to interface R with MOA (http://moa.cms.waikato.ac.nz/).

RMOA interfaces with MOA version 2014.04.
Documentation of MOA directed towards RMOA users can be found at http://jwijffels.github.io/RMOA/

Models
=========

Currently RMOA focusses on classification models (as the stream package in R already allows clustering).
Classification models which are possible through RMOA are:

- Classification trees: 
  * AdaHoeffdingOptionTree
  * ASHoeffdingTree
  * DecisionStump
  * HoeffdingAdaptiveTree
  * HoeffdingOptionTree
  * HoeffdingTree
  * LimAttHoeffdingTree
  * RandomHoeffdingTree
- Bayesian classification: 
  * NaiveBayes
  * NaiveBayesMultinomial
- Active learning classification:
  * ActiveClassifier
- Ensemble (meta) classifiers:
  * Bagging
      + LeveragingBag
      + OzaBag
      + OzaBagAdwin
      + OzaBagASHT
  * Boosting
      + OCBoost
      + OzaBoost
      + OzaBoostAdwin
  * Stacking
      + LimAttClassifier
  * Other
      + AccuracyUpdatedEnsemble
      + AccuracyWeightedEnsemble
      + ADACC
      + DACC
      + OnlineAccuracyUpdatedEnsemble
      + TemporallyAugmentedClassifier
      + WeightedMajorityAlgorithm
      
Streaming regression models are also included namely

- Rules: 
  * TargetMean and FadingTargetMean
  * Perceptron
  * AMRulesRegressor
- Trees: 
  * FIMTDD
  * ORTO
- Functions
  * SGD (Stochastic gradient descent)
  
Streaming recommendation engines which are made available are

  * BaselinePredictor
  * BRISMFPredictor
      
      
Installation
-----------
The package is currently available at CRAN.

To install the latest development version from github
```
library(devtools)
install.packages("ff")
install.packages("rJava")
install_github("jwijffels/RMOA", subdir="RMOAjars/pkg")
install_github("jwijffels/RMOA", subdir="RMOA/pkg")
```

Examples
=========

Examples below show how to construct, train and score using a HoeffdingTree and boosted ensemble of HoeffdingTree.

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


##
## Boosting example
##
irisdatastream$reset()
mymodel <- OzaBoost(baseLearner = "trees.HoeffdingTree", ensembleSize = 30)
mymodel <- trainMOA(model = mymodel, 
  formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length, 
  data = irisdatastream)

## Predict using the HoeffdingTree on the iris dataset
scores <- predict(mymodel, newdata=iris, type="response")
str(scores)
table(scores, iris$Species)
scores <- predict(mymodel, newdata=iris, type="votes")
head(scores)


##
## Streaming regressions and streaming recommendation engines. Examples can be found in the documentation
##
?trainMOA.MOA_regressor
?trainMOA.MOA_recommender
```

Streams
=========

Data streams are implemented for classic data in R (data.frame, matrix), data in files (csv, delimited, flat table)
as well as out-of memory data in an ffdf (ff package).



TODO
=========

Currently the following MOA models are not (yet) implemented in RMOA.
- Multilabel, drift, functions, rules classifiers
- Outlier detection
- Clustering





