modelclass <- function(model){
  knownmodels <- c("AdaHoeffdingOptionTree", "ASHoeffdingTree", "DecisionStump", "HoeffdingAdaptiveTree", "HoeffdingOptionTree",
                   "HoeffdingTree", "LimAttHoeffdingTree", "RandomHoeffdingTree", 
                   "NaiveBayes", "NaiveBayesMultinomial")
  if(!model %in% knownmodels){
    stop(sprintf("%s not an implemented model", as.character(model)))
  }
  recoder(model, 
          from = c("AdaHoeffdingOptionTree",
                   "ASHoeffdingTree",
                   "DecisionStump",
                   "HoeffdingAdaptiveTree",
                   "HoeffdingOptionTree",
                   "HoeffdingTree",
                   "LimAttHoeffdingTree", 
                   "RandomHoeffdingTree",
                   "NaiveBayes",
                   "NaiveBayesMultinomial"),
          to = c("moa/classifiers/trees/AdaHoeffdingOptionTree",
                 "moa/classifiers/trees/ASHoeffdingTree",
                 "moa/classifiers/trees/DecisionStump",
                 "moa/classifiers/trees/HoeffdingAdaptiveTree",
                 "moa/classifiers/trees/HoeffdingOptionTree",
                 "moa/classifiers/trees/HoeffdingTree", 
                 "moa/classifiers/trees/LimAttHoeffdingTree", 
                 "moa/classifiers/trees/RandomHoeffdingTree", 
                 "moa/classifiers/bayes/NaiveBayes",
                 "moa/classifiers/bayes/NaiveBayesMultinomial"))
}

