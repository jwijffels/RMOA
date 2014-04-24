modelclass <- function(model){
  knownmodels <- c("HoeffdingTree", "NaiveBayes")
  if(!model %in% knownmodels){
    stop(sprintf("%s not an implemented model", as.character(model)))
  }
  recoder(model, 
          from = c("HoeffdingTree","NaiveBayes"),
          to = c("moa/classifiers/trees/HoeffdingTree", "moa/classifiers/bayes/NaiveBayes"))
}