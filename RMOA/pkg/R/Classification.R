

#' Create a HoeffdingTree
#'
#' Create a HoeffdingTree
#'
#' @param ... options of parameters passed on to \code{MOAoptions}
#' @return An object of class \code{HoeffdingTree}
#' @export 
#' @examples
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
HoeffdingTree <- function(...) {
  out <- list()
  class(out) <- c("HoeffdingTree", "MOA_classifier", "MOA_model")
  out$type <- "HoeffdingTree"
  ## Create the model
  out$moamodel <- .jnew(modelclass(out$type))  
  ## Set MOA options
  out$options <- MOAoptions(out, ...)  
  out$response <- character(0)
  ## And prepare for usage
  #.jcall(out$moamodel, "V", "prepareForUse")
  out
}

##' @S3method print MOA_classifier
print.MOA_classifier <- function(x, ...){
  print(x$options)
  cat(x$moamodel$toString())  
}



#' Train a MOA classifier, like e.g. a HoeffdingTree
#'
#' Train a MOA classifier, like e.g. a HoeffdingTree
#'
#' @param data a data.frame
#' @param model an object of class \code{MOA_classifier}
#' @param class a character string with a column name in \code{data}
#' @param reset logical indicating to reset the \code{MOA_classifier}. Defaults to TRUE.
#' @param ... other arguments, currently not used yet
#' @return An object of class \code{MOA_classifier}
#' @export 
#' @examples
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
#' data(iris)
#' iris <- iris[c("Species","Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
#' iris <- factorise(iris)
#' trainMOA(data=iris[sample(nrow(iris), size=10, replace=TRUE), ], model=hdt, class="Species")
#' hdt
trainMOA <- function(data, model, class, reset=TRUE, ...){    
  if(class != colnames(data)[1]){
    stop(sprintf("In MOA, you need to put your column to predict (%s) as the first column in your provided data data.frame", class))
  }
  atts <- MOAattributes(data=data)
  allinstances <- .jnew("weka.core.Instances", "data", atts$columnattributes, 1L)
  ## Set the response data to predict
  .jcall(allinstances, "V", "setClassIndex", attribute(atts, class)$pos)
  .jcall(allinstances, "V", "setClass", attribute(atts, class)$attribute)
  ## Prepare for usage
  .jcall(model$moamodel, "V", "setModelContext", .jnew("moa.core.InstancesHeader", allinstances))
  .jcall(model$moamodel, "V", "prepareForUse")
  if(reset){
    .jcall(model$moamodel, "V", "resetLearning") 
  }
  .jcall(allinstances, "V", "delete")
  
  ## Levels go from 0-nlevels in MOA, while in R from 1:nlevels
  trainme <- as.train(data)
  ## Loop over the data and train
  for(j in 1:nrow(trainme)){
    allinstances$add(0L, .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(trainme[j, ]))))
    model$moamodel$trainOnInstance(.jcast(allinstances$instance(0L), "weka/core/Instance"))
  }
  invisible(model)
} 

#' Summary statistics of a MOA classifier 
#'
#' Summary statistics of a MOA classifier 
#'
#' @param object an object of class  \code{MOA_classifier}
#' @param ... other arguments, currently not used yet
#' @return the form of the return value depends on the type of MOA model
#' @export 
#' @S3method summary MOA_classifier
#' @examples
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
#' data(iris)
#' iris <- iris[c("Species","Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
#' iris <- factorise(iris)
#' trainMOA(data=iris[sample(nrow(iris), size=round(nrow(iris)/2), replace=TRUE), ], 
#'          model=hdt, class="Species")
#' summary(hdt)
summary.MOA_classifier <- function(object, ...){
  out <- list()
  out$trainingHasStarted <- .jcall(object$moamodel, "Z", "trainingHasStarted")
  out$isRandomizable <- .jcall(object$moamodel, "Z", "isRandomizable")
  out$type <- object$type
  out$options <- object$options$options
  out$fields <- fields(object)[c("attributes","attribute.names","response","responselevels")]
  cat(out$type, sep="\n")
  cat(sprintf("Model has trained: %s", out$trainingHasStarted), sep="\n")
  #print.MOAmodelOptions(out$options)  
  invisible(out)
}



#' Predict using a MOA classifier
#'
#' Predict using a MOA classifier
#'
#' @param object an object of class  \code{MOA_classifier}
#' @param newdata a data.frame with the same structure and the same levels as used in \code{trainMOA}
#' @param type a character string, either 'response' or 'votes'
#' @param ... other arguments, currently not used yet
#' @return A matrix of votes or a vector with the predicted class 
#' @export 
#' @S3method predict MOA_classifier
#' @examples
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
#' data(iris)
#' iris <- iris[c("Species","Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
#' iris <- factorise(iris)
#' trainMOA(data=iris[sample(nrow(iris), size=round(nrow(iris)/2), replace=TRUE), ], 
#'          model=hdt, class="Species")
#' hdt
#' scores <- predict(hdt, newdata= iris, type="response")
#' str(scores)
#' table(scores, iris$Species)
#' scores <- predict(hdt, newdata= iris, type="votes")
#' head(scores)
predict.MOA_classifier <- function(object, newdata, type="response", ...){
  if(!.jcall(object$moamodel, "Z", "trainingHasStarted")){
    stop("Model is not trained yet")
  }
  columnnames <- fields(object)
  newdata <- as.train(newdata[, columnnames$attribute.names, drop = FALSE])
  
  atts <- MOAattributes(data=newdata)
  allinstances <- .jnew("weka.core.Instances", "data", atts$columnattributes, 1L)
  scores <- matrix(nrow = nrow(newdata), ncol = length(columnnames$responselevels))
  for(j in 1:nrow(newdata)){
    allinstances$add(0L, .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(newdata[j, ]))))    
    i <- allinstances$instance(0L)
    scores[j, ] <- object$moamodel$getVotesForInstance(.jcast(i, "weka/core/Instance"))
  }
  if(type == "votes"){
    return(scores)
  }else if(type == "response"){
    scores <- apply(scores, MARGIN=1, which.max) 
    return(sapply(scores, FUN=function(x) columnnames$responselevels[x]))
  }    
}


fields <- function(x, ...){
  UseMethod(generic="fields", object=x)
}
fields.MOA_classifier <- function(x){
  ctx <- x$moamodel$getModelContext()
  out <- list()
  out$label <- ctx$relationName()
  out$attributes <- ctx$numAttributes()
  out$attribute.names <- character(0)
  for(idx in 0:(out$attributes-1)){
    out$attribute.names <- append(out$attribute.names, ctx$attribute(idx)$name())
  }
  out$response <- ctx$classAttribute()$name()
  out$responselevels <- character()
  levs <- ctx$classAttribute()$enumerateValues()
  while(levs$hasMoreElements()){
    out$responselevels <- append(out$responselevels, levs$nextElement())
  }  
  class(out) <- "fields"
  out
}
