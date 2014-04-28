
#' Create a MOA classifier
#'
#' Create a MOA classifier
#'
#' @param model character string with a model.
#' E.g. HoeffdingTree, DecisionStump, NaiveBayes, HoeffdingOptionTree, ...
#' The list of known models can be obtained by typing RMOA:::.moaknownmodels. 
#' See the examples and \code{\link{MOAoptions}}.
#' @param control an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}
#' @param ... options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
#' Ignored if \code{control} is supplied
#' @return An object of class \code{MOA_classifier}
#' @seealso \code{\link{MOAoptions}}
#' @export 
#' @examples
#' RMOA:::.moaknownmodels
#' ctrl <- MOAoptions(model = "HoeffdingTree", leafprediction = "MC", 
#'    removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)
#' hdt <- MOA_classifier(model = "HoeffdingTree", control=ctrl)
#' hdt
#' hdt <- MOA_classifier(
#'  model = "HoeffdingTree", 
#'  numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
MOA_classifier <- function(model, control=NULL, ...){
  out <- list()
  class(out) <- c(model, "MOA_classifier", "MOA_model")
  out$type <- model
  ## Create the model
  out$moamodel <- .jnew(modelclass(out$type))  
  ## Set MOA options
  if(inherits(control, "MOAmodelOptions")){
    if(control$model != out$type){
      stop("Make control contains options for the correct model")
    }
    out$options <- control
  }else{
    out$options <- MOAoptions(out, ...)   
  }  
  out
}

##' @S3method print MOA_classifier
print.MOA_classifier <- function(x, ...){
  print(x$options)
  cat(x$moamodel$toString())  
}


#' Create a HoeffdingTree
#'
#' Create a HoeffdingTree
#'
#' @param control an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}
#' @param ... options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
#' Ignored if \code{control} is supplied
#' @return An object of class \code{HoeffdingTree}
#' @seealso \code{\link{MOAoptions}}
#' @export 
#' @examples
#' ctrl <- MOAoptions(model = "HoeffdingTree", leafprediction = "MC", 
#'    removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)
#' hdt <- HoeffdingTree(control=ctrl)
#' hdt
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
HoeffdingTree <- function(control=NULL, ...) {
  MOA_classifier(model = "HoeffdingTree", control=control, ...)
}






#' Train a MOA classifier, like e.g. a HoeffdingTree
#'
#' Train a MOA classifier, like e.g. a HoeffdingTree
#'
#' @param data a data.frame
#' @param model an object of class \code{MOA_classifier}
#' @param class a character string with a column name in \code{data}
#' @param reset logical indicating to reset the \code{MOA_classifier}. Defaults to TRUE.
#' @param trace logical, indicating to show trace information on how many rows are already processed or a positive
#' integer number showing progress when the specified number of instances have been processed.
#' @return An object of class \code{MOA_classifier}
#' @export 
#' @examples
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
#' data(iris)
#' iris <- factorise(iris)
#' trainMOA(data=iris[sample(nrow(iris), size=10, replace=TRUE), ], model=hdt, class="Species")
#' hdt
trainMOA <- function(data, model, class, reset=TRUE, trace=FALSE){    
  atts <- MOAattributes(data=data)
  allinstances <- .jnew("weka.core.Instances", "data", atts$columnattributes, 1L)
  ## Set the response data to predict
  #.jcall(allinstances, "V", "setClassIndex", attribute(atts, class)$pos)
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
    if(trace & (j / trace) == round(j / trace)){
      message(sprintf("%s MOA processing instance %s", Sys.time(), j))
    }
    #.jcall(allinstances, "V", "add", 0L, .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(trainme[j, ]))))
    allinstances$add(0L, .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(trainme[j, ]))))
    .jcall(model$moamodel, "V", "trainOnInstance", .jcast(allinstances$instance(0L), "weka/core/Instance")) 
    #model$moamodel$trainOnInstance(.jcast(allinstances$instance(0L), "weka/core/Instance"))    
  }
  invisible(model)
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
  #.jcall(allinstances, "V", "setClassIndex", attribute(atts, columnnames$response)$pos)
  .jcall(allinstances, "V", "setClass", attribute(atts, columnnames$response)$attribute)
  
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
  class(out) <- "summary_MOA_classifier"
  out
}

##' @S3method print summary_MOA_classifier
print.summary_MOA_classifier <- function(x, ...){
  cat(x$type, sep="\n")  
  cat(sprintf("response: %s", x$fields$response), sep="\n")
  cat(sprintf("responselevels: %s", paste(x$fields$responselevels, collapse=", ")), sep="\n")
  cat(sprintf("data features: %s", paste(x$fields$attribute.names, collapse=", ")), sep="\n")
  cat(sprintf("Model has trained: %s", x$trainingHasStarted), sep="\n")
  #print.MOAmodelOptions(x$options) 
}







fields <- function(x, ...){
  UseMethod(generic="fields", object=x)
}
fields.MOA_classifier <- function(x){
  ctx <- x$moamodel$getModelContext()
  out <- list()
  out$label <- .jcall(ctx, "S", "relationName")
  out$attributes <- .jcall(ctx, "I", "numAttributes")
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
