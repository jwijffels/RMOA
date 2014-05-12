#' Train a MOA classifier, like e.g. a HoeffdingTree
#'
#' Train a MOA classifier, like e.g. a HoeffdingTree
#'
#' @param data a data.frame
#' @param model an object of class \code{MOA_classifier}
#' @param response a character string with a column name in \code{data}
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
#' trainMOA(data=iris[sample(nrow(iris), size=10, replace=TRUE), ], model=hdt, response="Species")
#' hdt
trainMOA <- function(data, model, response, reset=TRUE, trace=FALSE){    
  atts <- MOAattributes(data=data)
  allinstances <- .jnew("weka.core.Instances", "data", atts$columnattributes, 0L)
  ## Set the response data to predict
  .jcall(allinstances, "V", "setClass", attribute(atts, response)$attribute)
  ## Prepare for usage
  .jcall(model$moamodel, "V", "setModelContext", .jnew("moa.core.InstancesHeader", allinstances))
  .jcall(model$moamodel, "V", "prepareForUse")
  if(reset){
    .jcall(model$moamodel, "V", "resetLearning") 
  }  
  ## Levels go from 0-nlevels in MOA, while in R from 1:nlevels
  trainme <- as.train(data)
  ## Loop over the data and train
  for(j in 1:nrow(trainme)){
    if(trace & (j / trace) == round(j / trace)){
      message(sprintf("%s MOA processed instance %s", Sys.time(), j))
    }
    oneinstance <- .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(trainme[j, ])))  
    .jcall(oneinstance, "V", "setDataset", allinstances)
    oneinstance <- .jcast(oneinstance, "weka/core/Instance")
    .jcall(model$moamodel, "V", "trainOnInstance", oneinstance)
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
#'          model=hdt, response="Species")
#' hdt
#' scores <- predict(hdt, newdata= iris[, setdiff(names(iris), "Species")], type="response")
#' str(scores)
#' table(scores, iris$Species)
#' scores <- predict(hdt, newdata= iris[, setdiff(names(iris), "Species")], type="votes")
#' head(scores)
predict.MOA_classifier <- function(object, newdata, type="response", ...){
  if(!.jcall(object$moamodel, "Z", "trainingHasStarted")){
    stop("Model is not trained yet")
  }
  columnnames <- fields(object)
  newdata[[columnnames$response]] <- factor(NA, levels = columnnames$responselevels) ## Needs the response data to create DenseInstance but this is unknown
  newdata <- as.train(newdata[, columnnames$attribute.names, drop = FALSE])
  
  atts <- MOAattributes(data=newdata)
  allinstances <- .jnew("weka.core.Instances", "data", atts$columnattributes, 0L)
  .jcall(allinstances, "V", "setClass", attribute(atts, columnnames$response)$attribute)
  
  scores <- matrix(nrow = nrow(newdata), ncol = length(columnnames$responselevels))
  for(j in 1:nrow(newdata)){
    oneinstance <- .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(newdata[j, ])))  
    .jcall(oneinstance, "V", "setDataset", allinstances)
    oneinstance <- .jcast(oneinstance, "weka/core/Instance")
    scores[j, ] <- object$moamodel$getVotesForInstance(oneinstance)
  }
  if(type == "votes"){
    return(scores)
  }else if(type == "response"){
    scores <- apply(scores, MARGIN=1, which.max) 
    return(sapply(scores, FUN=function(x) columnnames$responselevels[x]))
  }    
}

