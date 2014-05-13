
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
#' irisdatastream <- datastream_dataframe(data=iris)
#' ## Train the model
#' hdtreetrained <- trainMOA(model = hdt, 
#'  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#'  data = irisdatastream)
#' summary(hdtreetrained$model)
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



