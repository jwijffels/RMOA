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
#' ctrl <- MOAoptions(model = "BRISMFPredictor", features = 10, lRate=0.002)
#' brism <- MOA_recommender(model = "BRISMFPredictor", control=ctrl)
#' brism
#' MOAoptions(model = "BaselinePredictor")
#' baseline <- MOA_recommender(model = "BaselinePredictor")
#' baseline
MOA_recommender <- function(model, control=NULL, ...){
  out <- list()
  class(out) <- c(model, "MOA_recommender", "MOA_model")
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

##' @S3method print MOA_recommender
print.MOA_recommender <- function(x, ...){
  print(x$options)
  try(cat(x$moamodel$toString()), silent=TRUE)
  invisible()
}

#' MOA recommendation engines
#'
#' MOA recommendation engines
#'
#' @name MOA_recommendation_engines
#' @param control an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}
#' @param ... options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
#' Ignored if \code{control} is supplied
#' @return An object of class \code{MOA_recommender} which sets up an untrained MOA model,
#' which can be trained using \code{\link{trainMOA}} 
#' @seealso \code{\link{MOAoptions}}, \code{\link{trainMOA}}
#' @examples
#' ctrl <- MOAoptions(model = "BRISMFPredictor", features = 10)
#' brism <- BRISMFPredictor(control=ctrl)
#' brism
#' baseline <- BaselinePredictor()
#' baseline
NULL
#' @export 
#' @rdname MOA_recommendation_engines
BRISMFPredictor <- function(control=NULL, ...) {
  MOA_recommender(model = "BRISMFPredictor", control=control, ...)
}
#' @export 
#' @rdname MOA_recommendation_engines
BaselinePredictor <- function(control=NULL, ...) {
  MOA_recommender(model = "BaselinePredictor", control=control, ...)
}