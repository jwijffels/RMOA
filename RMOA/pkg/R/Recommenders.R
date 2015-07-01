#' Create a MOA recommendation engine
#'
#' Create a MOA recommendation engine
#'
#' @param model character string with a model.
#' E.g. BRISMFPredictor, BaselinePredictor
#' The list of known models can be obtained by typing RMOA:::.moaknownmodels. 
#' See the examples and \code{\link{MOAoptions}}.
#' @param control an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}
#' @param ... options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
#' Ignored if \code{control} is supplied
#' @return An object of class \code{MOA_recommender}
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




#' Summary statistics of a MOA recommender 
#'
#' Summary statistics of a MOA recommender 
#'
#' @param object an object of class  \code{MOA_recommender}
#' @param ... other arguments, currently not used yet
#' @return the form of the return value depends on the type of MOA model
#' @export 
#' @S3method summary MOA_recommender
#' @examples
#' require(recommenderlab)
#' data(MovieLense)
#' x <- getData.frame(MovieLense)
#' x$itemid <- as.integer(as.factor(x$item))
#' x$userid <- as.integer(as.factor(x$user))
#' x$rating <- as.numeric(x$rating)
#' x <- head(x, 2000)
#' 
#' movielensestream <- datastream_dataframe(data=x)
#' movielensestream$get_points(3)
#' 
#' ctrl <- MOAoptions(model = "BRISMFPredictor", features = 10)
#' brism <- BRISMFPredictor(control=ctrl)
#' mymodel <- trainMOA(model = brism, rating ~ userid + itemid, 
#'  data = movielensestream, chunksize = 1000, trace=TRUE)
#' 
#' overview <- summary(mymodel$model)
#' str(overview)
#' predict(mymodel, head(x, 10), type = "response")
summary.MOA_recommender <- function(object, ...){
  out <- list()
  out$type <- object$type
  out$options <- object$options$options
  x <- try(object$moamodel$getData(), silent=TRUE)
  if(!inherits(x, "try-error")){
    out$nr.users <- x$getNumUsers()
    out$nr.items <- x$getNumItems()
    out$nr.rating <- x$getNumRatings()
    out$rating.range <- c(x$getMinRating(), x$getMaxRating())
    out$rating.globalmean <- x$getGlobalMean() 
    out$users <- x$getItems()$toArray()
    out$users <- sapply(out$users, FUN=function(item) .jcall(item, returnSig="I", method = "intValue", check=FALSE, use.true.class = FALSE))
    out$items <- x$getUsers()$toArray()
    out$items <- sapply(out$items, FUN=function(item) .jcall(item, returnSig="I", method = "intValue", check=FALSE, use.true.class = FALSE))
  }
  class(out) <- "summary_MOA_recommender"
  out
}

##' @S3method print summary_MOA_recommender
print.summary_MOA_recommender <- function(x, ...){
  cat(x$type, sep="\n")  
  cat(sprintf("number of users: %s, items: %s, ratings: %s", x$nr.users, x$nr.items, x$nr.rating), sep="\n")
  cat(sprintf("ratings min: %s, max: %s, mean: %s", x$rating.range[1], x$rating.range[2], x$rating.globalmean), sep="\n")
}