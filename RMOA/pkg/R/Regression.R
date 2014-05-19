#' Create a MOA regressor
#'
#' Create a MOA regressor
#'
#' @param model character string with a model.
#' E.g. AMRulesRegressor, FadingTargetMean, FIMTDD, ORTO, Perceptron, RandomRules, SGD, TargetMean, ...
#' The list of known models can be obtained by typing RMOA:::.moaknownmodels. 
#' See the examples and \code{\link{MOAoptions}}.
#' @param control an object of class \code{MOAmodelOptions} as obtained by calling \code{\link{MOAoptions}}
#' @param ... options of parameters passed on to \code{\link{MOAoptions}}, in case \code{control} is left to NULL. 
#' Ignored if \code{control} is supplied
#' @return An object of class \code{MOA_regressor}
#' @seealso \code{\link{MOAoptions}}
#' @export 
#' @examples
#' mymodel <- MOA_regressor(model = "Perceptron")
#' mymodel
#' data(iris)
#' iris <- factorise(iris)
#' irisdatastream <- datastream_dataframe(data=iris)
#' ## Train the model
#' mytrainedmodel <- trainMOA(model = mymodel, 
#'  Sepal.Length ~ Petal.Length, data = irisdatastream)
#' mytrainedmodel$model
#' 
#' summary(lm(Sepal.Length ~ Petal.Length, data = iris))
#' predict(mytrainedmodel, newdata=iris)
MOA_regressor <- function(model, control=NULL, ...){
  out <- list()
  class(out) <- c(model, "MOA_regressor", "MOA_model")
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

##' @S3method print MOA_regressor
print.MOA_regressor <- function(x, ...){
  print(x$options)
  try(cat(x$moamodel$toString()), silent=TRUE)
  invisible()
}