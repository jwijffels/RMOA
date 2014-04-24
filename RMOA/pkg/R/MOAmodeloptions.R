getMOAoptions <- function(x){
  result <- list()
  alloptions <- x$getOptionArray()
  for(i in seq_along(alloptions)){
    optionname <- .jcall(alloptions[[i]], "S", "getName")
    result[[optionname]] <- list()
    result[[optionname]]$Name <- optionname
    result[[optionname]]$Purpose <- .jcall(alloptions[[i]], "S", "getPurpose")    
    result[[optionname]]$Value <- .jcall(alloptions[[i]], "S", "getStateString")
    try(result[[optionname]]$Value <- alloptions[[i]]$getValue(), silent=TRUE)
  }
  result
}

setMOAoptions <- function(x, ...){
  params <- list(...)
  done <- sapply(params, function(x) FALSE)

  alloptions <- x$getOptionArray()
  for(i in seq_along(alloptions)){
    optionname <- .jcall(alloptions[[i]], "S", "getName")
    if(optionname %in% names(params)){
      value <- params[[optionname]]
      if(isTRUE(value)){
        value <- tolower(as.character(value))
      }
      .jcall(alloptions[[i]], "V", "setValueViaCLIString", as.character(value))    
      done[[optionname]] <- TRUE
    }    
  }
  if(sum(!done) > 0){
    warning(sprintf("Following MOA options do not exist and are hence not changed: %s", paste(names(done)[done == FALSE], collapse=", ")))
  }
  invisible(done)
}


#' Get and set options for models build with MOA.
#'
#' Get and set options for models build with MOA.
#'
#' @param model character string with a model or an object of class \code{MOA_model}
#' @param ... other parameters
#' @return An object of class \code{MOAmodelOptions}
#' @export 
#' @examples
#' control <- MOAoptions(model = "HoeffdingTree")
#' control
#' control <- MOAoptions(model = "NaiveBayes")
#' control
MOAoptions <- function(model, ...){
  if(inherits(model, "character")){
    moamodel <- .jnew(modelclass(model))  
  }else{
    moamodel <- model$moamodel
    model <- model$type
  }    
  moaoptions <- moamodel$getOptions()  
  setMOAoptions(moaoptions, ...)
  opts <- list(model = model, 
               moamodelname = .jcall(moamodel, "S", "getPurposeString"), 
               javaObj = moaoptions, 
               options = getMOAoptions(moaoptions))
  class(opts) <- "MOAmodelOptions"
  opts
}



##' @S3method print MOAmodelOptions
print.MOAmodelOptions <- function(x, ...){  
  cat(sprintf("%s modelling options: ", x$model), sep="\n")
  cat(sprintf("MOA model name: %s", x$moamodelname), sep="\n")
  x <- x$options
  for(i in seq_along(x)){
    cat(sprintf("  - %s: %s   (%s)", x[[i]]$Name, x[[i]]$Value, x[[i]]$Purpose), sep="\n")
  }
}


