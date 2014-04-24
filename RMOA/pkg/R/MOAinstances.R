#' Define the attributes of a dataset (factor levels, numeric or string data) in a MOA setting
#'
#' Define the attributes of a dataset (factor levels, numeric or string data) in a MOA setting
#'
#' @param data object of class data.frame
#' @param ... other parameters currently not used yet
#' @return An object of class \code{MOAmodelAttributes}
#' @export 
#' @examples
#' data(iris)
#' mydata <- factorise(iris)
#' atts <- MOAattributes(data=mydata)
#' atts
MOAattributes <- function(data, ...){
  UseMethod(generic="MOAattributes", object=data)
}


##' @S3method MOAattributes data.frame
MOAattributes.data.frame <- function(data, ...){  
  ok <- all(sapply(data, FUN=function(x) inherits(x, c("integer","numeric","factor"))))
  if(!ok){
    stop("all columns need to be integer, numeric or factor, consider using factorise on your dataset before modelling")
  }
  
  nrattributes <- as.integer(ncol(data))
  attributes <- .jnew("java.util.ArrayList", nrattributes)
  levs <- list()
  for(attr in names(data)){    
    alllevels <- levels(data[[attr]])
    if(length(alllevels) > 0){
      levs[[attr]] <- alllevels
    }else{
      levs[[attr]] <- character(0)
    }    
    if(inherits(data[[attr]], "factor")){
      attributes$add(.jnew("weka/core/Attribute", attr, as.java.util.List(alllevels)))      
    }else{
      attributes$add(.jnew("weka/core/Attribute", attr))
    }
  }
  out <- list(columnattributes = attributes, levels = levs)
  class(out) <- "MOAmodelAttributes"
  out  
}

attribute <- function(x, ...){
  UseMethod(generic="attribute", object=x)
}
attribute.MOAmodelAttributes <- function(x, value){
  idx <- which(names(x$levels) == value)
  if(length(idx) == 0){
    stop(sprintf("attribute %s not found", value))
  }
  idx <- as.integer(idx - 1L)
  return(list(attribute = x$columnattributes$get(idx), pos = idx))
  #.jcall(x$columnattributes, "Lweka.core.Attribute;", "get", idx)  
}

## Create a java.util.List from a vector
## @param x a vector (of characters e.g.)
as.java.util.List <- function(x){
  l <- .jnew("java.util.ArrayList", as.integer(length(x)))
  done <- sapply(seq_along(x), FUN=function(i) l$add(i-1L, x[i]))  
  .jcast(l, "java.util.List")
}

## Converts all factors in the dataframe to integers while subtracting 1
## @param x a data.frame
as.train <- function(x){
  factorcolumns <- sapply(x, FUN=function(x) inherits(x, "factor"))
  factorcolumns <- factorcolumns[factorcolumns == TRUE]
  for(column in names(factorcolumns)){
    x[[column]] <- as.integer(x[[column]]) - 1L ## MOA codes levels starting from 0
  }
  x
}