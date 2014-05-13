#' Train a MOA classifier (e.g. a HoeffdingTree) on a datastream
#'
#' Train a MOA classifier (e.g. a HoeffdingTree) on a datastream
#'
#' @param model an object of class \code{MOA_model}, as returned by \code{\link{MOA_classifier}}, e.g.
#' a \code{\link{HoeffdingTree}}
#' @param formula a symbolic description of the model to be fit.
#' @param data an object of class \code{\link{datastream}} set up e.g. with \code{\link{datastream_file}}, 
#' \code{\link{datastream_dataframe}}, \code{\link{datastream_matrix}}, \code{\link{datastream_ffdf}} or your own datastream.
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#' @param na.action a function which indicates what should happen when the data contain \code{NA}s. 
#' See \code{\link{model.frame}} for details.
#' @param transFUN a function which is used after obtaining \code{chunksize} number of rows 
#' from the \code{data} datastream before applying \code{\link{model.frame}}. Useful if you want to 
#' change the results \code{get_points} on the datastream 
#' (e.g. for making sure the factor levels are the same in each chunk of processing, some data cleaning, ...). 
#' Defaults to \code{\link{identity}}.
#' @param chunksize the number of rows to obtain from the \code{data} datastream in one chunk of model processing.
#' Defaults to 1000. Can be used to speed up things according to the backbone architecture of
#' the datastream.
#' @param reset logical indicating to reset the \code{MOA_classifier} so that it forgets what it 
#' already has learned. Defaults to TRUE.
#' @param trace logical, indicating to show information on how many datastream chunks are already processed
#' as a \code{message}.
#' @return An object of class MOA_trainedmodel which is a list with elements
#' \itemize{
#' \item{model: the updated supplied \code{model} object of class \code{MOA_classifier}}
#' \item{call: the matched call}
#' \item{na.action: the vatlue of na.action}
#' \item{terms: the \code{terms} in the model}
#' \item{transFUN: the transFUN argument}
#' }
#' @seealso \code{\link{MOA_classifier}}, \code{\link{datastream_file}}, \code{\link{datastream_dataframe}}, 
#' \code{\link{datastream_matrix}}, \code{\link{datastream_ffdf}}, \code{\link{datastream}}
#' @export 
#' @examples
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' hdt
#' data(iris)
#' iris <- factorise(iris)
#' irisdatastream <- datastream_dataframe(data=iris)
#' irisdatastream$get_points(3)
#' 
#' mymodel <- train(model = hdt, Species ~ Sepal.Length + Sepal.Width + Petal.Length, 
#'  data = irisdatastream, chunksize = 10)
#' mymodel$model
#' mymodel$model <- train(model = hdt, 
#'  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Length^2, 
#'  data = irisdatastream, chunksize = 10, reset=TRUE)
#' mymodel$model
train <- function(model, formula, data, subset, na.action, transFUN=identity, chunksize=1000, reset=TRUE, trace=FALSE){
  mc <- match.call()
  mf <- mc[c(1L, match(c("formula", "data", "subset", "na.action"), names(mc), 0L))]
  mf[[1L]] <- as.name("model.frame")
  mf[[3L]] <- as.name("datachunk")
  mf$drop.unused.levels <- FALSE
  setmodelcontext <- function(model, data, response){
    # build the weka instances structure
    atts <- MOAattributes(data=data)
    allinstances <- .jnew("weka.core.Instances", "data", atts$columnattributes, 0L)
    ## Set the response data to predict    
    .jcall(allinstances, "V", "setClass", attribute(atts, response)$attribute)
    ## Prepare for usage
    .jcall(model$moamodel, "V", "setModelContext", .jnew("moa.core.InstancesHeader", allinstances))
    .jcall(model$moamodel, "V", "prepareForUse")
    list(model = model, allinstances = allinstances)
  }
  trainchunk <- function(model, traindata, allinstances){
    ## Levels go from 0-nlevels in MOA, while in R from 1:nlevels
    traindata <- as.train(traindata)
    ## Loop over the data and train
    for(j in 1:nrow(traindata)){
      oneinstance <- .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(traindata[j, ])))  
      .jcall(oneinstance, "V", "setDataset", allinstances)
      oneinstance <- .jcast(oneinstance, "weka/core/Instance")
      .jcall(model$moamodel, "V", "trainOnInstance", oneinstance)
    }
    model
  }
  if(reset){
    .jcall(model$moamodel, "V", "resetLearning") 
  }  
  terms <- terms(formula)
  i <- 1
  while(!data$isfinished()){
    if(trace){
      message(sprintf("%s Running chunk %s: instances %s:%s", Sys.time(), i, (i*chunksize)-chunksize, i*chunksize))
    }
    ### Get data of chunk and extract the model.frame
    datachunk <- data$get_points(chunksize)
    if(is.null(datachunk)){
      break
    }
    data$hasread(nrow(datachunk))
    datachunk <- transFUN(datachunk)  
    traindata <- eval(mf)      
    if(i == 1){
      terms <- terms(traindata)
      ### Set up the data structure in MOA (levels, columns, ...)
      ct <- setmodelcontext(model=model, data=traindata, response=all.vars(formula)[1])
      model <- ct$model    
    }
    ### Learn the model
    model <- trainchunk(model = model, traindata = traindata, allinstances = ct$allinstances)  
    i <- i + 1
  }
  out <- list()
  out$model <- model
  out$call <- mc
  out$na.action <- attr(mf, "na.action")
  out$terms <- terms
  out$transFUN <- transFUN
  class(out) <- "MOA_trainedmodel"
  out
} 


#' Predict using a MOA classifier on a new dataset
#'
#' Predict using a MOA classifier on a new dataset. Make sure the new dataset has the same structure
#' and the same levels as \code{get_points} returns on the datastream which was used in \code{train}
#'
#' @param object an object of class \code{MOA_trainedmodel}, as returned by \code{\link{train}}
#' @param newdata a data.frame with the same structure and the same levels as used in \code{train}
#' @param type a character string, either 'response' or 'votes'
#' @param transFUN a function which is used on \code{newdata} 
#' before applying \code{\link{model.frame}}. 
#' Useful if you want to change the results \code{get_points} on the datastream 
#' (e.g. for making sure the factor levels are the same in each chunk of processing, some data cleaning, ...). 
#' Defaults to \code{transFUN} available in \code{object}.
#' @param ... other arguments, currently not used yet
#' @return A matrix of votes or a vector with the predicted class 
#' @export 
#' @S3method predict MOA_trainedmodel
#' @seealso \code{\link{train}}
#' @examples
#' ## Hoeffdingtree
#' hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#' data(iris)
#' ## Make a training set
#' iris <- factorise(iris)
#' traintest <- list()
#' traintest$trainidx <- sample(nrow(iris), size=nrow(iris)/2)
#' traintest$trainingset <- iris[traintest$trainidx, ]
#' traintest$testset <- iris[-traintest$trainidx, ]
#' irisdatastream <- datastream_dataframe(data=traintest$trainingset)
#' ## Train the model
#' hdtreetrained <- train(model = hdt, 
#'  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
#'  data = irisdatastream)
#' 
#' ## Score the model on the holdoutset
#' scores <- predict(hdtreetrained, 
#'    newdata=traintest$testset[, c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")], 
#'    type="response")
#' str(scores)
#' table(scores, traintest$testset$Species)
#' scores <- predict(hdtreetrained, newdata=traintest$testset, type="votes")
#' head(scores)
predict.MOA_trainedmodel <- function(object, newdata, type="response", transFUN=object$transFUN, ...){  
  if(!.jcall(object$model$moamodel, "Z", "trainingHasStarted")){
    stop("Model is not trained yet")
  }
  ## Apply transFUN and model.frame
  newdata <- transFUN(newdata)
  Terms <- delete.response(object$terms)
  newdata <- model.frame(Terms, newdata)
  
  object <- object$model
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
    colnames(scores) <- columnnames$responselevels
    return(scores)
  }else if(type == "response"){
    scores <- apply(scores, MARGIN=1, which.max) 
    scores <- sapply(scores, FUN=function(x) columnnames$responselevels[x])
    return(scores)
  }    
}

