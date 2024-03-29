% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Datastreams.R
\docType{class}
\name{datastream_dataframe}
\alias{datastream_dataframe}
\title{data streams on a data.frame}
\arguments{
\item{data}{a data.frame to extract data from in a streaming way}
}
\value{
A class of type \code{datastream_dataframe} which contains
\describe{
  \item{data: }{The data.frame to extract instances from}
  \item{all fields of the datastream superclass: }{See \code{\link{datastream}}}
}
}
\description{
Reference object of class \code{datastream_dataframe}.
This is a class which inherits from class \code{datastream} and which can be used to read in a stream
from a data.frame.
}

\section{Methods}{

\itemize{
  \item \code{get_points(n)} Get data from a datastream object.
     \describe{
       \item{n}{integer, indicating the number of instances to retrieve from the datastream}
     }
}
}

\examples{
x <- datastream_dataframe(data=iris)
x$get_points(10)
x
x$get_points(10)
x
}
\seealso{
\code{\link{datastream}}
}
