% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Datastreams.R
\docType{class}
\name{datastream}
\alias{datastream}
\title{Datastream objects and methods}
\arguments{
\item{description}{The name how the stream is labelled}

\item{args}{a list with arguments used to set up the stream and used in the datastream methods}
}
\value{
A class of type datastream which contains
\describe{
  \item{description: }{character with the name how the stream is labelled.}
  \item{state: }{integer with the current state at which the stream will read new instances of data}
  \item{processed: }{integer with the number of instances already processed}
  \item{finished: }{logical indicating if the stream has finished processing all the instances}
  \item{args: }{list with arguments passed on to the stream when it is created (e.g. arguments of read.table)}
}
}
\description{
Reference object of class datastream. This is a generic class which holds general 
information about the data stream.\cr
Currently streams are implemented for data in table format (streams of read.table, read.csv, read.csv2,
read.delim, read.delim2), data in RAM (data.frame, matrix), data in ff (on disk).\cr
See the documentation of \code{\link{datastream_file}}, \code{\link{datastream_dataframe}}, \code{\link{datastream_matrix}},
and \code{\link{datastream_ffdf}}
}

\examples{
## Basic example, showing the general methods available for a datastream object
x <- datastream(description = "My own datastream", args = list(a = "TEST"))
x
str(x)
try(x$get_points(x))
}
\seealso{
\code{\link{datastream_file}}
}
