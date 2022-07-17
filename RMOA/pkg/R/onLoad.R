
# get the class loader from RMOAjars
.onLoad <- function(libname, pkgname){
  .rJava.class.loader <- .jclassLoader(package="RMOAjars")
  assign(".rJava.class.loader", .rJava.class.loader, envir = parent.env(environment()))
}

