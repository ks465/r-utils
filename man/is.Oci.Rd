% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DbOracle.R
\name{is.Oci}
\alias{is.Oci}
\title{Test relationship between an object and DB object class}
\usage{
is.Oci(obj)
}
\value{
boolean.
}
\description{
Test relationship between an object and DB object class
}
\details{
Check to see if the given object has the class \code{Oci}.
}
\examples{
s1 <- Oci(db_host = '127.0.0.1', db_pass = '123456', db_name = 'test', db_user = 'root')
is.Oci(s1)
#[1] TRUE

s2 <- list(db_host = '127.0.0.1', db_pass = '123456', db_name = 'test', db_user = 'root')
is.Oci(s2)
#[1] FALSE

}
\seealso{
\code{\link{is}}
}
