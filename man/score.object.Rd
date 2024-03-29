% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3objectsdoc.R
\name{score.object}
\alias{score.object}
\title{Objects returned by the function \code{\link{score}}}
\value{
The functions \code{print()} and \code{summary()} are used to obtain and print a
 summary of the results. An object of class \code{score} is a list containing at least the following components:
 \enumerate{
 \item \code{la}: vector containing the values of lambda for which fan plot is constructed
 \item \code{Score}: a vector containing the values of the score test for
     each value of the transformation parameter.
 \item \code{Lik}: value of the likelihood. This output is produced only if lik=TRUE.
 }
}
\description{
An object of class \code{\link{score.object}} holds information about
 the result of a call to \code{\link{score}}.
}
\examples{

 \dontrun{
   data(wool)
   (out <- score(cycles~., data=wool, lik=TRUE))

   class(out)
   summary(out)
 }
}
\keyword{multivariate}
\keyword{robust}
