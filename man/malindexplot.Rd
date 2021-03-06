% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/malindexplot.R
\name{malindexplot}
\alias{malindexplot}
\title{Plots the trajectory of minimum Mahalanobis distance (mmd)}
\usage{
malindexplot(out, p, xlab, ylab, main, nameX, conflev, trace = FALSE,
  ...)
}
\arguments{
\item{out}{a numeric vector or an object of S3 class (one of \code{\link{fsmult.object}},
\code{\link{smult.object}} or \code{\link{mmmult.object}}) returned by
one of the functions
\code{\link{fsmult}} or \code{\link{smult}} or \code{\link{mmmult}} -
a list containing the monitoring of minimum Mahalanobis distance}

\item{p}{If \code{out} is a vector, p is its length (i.e. number of variables).}

\item{xlab}{A title for the x axis}

\item{ylab}{A title for the y axis}

\item{main}{An overall title for the plot}

\item{nameX}{Add variable labels in the plot. A vector of strings of length \code{p}
containing the labels of the variables of the original data matrix \code{X}.
If it is empty (default) the sequence \code{X1, ..., Xp} will be created automatically}

\item{conflev}{confidence interval for the horizontal bands. It can be a vector of
different confidence level values, e.g. c(0.95, 0.99, 0.999).
The confidence interval is based on the chi^2 distribution.}

\item{trace}{Whether to print intermediate results. Default is \code{trace=FALSE}.}

\item{...}{potential further arguments passed to lower level functions.}
}
\value{
none
}
\description{
Plots the trajectory of minimum Mahalanobis distance (mmd)
}
\examples{

 \dontrun{
 ##  Mahalanobis distance plot of 100 random numbers.
 ##  Numbers are from from the chisq with 5 degrees of freedom

 malindexplot(rchisq(100, 5), 5)
 }

}
\references{
Atkinson and Riani (2000), Robust Diagnostic Regression Analysis, Springer Verlag, New York.
}
\author{
FSDA team, \email{valentin.todorov@chello.at}
}
