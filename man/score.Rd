% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score.R
\name{score}
\alias{score}
\alias{score.formula}
\alias{score.default}
\title{Computes the score test for transformation in regression}
\usage{
score(x, ...)

\method{score}{formula}(
  formula,
  data,
  subset,
  weights,
  na.action,
  model = TRUE,
  contrasts = NULL,
  offset,
  ...
)

\method{score}{default}(
  x,
  y,
  intercept = TRUE,
  la = c(-1, -0.5, 0, 0.5, 1),
  lik = FALSE,
  nocheck = FALSE,
  trace = FALSE,
  ...
)
}
\arguments{
\item{x}{An \code{n x p} data matrix (\code{n} observations and \code{p} variables).
 Rows of \code{x} represent observations, and columns represent variables.

 Missing values (NA's) and infinite values (Inf's) are allowed,
 since observations (rows) with missing or infinite values will
 automatically be excluded from the computations.}

\item{\dots}{potential further arguments passed to lower level functions.}

\item{formula}{a \code{\link{formula}} of the form \code{y ~ x1 + x2 + ...}.}

\item{data}{data frame from which variables specified in
\code{formula} are to be taken.}

\item{subset}{an optional vector specifying a subset of observations
to be used in the fitting process.}

\item{weights}{an optional vector of weights to be used
 %%% If specified, weighted least squares is used
 %%% with weights \code{weights} (that is, minimizing \code{sum(w*e^2)});
 %%% otherwise ordinary least squares is used.
\bold{NOT USED YET}.}

\item{na.action}{a function which indicates what should happen
when the data contain \code{NA}s.  The default is set by
the \code{na.action} setting of \code{\link{options}}, and is
\code{\link{na.fail}} if that is unset.  The \dQuote{factory-fresh}
default is \code{\link{na.omit}}.  Another possible value is
\code{NULL}, no action.  Value \code{\link{na.exclude}} can be useful.}

\item{model}{\code{\link{logical}} indicating if the
model frame, is to be returned.}

\item{contrasts}{an optional list.  See the \code{contrasts.arg}
of \code{\link{model.matrix.default}}.}

\item{offset}{this can be used to specify an \emph{a priori}
known component to be included in the linear predictor
during fitting.  An \code{\link{offset}} term can be included in the}

\item{y}{Response variable. A vector with \code{n} elements that
contains the response variable.}

\item{intercept}{wheather to use constant term (default is \code{intercept=TRUE}}

\item{la}{values of the transformation parameter for which it is necessary
to compute the score test. Default value of lambda is
\code{la=c(-1, -0.5, 0, 0.5, 1)}, i.e., the five most common values of lambda.}

\item{lik}{likelihood for the augmented model. If true the value of the likelihood
for the augmented model will be calculated and returend otherwise (default) only
the value of the score test will be given}

\item{nocheck}{Whether to check input arguments. If \code{nocheck=TRUE} no check is performed
on matrix \code{y} and matrix \code{X}. Notice that \code{y} and \code{X}
are left unchanged. In other words the additional column of ones for the
intercept is not added. The default is \code{nocheck=FALSE}.}

\item{trace}{Whether to print intermediate results. Default is \code{trace=FALSE}.}
}
\value{
An S3 object of class \code{\link{score.object}} will be returned which is basically a list
 containing the following elements:
 \enumerate{
 \item \code{la}: vector containing the values of lambda for which fan plot is constructed
 \item \code{Score}: a vector containing the values of the score test for
     each value of the transformation parameter.
 \item \code{Lik}: value of the likelihood. This output is produced only if lik=TRUE.
 }
}
\description{
Computes the score test for transformation in regression
}
\examples{

 \dontrun{
   data(wool)
   XX <- wool
   y <- XX[, ncol(XX)]
   X <- XX[, 1:(ncol(XX)-1), drop=FALSE]

   (out <- score(X, y))                          # call 'score' with all default parameters
   (out <- score(cycles~., data=wool))           # use the formula interface
   (out <- score(cycles~., data=wool, lik=TRUE)) # return the likelihood

   data(loyalty)
   head(loyalty)
   ##    la is a vector containing the values of \lambda which have to be tested
   (out <- score(amount_spent~., data=loyalty, la=c(0.25, 1/3, 0.4, 0.5)))
   (out <- score(amount_spent~., data=loyalty, la=c(0.25, 1/3, 0.4, 0.5), lik=TRUE))
 }

}
\references{
Atkinson, A.C. and Riani, M. (2000), \emph{Robust Diagnostic Regression Analysis} Springer Verlag, New York.
}
\author{
FSDA team, \email{valentin.todorov@chello.at}
}
