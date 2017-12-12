\name{mdrplot}
\alias{mdrplot}
\title{
     Plots the trajectory of minimum deletion residual (mdr)
}
\description{
     Plots the trajectory of minimum deletion residual (mdr).
}
\usage{
    mdrplot(out, quant = c(0.01, 0.5, 0.99), exact = 0, sign = TRUE, 
        mplus1 = FALSE, envm, 
        xlim, ylim, xlab, ylab, main, 
        lwdenv, lwd, cex.lab, cex.axis, 
        tag, datatooltip, label, nameX, namey, databrush, 
        ...)
}
\arguments{

  \item{out}{
    An object returned by FSReda() (see \code{\link{FSReda_control}}).
    
    The needed elements of \code{out} are 
    \enumerate{
        \item \code{mdr}: Minimum deletion residual. A matrix containing the monitoring of 
            minimum deletion residual in each step of the forward search. The first column 
            of mdr must contain the fwd search index. 
        \item \code{Un}: (for FSR only) - matrix containing the order of entry in the subset of each unit 
            (required only when datatooltip is true or databrush is not empty).
        \item \code{y}: a vector containing the response (required only when option databrush is requested).
        \item \code{X}: a matrix containing the explanatory variables (required only when option databrush is requested).
        \item \code{Bols}: (n-init+1) x (p+1) matrix containing the estimated beta coefficients monitored in 
            each step of the robust procedure (required only when option databrush 
            is requested and suboption multivarfit is requested).
    }
}
  \item{quant}{
    Quantiles for which envelopes have to be computed. The default is to 
    produce 1\%, 50\% and 99\% envelopes. In other words the default 
    is \code{quant=c(0.01, 0.5, 0.99)}.
}
  \item{exact}{
    Exact of approximate cdf for envelope calculation. If it \code{exact=TRUE} 
    the calculation of the quantiles of the T and F distribution is based on 
    functions \code{finv()} and \code{tinv()} from the Matlab statistics toolbox, 
    otherwise the calculations of the former quantiles is based on functions 
    \code{invcdff()} and \code{invcdft()}. The solution has a tolerance of 
    1e-8.
    
    Remark: the use of functions tinv and finv is more precise but requires more time. 
    The default value is \code{exact=0} (approximate solution).
}
  \item{sign}{
    Wheather to use MDR with sign: if \code{sign=TRUE} (default) we 
    distinguish steps for which minimum deletion 
    residual was associated with positive or negative value of the residual. 
    Steps associated with positive values of mdr are plotted in black, 
    while other steps are plotted in red.
}
  \item{mplus1}{
    Wheather to plot the (m+1)-th order statistic. Specifies if it is necessary to plot 
    the curve associated with (m+1)-th order statistic.
}
  \item{envm}{
    Sample size for drawing enevlopes. Specifies the size of the sample which is 
    used to superimpose the envelope. The default is to add an envelope based on 
    all the observations (size n envelope).
}
  \item{ylim}{Control \code{y} scale in plot. Vector with two elements controlling 
    minimum and maximum on the y axis. Default is to use automatic scale.
}
  \item{xlim}{Control \code{x} scale in plot. Vector with two elements controlling 
    minimum and maximum on the x axis. Default is to use automatic scale.
}   
  \item{xlab}{
    a title for the x axis
}
  \item{ylab}{
    a title for the y axis
}
  \item{main}{
    an overall title for the plot
}
  \item{lwdenv}{
    Controls the width of the lines associated with the envelopes, default is \code{lvdenv=1}.
}
  \item{lwd}{
    Controls the linewidth of the curve which contains the monitoring of minimum deletion residual.
}
  \item{cex.lab}{
    The magnification to be used for x and y labels relative to the current setting of cex
}
  \item{cex.axis}{
    The magnification to be used for axis annotation relative to the current setting of cex
}
  \item{tag}{
    Plot handle. String which identifies the handle of the plot which is about to be created. 
    The default is to use tag 'pl_mdr'. Notice that if the program finds a plot which has 
    a tag equal to the one specified by the user, then the output of the new plot overwrites 
    the existing one in the same window else a new window is created.
}
  \item{datatooltip}{
%%     ~~Describe \code{datatooltip} here~~
}
  \item{label}{
    Character vector containing the labels of the units (optional argument used when 
    \code{datatooltip=TRUE}. If this field is not present labels 
    row1, ..., rown will be automatically created and included 
    in the pop up datatooltip window).
}
  \item{nameX}{Add variable labels in plot. A vector of strings of length \code{p} 
  containing the labels of the variables of the regression dataset. 
  If it is empty (default) the sequence \code{X1, ..., Xp} will be created automatically
}
  \item{namey}{Add response label. A string containing the label of the response}
  \item{databrush}{
%%     ~~Describe \code{databrush} here~~
}
\item{\dots}{
    potential further arguments passed to lower level functions.
}
}
\details{
    No details
}
\value{
    No value returned
}

\author{
FSDA team
}

\examples{
\dontrun{

n <- 100
y <- rnorm(n)
X <- matrix(rnorm(n*4), nrow=n)

out <- fsreg(y~X, method="LTS")
out <- fsreg(y~X, method="FS", bsb=out$bs, monitoring=TRUE)
mdrplot(out)
}
}
\keyword{robust}
\keyword{regression}


