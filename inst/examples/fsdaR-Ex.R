library(tictoc)

pkgname <- "fsdaR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('fsdaR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')

nameEx("FSR_control")
### * FSR_control

flush(stderr()); flush(stdout())

### Name: FSR_control
### Title: Creates an 'FSR_control' object
### Aliases: FSR_control
### Keywords: robust regression

### ** Examples


(out <- fsreg(Y~., data=hbk, method="FS", control=FSR_control(h=56, nsamp=500, lms=2)))
summary(out)




nameEx("FSReda_control")
### * FSReda_control

flush(stderr()); flush(stdout())

### Name: FSReda_control
### Title: Creates an 'FSReda_control' object
### Aliases: FSReda_control
### Keywords: robust regression

### ** Examples

(out <- fsreg(Y~., data=hbk, method="FS", monitoring=TRUE,
    control=FSReda_control(tstat="scal")))




nameEx("LXS_control")
### * LXS_control

flush(stderr()); flush(stdout())

### Name: LXS_control
### Title: Creates an 'LSX_control' object
### Aliases: LXS_control
### Keywords: robust regression

### ** Examples

(out <- fsreg(Y~., data=hbk, method="LMS", control=LXS_control(h=56, nsamp=500, lms=2)))




nameEx("MMreg_control")
### * MMreg_control

flush(stderr()); flush(stdout())

### Name: MMreg_control
### Title: Creates an 'MMreg_control' object
### Aliases: MMreg_control
### Keywords: robust regression

### ** Examples

## (out <- fsreg(Y~., data=hbk, method="MM", control=MMreg_control(eff=0.99, rhofunc="optimal")))
(out <- fsreg(Y~., data=hbk, method="MM", control=MMreg_control(eff=0.99)))




nameEx("MMregeda_control")
### * MMregeda_control

flush(stderr()); flush(stdout())

### Name: MMregeda_control
### Title: Creates an 'MMregeda_control' object
### Aliases: MMregeda_control
### Keywords: robust regression

### ** Examples

(out <- fsreg(Y~., data=hbk, method="MM", monitoring=TRUE,
    control=MMregeda_control(eff=seq(0.75, 0.99, 0.01))))




nameEx("Sreg_control")
### * Sreg_control

flush(stderr()); flush(stdout())

### Name: Sreg_control
### Title: Creates an 'Sreg_control' object
### Aliases: Sreg_control
### Keywords: robust regression

### ** Examples

(out <- fsreg(Y~., data=hbk, method="S", control=Sreg_control(bdp=0.25, nsamp=500)))




## VT::13.11.2021 - this usually takes to long - lets jump over it ...
if(FALSE)
{
nameEx("Sregeda_control")
### * Sregeda_control

flush(stderr()); flush(stdout())

### Name: Sregeda_control
### Title: Creates an 'Sregeda_control' object
### Aliases: Sregeda_control
### Keywords: robust regression

### ** Examples


(out <- fsreg(Y~., data=hbk, method="S", monitoring=TRUE,
    control=Sregeda_control(nsamp=500, rhofunc='hyperbolic')))

}


## VT::13.11.2021 - it is very slow, also carbikeplot() throws an error, have to invetigate it...
if(FALSE)
{

nameEx("carbikeplot")
### * carbikeplot

flush(stderr()); flush(stdout())

### Name: carbikeplot
### Title: Produces the carbike plot to find best relevant clustering
###   solutions obtained by 'tclustICsol'
### Aliases: carbikeplot

### ** Examples

##  car-bike plot for the geyser data

 data(geyser2)

tic()
out <- tclustIC(geyser2, whichIC="MIXMIX", plot=FALSE, alpha=0.1)
toc()

 ## Plot first two best solutions using as Information criterion MIXMIX
tic()
 print("Best solutions using MIXMIX")
 outMIXMIX <- tclustICsol(out, whichIC="MIXMIX", plot=FALSE, NumberOfBestSolutions=6)
 print(outMIXMIX$MIXMIXbs)
toc()

 carbikeplot(outMIXMIX)



##  car-bike plot for the flea data

 data(flea)
    ## we would need this if the numerical variables of flea were integer!
    ## Y <- matrix(as.double(as.matrix(flea[, 1:(ncol(flea)-1)])), ncol=ncol(flea)-1)
 Y <- as.matrix(flea[, 1:(ncol(flea)-1)])
 rownames(Y) <- 1:nrow(Y)
 head(Y)

tic()
out <- tclustIC(Y, whichIC="CLACLA", plot=FALSE, alpha=0.1, nsamp=100)
toc()

 ##  Find the best solutions using as Information criterion CLACLA
 print("Best solutions using CLACLA")
 outCLACLA <- tclustICsol(out,whichIC="CLACLA", plot=FALSE, NumberOfBestSolutions=66)

 ##  Produce the car-bike plot
 carbikeplot(outCLACLA)

}


nameEx("corfwdplot")
### * corfwdplot

flush(stderr()); flush(stdout())

### Name: corfwdplot
### Title: Monitoring the correlations between consecutive distances or
###   residuals
### Aliases: corfwdplot

### ** Examples


 data(hbk)
 (out <- fsmult(hbk[,1:3], monitoring=TRUE))
 corfwdplot(out)

 (out <- fsmult(hbk[,1:3], monitoring=TRUE))
 corfwdplot(out)

 (out1 <- smult(hbk, monitoring=TRUE, trace=TRUE))
 corfwdplot(out1)

 (out2 <- mmmult(hbk[,1:3], monitoring=TRUE, trace=TRUE))
 corfwdplot(out2)

 (out3 <- fsreg(hbk[,1:3], hbk[,4], monitoring=TRUE, trace=TRUE, method="FS"))
 corfwdplot(out3)

 (out4 <- fsreg(hbk[,1:3], hbk[,4], monitoring=TRUE, trace=TRUE, method="S"))
 corfwdplot(out4)

 (out5 <- fsreg(hbk[,1:3], hbk[,4], monitoring=TRUE, trace=TRUE, method="MM"))
 corfwdplot(out5)





nameEx("covplot")
### * covplot

flush(stderr()); flush(stdout())

### Name: covplot
### Title: Monitoring of the covariance matrix
### Aliases: covplot

### ** Examples


 X <- iris[,1:4]
 out <- fsmult(X, monitoring=TRUE)

 ##  Produce monitoring covariances plot with all the default options
 covplot(out)





nameEx("diabetes")
### * diabetes

flush(stderr()); flush(stdout())

### Name: diabetes
### Title: Diabetes data
### Aliases: diabetes
### Keywords: datasets

### ** Examples

data(diabetes)
head(diabetes)
plot(CovMcd(diabetes[, 1:3]), which="pairs", col=diabetes$class)




nameEx("fsdalms.object")
### * fsdalms.object

flush(stderr()); flush(stdout())

### Name: fsdalms.object
### Title: Description of 'fsdalms' Objects
### Aliases: fsdalms.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="LMS"))
    class(out)
    summary(out)




nameEx("fsdalts.object")
### * fsdalts.object

flush(stderr()); flush(stdout())

### Name: fsdalts.object
### Title: Description of 'fsdalts' Objects
### Aliases: fsdalts.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="LTS"))
    class(out)
    summary(out)




nameEx("fsmeda.object")
### * fsmeda.object

flush(stderr()); flush(stdout())

### Name: fsmeda.object
### Title: Description of 'fsmeda.object' Objects
### Aliases: fsmeda.object
### Keywords: robust multivariate

### ** Examples

    (out <- fsmult(hbk[,1:3], monitoring=TRUE))
    class(out)
    summary(out)


nameEx("fsmmmdrs")
### * fsmmmdrs

flush(stderr()); flush(stdout())

### Name: fsmmmdrs
### Title: Performs random start monitoring of minimum Mahalanobis distance
### Aliases: fsmmmdrs

### ** Examples

 data(hbk)
 out <- fsmmmdrs(hbk[,1:3])
 class(out)
 summary(out)


nameEx("fsmmmdrs.object")
### * fsmmmdrs.object

flush(stderr()); flush(stdout())

### Name: fsmmmdrs.object
### Title: Description of 'fsmmmdrs.object' Objects
### Aliases: fsmmmdrs.object
### Keywords: robust multivariate

### ** Examples


    out <- fsmmmdrs(hbk[,1:3])
    class(out)
    summary(out)


nameEx("fsmult")
### * fsmult

flush(stderr()); flush(stdout())

### Name: fsmult
### Title: Gives an automatic outlier detection procedure in multivariate
###   analysis
### Aliases: fsmult

### ** Examples


 data(hbk)
 (out <- fsmult(hbk[,1:3]))
 class(out)
 summary(out)

 ##  Generate contaminated data (200,3)
 n <- 200
 p <- 3
 set.seed(123456)
 X <- matrix(rnorm(n*p), nrow=n)
 Xcont <- X
 Xcont[1:5, ] <- Xcont[1:5,] + 3

 out1 <- fsmult(Xcont, trace=TRUE)           # no plots (plot defaults to FALSE)
 names(out1)

 (out1 <- fsmult(Xcont, trace=TRUE, plot=TRUE))    # identical to plot=1

 ## plot=1 - minimum MD with envelopes based on n observations
 ##  and the scatterplot matrix with the outliers highlighted
 (out1 <- fsmult(Xcont, trace=TRUE, plot=1))

 ## plot=2 - additional plots of envelope resuperimposition
 (out1 <- fsmult(Xcont, trace=TRUE, plot=2))

 ## plots is a list: plots showing envelope superimposition in normal coordinates.
 (out1 <- fsmult(Xcont, trace=TRUE, plot=list(ncoord=1)))

 ##  Choosing an initial subset formed by the three observations with
 ##  the smallest Mahalanobis Distance.

 (out1 <- fsmult(Xcont, m0=5, crit="md", trace=TRUE))

 ## fsmult() with monitoring
 (out2 <- fsmult(Xcont, monitoring=TRUE, trace=TRUE))
 names(out2)

 ## Monitor the exceedances from m=200 without showing plots.
 n <- 1000
 p <- 10
 Y <- matrix(rnorm(10000), ncol=10)
 (out <- fsmult(Y, init=200))

 ##  Forgery Swiss banknotes examples.

 data(swissbanknotes)

 ## Monitor the exceedances of Minimum Mahalanobis Distance
 (out1 <- fsmult(swissbanknotes[101:200,], plot=1))

 ##  Control minimum and maximum on the x axis
 (out1 <- fsmult(swissbanknotes[101:200,], plot=list(xlim=c(60,90))))

 ##  Monitor the exceedances of Minimum Mahalanobis Distance using
 ##  normal coordinates for mmd.
 (out1 <- fsmult(swissbanknotes[101:200,], plot=list(ncoord=1)))





nameEx("fsmult.object")
### * fsmult.object

flush(stderr()); flush(stdout())

### Name: fsmult.object
### Title: Description of 'fsmult.object' Objects
### Aliases: fsmult.object
### Keywords: robust multivariate

### ** Examples

    (out <- fsmult(hbk[,1:3]))
    class(out)
    summary(out)




nameEx("fsr.object")
### * fsr.object

flush(stderr()); flush(stdout())

### Name: fsr.object
### Title: Description of 'fsr' Objects
### Aliases: fsr.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="FS"))
    class(out)
    summary(out)




nameEx("fsreda.object")
### * fsreda.object

flush(stderr()); flush(stdout())

### Name: fsreda.object
### Title: Description of 'fsreda' Objects
### Aliases: fsreda.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="FS", monitoring=TRUE))
    class(out)
    summary(out)




nameEx("fsreg")
### * fsreg

flush(stderr()); flush(stdout())

### Name: fsreg
### Title: fsreg: an automatic outlier detection procedure in linear
###   regression
### Aliases: fsreg fsreg.default fsreg.formula print.fsr print.sreg
###   print.mmreg print.fsdalms print.fsdalts print.fsreda print.sregeda
###   print.mmregeda
### Keywords: robust regression

### ** Examples


    n <- 200
    p <- 3

    X <- matrix(data=rnorm(n*p), nrow=n, ncol=p)
    y <- matrix(data=rnorm(n*1), nrow=n, ncol=1)
    (out = fsreg(X, y))

    ## Now we use the formula interface:
    (out1 = fsreg(y~X, control=FSR_control(plot=FALSE)))

    ## Or use the variables in a data frame
    (out2 = fsreg(Y~., data=hbk, control=FSR_control(plot=FALSE)))

    ## let us compare to the LTS solution
    (out3 = ltsReg(Y~., data=hbk))

    ## Now compute the model without intercept
    (out4 = fsreg(Y~.-1, data=hbk, control=FSR_control(plot=FALSE)))

    ## And compare again with the LTS solution
    (out5 = ltsReg(Y~.-1, data=hbk))

    ## using default (optional arguments)
    (out6 = fsreg(Y~.-1, data=hbk, control=FSR_control(plot=FALSE, nsamp=1500, h=50)))




nameEx("levfwdplot")
### * levfwdplot

flush(stderr()); flush(stdout())

### Name: levfwdplot
### Title: Plots the trajectories of the monitored scaled (squared)
###   residuals
### Aliases: levfwdplot
### Keywords: robust regression

### ** Examples


n <- 100
y <- rnorm(n)
X <- matrix(rnorm(n*4), nrow=n)

out <- fsreg(y~X, method="LTS")
out <- fsreg(y~X, method="FS", bsb=out$bs, monitoring=TRUE)
levfwdplot(out)





nameEx("loyalty")
### * loyalty

flush(stderr()); flush(stdout())

### Name: loyalty
### Title: Loyalty data
### Aliases: loyalty
### Keywords: datasets

### ** Examples

data(loyalty)




nameEx("malfwdplot")
### * malfwdplot

flush(stderr()); flush(stdout())

### Name: malfwdplot
### Title: Plots the trajectories of scaled Mahalanobis distances along the
###   search
### Aliases: malfwdplot

### ** Examples


 ## Produce monitoring MD plot with all the default options.
 ##  Generate input structure for malfwdplot
 n <- 100
 p <- 4
 Y <- matrix(rnorm(n*p), ncol=p)
 Y[1:10,] <- Y[1:10,] + 4

 out <- fsmult(Y, monitoring=TRUE, init=30)

 ##  Produce monitoring MD plot with all the default options
 malfwdplot(out)




nameEx("malindexplot")
### * malindexplot

flush(stderr()); flush(stdout())

### Name: malindexplot
### Title: Plots the trajectory of minimum Mahalanobis distance (mmd)
### Aliases: malindexplot

### ** Examples

 ##  Mahalanobis distance plot of 100 random numbers.
 ##  Numbers are from from the chisq with 5 degrees of freedom

 malindexplot(rchisq(100, 5), 5)





nameEx("mdrplot")
### * mdrplot

flush(stderr()); flush(stdout())

### Name: mdrplot
### Title: Plots the trajectory of minimum deletion residual (mdr)
### Aliases: mdrplot
### Keywords: robust regression

### ** Examples


n <- 100
y <- rnorm(n)
X <- matrix(rnorm(n*4), nrow=n)

out <- fsreg(y~X, method="LTS")
out <- fsreg(y~X, method="FS", bsb=out$bs, monitoring=TRUE)
mdrplot(out)




nameEx("mmdplot")
### * mmdplot

flush(stderr()); flush(stdout())

### Name: mmdplot
### Title: Plots the trajectory of minimum Mahalanobis distance (mmd)
### Aliases: mmdplot

### ** Examples


 data(hbk)
 (out <- fsmult(hbk[,1:3], monitoring=TRUE))
 mmdplot(out)



##  VT::13.11.2021 - we still have the bug in fsmmmdrs and related functions - Unrecognized function or variable 'progbar'.
if(FALSE)
{

nameEx("mmdrsplot")
### * mmdrsplot

flush(stderr()); flush(stdout())

### Name: mmdrsplot
### Title: Plots the trajectories of minimum Mahalanobis distances from
###   different starting points
### Aliases: mmdrsplot

### ** Examples

 data(hbk)
 out <- fsmmmdrs(hbk[,1:3])
 mmdrsplot(out)

}



nameEx("mmmult")
### * mmmult

flush(stderr()); flush(stdout())

### Name: mmmult
### Title: Computes MM estimators in multivariate analysis with auxiliary
###   S-scale
### Aliases: mmmult

### ** Examples

 data(hbk)
 (out <- mmmult(hbk[,1:3]))
 class(out)
 summary(out)

 ##  Generate contaminated data (200,3)
 n <- 200
 p <- 3
 set.seed(123456)
 X <- matrix(rnorm(n*p), nrow=n)
 Xcont <- X
 Xcont[1:5, ] <- Xcont[1:5,] + 3

 out1 <- mmmult(Xcont, trace=TRUE)           # no plots (plot defaults to FALSE)
 names(out1)

 ## plot=TRUE - generates: (1) a plot of Mahalanobis distances against
 ##    index number. The confidence level used to draw the confidence bands for
 ##    the MD is given by the input option conflev. If conflev is
 ##    not specified a nominal 0.975 confidence interval will be used and
 ##    (2) a scatter plot matrix with the outliers highlighted.

 (out1 <- mmmult(Xcont, trace=TRUE, plot=TRUE))

 ## plots is a list: the spm shows the labels of the outliers.
 (out1 <- mmmult(Xcont, trace=TRUE, plot=list(labeladd="1")))

 ## plots is a list: the spm uses the variable names provided by 'nameY'.
 (out1 <- mmmult(Xcont, trace=TRUE, plot=list(nameY=c("A", "B", "C"))))

 ## mmmult() with monitoring
 (out2 <- mmmult(Xcont, monitoring=TRUE, trace=TRUE))
 names(out2)

 ##  Forgery Swiss banknotes examples.

 data(swissbanknotes)

 (out1 <- mmmult(swissbanknotes[101:200,], plot=TRUE))

 (out1 <- mmmult(swissbanknotes[101:200,], plot=list(labeladd="1")))




nameEx("mmmult.object")
### * mmmult.object

flush(stderr()); flush(stdout())

### Name: mmmult.object
### Title: Description of 'mmmult.object' Objects
### Aliases: mmmult.object
### Keywords: robust multivariate

### ** Examples

    data(hbk)
    (out <- mmmult(hbk[,1:3]))
    class(out)
    summary(out)




nameEx("mmmulteda.object")
### * mmmulteda.object

flush(stderr()); flush(stdout())

### Name: mmmulteda.object
### Title: Description of 'mmmulteda.object' Objects
### Aliases: mmmulteda.object
### Keywords: robust multivariate

### ** Examples

    data(hbk)
    (out <- mmmult(hbk[,1:3], monitoring=TRUE))
    class(out)
    summary(out)




nameEx("mmreg.object")
### * mmreg.object

flush(stderr()); flush(stdout())

### Name: mmreg.object
### Title: Description of mmreg Objects
### Aliases: mmreg.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="MM"))
    class(out)
    summary(out)




nameEx("mmregeda.object")
### * mmregeda.object

flush(stderr()); flush(stdout())

### Name: mmregeda.object
### Title: Description of 'mmregeda' Objects
### Aliases: mmregeda.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="MM", monitoring=TRUE))
    class(out)
    summary(out)




nameEx("poison")
### * poison

flush(stderr()); flush(stdout())

### Name: poison
### Title: Poison
### Aliases: poison
### Keywords: datasets

### ** Examples

 data(poison)
 head(poison)





nameEx("regspmplot")
### * regspmplot

flush(stderr()); flush(stdout())

### Name: regspmplot
### Title: Interactive scatterplot matrix for regression
### Aliases: regspmplot

### ** Examples

 ##  Example of the use of function regspmplot with all the default options
 ##  regsmplot() with first argument vector y and no option.
 ##  In the first example as input there are two matrices: y and X respectively
 ##  A simple plot is created

 n <- 100
 p <- 3
 X <- matrix(data=rnorm(n*p), nrow=n, ncol=p)
 y <- matrix(data=rnorm(n*1), nrow=n, ncol=1)
 regspmplot(y, X)

 ##  Example of the use of function regspmplot with first argument
 ##  vector y and third argument group.
 ##  Different groups are shown in the yXplot

 group <- rep(0, n)
 group[1:(n/2)] <- rep(1, n/2)
 regspmplot(y, X, group)

 ##  Example of the use of function regspmplot with first argument
 ##  vector y, third argument group and fourth argument plot
 ##  (Ex1) plot=TRUE

 regspmplot(y, X, group, plot=TRUE)

 ##  (Ex1) Set the scale for the x axes, the y axis and control symbol type
 regspmplot(y, X, group, xlim=c(-1,2), ylim=c(0,2), pch=c(10,11), trace=TRUE)

 ##  When the first input argument is an object.
 ##  In the following example the input is an object which also contains
 ##  information about the forward search.
     (out <- fsreg(y~X, method="LMS", control=LXS_control(nsamp=1000)))
     (out <- fsreg(y~X, bsb=out$bs, monitoring=TRUE))

     regspmplot(out, plot=0)




nameEx("resfwdplot")
### * resfwdplot

flush(stderr()); flush(stdout())

### Name: resfwdplot
### Title: Plots the trajectories of the monitored scaled (squared)
###   residuals
### Aliases: resfwdplot
### Keywords: robust regression

### ** Examples

n <- 100
y <- rnorm(n)
X <- matrix(rnorm(n*4), nrow=n)

out <- fsreg(y~X, method="LTS")
out <- fsreg(y~X, method="FS", bsb=out$bs, monitoring=TRUE)
resfwdplot(out)




nameEx("resindexplot")
### * resindexplot

flush(stderr()); flush(stdout())

### Name: resindexplot
### Title: Plots the residuals from a regression analysis versus index
###   number or any other variable
### Aliases: resindexplot
### Keywords: robust regression

### ** Examples

out <- fsreg(stack.loss~., data=stackloss)
resindexplot(out, conflev=c(0.95,0.99), col="green")




nameEx("smult")
### * smult

flush(stderr()); flush(stdout())

### Name: smult
### Title: Computes S estimators in multivariate analysis
### Aliases: smult

### ** Examples

 data(hbk)
 (out <- smult(hbk[,1:3]))
 class(out)
 summary(out)

 ##  Generate contaminated data (200,3)
 n <- 200
 p <- 3
 set.seed(123456)
 X <- matrix(rnorm(n*p), nrow=n)
 Xcont <- X
 Xcont[1:5, ] <- Xcont[1:5,] + 3

 out1 <- smult(Xcont, trace=TRUE)           # no plots (plot defaults to FALSE)
 names(out1)

 ## plot=TRUE - generates: (1) a plot of Mahalanobis distances against
 ##    index number. The confidence level used to draw the confidence bands for
 ##    the MD is given by the input option conflev. If conflev is
 ##    not specified a nominal 0.975 confidence interval will be used and
 ##    (2) a scatter plot matrix with the outliers highlighted.

 (out1 <- smult(Xcont, trace=TRUE, plot=TRUE))

 ## plots is a list: the spm shows the labels of the outliers.
 (out1 <- smult(Xcont, trace=TRUE, plot=list(labeladd="1")))

 ## plots is a list: the spm uses the variable names provided by 'nameY'.
 (out1 <- smult(Xcont, trace=TRUE, plot=list(nameY=c("A", "B", "C"))))

 ## smult() with monitoring
 (out2 <- smult(Xcont, monitoring=TRUE, trace=TRUE))
 names(out2)

 ##  Forgery Swiss banknotes examples.

 data(swissbanknotes)

 (out1 <- smult(swissbanknotes[101:200,], plot=TRUE))

 (out1 <- smult(swissbanknotes[101:200,], plot=list(labeladd="1")))




nameEx("smult.object")
### * smult.object

flush(stderr()); flush(stdout())

### Name: smult.object
### Title: Description of 'smult.object' Objects
### Aliases: smult.object
### Keywords: robust multivariate

### ** Examples

    data(hbk)
    (out <- smult(hbk[,1:3]))
    class(out)
    summary(out)




nameEx("smulteda.object")
### * smulteda.object

flush(stderr()); flush(stdout())

### Name: smulteda.object
### Title: Description of 'smulteda.object' Objects
### Aliases: smulteda.object
### Keywords: robust multivariate

### ** Examples

    data(hbk)
    (out <- smult(hbk[,1:3], monitoring=TRUE))
    class(out)
    summary(out)




nameEx("spmplot")
### * spmplot

flush(stderr()); flush(stdout())

### Name: spmplot
### Title: Interactive scatterplot matrix
### Aliases: spmplot

### ** Examples

 ##  Call of spmplot() without optional parameters.
 ##  Iris data: scatter plot matrix with univariate boxplots on the main
 ##  diagonal.

 X <- iris[,1:4]
 group <- iris[,5]
 spmplot(X, group, variables=c('SL','SW','PL','PW'), dispopt="box")


 ##  Example of spmplot() called by routine fsmult().
 ##  Generate contaminated data.
     n <- 200; p <- 3
     X <- matrix(rnorm(n*p), ncol=3)
     Xcont <- X
     Xcont[1:5,] <- Xcont[1:5,] + 3

 ##  spmplot is called automatically by all outlier detection methods, e.g. fsmult()
     out <- fsmult(Xcont, plot=TRUE);

 ##  Now test the direct use of fsmult(). Set two groups, e.g. those obtained
 ##  from fsmult().

     group = rep(0, n)
     group[out$outliers] <- 1
 ##  option 'labeladd' is used to label the outliers
 ##  By default, the legend identifies the groups with the identifiers
 ##  given in vector 'group'.
 ##  Set the colors for the two groups to blue and red.

     spmplot(Xcont, group, col=c("blue", "red"), labeladd=1, dispopt="box")




nameEx("sreg.object")
### * sreg.object

flush(stderr()); flush(stdout())

### Name: sreg.object
### Title: Description of sreg Objects
### Aliases: sreg.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="S"))
    class(out)
    summary(out)




nameEx("sregeda.object")
### * sregeda.object

flush(stderr()); flush(stdout())

### Name: sregeda.object
### Title: Description of 'sregeda' Objects
### Aliases: sregeda.object
### Keywords: robust regression

### ** Examples

    (out <- fsreg(Y~., data=hbk, method="S", monitoring=TRUE))
    class(out)
    summary(out)




nameEx("summary.fsr")
### * summary.fsr

flush(stderr()); flush(stdout())

### Name: summary.fsr
### Title: Summary Method for FSR objects
### Aliases: summary.fsr print.summary.fsr
### Keywords: multivariate robust

### ** Examples


    data(Animals, package = "MASS")
    brain <- Animals[c(1:24, 26:25, 27:28),]
    lbrain <- log(brain)
    (fs <- fsreg(brain~body, data=lbrain, method="FS"))
    summary(fs)




nameEx("summary.lms")
### * summary.lms

flush(stderr()); flush(stdout())

### Name: summary.fsdalms
### Title: Summary Method for 'fsdalms' objects
### Aliases: summary.fsdalms print.summary.fsdalms
### Keywords: multivariate robust

### ** Examples


    data(Animals, package = "MASS")
    brain <- Animals[c(1:24, 26:25, 27:28),]
    lbrain <- log(brain)
    (fs <- fsreg(brain~body, data=lbrain, method="LTS"))
    summary(fs)

    ## compare to the result of ltsReg() from 'robustbase'
    (lts <- ltsReg(brain~body, data=lbrain))
    summary(lts)




nameEx("summary.lts")
### * summary.lts

flush(stderr()); flush(stdout())

### Name: summary.fsdalts
### Title: Summary Method for 'fsdalts' objects
### Aliases: summary.fsdalts print.summary.fsdalts
### Keywords: multivariate robust

### ** Examples


    data(Animals, package = "MASS")
    brain <- Animals[c(1:24, 26:25, 27:28),]
    lbrain <- log(brain)
    (fs <- fsreg(brain~body, data=lbrain, method="LTS"))
    summary(fs)

    ## compare to the result of ltsReg() from 'robustbase'
    (lts <- ltsReg(brain~body, data=lbrain))
    summary(lts)




nameEx("swissbanknotes")
### * swissbanknotes

flush(stderr()); flush(stdout())

### Name: swissbanknotes
### Title: Swiss banknote data
### Aliases: swissbanknotes
### Keywords: datasets

### ** Examples

data(swissbanknotes)
head(swissbanknotes)
plot(CovMcd(swissbanknotes[, 1:6]), which="pairs", col=swissbanknotes$class)




nameEx("swissheads")
### * swissheads

flush(stderr()); flush(stdout())

### Name: swissheads
### Title: Swiss heads data
### Aliases: swissheads
### Keywords: datasets

### ** Examples

data(swissheads)
head(swissheads)
plot(CovMcd(swissheads), which="pairs")


## VT::13.11.2021 - this takes too long...
if(FALSE)
{

nameEx("tclustIC")
### * tclustIC

flush(stderr()); flush(stdout())

### Name: tclustIC
### Title: Performs cluster analysis by calling 'tclustfsda' for different
###   number of groups 'k' and restriction factors 'c'
### Aliases: tclustIC

### ** Examples

 data(geyser2)
 out <- tclustIC(geyser2, whichIC="MIXMIX", plot=FALSE, alpha=0.1)
 out
 summary(out)
}


tic()
    nameEx("tclustICplot")
    ### * tclustICplot

    flush(stderr()); flush(stdout())

    ### Name: tclustICplot
    ### Title: Plots information criterion as a function of 'c' and 'k', based
    ###   on the solutions obtained by 'tclustIC'
    ### Aliases: tclustICplot

    ### ** Examples

     data(geyser2)
     out <- tclustIC(geyser2, whichIC="MIXMIX", plot=FALSE, alpha=0.1)
     tclustICplot(out, whichIC="MIXMIX")
toc()
## 739.37 sec elapsed


tic()
nameEx("tclustICsol")
### * tclustICsol

flush(stderr()); flush(stdout())

### Name: tclustICsol
### Title: Extracts a set of best relevant solutions obtained by 'tclustIC'
### Aliases: tclustICsol

### ** Examples

 data(geyser2)
 out <- tclustIC(geyser2, whichIC="MIXMIX", plot=FALSE, alpha=0.1)

 ## Plot first two best solutions using as Information criterion MIXMIX
 print("Best solutions using MIXMIX")
 outMIXMIX <- tclustICsol(out, whichIC="MIXMIX", plot=TRUE, NumberOfBestSolutions=2)
 print(outMIXMIX$MIXMIXbs)
toc()
##  621.26 sec elapsed


nameEx("tclusteda.object")
### * tclusteda.object

flush(stderr()); flush(stdout())

### Name: tclusteda.object
### Title: Objects returned by the function 'tclustfsda' with the option
###   'monitoring=TRUE'
### Aliases: tclusteda.object
### Keywords: multivariate robust

### ** Examples

 data(hbk)
 (out <- tclustfsda(hbk[, 1:3], k=2, monitoring=TRUE))
 class(out)
 summary(out)




nameEx("tclustfsda")
### * tclustfsda

flush(stderr()); flush(stdout())

### Name: tclustfsda
### Title: Computes trimmed clustering with scatter restrictions
### Aliases: tclustfsda

### ** Examples

 data(hbk)
 (out <- tclustfsda(hbk[, 1:3], k=2))
 class(out)
 summary(out)

 ##  TCLUST of Gayser data with three groups (k=3), 10%% trimming (alpha=0.1)
 ##      and restriction factor (c=10000)
 data(geyser2)
 (out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000))

 ## Use the plot options to produce more complex plots ----------

 ##  Plot with all default options
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=TRUE)

 ##  Default confidence ellipses.
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot="ellipse")

 ##  Confidence ellipses specified by the user: confidence ellipses set to 0.5
 plots <- list(type="ellipse", conflev=0.5)
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=plots)

 ##  Confidence ellipses set to 0.9
 plots <- list(type="ellipse", conflev=0.9)
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=plots)

 ##  Contour plots
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot="contour")

##  Filled contour plots with additional options: contourf plot with a named colormap.
##  Here we define four MATLAB-like colormaps, but the user can define anything else,
##  presented by a matrix with three columns which are the RGB triplets.

summer <- as.matrix(data.frame(x1=seq(from=0, to=1, length=65), x2=seq(from=0.5, to=1, length=65), x3=rep(0.4,65)))
spring <- as.matrix(data.frame(x1=rep(1, 65), x2=seq(from=0, to=1, length=65), x3=seq(from=1, to=0, length=65)))
winter <- as.matrix(data.frame(x1=rep(0, 65), x2=seq(from=0, to=1, length=65), x3=seq(from=1, to=0, length=65)))
autumn <- as.matrix(data.frame(x1=rep(1, 65), x2=seq(from=0, to=1, length=65), x3=rep(0, 65)))

out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=list(type="contourf", cmap=autumn))
out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=list(type="contourf", cmap=winter))
out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=list(type="contourf", cmap=spring))
out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, plot=list(type="contourf", cmap=summer))

 ##  We compare the output using three different values of restriction factor
 ##      nsamp is the number of subsamples which will be extracted
 data(geyser2)
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10000, nsamp=500, plot="ellipse")
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=10, nsamp=500, refsteps=10, plot="ellipse")
 out <- tclustfsda(geyser2, k=3, alpha=0.1, restrfactor=1, nsamp=500, refsteps=10, plot="ellipse")

 ##  TCLUST applied to M5 data: A bivariate data set obtained from three normal
 ##  bivariate distributions with different scales and proportions 1:2:2. One of the
 ##  components is very overlapped with another one. A 10 per cent background noise is
 ##  added uniformly distributed in a rectangle containing the three normal components
 ##  and not very overlapped with the three mixture components. A precise description
 ##  of the M5 data set can be found in Garcia-Escudero et al. (2008).
 ##

 data(M5data)
 plot(M5data[, 1:2])

 ##  Scatter plot matrix
 plot(CovClassic(M5data[,1:2]), which="pairs")

 out <- tclustfsda(M5data[,1:2], k=3, alpha=0, restrfactor=1000, nsamp=100, plot=TRUE)
 out <- tclustfsda(M5data[,1:2], k=3, alpha=0, restrfactor=10, nsamp=100, plot=TRUE)
 out <- tclustfsda(M5data[,1:2], k=3, alpha=0.1, restrfactor=1, nsamp=1000,
         plot=TRUE, equalweights=TRUE)
 out <- tclustfsda(M5data[,1:2], k=3, alpha=0.1, restrfactor=1000, nsamp=100, plot=TRUE)

 ##  TCLUST with simulated data: 5 groups and 5 variables
 ##
 n1 <- 100
 n2 <- 80
 n3 <- 50
 n4 <- 80
 n5 <- 70
 p <- 5
 Y1 <- matrix(rnorm(n1 * p) + 5, ncol=p)
 Y2 <- matrix(rnorm(n2 * p) + 3, ncol=p)
 Y3 <- matrix(rnorm(n3 * p) - 2, ncol=p)
 Y4 <- matrix(rnorm(n4 * p) + 2, ncol=p)
 Y5 <- matrix(rnorm(n5 * p), ncol=p)

 group <- c(rep(1, n1), rep(2, n2), rep(3, n3), rep(4, n4), rep(5, n5))
 Y <- Y1
 Y <- rbind(Y, Y2)
 Y <- rbind(Y, Y3)
 Y <- rbind(Y, Y4)
 Y <- rbind(Y, Y5)
 dim(Y)
 table(group)
 out <- tclustfsda(Y, k=5, alpha=0.05, restrfactor=1.3, refsteps=20, plot=TRUE)

 ##  Automatic choice of best number of groups for Geyser data ------------------------
 ##
 data(geyser2)
 maxk <- 6
 CLACLA <- matrix(0, nrow=maxk, ncol=2)
 CLACLA[,1] <- 1:maxk
 MIXCLA <- MIXMIX <- CLACLA

 for(j in 1:maxk) {
     out <- tclustfsda(geyser2, k=j, alpha=0.1, restrfactor=5, msg=FALSE)
     CLACLA[j, 2] <- out$CLACLA
 }

 for(j in 1:maxk) {
     out <- tclustfsda(geyser2, k=j, alpha=0.1, restrfactor=5, mixt=2, msg=FALSE)
     MIXMIX[j ,2] <- out$MIXMIX
     MIXCLA[j, 2] <- out$MIXCLA
 }

 oldpar <- par(mfrow=c(1,3))
 plot(CLACLA[,1:2], type="l", xlim=c(1, maxk), xlab="Number of groups", ylab="CLACLA")
 plot(MIXMIX[,1:2], type="l", xlim=c(1, maxk), xlab="Number of groups", ylab="MIXMIX")
 plot(MIXCLA[,1:2], type="l", xlim=c(1, maxk), xlab="Number of groups", ylab="MIXCLA")
 par(oldpar)


 ##  Monitoring examples ------------------------------------------

 ##  Monitoring using Geyser data

 ##  Monitoring using Geyser data (all default options)
 ##  alpha and restriction factor are not specified therefore alpha=c(0.10, 0.05, 0)
 ##  is used while the restriction factor is set to c=12
 out <- tclustfsda(geyser2, k=3, monitoring=TRUE)

 ##  Monitoring using Geyser data with alpha and c specified.
 out <- tclustfsda(geyser2, k=3, restrfac=100, alpha=seq(0.10, 0, by=-0.01), monitoring=TRUE)

 ##  Monitoring using Geyser data with plot argument specified as a list.
 ##      The trimming levels to consider in this case are 31 values of alpha
 ##
 out <- tclustfsda(geyser2, k=3, restrfac=100, alpha=seq(0.30, 0, by=-0.01), monitoring=TRUE,
         plot=list(alphasel=c(0.2, 0.10, 0.05, 0.01)), trace=TRUE)

 ##  Monitoring using Geyser data with argument UnitsSameGroup
 ##
 ##      Make sure that group containing unit 10 is in a group which is labelled "group 1"
 ##      and group containing unit 12 is in group which is labelled "group 2"
 ##
 ##      Mixture model is used (mixt=2)
 ##
 out <- tclustfsda(geyser2, k=3, restrfac=100, alpha=seq(0.30, 0, by=-0.01), monitoring=TRUE,
         mixt=2, UnitsSameGroup=c(10, 12), trace=TRUE)

 ##  Monitoring using M5 data
 data(M5data)

 ##  alphavec=vector which contains the trimming levels to consider
 ##  in this case 31 values of alpha are considered
 alphavec <- seq(0.10, 0, by=-0.02)
 out <- tclustfsda(M5data[, 1:2], 3, alpha=alphavec, restrfac=1000, monitoring=TRUE,
     nsamp=1000, plots=TRUE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))

nameEx("tclustfsda.object")
### * tclustfsda.object

flush(stderr()); flush(stdout())

### Name: tclustfsda.object
### Title: Objects returned by the function 'tclustfsda'
### Aliases: tclustfsda.object
### Keywords: multivariate robust

### ** Examples


 data(hbk)
 (out <- tclustfsda(hbk[, 1:3], k=2))
 class(out)
 summary(out)




nameEx("tclustic.object")
### * tclustic.object

flush(stderr()); flush(stdout())

### Name: tclustic.object
### Title: Objects returned by the function 'tclustIC'
### Aliases: tclustic.object
### Keywords: multivariate robust

### ** Examples


 data(hbk)
 (out <- tclustIC(hbk[, 1:3]))
 class(out)
 summary(out)




nameEx("tclusticsol.object")
### * tclusticsol.object

flush(stderr()); flush(stdout())

### Name: tclusticsol.object
### Title: Objects returned by the function 'tclustICsol'
### Aliases: tclusticsol.object
### Keywords: multivariate robust

### ** Examples

 data(hbk)
 (out <- tclustIC(hbk[, 1:3]))

 ## Plot first two best solutions using as Information criterion MIXMIX
 print("Best solutions using MIXMIX")
 outMIXMIX <- tclustICsol(out, whichIC="MIXMIX", plot=TRUE, NumberOfBestSolutions=2)
 class(outMIXMIX)
 summary(outMIXMIX)
 print(outMIXMIX$MIXMIXbs)


nameEx("tclustreg")
### * tclustreg

flush(stderr()); flush(stdout())

### Name: tclustreg
### Title: Computes robust linear grouping analysis
### Aliases: tclustreg

### ** Examples


 ## The X data have been introduced by Gordaliza, Garcia-Escudero & Mayo-Iscar (2013).
 ## The dataset presents two parallel components without contamination.

 data(X)
 y1 = X[, ncol(X)]
 X1 = X[,-ncol(X), drop=FALSE]

 (out <- tclustreg(y1, X1, k=2, alphaLik=0.05, alphaX=0.01, restrfactor=5, plot=TRUE, trace=TRUE))

 (out <- tclustreg(y1, X1, k=2, alphaLik=0.05, alphaX=0.01, restrfactor=2,
         mixt=2, plot=TRUE, trace=TRUE))

 ##  Examples with fishery data

 data(fishery)
 X <- fishery

 ## some jittering is necessary because duplicated units are not treated:
 ## this needs to be addressed
 X <- X + 10^(-8) * abs(matrix(rnorm(nrow(X)*ncol(X)), ncol=2))

 y1 <- X[, ncol(X)]
 X1 <- X[, -ncol(X), drop=FALSE]

 (out <- tclustreg(y1, X1, k=3, restrfact=50, alphaLik=0.04, alphaX=0.01, trace=TRUE))
 ## Example 2:

 ## Define some arbitrary weightssome arbitrary weights for the units
     we <- sqrt(X1)/sum(sqrt(X1))

 ##  tclustreg required parameters
     k <- 2; restrfact <- 50; alpha1 <- 0.04; alpha2 <- 0.01

 ##  Now tclust is run on each combination of mixt and wtrim options

     cat("\nmixt=0; wtrim=0",
         "\nStandard tclustreg, with classification likelihood and without thinning\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=0, wtrim=0, trace=TRUE))

     cat("\nmixt=2; wtrim=0",
         "\nMixture likelihood, no thinning\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=2, wtrim=0, trace=TRUE))

     cat("\nmixt=0; wtrim=1",
         "\nClassification likelihood, thinning based on user weights\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=0, we=we, wtrim=1, trace=TRUE))

     cat("\nmixt=2; wtrim=1",
         "\nMixture likelihood, thinning based on user weights\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=2, we=we, wtrim=1, trace=TRUE))

     cat("\nmixt=0; wtrim=2",
         "\nClassification likelihood, thinning based on retention probabilities\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=0, wtrim=2, trace=TRUE))

     cat("\nmixt=2; wtrim=2",
         "\nMixture likelihood, thinning based on retention probabilities\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=2, wtrim=2, trace=TRUE))

     cat("\nmixt=0; wtrim=3",
         "\nClassification likelihood, thinning based on bernoulli weights\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=0, wtrim=3, trace=TRUE))

     cat("\nmixt=2; wtrim=3",
         "\nMixture likelihood, thinning based on bernoulli weights\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=2, wtrim=3, trace=TRUE))

     cat("\nmixt=0; wtrim=4",
         "\nClassification likelihood, tandem thinning based on bernoulli weights\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=0, wtrim=4, trace=TRUE))

     cat("\nmixt=2; wtrim=4",
         "\nMixture likelihood, tandem thinning based on bernoulli weights\n")
     (out <- tclustreg(y1, X1, k=k, restrfact=restrfact, alphaLik=alpha1, alphaX=alpha2,
             mixt=2, wtrim=4, trace=TRUE))





nameEx("tclustreg.object")
### * tclustreg.object

flush(stderr()); flush(stdout())

### Name: tclustreg.object
### Title: Objects returned by the function 'tclustreg'
### Aliases: tclustreg.object
### Keywords: multivariate robust

### ** Examples



 ## The X data have been introduced by Gordaliza, Garcia-Escudero & Mayo-Iscar (2013).
 ## The dataset presents two parallel components without contamination.

 data(X)
 y1 = X[, ncol(X)]
 X1 = X[,-ncol(X), drop=FALSE]

 out <- tclustreg(y1, X1, k=2, alphaLik=0.05, alphaX=0.01, restrfactor=5, trace=TRUE)
 class(out)
 str(out)




### * <FOOTER>
###

options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
