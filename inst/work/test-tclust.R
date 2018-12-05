##detach(name="package:fsdaR", unload=TRUE)
##unloadNamespace("fsdaR")

library(fsdaR)

## The data have been generated using the following R instructions:
library(mvtnorm)
set.seed (100)
Xmix <- rbind (rmvnorm (360, c( 0, 0), matrix(c (1, 0, 0, 1), ncol = 2)),
               rmvnorm (540, c( 5, 10), matrix(c (6, -2, -2, 6), ncol = 2)),
               rmvnorm (100, c(2.5, 5), matrix(c (50, 0, 0, 50), ncol = 2)))

##---------------------------------------------------------------
## Testing tclust()

out1 <- tclustfsda(Xmix, k=3, alpha=0, trace=TRUE)
names(out1)

out2 <- tclustfsda(Xmix, k=3, alpha=0, monitoring=TRUE, trace=TRUE)
names(out2)

##----------------------------------------------------------------
##    %% Monitoring using geyser data (all default options).

library(fsdaR)
library(tclust)
data(geyser2)

##  alpha and restriction factor are not specified therefore for alpha
##  vector [0.10 0.05 0] is used while for the restriction factor, value c=12
##  used

out <- tclustfsda(geyser2, k=3, monitoring=TRUE, trace=TRUE)

out <- tclustfsda(geyser2, k=3, restrfac=100, alpha=seq(0.10, 0, by=-0.01), monitoring=TRUE, trace=TRUE)

out <- tclustfsda(geyser2, k=3, restrfac=100, alpha=seq(0.30, 0, by=-0.01), monitoring=TRUE, trace=TRUE)

## Use M5 data.
    data(M5data)

##    % alphavec= vector which contains the trimming levels to consider
##    % in this case 31 values of alpha are considered
    alphavec <- seq(0.10, 0, by=-0.02)
    out=tclustfsda(M5data[,1:2], 3, alpha=alphavec, restrfac=1000, monitoring=TRUE, nsamp=1000, plots=TRUE)
