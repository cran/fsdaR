##  %% Plot BIC, ICL and CLA for for Geyser data with all default options.
##  Y=load('geyser2.txt');
##  % Make sure (whenever possible) that units 15, 30 and 69 are inside
##  % groups which have labels respectively equal to 1, 2 and 3.
##  UnitsSameGroup=[15 30 69];
##  out=tclustIC(Y,'cleanpool',false,'plots',0,'alpha',0.1,'UnitsSameGroup',UnitsSameGroup);
##  tclustICplot(out)

library(fsdaR)
library(tclust)
data(geyser2)

##  alpha and restriction factor are not specified therefore for alpha
##  vector [0.10 0.05 0] is used while for the restriction factor, value c=12
##  used

out <- tclustfsda(geyser2, k=3, monitoring=FALSE, trace=TRUE)
names(out)

out <- tclustIC(geyser2, plot=FALSE, alpha=0.1, trace = TRUE)

tclustICplot(out)

outsol<- tclustICsol(out)

carbikeplot(outsol)
