library(fsdaR)

##  Stars data from 'robustbase'
data(starsCYG)
head(starsCYG)

## We get the following picture with MM-estimates, Tukey’s bisquare 
##  function. The three sets of residuals are clearly seen for virtually all efficiency values.
(out <- fsreg(log.light~log.Te, data=starsCYG, method="MM", monitoring=TRUE))
resfwdplot(out, fg.col="red", fg.lty="dotdash")
