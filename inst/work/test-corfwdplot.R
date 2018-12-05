library(fsdaR)

data(hbk)

(out <- fsmult(hbk[,1:3], monitoring=TRUE))
corfwdplot(out)

(out <- smult(hbk[,1:3], monitoring=TRUE))
corfwdplot(out)

(out <- mmmult(hbk[,1:3], monitoring=TRUE))
corfwdplot(out)

(out <- fsreg(Y~., data=hbk, monitoring=TRUE))
corfwdplot(out)

(out <- fsreg(Y~., data=hbk, method="S", monitoring=TRUE))
corfwdplot(out)

(out <- fsreg(Y~., data=hbk, method="MM", monitoring=TRUE))
corfwdplot(out)


##  Test for regression
##
data(starsCYG)
(out <- fsreg(log.light~log.Te, data=starsCYG, monitoring=TRUE))
corfwdplot(out)

(out <- fsreg(log.light~log.Te, data=starsCYG, method="S", monitoring=TRUE))
corfwdplot(out)

(out <- fsreg(log.light~log.Te, data=starsCYG, method="MM", monitoring=TRUE))
corfwdplot(out)
