% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datadoc.R
\docType{data}
\name{bank_data}
\alias{bank_data}
\title{Bank data (Riani et al., 2014).}
\format{
A data frame with 1949 rows and 14 variables.
 The variables are as follows:
\itemize{
  \item x1: Personal loans
  \item x2: Financing and hire-purchase
  \item x3: Mortgages
  \item x4: Life insurance
  \item x5: Share amount
  \item x6: Bond account
  \item x7: Current account
  \item x8: Salary deposits
  \item x9: Debit cards
  \item x10: Credit cards
  \item x11: Telephone banking
  \item x12: Domestic direct debits
  \item x13: Money transfers
  \item y: Profit/loss
}
}
\source{
Riani, M., Cerioli, A., Atkinson, A. C., and Perrotta, D. (2014). Supplement to ''Monitoring robust regression''. doi:10.1214/14-EJS897SUPP.
}
\usage{
data(bank_data)
}
\description{
There are 60 observations on a response y with the values of three explanatory variables.
 The scatter plot matrix of the data shows y increasing with each of x1, x2 and x3.
 The plot of residuals against fitted values shows no obvious pattern. However the
 FS finds that there are 6 masked outliers.
}
\references{
Riani, M., Cerioli, A., Atkinson, A. C., and Perrotta, D. (2014). Monitoring robust regression. \emph{Electronic Journal of Statistics}, 8, 642-673.
}
\keyword{datasets}
