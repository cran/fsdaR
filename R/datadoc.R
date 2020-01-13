######
##  VT::04.03.2019
##
##
##  roxygen2::roxygenise("C:/projects/statproj/R/fsdaR")
##
#'
#'
#'
#' Demographic data from the 341 miniciplaities in Emilia Romagna (an Italian region).
#'
#' A data set containing 28 demographic variables for 341 municipalities in Emilia Romagna (an Italian region).
#'
#' @name emilia2001
#' @docType data
#' @usage data(emilia2001)
#' @format A data frame with 341 rows and 28 variables
#' The variables are as follows:
#'
#' \itemize{
#'   \item less10: population aged less than 10
#'   \item more75: population aged more than 75
#'   \item single single-member families
#'   \item divorced": divorsed
#'   \item widows: widows and widowers
#'   \item graduates: population aged more than 25 who are graduates
#'   \item no_education:  of those aged over 6 having no education
#'   \item employed: activity rate
#'   \item unemployed: unemployment rate
#'   \item increase_popul: standardised natural increase in population
#'   \item migration: standardised change in population due to migration
#'   \item birth_92_94: average birth rate over 1992-94
#'   \item fecundity: three-year average birth rate amongst women of child-bearing age
#'   \item houses: occupied houses built since 1982
#'   \item houses_2WCs: occupied houses with 2 or more WCs
#'   \item houses_heating: occupied houses with fixed heating system
#'   \item TV: TV licence holders
#'   \item cars: number of cars for 100 inhabitants
#'   \item luxury_cars: luxury cars
#'   \item hotels: working in hotels and restaurants
#'   \item banking: working in banking and finance
#'   \item income: average declared income amongst those filing income tax returns
#'   \item income_tax_returns: inhabitants filing income tax returns
#'   \item factories: residents employed in factories and public services
#'   \item factories_more10: employees employed in factories withy more tha 10 employees
#'   \item factories_more50: employees employed in factories withy more tha 50 employees
#'   \item artisanal: artisanal enterprises
#'   \item entrepreneurs: enterpreneous and skilled self-employed among those of working age
#' }
#'
#'  @references
#'  Atkinson, A. C., Riani, M., and Cerioli, A. (2004). \emph{Exploring Multivariate Data with the Forward Search}. Springer-Verlag, New York.
#' @keywords datasets
NULL
#' Old Faithful Geyser Data.
#'
#' A bivariate data set obtained from the Old Faithful Geyser, containing the eruption
#'  length and the length of the previous eruption for 271 eruptions of this geyser in minutes.
#'
#' @name geyser2
#' @docType data
#' @usage data(geyser2)
#' @format A data frame with 271 rows and 2 variables
#' The variables are as follows:
#'
#' \itemize{
#'   \item Eruption length: The eruption length in minutes.
#'   \item Previous eruption length: The length of the previous eruption in minutes.
#' }
#'
#' @references
#'      Garcia-Escudero, L.A., Gordaliza, A. (1999). Robustness properties of k-means and trimmed k-means,
#'      \emph{Journal of the American Statistical Assoc.}, Vol.\strong{94}, No.447, 956-969.
#'
#'      Haerdle, W. (1991). \emph{Smoothing Techniques with Implementation in S}, New York: Springer.
#'
#' @keywords datasets
NULL
#' Mixture M5 Data.
#'
#' A bivariate data set obtained from three normal bivariate distributions with different scales and
#'  proportions 1:2:2. One of the components is strongly overlapping with another one. A 10%% background
#'  noise is added uniformly distributed in a rectangle containing the three normal components and not
#'  strongly overlapping with the three mixture components. A precise description of the M5 data set
#'  can be found in Garcia-Escudero et al. (2008).
#'
#' @name M5data
#' @docType data
#' @usage data(M5data)
#' @format A data frame with 2000 rows and 3 variables
#' The first two columns are the two variables. The last column is the true classification vector where symbol "0" stands for the contaminating data points.
#'
#' @source
#'      Garcia-Escudero, L.A., Gordaliza, A., Matran, C. and Mayo-Iscar, A. (2008). A General Trimming Approach to Robust Cluster Analysis,
#'      \emph{Annals of Statistics}, Vol.\strong{36}, 1324-1345. Technical report available at \url{http://www.eio.uva.es/inves/grupos/representaciones/trTCLUST.pdf}
#'
#' @keywords datasets
NULL
