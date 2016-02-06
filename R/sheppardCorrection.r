#A function to apply the Sheppard Correction
#'Calculate the mean of any binned data and the variance and corrected variance
#'
#'@param counts A row vector the cells associated with count of objects in each bin,
# Must been the same length \code{binLower} and \code{binUpper}
#'@param binLower A vector of the lower bounds of each bin
#'@param binUpper A vector of the upper bounds of each bin
#'@return A list with three components \enumerate{
#'  \item The mean of the matrix of binned vectors \code{mu}
#'  \item The unadjusted variance \code{sigma2}
#'  \item The variance with the adjusted shepherd correction \code{sigma2Adj}
#'  }
#'The the sheppard corrected variance
#'@export
#'@examples
#'upper <- seq(10, 100, 10)
#'lower <- seq(0, 90, 10)
#'counts <- c(0, 0, 4, 4, 4, 2, 0, 0, 0, 0)
#'sheppardCorrection(counts, upper, lower)
#'
sheppardCorrection <- function(counts, binLower, binUpper) {
  binLength <- binUpper[1] - binLower[1]
  binMid <- (binUpper + binLower) / 2
  n <- sum(counts)
  mu <- sum(binMid * counts) / n
  sigma2 <- (sum(binMid^2 * counts) - n * mu^2) / (n - 1)
  sigma2Adj <- sigma2 - ( (binLength)^2) / 12
  return(list(mu = mu, sigma2 = sigma2, sigma2Adj = sigma2Adj) )
}
