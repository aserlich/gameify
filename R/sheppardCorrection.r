#A function to apply the Sheppard Correction
sheppardCorrection <- function(counts, binLower, binUpper) {
  binLength = binUpper[1] - binLower[1]
  binMid <- (binUpper + binLower)/2
  n <- sum(counts)
  mu <- sum(binMid * counts) / n
  sigma2 <- (sum(binMid^2 * counts) - n * mu^2) / (n-1)
  sigma2Adj <- sigma2 - ((binLength)^2)/12
  return(list(mu=mu, sigma2=sigma2, sigma2Adj = sigma2Adj))
}