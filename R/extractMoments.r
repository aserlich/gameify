#
#'Calculate the mean for a matrix of binned data
#'@param counts
#'@param lowerBounds A vector of the lower cut points of the binnned data
#'@param upperBounds A vector of the upper cutpoints of the original binnned data
#'@return A vector of estimated means
#'@examples
#'upper <- seq(10, 100, 10)
#'lower <- seq(0, 90, 10)
#'counts2 <- c(1, 2, 1, 4, 4, 2, 0, 0, 0, 0)
#'counts3 <- c(1, 2, 1, 4, 4, 0, 0, 2, 0, 0)
#'detectNonContig(counts2)
#'detectNonContig(counts3)
#'binnedMean(rbind(counts2, counts3), lower, upper)
binnedMean <- function(counts,
                       lowerBounds = seq(0, 95, 5),
                       upperBounds =seq(5, 100, 5)) {
  if(any(!(class(counts) %in% c("matrix", "data.frame") ) ) )  {
    stop("counts must be a matrix or data.frame")
  }
  binMid <- (upperBounds + lowerBounds)/2
  print(binMid)
  n <- rowSums(counts, na.rm = FALSE)
  #print(n)
  wc <- sweep(counts, 2, binMid, `*`)
  #print(wc)
  muhat <- rowSums(wc, na.rm = FALSE) / n
  return(muhat)
}


#'Calculate the MSE for binned data
#'@param counts A matrix of data frame of observed counts of binned data
#'@param mu The true observed mean
#'@param lowerBounds A vector of the lower cut points of the binnned data
#'@param upperBounds A vector of the upper cutpoints of the original binnned data
#'@details Assumes that the midpoint of the bin is the mean of that prediction.
#'@examples upper <- seq(10, 100, 10)
#'lower <- seq(0, 90, 10)
#'counts <- c(0, 0, 4, 4, 4, 2, 0, 0, 0, 0)
#'counts2 <- c(1, 2, 1, 4, 4, 2, 0, 0, 0, 0
#'counts_df <- as.data.frame(rbind(counts, counts2))
#'binnedMSE(counts_df, mu = 33, lowerBounds = lower, upperBounds = upper)
#'@return A vector with one observation for each row in \code{counts}
#'@export
binnedMSE <- function(counts, mu,
                      lowerBounds = seq(0, 95, 5),
                      upperBounds = seq(5, 100, 5)) {
  binMid <- (lowerBounds + upperBounds)/2
  unnorm <- sweep(counts, 2, ((binMid-mu)^2),  `*`)
  print(unnorm)
  MSE <- rowSums(unnorm, na.rm = TRUE) / rowSums(counts, na.rm = TRUE)
  return(MSE)
}


#'Function to find non-contiguity in response patterns
#'@param row A vector of binned counts
#'@param bins The number of bins: should be the same as the length of \code{row}
#'@details Assumes there are at least some coins in some bins.
#'@return returns either a 0 if there are non-contiguous counts of 1 if there are.
#'Will often be used with \code{apply} to operate on a matrix
#'@examples counts2 <- c(1, 2, 1, 4, 4, 2, 0, 0, 0, 0)
#'counts3 <- c(1, 2, 0, 4, 4, 2, 0, 0, 0, 0)
#'detectNonContig(counts2)
#'detectNonContig(counts3)
#'apply(rbind(counts2, counts3), 1, detectNonContig)
#'
detectNonContig <- function(row, bins = length(row)) {
  #print(row)
  #print(row[1]==0)
  if(is.na(row[1])) {
    rep <- NA
  } else {
    rep <- 0
    for(i in 2:(bins - 1)) {
      #print(i)
      #print(x[(i+1)])
      if(row[i-1] > 0 & row[i+1] > 0 & row[i] == 0) {
        rep <- 1
        break
      }
    }
  }
  return(rep)
}
