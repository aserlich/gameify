
#list of original bins inside is a list of observations
#inside the list observation is either NULL or a matrix of simulations
#now columns are the replicates and rows are the coins
#'Export simulations of re-binning process from \code{rebin} function
#'
#'@param simulates a list object created from \code{rebin}
#'@param numObs The number of observations that were simulated from
#'@param binLower A vector of the lower cut points of the of the original binned ata
#'@param binUPper A vector of the upper cutponts of the original binnned data
#'@param A scalar that is the the length of the new (less coarse) simulated data bin
#'@param repNum The number of replications carried out
#'@param outputType currently either \code{"list_of_bins} or \code{list_of_draws}. \code{"list_of_bins} returns a list object
#'whose length is the number of simulations in each list there is a matrix of counts of how many objects were places in each bin
#'for each observation. \code{list_of_draws} has a similar data structure but only includes the matrix of draws.
#'@details The idea of this data structure ist that it is similar to an imputation matrix.
#'

formatSimsOutput <- function(simulates, numObs, binLower, binUpper,
                             cutSize, repNum, outputType = "list_of_bins") {
  binNum <- length(binLower)
  listofDf <- list()
  for (obs in 1:numObs) { #reverse the for loops
    for(bin in 1:binNum) {
      print(bin)
      print(obs)
      if(is.null(simulates[[bin]][[obs]])) {
        next
      }
      print(simulates[[bin]][[obs]])
      if(length(listofDf) < obs) { #trick for if list emlement exists
        print("here")
        listofDf[[obs]] <- simulates[[bin]][[obs]]
      } else {
        listofDf[[obs]] <- rbind(listofDf[[obs]], simulates[[bin]][[obs]])
      }
      #listofDf[[obs]] <- lapply(listofDf[[obs]], factor, levels = rnames)
    }
    #listofDf[[obs]] < - data.frame(listofDf[[obs]])
  }
  #different ways to out put data
  rnames <- levels(cut(1, breaks = seq(min(binLower), max(binUpper), cutSize) ) )
  listofDf <- lapply(listofDf, as.data.frame)
  #listofDf <- lapply(listofDf, factor, levels = rnames)
  outputList <- list()
  if(outputType == "list_of_draws") {
    for(df in 1:repNum) {
      outputList[[df]] <- plyr::ldply(listofDf, function(x) t(x[, df, drop=FALSE]))
    }
  }
  if(outputType == "list_of_bins") {
    toFactor <- lapply(listofDf, lapply, function(x) unlist( factor(x, levels =rnames)))
    tableX <-lapply(toFactor, function(x) sapply(x, table))
    for(df in 1:repNum) {
      outputList[[df]] <- plyr::ldply(tableX, function(x) t(x[, df, drop=FALSE]))
    }
  }
  return(outputList)
}

