#CREATE THE SIMULATION DATA FOR THE REBINNING OF DATA
#apply a cut of size x to each bin and uniformly sample number
#and then assign them to the bin

#Function that takes in all of the binned data and simulates
#This is the function that is called
#'Function to simulate re-binned counts when each count of a data a bin is considered to come from a uniform distribution
#'and we want to simulate the data as if it comes from a less coarse bin
#'
#'@param counts a matrix of counts of objects placed in bins where each column is a bin and each row is on abservation
#'@param lowerBounds A vector of lower cut points of the original binned data
#'@param upperBounds A vector of the upper cutponts of the original binnned data
#'@param cutSize A scalar that is the the length of the new (less coarse) bin for simulated data
#'@param repNum The number of replications to carry out
#'@param seed Seed
#'@return If \code{simplify=TRUE}, Returns a matrix the same size as the original data however, each cell of the matrix contains a list of simulations.
#'If \code{simplify=FALSE} then the function returns a list where each bin is a list element. Inside each bin there is another list that represents each observation.
#'For all non-empty bins each observation contains \bold{\code{n}} rows, which represent objects places in the bin and \bold{\code{p}} coulmns, were each column represents a simulation. This
#'data structure is designed to be passed to  \code{\link{formatSimsOutput} in order} to get the data in the format you desire for analysis.
#'@details The function uses \code{mapply} to simulate the placing of object within each bin into less coarse bins. It returns an unusual
#'two different unusual data structures. This is good for checking the simulation process but is not useful. Depending on the usage
#'you may want the data in different structures. Use \code{\link{formatSimsOutput}} to get the data in the format you desire
#'@export
rebin <- function(counts, lowerBounds, upperBounds, cutSize = 1, repNum = 10, seed = 1234, simplify = FALSE) {
  set.seed(seed)
  if(any(class(counts) == "matrix")) {
    counts <- as.data.frame(counts)
  }
  mapply(drawUnif, counts, lowerBounds, upperBounds,
         MoreArgs = list(cutSize = cutSize, repNum =repNum), SIMPLIFY = simplify) #returns a matrix with lists inside
}

#######################################
#INTERNAL FUNCTIONS
#'Applies the function replicateBins
drawUnif <- function(count, lowerBound, upperBound, cutSize, repNum) {
  lapply(as.list(count),
         replicateBins,
         n = repNum,
         lowerBound = lowerBound,
         upperBound = upperBound,
         cutSize=cutSize
  )
  #RecutData(UnifSims, lower.bound, upper.bound, cut.size)
}

#'For any bin creates replicates from the uniform distribution of coins places in that bin
replicateBins <- function(count, n, lowerBound, upperBound, cutSize){
  if(count == 0){
    NULL
  } else {
    #print(coins)
    replicates <- replicate(n,
                            runif(count,
                                  min = lowerBound,
                                  max = upperBound),
                            simplify="matrix")
    if(class(replicates) == "numeric"){
      replicates <- as.matrix(replicates)
    }
    outcome <- apply(X = replicates,
                     MARGIN= 2,
                     FUN = recutData,
                     lowerBound = lowerBound,
                     upperBound = upperBound,
                     cutSize = cutSize)
    if(ncol(outcome) == 1){ #stupid apply simplifies, transpose
      outcome <- t(outcome)
    }
    return(outcome)
  }
}

#'takes data that is drawn from a continous distribution and recuts it.
recutData <- function(simulations, lowerBound, upperBound, cutSize) {
  #print(cat(cutSize,"\n", lowerBound, "\n",upperBound))
  cut(simulations,
      breaks = seq(lowerBound, upperBound, by = cutSize))
}
#For us in remove NAs because of
#empty bins in the simulating process
RemoveNAs <- function(x) {
  x[is.na(x)] <-0
  return(x)
}
