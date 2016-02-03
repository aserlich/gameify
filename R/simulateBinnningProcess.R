#CREATE THE SIMULATION DATA FOR THE REBINNING OF DATA
#apply a cut of size x to each bin and uniformly sample number
#and then assign them to the bin

#Function that takes in all of the binned data and simulates
#This is the function that is called
rebin <- function(bins, lower.bounds, upper.bounds, cut.size=1,rep.num=1000, seed=1234) {
  mapply(DrawUnif, bins, lower.bounds, upper.bounds, rep.num, cut.size) #returns a matrix with lists inside
}

#######################################
#INTERNAL FUNCTIONS
#Applies the function ReplicateBins
DrawUnif <- function(coins.in.bin, lower.bound, upper.bound, rep.num, cut.size) {
  lapply(as.list(coins.in.bin),
         ReplicateBins,
         n=rep.num,
         lower.bound=lower.bound,
         upper.bound=upper.bound,
         cut.size=cut.size
  )
  #RecutData(UnifSims, lower.bound, upper.bound, cut.size)
}

ReplicateBins <- function(coins, n, lower.bound, upper.bound, cut.size){
  if(coins==0){
    0
  } else {
    #print(coins)
    replicates <- replicate(n,
                            runif(coins,
                                  min=lower.bound,
                                  max=upper.bound
                            ),
                            simplify="matrix"
    )
    if(class(replicates)=="numeric"){
      replicates <- as.matrix(replicates)
    }
    outcome <- apply(X=replicates,
                     MARGIN=2,
                     FUN=RecutData,
                     lower.bound=lower.bound,
                     upper.bound=upper.bound,
                     cut.size=cut.size
    )
    if(ncol(outcome)==1){ #stupid apply simplifies, transpose
      outcome <- t(outcome)
    }
    return(outcome)
  }
}

RecutData <- function(simulations, lower.bound, upper.bound, cut.size) {
  cut(simulations, breaks=seq(lower.bound, upper.bound, by=cut.size))
}
#For us in remove NAs because of
#empty bins in the simulating process
RemoveNAs <- function(x) {
  x[is.na(x)] <-0
  return(x)
}

#############################################################

#currently returns 100 simulates for each coin and 
#the bin they are in
#pre

set.seed(20150219)
RebinPre <- rebin(bins=as.list(AllBetsPre),
                  lower.bounds=BinLowerOrig,
                  upper.bounds=BinUpperOrig,
                  rep.num=100,
                  cut.size=1
)
#post

set.seed(20150220)
RebinPost <- rebin(bins=as.list(AllBetsPost),
                   lower.bounds=BinLowerOrig,
                   upper.bounds=BinUpperOrig,
                   rep.num=100,
                   cut.size=1
)

#now need to return 100 datasets with the new bins
CutData <- data.frame(matrix(nrow=nrow(AllBetsPost), ncol=length(levels(cut( seq(0,100, .2), breaks=AllBins)))))
names(CutData) <- levels(cut( seq(0,100, .2), breaks=AllBins))
ListCutDataPre <- replicate(100, CutData, simplify=FALSE)
ListCutDataPost <- ListCutDataPre
#there has to be a better way!
for(rep in 1:NumReplications){ #rep though all simulations
  q<-1
  z <- 5
  for(i in 1:ncol(RebinPre)) {
    for(j in 1:nrow(RebinPre)) {
      if(class(RebinPre[j,i][[1]])=="numeric" && RebinPre[j,i]==0 )  { #if there are zeros put them ther
        ListCutDataPre[[rep]][j, q:z] <-0 
      }else {
        #print("here")
        temp <- RebinPre[j,i][[1]][,rep]
        TableTemp <- table(temp)
        ListCutDataPre[[rep]][j,names(ListCutDataPre[[rep]]) %in% names(TableTemp)] <- TableTemp
        # print(list(ListCutDataPre[[rep]][j,names(ListCutDataPre[[rep]]) %in% names(TableTemp)], i, j,q, rep))
      }
      ##print(ListCutData[[rep]][j, q:10])
      ##   print(list(i,j))
    }
    q<-q+5
    z <- z+5
  }
  print(paste0(" rep", rep, " completed"))
}

for(rep in 1:NumReplications){ #rep though all simulations
  q<-1
  z <- 5
  for(i in 1:ncol(RebinPost)) {
    for(j in 1:nrow(RebinPost)) {
      if(class(RebinPost[j,i][[1]])=="numeric" && RebinPost[j,i]==0 )  { #if there are zeros put them ther
        ListCutDataPost[[rep]][j, q:z] <-0 
      }else {
        #print("here")
        temp <- RebinPost[j,i][[1]][,rep]
        TableTemp <- table(temp)
        ListCutDataPost[[rep]][j,names(ListCutDataPost[[rep]]) %in% names(TableTemp)] <- TableTemp
        #print(list(ListCutData[[rep]][j,names(ListCutData[[rep]]) %in% names(TableTemp)], i, j,q, rep))
      }
      ##print(ListCutData[[rep]][j, q:10])
      ##   print(list(i,j))
    }
    q<-q+5
    z <- z+5
  }
  print(paste0(" rep", rep, " completed"))
  
}

output <- list(ListCutDataPre=ListCutDataPre, ListCutDataPost=ListCutDataPost)
save(output,
     file=file.path(dataDir4,
                    paste0("ListCutData",
                           Sys.Date(),
                           ".Rdata")
     )
)
