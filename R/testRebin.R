upper <- seq(10, 100, 10)
lower <- seq(0, 90, 10)
counts <- c(0, 0, 4, 4, 4, 2, 0, 0, 0, 0)
counts2 <- c(1, 2, 1, 4, 4, 2, 0, 0, 0, 0)
counts3 <- c(1, 2, 0, 4, 4, 2, 0, 0, 0, 0)
counts_df <- as.data.frame(rbind(counts, counts2))
#binnedMSE(counts_df, mu = 33, binLower = lower, binUpper = upper)

tr <- rebin(counts_df, lower, upper, 5, 6)


tr2 <- rebin(counts_df, lower, upper, 5, 6, simplify =TRUE)

tp <- formatSimsOutput(tr, 2, lower, upper, 5, 6)
tp2 <- formatSimsOutput(tr, 2, lower, upper, 5, 6, outputType = "list_of_draws")


#binnedMSE(counts_df, mu = 33, binLower = lower, binUpper = upper)
