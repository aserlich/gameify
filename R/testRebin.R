upper <- seq(10, 100, 10)
lower <- seq(0, 90, 10)
counts <- c(0, 0, 4, 4, 4, 2, 0, 0, 0, 0)
counts2 <- c(1, 2, 1, 4, 4, 2, 0, 0, 0, 0)
ac <- as.data.frame(rbind(counts, counts2))
tr <- rebin(ac, lower, upper, 5, 6)
tr2 <- rebin(ac, lower, upper, 5, 6, simplify =TRUE)

tp <- listOfMat(tr, 2, lower, upper, 5, 6)
tp2 <- listOfMat(tr, 2, lower, upper, 5, 6, outputType = "list_of_draws")


