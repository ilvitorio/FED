set.seed(2011)
library(MASS)
data(UScrime); attach(UScrime)
UScrime1 <- (cbind(log(UScrime[,c(16,1,3:15)]), So))
noise<- matrix(rnorm(35*nrow(UScrime)), ncol=35)
colnames(noise)<- paste('noise', 1:35, sep='')
UScrime.log <- cbind(UScrime1,noise)
X <- UScrime1[,-1]; Y <- UScrime1[,1]
x <- UScrime.log[,-1]; y <- UScrime.log[,1]


library("BMA")
bma_enu <- iBMA.bicreg(X, Y, thresProbne0 = 5, verbose = TRUE, maxNvar = 30)
summary(bma_enu)
detach("package:BMA")

library("BMA")
bma.time <- system.time(bma_sam <- iBMA.bicreg(x, y, thresProbne0 = 5,
                                               verbose = TRUE, maxNvar = 30, nIter = 100000))
cat("Elapsed Time=", bma.time[3], "Seconds")
#Figure 3(b)
bma_fig <- bicreg(x, y)
plot(bma_fig, include=5, include.intercept=FALSE)
detach("package:BMA")