# Density estimation

# Reading the fish data from file
setwd('~/Dropbox/Teaching/IntroToML/Introduction/Code/')
fish <- read.table('~/Dropbox/Teaching/IntroToML/Introduction/Data/fish.dat', header = FALSE)
names(fish) <- 'fLength'
attach(fish)
xGrid <- seq(0, 80, by = 0.1)

# Histograms
par(mfrow = c(2,2))
hist(fLength, main = 'Default', xlab ="fLength", freq = FALSE)
hist(fLength,20, main = '20 bins', xlab ="fLength", freq = FALSE)
hist(fLength,40, main = '40 bins', xlab ="fLength", freq = FALSE)
hist(fLength,60, main = '60 bins', xlab ="fLength", freq = FALSE)

# Kernel density estimators
par(mfrow = c(2,2))
hist(fLength,40,  main = 'Default bandwidth', xlab ="fLength", freq = FALSE)
lines(density(fLength, adjust = 1), col = "red", lwd= 2)
hist(fLength,40,  main = 'Twice default bandwidth', xlab ="fLength", freq = FALSE)
lines(density(fLength, adjust = 2), col = "red", lwd= 2)
hist(fLength,40,  main = 'Half default bandwidth', xlab ="fLength", freq = FALSE)
lines(density(fLength, adjust = 0.5), col = "red", lwd= 2)
hist(fLength,40,  main = 'Quarter default bandwidth', xlab ="fLength", freq = FALSE)
lines(density(fLength, adjust = 0.25), col = "red", lwd= 2)

######## k Nearest Neighbours (kNN) ##########
par(mfrow = c(1,2))
kNNDensEst <- rep(0,length(xGrid))
sortedData <- sort(fLength+0.000000*runif(length(fLength)))
N <- length(fLength)
K <- 15
count <- 0
for (i in xGrid){
  count <- count + 1
  dist <- (sortedData-i)^2 # Distance 
  indexKNeighbours <- order(dist)[1:K]
  
  V <- max(sortedData[indexKNeighbours])-min(sortedData[indexKNeighbours])
  print(V)
  kNNDensEst[count] <- K/(V*N)
}
hist(fLength, 40, freq = FALSE, ylim = c(0,0.1), main = 'k=15')
lines(xGrid,kNNDensEst,type="l", col='red', lwd = 2)

kNNDensEst <- rep(0,length(xGrid))
sortedData <- sort(fLength+0.000000*runif(length(fLength)))
N <- length(fLength)
K <- 25
count <- 0
for (i in xGrid){
  count <- count + 1
  dist <- (sortedData-i)^2 # Distance 
  indexKNeighbours <- order(dist)[1:K]
  
  V <- max(sortedData[indexKNeighbours])-min(sortedData[indexKNeighbours])
  print(V)
  kNNDensEst[count] <- K/(V*N)
}
hist(fLength, 40, freq = FALSE, ylim = c(0,0.1), main = 'k=25')
lines(xGrid,kNNDensEst,type="l", col='red', lwd = 2)




#install.packages('class')
library(class)




# MIXTURES OF NORMALS
#install.packages('mixtools') # Package for estimating mixture models, uncomment if first time
library(mixtools)

# Probability density function for a Gaussian mixture
# Presumes the mixture object has the structure used by mixtools
# Taken from Cosma Shalizi at http://www.stat.cmu.edu/~cshalizi/
dnormalmix <- function(x,mixture,log=FALSE) {
  lambda <- mixture$lambda
  k <- length(lambda)
  # Calculate share of likelihood for all data for one component
  like.component <- function(x,component) {
    lambda[component]*dnorm(x,mean=mixture$mu[component],
                            sd=mixture$sigma[component])
  }
  # Create array with likelihood shares from all components over all data
  likes <- sapply(1:k,like.component,x=x)
  # Add up contributions from components
  d <- rowSums(likes)
  if (log) {
    d <- log(d)
  }
  return(d)
}

par(mfrow = c(2,2))
hist(fLength, freq = FALSE, main = "K = 1")
lines(xGrid, dnorm( xGrid, mean = mean(fLength), sd = sd(fLength) ),  col='purple', lwd = 3)

for (K in 2:4){
  lambdaInit <- rep(1/K,K)
  muInit <- as.vector(quantile(fLength,cumsum(c(0,rep(1/(K-1),K-1)))))
  sigmaInit <- rep(sd(fLength),K)
  mixNormalModel <- normalmixEM(fLength, lambda = lambdaInit, mu = muInit, sigma = sigmaInit, maxit = 10000) # Estimate mixture of normals using EM algorithm
  summary(mixNormalModel)
  plot(mixNormalModel, density = TRUE, loglik = FALSE, main2 = paste("K = ", toString(K)), xlab2="fLength")
  lines(xGrid,dnormalmix(xGrid,mixNormalModel), col='purple', lwd = 3)
}

