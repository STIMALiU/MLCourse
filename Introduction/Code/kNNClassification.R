#install.packages("ElemStatLearn")
#install.packages("class")
library(ElemStatLearn)
require(class)
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
data <- data.frame(g = g, x = x)
xNewData <- data.frame(x.1 = xnew[,1], x.2 = xnew[,2])
par(mfrow = c(2,2))

# logistic regression
logisticModel <- glm( g ~ x.1 + x.2, family = "binomial", data = data)
prob <- predict(logisticModel, newdata = xNewData, type="response")
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
plot(x, col=ifelse(g==1, "coral", "cornflowerblue"), labels="", xlab="", ylab="", main=
       "logistic regression", axes=FALSE)
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
abline( (0.0-coef(logisticModel)[1])/coef(logisticModel)[3], -coef(logisticModel)[2]/coef(logisticModel)[3])
box()




# 1 nearest
mod <- knn(x, xnew, g, k=1, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, probFit, levels=0.5, labels="", xlab="", ylab="", main=
          "1-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
box()


# 5 nearest
mod <- knn(x, xnew, g, k=5, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, probFit, levels=0.5, labels="", xlab="", ylab="", main=
          "5-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
box()

# 15 nearest
mod <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, probFit, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
box()




############# BOOTSTRAP TO SHOW VARIABILITY
x <- mixture.example$x
g <- mixture.example$y
selected <- sample(1:200, replace = TRUE)
x <- x[selected,]
g <- g[selected]

xnew <- mixture.example$xnew
data <- data.frame(g = g, x = x)
xNewData <- data.frame(x.1 = xnew[,1], x.2 = xnew[,2])

par(mfrow = c(2,2))

# logistic regression
logisticModel <- glm( g ~ x.1 + x.2, family = "binomial", data = data)
prob <- predict(logisticModel, newdata = xNewData, type="response")
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
plot(x, col=ifelse(g==1, "coral", "cornflowerblue"), labels="", xlab="", ylab="", main=
       "logistic regression", axes=FALSE)
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
abline( (0.0-coef(logisticModel)[1])/coef(logisticModel)[3], -coef(logisticModel)[2]/coef(logisticModel)[3])
box()




# 1 nearest
mod <- knn(x, xnew, g, k=1, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, probFit, levels=0.5, labels="", xlab="", ylab="", main=
          "1-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
box()


# 5 nearest
mod <- knn(x, xnew, g, k=5, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, probFit, levels=0.5, labels="", xlab="", ylab="", main=
          "5-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
box()

# 15 nearest
mod <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
probFit <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, probFit, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probFit>0.5, "coral", "cornflowerblue"))
box()