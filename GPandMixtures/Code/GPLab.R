# GP Lab
data <- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", sep=",",head=T,row.names=1)
y <- data['chd']
X <- data[c('sbp','tobacco','ldl','obesity','age')]
var1 <- 'age'
var2 <- 'tobacco'

plot(X[y==1,var1],X[y==1,var2], 'col'="red")
points(X[y==0,var1],X[y==0,var2], 'col'="blue")
