calif <- read.table("http://www.stat.cmu.edu/~cshalizi/350/hw/06/cadata.dat", header=TRUE)
library(tree)
Warning message:
  package 'tree' was built under R version 3.1.3 
treefit = tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)
plot(treefit)
text(treefit,cex=0.75)
price.deciles = quantile(calif$MedianHouseValue,0:10/10)
cut.prices = cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,xlab="Longitude",ylab="Latitude")
partition.tree(treefit,ordvars=c("Longitude","Latitude"),add=TRUE)
summary(treefit)
## Regression tree:
## tree(formula = log(MedianHouseValue) ~ Longitude + Latitude, 
##        data = calif)
## Number of terminal nodes:  12 
## Residual mean deviance:  0.1662 = 3429 / 20630 
## Distribution of residuals:
##   Min.  1st Qu.   Median     Mean  3rd Qu. 
## -2.75900 -0.26080 -0.01359  0.00000  0.26310 
## Max. 
## 1.84100 
treefit3 <- tree(log(MedianHouseValue) ~., data=calif)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,xlab="Longitude",ylab="Latitude")
plot(treefit3)
text(treefit3,cex=0,digits=3)
cut.predictions = cut(predict(treefit3),log(price.deciles),include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.predictions],pch=20,xlab="Longitude",ylab="Latitude")
