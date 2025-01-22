rm(list = ls())

set.seed(2025)
library(multiColl)

#

n = 50 
k = 40
X = matrix(1, nrow=n, ncol=k)
vifs = numeric()

for (j in 2:k){
  X[,j] = rnorm(n, sample(c(-5, -3, -1, 1, 3, 5), 1), sample(c(1, 3, 5), 1))
  if (j==2) vifs[j-1] = 1 # the VIF in the simple linear regression model is always equal to 1 (see Diagnosis and quantification of the non-essential collinearity, doi: https://doi.org/10.1007/s00180-019-00922-x )
  if (j>2) vifs[j-1] = max(VIF(X[,1:j]))  
}
vifs

pdf("01_simulation311a.pdf")
  plot(2:k, vifs, type="b", xlab="k", ylab="VIF", col="blue", lwd=2)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  abline(h=10, col="black", lty=2, lwd=2)
dev.off()

#

n = 150
k = 40
X = matrix(1, nrow=n, ncol=k)
vifs = numeric()

for (j in 2:k){
  X[,j] = rnorm(n, sample(c(-5, -3, -1, 1, 3, 5), 1), sample(c(1, 3, 5), 1))
  if (j==2) vifs[j-1] = 1 # the VIF in the simple linear regression model is always equal to 1
  if (j>2) vifs[j-1] = max(VIF(X[,1:j]))  
}
vifs

pdf("01_simulation311b.pdf")
  plot(2:k, vifs, type="b", xlab="k", ylab="VIF", col="blue", lwd=2)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  abline(h=10, col="black", lty=2, lwd=2)
dev.off()
