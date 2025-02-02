rm(list=ls())
source("04_functions.txt")

set.seed(2025)
n = 50 # number of observations
k = 30 # number of variables

#####################################################################
# the observations of the independent variables are generated

X = matrix(0, nrow = n, ncol = k)
coef_mu = numeric()
coef_sigma = numeric()
for (i in 1:k) {
  mu = sample(seq(-10,10,2), 1)
  sigma = sample(1:5, 1)
  X[,i] = rnorm(n, mu, sigma) # gender ‘k’ variables independently
  coef_mu = c(coef_mu, mu)
  coef_sigma = c(coef_sigma, sigma)
} 
#write.table(t(coef_mu), "04_coef_mu.txt", row.names = FALSE, col.names = FALSE, sep=", ")
#write.table(t(coef_sigma), "04_coef_sigma.txt", row.names = FALSE, col.names = FALSE, sep=", ")

# linearly related variables are added to the existing ones
p = rnorm(n, 0, 2)
  Z = 4*X[,1] - 3*X[,2] + X[,4] + p
p = rnorm(n, 0, 3)
  W = X[,6] - X[,7] - p
p = rnorm(n, 0, 2)
  V = 5*X[,9] - 3*X[,12] - p
p = rnorm(n, 0, 3)
  E = X[,14] + X[,16] + p

X = cbind(X, Z, W, V, E)

#####################################################################
# observations of the dependent variable are generated using a subset of the independent variables

u = rnorm(n, 0, 7)
coef_var = 3
Y = coef_var + u

subset = sort(sample(1:ncol(X), ncol(X)*0.6))
#write.table(t(subset), "04_subset.txt", row.names = FALSE, col.names = FALSE, sep=", ")

for (i in 1:ncol(X)){
  if (sum(i == subset) == 1) beta = sample(seq(-7,7,2), 1) else beta = 0
  Y = Y + beta*X[,i]
  coef_var = c(coef_var, beta)
} 
#write.table(t(coef_var), "04_betas.txt", row.names = FALSE, col.names = FALSE, sep=", ")

datos = cbind(Y, X)
write.table(datos, "04_datos.txt", row.names = FALSE, col.names = FALSE, sep="; ")

  cor(Y, X)
  
###  
  
variable = 1:(ncol(X)+1)
coef_mu = c(NA, coef_mu, NA, NA, NA, NA)  
coef_sigma = c(NA, coef_sigma, NA, NA, NA, NA)
#tabular = cbind(variable, coef_mu, coef_sigma, coef_var)
#write.table(tabular, "04_tabular.txt", row.names = FALSE, col.names = TRUE, sep=" & ")

#####################################################################

reg = lm(Y~X)
summary(reg)

avif = aVIF(X, constant=FALSE)
avif[[1]]
tabular = table(reg, X, avif)[,-1]

library(dplyr)
tabular = tabular %>% mutate(across(is.numeric, round, digits=3))
tabular = cbind(variable, coef_mu, coef_sigma, coef_var, tabular)
tabular
write.table(tabular, "04_tabular.txt", row.names = FALSE, sep=" & ")

#####################################################################

X_final = cbind(X[,1], X[,3], X[,4], X[,7], X[,11], X[,12], X[,13], X[,16], X[,17], X[,23], X[,24], X[,25], X[,26], X[,27], X[,29], X[,30], X[,31], X[,32], X[,33])
reg_final = lm(Y~X_final)
summary(reg_final)

avif = aVIF(X_final, constant=FALSE)
avif[[1]]
table(reg_final, X_final, avif)[,-1]

#####################################################################

reg_initial = lm(Y~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+
                X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+
                X[,20]+X[,21]+X[,22]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27]+X[,28]+X[,29]+
                X[,30]+X[,31]+X[,32]+X[,33]+X[,34])
summary(reg_initial)
summary(reg_initial)[[8]] # R^2

reg_final = lm(Y~X[,1]+X[,3]+X[,4]+X[,7]+X[,11]+X[,12]+X[,13]+X[,16]+X[,17]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27]+X[,29]+X[,30]+X[,31]+X[,32]+X[,33])
summary(reg_final)
summary(reg_final)[[8]] # R^2

#

library(memisc)
regs = mtable("Model 1" = reg_initial, "Model 2" = reg_final, summary.stats=c("sigma","R-squared","F","p","N", "AIC")) 
regs
write.mtable(regs, format="LaTeX") # https://www.rdocumentation.org/packages/memisc/versions/0.99.31.8/topics/mtable
#mtable_format_latex(regs)

#

library(xtable) #install.packages("xtable") # https://verso.mat.uam.es/~joser.berrendero/blog/tablas.html

print(xtable(reg_initial), include.rownames = TRUE)
print(xtable(reg_final), include.rownames = TRUE)
