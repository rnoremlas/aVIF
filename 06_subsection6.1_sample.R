rm(list=ls())
source("04_functions.txt")

set.seed(2025)
n = 150 # number of observations
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

#datos = cbind(Y, X)
#write.table(datos, "04_datos.txt", row.names = FALSE, col.names = FALSE, sep="; ")

cor(Y, X)

###  

variable = 1:(ncol(X)+1)
coef_mu = c(NA, coef_mu, NA, NA, NA, NA)  
coef_sigma = c(NA, coef_sigma, NA, NA, NA, NA)
#tabular = cbind(variable, coef_mu, coef_sigma, coef_var)
#write.table(tabular, "04_tabular.txt", row.names = FALSE, col.names = TRUE, sep=" & ")

###

fin = 0
i = 0
adjustment = 0
while(fin == 0){
  i = i + 1
  reg = lm(Y~X)
  summary(reg) 
  #
  avif = aVIF(X, constant=FALSE)
  avif[[1]]
  tabular = table(reg, X, avif)[,-1]
  tabular = tabular %>% mutate(across(is.numeric, round, digits=3))
  if (i == 1) write.table(tabular, "04_tabular_initial_sample.txt", row.names = FALSE, sep=" & ")
  variable = rownames(tabular)
  tabular = cbind(tabular, variable)
  write.table(tabular, "06_tabular_iterative_sample.txt", row.names = FALSE, sep="\t", append = TRUE)
  #
  t_exp = tabular[[4]]
  candidato = which.min(t_exp[-1]) + 1 # I remove the 'intercept'
  if (adjustment == 0) rejection = tabular[[6]] else rejection = tabular[[8]] # better with reject = tabular[[6]] or with aReject = tabular[[8]]? 
  if (rejection[candidato] == "Yes") fin = 1 else X = X[,-(candidato-1)] # in t_exp there is ‘intercept’ and in X there is no ‘intercept’.
  write.table(candidato, "06_tabular_iterative_sample.txt", row.names = c("candidate"), col.names = FALSE, sep="\t", append = TRUE)
}
tabular
tabular = cbind(tabular[10], tabular[-10])
write.table(tabular, "06_tabular_final_sample.txt", row.names = FALSE, sep=" & ")

reg_final = lm(Y~X)
summary(reg_final)