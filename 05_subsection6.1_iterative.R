rm(list=ls())

###

library(dplyr)
source("04_functions.txt")

### 

datos = read.table("04_datos.txt", header=FALSE, sep=";")
head(datos)

Y = datos[,1]
X = as.matrix(datos[,-1])

###

fin = 0
i = 0
adjustment = 1 # change 0/1
while(fin == 0){
  i = i + 1
  reg = lm(Y~X)
  summary(reg) 
  #
  avif = aVIF(X, constant=FALSE)
  avif[[1]]
  tabular = table(reg, X, avif)[,-1]
  tabular = tabular %>% mutate(across(is.numeric, round, digits=3))
  variable = rownames(tabular)
  tabular = cbind(tabular, variable)
  write.table(tabular, paste("05_tabular_iterative_adjustment=", adjustment,".txt", sep=""), row.names = FALSE, sep="\t", append = TRUE)
  #
  t_exp = tabular[[4]]
  candidato = which.min(t_exp[-1]) + 1 # I remove the 'intercept'
  if (adjustment == 0) rejection = tabular[[6]] else rejection = tabular[[8]] # better with reject = tabular[[6]] or with aReject = tabular[[8]]? 
  if (rejection[candidato] == "Yes") fin = 1 else X = X[,-(candidato-1)] # in t_exp there is ‘intercept’ and in X there is no ‘intercept’.
  write.table(candidato, paste("05_tabular_iterative_adjustment=", adjustment,".txt", sep=""), row.names = c("candidate"), col.names = FALSE, sep="\t", append = TRUE)
}
tabular
tabular = cbind(tabular[10], tabular[-10])
write.table(tabular, paste("05_tabular_final_adjustment=", adjustment,".txt", sep=""), row.names = FALSE, sep=" & ")

reg_final = lm(Y~X)
summary(reg_final)

#####################################################################

X = as.matrix(datos[,-1])

reg_initial = lm(Y~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+
                   X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+
                   X[,20]+X[,21]+X[,22]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27]+X[,28]+X[,29]+
                   X[,30]+X[,31]+X[,32]+X[,33]+X[,34])
summary(reg_initial)
summary(reg_initial)[[8]] # R^2

reg_final = lm(Y~X[,1]+X[,3]+X[,4]+X[,7]+X[,11]+X[,12]+X[,13]+X[,16]+X[,17]+X[,21]+X[,22]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27]+X[,29]+X[,30]+X[,31]+X[,32]+X[,33])
summary(reg_final)
summary(reg_final)[[8]] # R^2

#

library(memisc)
regs = mtable("Model 1" = reg_initial, "Model 2" = reg_final, summary.stats=c("sigma","R-squared","F","p","N", "AIC")) 
regs
write.mtable(regs, format="LaTeX") # https://www.rdocumentation.org/packages/memisc/versions/0.99.31.8/topics/mtable
#mtable_format_latex(regs)