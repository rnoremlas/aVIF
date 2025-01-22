rm(list=ls())
# library(multiColl)

#####################################################################

VIFlocal <- function(X, constante = TRUE){
  if (constante) X = X[,-1]
  p = ncol(X)
  vifs = numeric()
  for (i in 1:p){
    reg_aux = lm(X[,i]~X[,-i])
    R2_aux = as.numeric(summary(reg_aux)[8])
    vifs[i] = 1/(1-R2_aux)
  }
  return(vifs)
}

######################################################################

set.seed(2023)

num_var = 250

observaciones = c(25, 50, 75, 100, 125, 150, 175, 200) # setting here a value equal to or greater than num_var gives problems (obvious)
gammas = c(0, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95)
pes = matrix(0, length(observaciones), length(gammas))
rownames(pes) = observaciones
colnames(pes) = gammas

l = 0
for (obs in observaciones){
  l = l + 1
  X = matrix(1, obs, num_var)
  for (i in 2:num_var) X[,i] = rnorm(obs, sample(c(2,3,4,5), 1), sample(c(2,3,4,5), 1))
  #
  h = 0
  for(gamma in gammas){
    h = h + 1
    Z = matrix(1, obs, num_var)
    for (i in 2:num_var) Z[,i] = (1-gamma^2)*X[,i] + gamma*X[,num_var]
    #
    vifs = numeric()
    vifs[1] = 1
    vifs[2] = 1
    cvifs = numeric()
    tope = 10
    j = 2
    max_vifs = 0
    while(max_vifs < tope){
      j = j + 1
      Zi = Z[,1:j]
      vifs[j] = max(VIFlocal(Zi))
      cvifs[j] = ((obs-j)/(obs-1))*vifs[j]
      max_vifs = max(vifs, na.rm = TRUE)
    }
    #
    pes[l,h] = j
    #
    win.graph()
      series = ts(cbind(vifs, cvifs), start=1, frequency = 1)
      plot(series, plot.type="single", xlab="Number of independent variables", ylab="VIF and aVIF", lwd=2, col=c("blue", "lightblue"), main=paste("n=", obs, ", gamma=", gamma, sep=""))
      abline(h=10, lwd=2, col="black")
      text(j, vifs[j], label=j, pos = 2, lwd=2)
      grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
      gamma_text = gamma*100
      #savePlot(paste("simulation4_Fig2_Obs", obs, "_Gamma", gamma_text, sep=""), type="pdf")
    dev.off()
  }
  #
  print(obs)
}
pes
write.table(pes, "03_p_gamma_b.txt", sep=" & ", dec=".")