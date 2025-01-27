ene = seq(15,200,5)
kas = seq(3,15,1)

a = matrix( , length(ene), length(kas))
i = 1
for(n in ene){
  j = 1
  for (k in kas){
    a[i,j] = round((n-k+1)/(n-1), digits = 3)
    j = j + 1
  }
  i = i + 1
}
colnames(a) = as.character(kas)
rownames(a) = as.character(ene)

a
write.table(a, "07_matrix_a.txt", sep=" & ", dec=".")

sqrt_a = round(sqrt(a), digits = 3)
sqrt_a
write.table(sqrt_a, "07_matrix_sqrt_a.txt", sep=" & ", dec=".")

b = round(1/sqrt_a, digits = 3)
write.table(b, "07_matrix_b.txt", sep=" & ", dec=".")