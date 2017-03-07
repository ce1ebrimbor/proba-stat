#!/usr/bin/env Rscript

## PROBA-STAT: TP 2
## LOI FORTE DES GRANDS NOMBRES
##
## Pour une suite x distribuée selon une loi de probabilité 
## ayant m = E(X) < +inf la somme cumulative va tendre vers la moyenne. 
##
## Exercice 1

# Loi uniforme
n <- 1000000
term_index = 1:n
set.seed(n); dist <- runif(n)
esp <- cumsum(dist)/term_index

plot(term_index, esp, type="l", 
	 main="Espérance: Loi Uniforme", 
	 xlab="Réalisations", ylab="Esperance")

# Loi exponentielle
set.seed(n); dist_exp <- rexp(n, rate=1)
exp_esp <- cumsum(dist_exp)/term_index

x11()
plot(term_index, exp_esp, type="l",
	 main="Espérance: Loi Exponentielle",
	 xlab="Réalisations", ylab="Esperance")
	 

# Observations: Pour un n assez grand l'espérence tend vers sa 
#               valeur théorique.
set.seed(n); dist_cauchy <- rcauchy(n)
esp_cauchy <- cumsum(dist_cauchy)/term_index

x11()
plot(term_index, esp_cauchy, type="l",
	 main="Espérance: Loi de Cauchy",
	 xlab="Réalisations", ylab="Esperance")


# Répartition observée et répartition théorique
# !! Rajouter la densité

# Loi Uniforme
set.seed(n); dist_unif <- runif(n, min=-2, max=3)
x11()
hist(dist_unif, prob=TRUE,
	 main="Histogramme de la loi Uniforme",
	 xlab="Intervales", ylab="Frequence")


# Loi Normale
set.seed(n); dist_norm <- rnorm(n, -1, 4)
x11()
hist(dist_norm, prob=TRUE,
	 main="Histogramme de la loi Normale",
	 xlab="Intervales", ylab="Frequence")
par(new=TRUE)
lines(density(dist_norm))



#Théorème de la limite centrale
X <- matrix(rbinom(500*1000, 10, 0.8), ncol=500, nrow=1000)
sum <- c()
for(i in 1:500){
	sum[i] <- sum(X[,i])
}
yr <- sqrt(1000)*(sum/1000 - 8)
x11()
hist(yr)
par(new=TRUE)
lines(density(rnorm(500,sqrt(1.6))))
