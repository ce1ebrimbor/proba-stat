#!/usr/bin/env Rscript

## PROBA-STAT: Controle TP
## Author: SASU Daniel
## Groupe: 1

n <- 1000000
term_index <- 1:n

# Exercice 1: Les densitées
# Loi Normale de moyenne 0 et variance 1

set.seed(n); dist_norm <- rnorm(n, 0, 1)
x11()
plot(density(dist_norm), xlab="x", ylab="f(x)", main="N(0,1)")

# Loi normale de moyenne 1 et variance 1
set.seed(n); dist_norm <- rnorm(n, 1, 1)
x11()
plot(density(dist_norm), xlab="x", ylab="f(x)", main="N(1,1)")


# Loi Uniforme sur l'intervalle [0, 3]
set.seed(n); dist_unif <- runif(n, min=0, max=3)
x11()
plot(density(dist_unif), xlab="x", ylab="f(x)", main="U(0,3)")


# Loi exponentielle 
set.seed(n); dist_exp <- rexp(n, rate=3)
x11()

plot(density(dist_exp), xlab="x", ylab="f(x)", main="Exp(0,3)")


# Exercice 2: Loi forte des grands nombres
# Loi exponentielle lambda=2
set.seed(n); dist_exp <- rexp(n, rate=2)
exp_esp <- cumsum(dist_exp)/term_index

x11()
plot(term_index, exp_esp, main="Espérance: Loi Exponentielle",
	 xlab="Réalisations", ylab="Esperance")


# Loi binomiale
set.seed(n); dist_bin <- rbinom(n=5000, size=10, p=0.6)
bin_esp <- cumsum(dist_bin)/term_index

x11()
plot(term_index, bin_esp, main="Espérance: Loi Binomiale B(10,0.6)",
	 xlab="Réalisations", ylab="Esperance")



# Loi normale de moyenne 1 et variance 2
set.seed(n); dist_norm <- rnorm(n=5000, 1, sqrt(2))
norm_esp <- cumsum(dist_norm)/term_index

x11()
plot(term_index, norm_esp, main="Espérance: Loi Normale moyenne=1 variance=2",
	 xlab="Réalisations", ylab="Esperance")


# Exercice 3
# Theoreme de la limite centrale

X <- matrix(rexp(n=500*1000, 1), ncol=500, nrow=1000)
sum <- c()
for(i in 1:500){
	sum[i] <- sum(X[,i])
}
yr <- sqrt(1000)*(sum/1000 - 1)
x11()
hist(yr)
par(new=TRUE)
plot(density(rnorm(500, 0, 1)))


for(d in dev.list()) {
        dev.set(d)
        Name = paste("Image", d, ".jpg", sep="")
        dev.copy(jpeg, Name)
        dev.off()
}
