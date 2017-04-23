#!/usr/bin/env Rscript

## PROBA-STAT: TP 1

## 9.2 Données : les charger, les créer, les manipuler

# 9.2.2
taille <- c(168, 175, 172, 183)
poids <- c(67, 75, 69, 81)

table <- data.frame(taille=taille, poids=poids)
print("9.2.2 Data table: ")
print(table)

#9.2.3.a
x <- 1:20
print("9.2.3.a 1..20 Sequence: ")
print(x)

#9.2.3.b
y <- seq(from=1, to=10, by=0.5)
print("9.2.3.b 1..10 Sequence by 0.5: ")
print(y)

#9.2.3.c
z <- seq(from=2, to=2, length.out=12)
print("9.2.3.c 2 reapeated 12 times: ")
print(z)

#9.2.3.d
a_seq <- LETTERS[seq(from=1, to=1, length.out=20)]
b_seq <- LETTERS[seq(from=2, to=2, length.out=20)]
abc_seq <- LETTERS[seq(from=1, to=3, length.out=20)]
print("9.2.3.d  Sequences of letters: ")
print(a_seq)
print(b_seq)
print(abc_seq)



# 9.2.4 Nombres aléatoires

# 9.2.4.a
first_sample <- sample(c(0, 1), size=1, replace=FALSE)


# 9.2.4.b Wrapping the Bernoulli sampling in a function
bernoulli <- function( n ){
	return (rbinom(n, size=1, prob=0.5))
}

bernoulli_seq = bernoulli(100)

print("The Bernoulli trial  : ")
print(bernoulli_seq)

# 9.2.5 Creating a matrix and extracting data
values <- 1:30 # matrix values 
A = matrix(values, nrow=10, ncol=3)
B = A[8:10,] # extracting the last three columns

print("The matrix: ")
print(A)
print("The last three columns")
print(B)


# Des données aux graphiques:

# Jeu de Pile ou face
# pile - 1
# face - 0
jeu <- sample(c(1, 0), size=1000, replace=TRUE, prob=c( 0.25, 0.75))
moyenne <- mean(jeu)
# ? hist?


# graphiques
# curve(exp(-x*x/2)/sqrt(2*pi), from=0, to=100)

test5 <- function(x, miu, sigma){
	return (exp(-(x-miu)**2/(2*sigma**2))/(sigma*sqrt(2*pi)))
}

#curve(test5(x, miu=10,sigma=0.5), from=0, to=100)
# le sommet de la courbe est la coordonée (miu, sigma) !
