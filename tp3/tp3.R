#!/usr/bin/env Rscript

## PROBA-STAT: TP 3

# 11.1 Loi uniforme

# Generate a random point on the rectangle 
# determined by the cartesian product of 
# the ranges [a,b] x [c,d]

unif <- function(n, a, b, c, d){
    x <- runif(n, min=a, max=b)
    y <- runif(n, min=c, max=b)

    random_points <- cbind(x, y)

    return(random_points)
}


# LFGN
n <- 100000
dist_norm = rnorm(n)

lfgn <- function(dist){
    mx <- max(exp(dist) - 1, 0)
    esp <- mx/length(dist)

    return(esp)
}

# Exercice 3
distances <-(points_dist){
    sum <- 0
    for(i in 1:points_dist - 1){
        sum <- sum + dist(c(points_dist[i], points_dist[i]))
    }
    return mean(sum)
} 
