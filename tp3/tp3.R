#!/usr/bin/env Rscript

## PROBA-STAT: TP 3

# 11.1 Loi uniforme

# Generate a random point on the rectangle 
# determined by the cartesian product of 
# the ranges [a,b] x [c,d]

require("plotrix")

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
distance <- function(p1, p2){
	d <- sqrt((p2[1] - p1[1])**2+(p2[2] - p1[2])**2)
	return(d)
} 


mean_distance <- function(points_pair_nb){
	count <- points_pair_nb
	sum <- 0
	while(count > 0){
		p1 <- unif(1, 0, 1, 0, 1)
		p2 <- unif(1, 0, 1, 0, 1)
		sum <- sum + distance(p1, p2)
		
		count <- count - 1
	}

	return(sum/points_pair_nb)
	
}


#Exercice 4
circle_distances <- function(){
	#Setting up circle limits
	a <- -1
	b <- 1
	c <- -1
	d <- 1

	n <- 10000
	counter <- 0

	circle <- matrix(c(0,0), ncol=2)

	while(counter < n){
		test <- unif(1, a, b, c, d)
		x <- test[1]
		y <- test[2]

		if( x**2 + y**2 <= 1){
			circle <- rbind(circle, c(x,y))
		}

		counter <- counter + 1

	}

	plot(circle[,1], circle[,2])
	draw.circle(x=0, y=0, radius=1)
}
	

