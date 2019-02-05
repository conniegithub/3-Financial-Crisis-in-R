#---------------------------------------------------------------------------------------------------------------
#load X, Y values
data <- read.csv("C:/Users/../financial crisis project/2001-2007returns.csv", header=TRUE)
#data <- read.csv("C:/Users/../financial crisis project/2007-2009returns.csv", header=TRUE)
#data <- read.csv("C:/Users/../financial crisis project/2009-2016returns.csv", header=TRUE)

u <- as.numeric(unlist(data[1:500,1]))
L <- length(u)

test <- function(x){
	v <- array(0, c(L,1))
	v[1] <- u[1]^2
	y <- numeric(1)
	for(i in 1:(L-1)){
		y = y - log(v[i])-u[i+1]^2/v[i]
		v[i+1] = x[1] + x[2]*v[i] + x[3]*u[i+1]^2 #w,a,b for GARCH
	}
	y
}

library(stats)
param <- unlist(optim(c(0.0001, 0.8, 0.09), test, method = "L-BFGS-B", 
			lower = c(0,0,0), upper = c(1,1,1),
			control=list(fnscale = -1))) #turns into maximization problem

w <- as.numeric(param[1])
a <- as.numeric(param[2])
b <- as.numeric(param[3])
for(i in 2:L){
	v[i] = w + a*v[i-1] + b*u[i-1]^2
	y = y - log(v[i])-u[i]^2/v[i]
}
#---------------------------------------------------------------------------------------------------------------
#multivariate versions
#---------------------------------------------------------------------------------------------------------------
#GARCH
#---------------------------------------------------------------------------------------------------------------
#load X, Y values
data <- read.csv("C:/Users/../financial crisis project/2001-2007returns.csv", header=TRUE)
#data <- read.csv("C:/Users/../financial crisis project/2007-2009returns.csv", header=TRUE)
#data <- read.csv("C:/Users/../financial crisis project/2009-2016returns.csv", header=TRUE)

N = 500
#N = 250
X <- array(0, c(N,5)) #determines the size of storage we need
X[,1] <- as.numeric(unlist(data[1:N,1])) #daily returns
X[,2] <- as.numeric(unlist(data[1:N,2]))
X[,3] <- as.numeric(unlist(data[1:N,3]))
X[,4] <- as.numeric(unlist(data[1:N,4]))
X[,5] <- as.numeric(unlist(data[1:N,5]))

garch <- function(x){
	y <- numeric(1)
	V <- cov(X,X)
	vl <- V
	for(i in 2:N){
		U <- as.matrix(X[i,])
		y = y - log(det(V))-t(U)%*%chol2inv(chol(V))%*%U #solve gives inverse matrix
		V = x[1]*(vl) + x[2]*U%*%t(U) + x[3]*V #updates covariance matrix
	}
	y
}

library(stats)
pgarch <- unlist(optim(c(runif(3,0,0.5)), garch, 
			method = "L-BFGS-B", 
			lower = c(0,0,0), 
			upper = c(1,1,1),
			control=list(fnscale = -1))) #turns into maximization problem
r <- as.numeric(pgarch[1]) #w = r*vl
a <- as.numeric(pgarch[2])
b <- as.numeric(pgarch[3])

r+a+b

#w matrix
w <- r*cov(X,X)


#2001-2007: r = 0.02937602, a = 0.02989006, b = 0.9396944
#2007-2009: r = 0.01201369, a = 0.04957637, b = 0.9479066
#2009-2016: r = 0.07259034, a = 0.01567179, b = 0.9106509
#---------------------------------------------------------------------------------------------------------------
#EWMA
#---------------------------------------------------------------------------------------------------------------
#load X, Y values
#data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

N = 500
#N = 250
X <- array(0, c(N,5)) #determines the size of storage we need
X[,1] <- as.numeric(unlist(data[1:N,1])) #daily returns
X[,2] <- as.numeric(unlist(data[1:N,2]))
X[,3] <- as.numeric(unlist(data[1:N,3]))
X[,4] <- as.numeric(unlist(data[1:N,4]))
X[,5] <- as.numeric(unlist(data[1:N,5]))

library(MASS)
ewma <- function(x){
	y <- numeric(1)
	V <- cov(X,X)
	for(i in 2:N){
		U <- as.matrix(X[i,])
		y = y - log(det(V))-t(U)%*%chol2inv(chol(V))%*%U #solve gives inverse matrix
		V = (1 - x[1])*U%*%t(U) + x[1]*V #updates covariance matrix
	}
	y
}

pewma <- unlist(optim(0.9, ewma, method = "L-BFGS-B", 
			lower = 0.01, upper = 0.99,
			control=list(fnscale = -1))) #turns into maximization problem
lambda <- as.numeric(pewma[1])

#2001-2007: lambda = 0.9832675
#2007-2009: lambda = 0.9691219
#2009-2016: lambda = 0.99

#calculate VaR

	V <- cov(X,X)
	for(i in 2:N){
		U <- as.matrix(X[i,])
		V = (1 - x[1])*U%*%t(U) + x[1]*V #updates covariance matrix
	}
	V #final updated covariance matrix
	invest <- as.matrix(c(10,20,30,40,50))
	sqrt(t(invest)%*%V%*%invest)*2.33 #VaR

#---------------------------------------------------------------------------------------------------------------
