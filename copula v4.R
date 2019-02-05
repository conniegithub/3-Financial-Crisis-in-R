#---------------------------------------------------------------------------------------------------------------
#LOOP VERSION
data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
#data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

N = 500
#N = 250

L <- length(data$amazon)
return <- array(0, c(L,5)) #determines the size of storage we need
return[,1] <- data$amazon #gives numeric
return[,2] <- data$att
return[,3] <- data$chase
return[,4] <- data$exxon
return[,5] <- data$walmart
head(return) #look at first few observations

VaR <- matrix(0,L-N)
error <- 0

for (j in (N+1):L){
	X <- return[(j-N):j,] #in-sample
	n <- length(X)/5

#---------------------------------------------------------------------------------------------------------------
library(nleqslv)
#estimate alphas to pre-determine correlations
r <- cor(returnIn, returnIn)
test <- function(x) {
	y <- numeric(15)

	y[1] <- x[15]*x[1] - r[5,1]
	y[2] <- x[15]*x[3] + x[14]*x[2] - r[5,2]
	y[3] <- x[15]*x[6] + x[14]*x[5] + x[13]*x[4] - r[5,3]
	y[4] <- x[15]*x[10] + x[14]*x[9] + x[13]*x[8] + x[12]*x[7] - r[5,4]
	y[5] <- x[10]*x[1] - r[4,1]
	y[6] <- x[10]*x[3] + x[9]*x[2] - r[4,2]
	y[7] <- x[10]*x[6] + x[9]*x[5] + x[8]*x[4] - r[4,3]
	y[8] <- x[6]*x[1] - r[3,1]
	y[9] <- x[6]*x[3] + x[5]*x[2] - r[3,2]
	y[10] <- x[3]*x[1] - r[2,1]

	y[11] <- x[1]^2 - 1
	y[12] <- x[2]^2 + x[3]^2 - 1
	y[13] <- x[4]^2 + x[5]^2 + x[6]^2 - 1
	y[14] <- x[7]^2 + x[8]^2 + x[9]^2 + x[10]^2 - 1
	y[15] <- x[11]^2 + x[12]^2 + x[13]^2 + x[14]^2 + x[15]^2 - 1
	y
}

xstart <- c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
fstart <- test(xstart)
xstart
fstart
x <- nleqslv(xstart, test)
x <- unlist(x)
x1 <- as.numeric(x[1]) #convert from character to numeric
x2 <- as.numeric(x[2])
x3 <- as.numeric(x[3])
x4 <- as.numeric(x[4])
x5 <- as.numeric(x[5])
x6 <- as.numeric(x[6])
x7 <- as.numeric(x[7])
x8 <- as.numeric(x[8])
x9 <- as.numeric(x[9])
x10 <- as.numeric(x[10])
x11 <- as.numeric(x[11])
x12 <- as.numeric(x[12])
x13 <- as.numeric(x[13])
x14 <- as.numeric(x[14])
x15 <- as.numeric(x[15])
#---------------------------------------------------------------------------------------------------------------
library(MASS)
#maximum likelihood estimates of student t parameters
F1 <- fitdistr(X[,1], "t", start = list(m=mean(X[,1]),s=sd(X[,1]), df=3), lower=c(-1,0.0000001,1))
F2 <- fitdistr(X[,2], "t", start = list(m=mean(X[,2]),s=sd(X[,2]), df=3), lower=c(-1,0.000001,1))
F3 <- fitdistr(X[,3], "t", start = list(m=mean(X[,3]),s=sd(X[,3]), df=3), lower=c(-1,0.00001,1))
F4 <- fitdistr(X[,4], "t", start = list(m=mean(X[,4]),s=sd(X[,4]), df=3), lower=c(-1,0.00001,1))
F5 <- fitdistr(X[,5], "t", start = list(m=mean(X[,5]),s=sd(X[,5]), df=3), lower=c(-1,0.0000001,1))
ml1 <- unlist(F1) #convert list result to vector
ml2 <- unlist(F2)
ml3 <- unlist(F3)
ml4 <- unlist(F4)
ml5 <- unlist(F5)
#---------------------------------------------------------------------------------------------------------------
#marginal normal from joint normal
k = 1000 #number of simulations
set.seed(1234)
s <- array(0, c(k,5))
for(j in 1:5){
	for(i in 1:k){
		s[i,j] <- rnorm(1,0,1) #random variables
	}
}

#define specific correlations
c <- array(0, c(k,5))
c[,1] <- x1*s[,1]
c[,2] <- x3*s[,1] + x2*s[,2]
c[,3] <- x6*s[,1] + x5*s[,2] + x4*s[,3]
c[,4] <- x10*s[,1] + x9*s[,2] + x8*s[,3] + x7*s[,4]
c[,5] <- x15*s[,1] + x14*s[,2] + x13*s[,3] + x12*s[,4] + x11*s[,5]
cor(c,c)

#standardize and transfrom to uniform
U <- array(0, c(k,5))
C <- array(0, c(k,1))
for(i in 1:5){
	C <- c[,i] #saves computation time
	U[,i] = pnorm((C-mean(C))/sqrt(var(C))) #cdf
}
cor(U,U) #seems to preserve the correlation structure

#inverse transform to desired distribution
library(metRology)
X <- array(0, c(k,5))
X[,1] = qt.scaled(U[,1], df = ml1[3], mean = ml1[1], sd = ml1[2])
X[,2] = qt.scaled(U[,2], df = ml2[3], mean = ml2[1], sd = ml2[2])
X[,3] = qt.scaled(U[,3], df = ml3[3], mean = ml3[1], sd = ml3[2])
X[,4] = qt.scaled(U[,4], df = ml4[3], mean = ml4[1], sd = ml4[2])
X[,5] = qt.scaled(U[,5], df = ml5[3], mean = ml5[1], sd = ml5[2])
cor(X,X)
summary(X)

invest <- as.matrix(c(10,20,30,40,50))
loss <- -(X%*%invest) #loss
summary(loss)
VaR <- quantile(loss, c(0.99))
#---------------------------------------------------------------------------------------------------------------

error = error + 1
}


#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#NON-LOOP VERSION
#data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
data <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

L <- length(data$amazon)
X <- array(0, c(L,5)) #determines the size of storage we need
X[,1] <- data$amazon #gives numeric
X[,2] <- data$att
X[,3] <- data$chase
X[,4] <- data$exxon
X[,5] <- data$walmart
head(X) #look at first few observations

#---------------------------------------------------------------------------------------------------------------
library(nleqslv)
#estimate alphas to pre-determine correlations
r <- cor(X, X)
test <- function(x) {
	y <- numeric(15)

	y[1] <- x[15]*x[1] - r[5,1]
	y[2] <- x[15]*x[3] + x[14]*x[2] - r[5,2]
	y[3] <- x[15]*x[6] + x[14]*x[5] + x[13]*x[4] - r[5,3]
	y[4] <- x[15]*x[10] + x[14]*x[9] + x[13]*x[8] + x[12]*x[7] - r[5,4]
	y[5] <- x[10]*x[1] - r[4,1]
	y[6] <- x[10]*x[3] + x[9]*x[2] - r[4,2]
	y[7] <- x[10]*x[6] + x[9]*x[5] + x[8]*x[4] - r[4,3]
	y[8] <- x[6]*x[1] - r[3,1]
	y[9] <- x[6]*x[3] + x[5]*x[2] - r[3,2]
	y[10] <- x[3]*x[1] - r[2,1]

	y[11] <- x[1]^2 - 1
	y[12] <- x[2]^2 + x[3]^2 - 1
	y[13] <- x[4]^2 + x[5]^2 + x[6]^2 - 1
	y[14] <- x[7]^2 + x[8]^2 + x[9]^2 + x[10]^2 - 1
	y[15] <- x[11]^2 + x[12]^2 + x[13]^2 + x[14]^2 + x[15]^2 - 1
	y
}

xstart <- c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
fstart <- test(xstart)
xstart
fstart
x <- nleqslv(xstart, test)
x <- unlist(x)
x1 <- as.numeric(x[1]) #convert from character to numeric
x2 <- as.numeric(x[2])
x3 <- as.numeric(x[3])
x4 <- as.numeric(x[4])
x5 <- as.numeric(x[5])
x6 <- as.numeric(x[6])
x7 <- as.numeric(x[7])
x8 <- as.numeric(x[8])
x9 <- as.numeric(x[9])
x10 <- as.numeric(x[10])
x11 <- as.numeric(x[11])
x12 <- as.numeric(x[12])
x13 <- as.numeric(x[13])
x14 <- as.numeric(x[14])
x15 <- as.numeric(x[15])
#---------------------------------------------------------------------------------------------------------------
library(MASS)
#maximum likelihood estimates of student t parameters
F1 <- fitdistr(X[,1], "t", start = list(m=mean(X[,1]),s=sd(X[,1]), df=3), lower=c(-1,0.0000001,1))
F2 <- fitdistr(X[,2], "t", start = list(m=mean(X[,2]),s=sd(X[,2]), df=3), lower=c(-1,0.000001,1))
F3 <- fitdistr(X[,3], "t", start = list(m=mean(X[,3]),s=sd(X[,3]), df=3), lower=c(-1,0.00001,1))
F4 <- fitdistr(X[,4], "t", start = list(m=mean(X[,4]),s=sd(X[,4]), df=3), lower=c(-1,0.00001,1))
F5 <- fitdistr(X[,5], "t", start = list(m=mean(X[,5]),s=sd(X[,5]), df=3), lower=c(-1,0.0000001,1))
ml1 <- unlist(F1) #convert list result to vector
ml2 <- unlist(F2)
ml3 <- unlist(F3)
ml4 <- unlist(F4)
ml5 <- unlist(F5)
#---------------------------------------------------------------------------------------------------------------
#marginal normal from joint normal
k = 1000 #number of simulations
set.seed(1234)
s <- array(0, c(k,5))
for(j in 1:5){
	for(i in 1:k){
		s[i,j] <- rnorm(1,0,1) #random variables
	}
}

#define specific correlations
c <- array(0, c(k,5))
c[,1] <- x1*s[,1]
c[,2] <- x3*s[,1] + x2*s[,2]
c[,3] <- x6*s[,1] + x5*s[,2] + x4*s[,3]
c[,4] <- x10*s[,1] + x9*s[,2] + x8*s[,3] + x7*s[,4]
c[,5] <- x15*s[,1] + x14*s[,2] + x13*s[,3] + x12*s[,4] + x11*s[,5]
cor(c,c)

#standardize and transfrom to uniform
U <- array(0, c(k,5))
C <- array(0, c(k,1))
for(i in 1:5){
	C <- c[,i] #saves computation time
	U[,i] = pnorm((C-mean(C))/sqrt(var(C))) #cdf
}
cor(U,U) #seems to preserve the correlation structure

#inverse transform to desired distribution
library(metRology)
X <- array(0, c(k,5))
X[,1] = qt.scaled(U[,1], df = ml1[3], mean = ml1[1], sd = ml1[2])
X[,2] = qt.scaled(U[,2], df = ml2[3], mean = ml2[1], sd = ml2[2])
X[,3] = qt.scaled(U[,3], df = ml3[3], mean = ml3[1], sd = ml3[2])
X[,4] = qt.scaled(U[,4], df = ml4[3], mean = ml4[1], sd = ml4[2])
X[,5] = qt.scaled(U[,5], df = ml5[3], mean = ml5[1], sd = ml5[2])
cor(X,X)
summary(X)

invest <- as.matrix(c(10,20,30,40,50))
loss <- -(X%*%invest) #loss
summary(loss)
VaR <- quantile(loss, c(0.99))
#---------------------------------------------------------------------------------------------------------------
