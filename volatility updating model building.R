#----------------------------------------------------------------------------------------------------------------------------
#EWMA
#----------------------------------------------------------------------------------------------------------------------------
#dataR <- read.csv("C:/Users/../financial crisis project/2001-2007returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/../financial crisis project/2007-2009returns.csv", header=TRUE)
dataR <- read.csv("C:/Users/../financial crisis project/2009-2016returns.csv", header=TRUE)

#lambda = 0.9832675
#lambda = 0.9691219
lambda = 0.99

N = 500
#N = 250

L <- length(dataR$amazon)
return <- array(0, c(L,5)) #determines the size of storage we need
return[,1] <- dataR$amazon #gives numeric
return[,2] <- dataR$att
return[,3] <- dataR$chase
return[,4] <- dataR$exxon
return[,5] <- dataR$walmart
head(return) #look at first few observations

VaR <- matrix(0,L-N)
ex <- matrix(0,L-N)
actualloss <- matrix(0,L-N)
invest <- as.matrix(c(10,20,30,40,50))

for (j in (N+1):L){
	returnIn <- return[j-N:(j-1),] #in-sample
	n <- length(returnIn)/5
	variance <- cov(returnIn, returnIn)

	for (i in (j-N):j){ #500 previous observations
		U <- as.matrix(return[i,])
		variance = (1 - lambda)*U%*%t(U) + lambda*variance #updates covariance matrix
	}
	
	VaR[j-N] <- 2.33*sqrt(t(invest)%*%variance%*%invest)
	actualloss[j-N] <- -return[j,]%*%invest

	if(VaR[j-N] >= actualloss[j-N]){
		ex[j-N] <- 0
	}else if(VaR[j-N] < actualloss[j-N]){
		ex[j-N] <- 1 #exception
	}
}

sum(ex) #total number of exceptions

#christoffersen
end <- L-N
T01 = 0
T11 = 0
T00 = 0
T10 = 0

for (i in 2:end){
	if ((ex[i-1]==0) && (ex[i]==1)){
		T01 = T01 + 1
	}else if ((ex[i-1]==1) && (ex[i]==1)){
		T11 = T11 + 1
	}else if ((ex[i-1]==0) && (ex[i]==0)){
		T00 = T00 + 1
	}else if ((ex[i-1]==1) && (ex[i]==0)){
		T10 = T10 + 1
	}
}
p = (T01+T11)/(T00+T01+T10+T11)
p01 = T01/(T00+T01)
p11 = T11/(T10+T11)
li <- -2*log((1-p)^(T00+T10)*p^(T01+T11)) + 2*log((1-p01)^(T00)*p01^T01*(1-p11)^T10*p11^T11)

#2001 - 2007 14 exceptions
#2007 - 2009 0 exceptions
#2009 - 2016 21 exceptions
#----------------------------------------------------------------------------------------------------------------------------
#GARCH
#----------------------------------------------------------------------------------------------------------------------------
dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

dataW <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007w.csv", header=TRUE)
#dataW <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009w.csv", header=TRUE)
#dataW <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016w.csv", header=TRUE)

ba <- c(0.9396944, 0.02989006)
#ba <- c(0.9479066, 0.04957637)
#ba <- c(0.9106509, 0.01567179)

N = 500
#N = 250

L <- length(dataR$amazon)
return <- array(0, c(L,5)) #determines the size of storage we need
return[,1] <- dataR$amazon #gives numeric
return[,2] <- dataR$att
return[,3] <- dataR$chase
return[,4] <- dataR$exxon
return[,5] <- dataR$walmart
head(return) #look at first few observations

w <- as.matrix(dataW)

VaR <- matrix(0,L-N)
ex <- matrix(0,L-N)
actualloss <- matrix(0,L-N)
invest <- as.matrix(c(10,20,30,40,50))

for (j in (N+1):L){
	returnIn <- return[j-N:(j-1),] #in-sample
	n <- length(returnIn)/5
	variance <- cov(returnIn, returnIn)

	for (i in (j-N):j){ #500 previous observations
		U <- as.matrix(return[i,])
		variance = w + ba[2]*U%*%t(U) + ba[1]*variance #updates covariance matrix
	}
	
	VaR[j-N] <- 2.33*sqrt(t(invest)%*%variance%*%invest)
	actualloss[j-N] <- -return[j,]%*%invest

	if(VaR[j-N] >= actualloss[j-N]){
		ex[j-N] <- 0
	}else if(VaR[j-N] < actualloss[j-N]){
		ex[j-N] <- 1 #exception
	}
}

sum(ex) #total number of exceptions

#christoffersen
end <- L-N
T01 = 0
T11 = 0
T00 = 0
T10 = 0

for (i in 2:end){
	if ((ex[i-1]==0) && (ex[i]==1)){
		T01 = T01 + 1
	}else if ((ex[i-1]==1) && (ex[i]==1)){
		T11 = T11 + 1
	}else if ((ex[i-1]==0) && (ex[i]==0)){
		T00 = T00 + 1
	}else if ((ex[i-1]==1) && (ex[i]==0)){
		T10 = T10 + 1
	}
}
p = (T01+T11)/(T00+T01+T10+T11)
p01 = T01/(T00+T01)
p11 = T11/(T10+T11)
li <- -2*log((1-p)^(T00+T10)*p^(T01+T11)) + 2*log((1-p01)^(T00)*p01^T01*(1-p11)^T10*p11^T11)

#2001 - 2007 3 exceptions
#2007 - 2009 0 exceptions
#2009 - 2016 17 exceptions


