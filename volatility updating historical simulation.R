#----------------------------------------------------------------------------------------------------------------------------
#EWMA
#----------------------------------------------------------------------------------------------------------------------------
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

#dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007value.csv", header=TRUE)
#dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009value.csv", header=TRUE)
dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016value.csv", header=TRUE)

#dataP <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007parameters.csv", header=TRUE)
#dataP <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009parameters.csv", header=TRUE)
dataP <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016parameters.csv", header=TRUE)

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

value <- array(0, c(L+1,5)) #determines the size of storage we need
value[,1] <- dataV$amazon #gives numeric
value[,2] <- dataV$att
value[,3] <- dataV$chase
value[,4] <- dataV$exxon
value[,5] <- dataV$walmart
head(value) #look at first few observations

invest <- as.matrix(c(10,20,30,40,50))
VaR <- matrix(0,L-N)
ex <- matrix(0,L-N)
lambda <- as.numeric(dataP[4,])

for (j in (N+1):L){
	returnIn <- return[(j-N):(j-1),] #in-sample
	vIn <- value[(j-N):j,]
	n <- length(returnIn)/5

	variance <- cov(returnIn)

	s <- matrix(0,n+1,5)
	v <- matrix(0,n,5)
	sd <- matrix(0,5)

	for (k in 1:5){
		s[1,k] <- return[1,k]^2
		sd[k] <- variance[k,k] #current estimate

		for (i in 1:n){ #store all scenarios
			#price of tomorrow in ith scenario
			v[i,k] = vIn[n,k]*((vIn[i+1,k]+(vIn[i+1,k]-vIn[i,k])*sd[k]/s[i,k])/vIn[i,k])
			#update volatility for next iteration
			s[i+1,k] = sqrt(s[i,k]^2*lambda[k] + return[i,k]^2*(1-lambda[k]))
		}
	}

	#calculate losses

	simreturn <- matrix(0,n)
	actualv <- t(matrix(vIn[n,],5,n))
	simreturn <- (v-actualv)/actualv #presumed return from simulation on the next day

	simloss <- -simreturn%*%invest #negative return is loss
	sortedloss <- simloss[order(-simloss)] #sort from greatest to least
	VaR[j-N] <- sortedloss[5]

	actualloss <- -return[j,]%*%invest

	if(sortedloss[5]>=actualloss){
		ex[j-N] <- 0
	}else if(sortedloss[5]<actualloss){
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
#2007 - 2009 2 exceptions
#2009 - 2016 15 exceptions

#----------------------------------------------------------------------------------------------------------------------------
#GARCH
#----------------------------------------------------------------------------------------------------------------------------
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

#dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007value.csv", header=TRUE)
dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009value.csv", header=TRUE)
#dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016value.csv", header=TRUE)

#dataP <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007parameters.csv", header=TRUE)
dataP <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009parameters.csv", header=TRUE)
#dataP <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016parameters.csv", header=TRUE)

#N = 500
N = 250

L <- length(dataR$amazon)
return <- array(0, c(L,5)) #determines the size of storage we need
return[,1] <- dataR$amazon #gives numeric
return[,2] <- dataR$att
return[,3] <- dataR$chase
return[,4] <- dataR$exxon
return[,5] <- dataR$walmart
head(return) #look at first few observations

value <- array(0, c(L+1,5)) #determines the size of storage we need
value[,1] <- dataV$amazon #gives numeric
value[,2] <- dataV$att
value[,3] <- dataV$chase
value[,4] <- dataV$exxon
value[,5] <- dataV$walmart
head(value) #look at first few observations

invest <- as.matrix(c(10,20,30,40,50))
VaR <- matrix(0,L-N)
ex <- matrix(0,L-N)
w <- as.numeric(dataP[1,])
b <- as.numeric(dataP[2,])
a <- as.numeric(dataP[3,])

for (j in (N+1):L){
	returnIn <- return[(j-N):(j-1),] #in-sample
	vIn <- value[(j-N):j,]
	n <- length(returnIn)/5

	variance <- cov(returnIn)

	s <- matrix(0,n+1,5)
	v <- matrix(0,n,5)
	sd <- matrix(0,5)

	for (k in 1:5){
		s[1,k] <- return[1,k]^2
		sd[k] <- variance[k,k] #current estimate

		for (i in 1:n){
			#price of tomorrow in ith scenario
			v[i,k] = vIn[n,k]*((vIn[i+1,k]+(vIn[i+1,k]-vIn[i,k])*sd[k]/s[i,k])/vIn[i,k])
			#update volatility for next iteration
			s[i+1,k] = sqrt(w[k] + s[i,k]^2*b[k] + return[i,k]^2*a[k])
		}
	}

	#calculate losses

	simreturn <- matrix(0,n)
	actualv <- t(matrix(vIn[n,],5,n))
	simreturn <- (v-actualv)/actualv #presumed return from simulation on the next day

	simloss <- -simreturn%*%invest #negative return is loss
	sortedloss <- simloss[order(-simloss)] #sort from greatest to least
	VaR[j-N] <- sortedloss[5]

	actualloss <- -return[j,]%*%invest

	if(sortedloss[5]>=actualloss){
		ex[j-N] <- 0
	}else if(sortedloss[5]<actualloss){
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
#2007 - 2009 2 exceptions
#2009 - 2016 15 exceptions

