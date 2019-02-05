#----------------------------------------------------------------------------------------------------------------------------
#BASIC
#----------------------------------------------------------------------------------------------------------------------------
#dataR <- read.csv("C:/Users/../financial crisis project/2001-2007returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/../financial crisis project/2007-2009returns.csv", header=TRUE)
dataR <- read.csv("C:/Users/../financial crisis project/2009-2016returns.csv", header=TRUE)

#dataV <- read.csv("C:/Users/../financial crisis project/2001-2007value.csv", header=TRUE)
#dataV <- read.csv("C:/Users/../financial crisis project/2007-2009value.csv", header=TRUE)
dataV <- read.csv("C:/Users/../financial crisis project/2009-2016value.csv", header=TRUE)

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

returnIn <- return[1:N,] #in-sample
vIn <- value[1:(N+1),]
n <- length(returnIn)/5

v <- matrix(0,n,5)

for (k in 1:5){
	for (i in 1:n){
		#price of tomorrow in ith scenario
		v[i,k] = vIn[n,k]*vIn[i+1,k]/vIn[i,k]
	}
}

#calculate losses

simreturn <- matrix(0,n)
actualv <- t(matrix(vIn[n,],5,n))
simreturn <- (v-actualv)/actualv #presumed return from simulation on the next day

invest <- as.matrix(c(10,20,30,40,50))
simloss <- -simreturn%*%invest #negative return is loss
sortedloss <- simloss[order(-simloss)] #sort from greatest to least
VaR <- sortedloss[5]

#out-sample
exo <- matrix(0,L-N)

for (j in (N+1):L){
	actualloss <- -return[j,]%*%invest

	if(VaR >= actualloss){
		exo[j-N] <- 0
	}else if(VaR < actualloss){
		exo[j-N] <- 1 #exception
	}
}

sum(exo) #total number of exceptions

#in-sample
exi <- matrix(0,N)

for (j in 1:N){
	actualloss <- -return[j,]%*%invest

	if(VaR >= actualloss){
		exi[j] <- 0
	}else if(VaR < actualloss){
		exi[j] <- 1 #exception
	}
}

sum(exi) #total number of exceptions

#christoffersen
ex <- exi
#ex <- exo
end <- N
#end <- L-N
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

#2001 - 2007 1(out) 6(in) exception VaR = 6.007764
#2007 - 2009 2(out) 5(in) exceptions VaR = 8.684822
#2009 - 2016 8(out) 5(in) exceptions VaR = 4.432549
#----------------------------------------------------------------------------------------------------------------------------
#TIME WEIGHTED
#----------------------------------------------------------------------------------------------------------------------------
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

#dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007value.csv", header=TRUE)
#dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009value.csv", header=TRUE)
dataV <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016value.csv", header=TRUE)

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

returnIn <- return[1:N,] #in-sample
vIn <- value[1:(N+1),]
n <- length(returnIn)/5

v <- matrix(0,n,5)

for (k in 1:5){
	for (i in 1:n){
		#price of tomorrow in ith scenario
		v[i,k] = vIn[n,k]*vIn[i+1,k]/vIn[i,k]
	}
}

#calculate losses

simreturn <- matrix(0,n)
actualv <- t(matrix(vIn[n,],5,n))
simreturn <- (v-actualv)/actualv #presumed return from simulation on the next day

invest <- as.matrix(c(10,20,30,40,50))
simloss <- -simreturn%*%invest #negative return is loss
lambda = 0.999
simlossw <- cbind(simloss, matrix(0,N))

#adding weights
for (i in 1:n){
	simlossw[i,2] = lambda^(n-i)*(1-lambda)/(1-lambda^n)
}
colnames(simlossw) <- c("loss","weight")
simlossw <- as.data.frame(simlossw)

sortedloss <- simlossw[order(-simlossw[,1], simlossw[,2]),] #sort from greatest to least

simlosscw <- cbind(sortedloss, matrix(0,N)) #cumulative weight
simlosscw[1,3] <- simlosscw[1,2]

for (i in 2:n){
	if(simlosscw[i-1,3] < 0.01){
		VaR <- simlosscw[i,1] #VaR will be the next one
	}
	simlosscw[i,3] = simlosscw[i,2] + simlosscw[i-1,3]
}

#out-sample
exo <- matrix(0,L-N)

for (j in (N+1):L){
	actualloss <- -return[j,]%*%invest

	if(VaR >= actualloss){
		exo[j-N] <- 0
	}else if(VaR < actualloss){
		exo[j-N] <- 1 #exception
	}
}

sum(exo) #total number of exceptions

#in-sample
exi <- matrix(0,N)

for (j in 1:N){
	actualloss <- -return[j,]%*%invest

	if(VaR >= actualloss){
		exi[j] <- 0
	}else if(VaR < actualloss){
		exi[j] <- 1 #exception
	}
}

sum(exi) #total number of exceptions

#christoffersen
ex <- exi
#ex <- exo
end <- N
#end <- L-N
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

#2001 - 2007 4(out) 12(in) exceptions VaR = 6.984871 lambda = 0.97
#2001 - 2007 7(out) 24(in) exceptions VaR = 6.984871 lambda = 0.96
#2007 - 2009 1(out) 3(in) exceptions VaR = 12.17184506 lambda = 0.99
#2009 - 2016 18(out) 7(in) exceptions VaR = 3.01519 lambda = 0.999




