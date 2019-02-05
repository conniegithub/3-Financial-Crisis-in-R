#----------------------------------------------------------------------------------------------------------------------------
#BASIC
#----------------------------------------------------------------------------------------------------------------------------
dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2001-2007returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2007-2009returns.csv", header=TRUE)
#dataR <- read.csv("C:/Users/yongy/Documents/My SAS Files/9.4/financial crisis project/2009-2016returns.csv", header=TRUE)

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

returnIn <- return[1:N,] #in-sample
n <- length(returnIn)/5
variance <- cov(returnIn, returnIn)
invest <- as.matrix(c(10,20,30,40,50))

VaR <- 2.33*sqrt(t(invest)%*%variance%*%invest)

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
#ex <- exi
ex <- exo
#end <- N
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

#2001 - 2007 1(out) 5(in) exception VaR = 6.089688
#2007 - 2009 2(out) 5(in) exceptions VaR = 8.885894
#2009 - 2016 21(out) 10(in) exceptions VaR = 3.173005

