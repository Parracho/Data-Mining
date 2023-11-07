library(Rfast)
library(ellipse)
library(MASS)
library(datasets)
################## Ex 2 ###################
'
d=c(1,4,9)
mu1=1
mu2=2
n=1000
sigma1=1
sigma2=2
rho=c(-0.5,0.5,0.9)
mu=c(mu1,mu2)
par(mfcol=c(1,3))
for (r in rho){
dataVec=c(sigma1,r*sqrt(sigma1*sigma2),r*sqrt(sigma1*sigma2),sigma2)
sigma=matrix(dataVec,ncol=2,nrow=2,byrow=TRUE)
data=rmvnorm(n,mu,sigma)
plot(data)
for (val in d){
lines(ellipse(x=sigma,centre=mu,level=pchisq(val,df=2)))
}
}
'


##################### Ex 3 ###############
'
mu1=c(0,0,0,0)
mu2=c(1,0.7,2.8,1)
mu=rbind(mu1,mu2)
n=100
sigma=matrix(c(0.1953,0.0922,0.0997,0.0331,0,0.1211,0.0472,0.0253,0,0,0.1255,0.0396,0,0,0,0.0251),ncol=4,nrow=4,byrow=TRUE)
sigma[lower.tri(sigma)] <- t(sigma)[lower.tri(sigma)]
par(mfcol=c(1,2))
for (i in c("mu1","mu2")){
data=rmvnorm(n,mu[i,],sigma)
plot(data)
}

##################### Ex 4 #################
par(c(1,1))
data1=rmvnorm(n,mu1,sigma)
data2=rmvnorm(n,mu2,sigma)


data=rbind(data1,data2)

pc=princomp(data,cor=FALSE) #analise das componentes principais
summary(pc)
biplot(pc,choice=c(1,2))
biplot(pc,choice=c(2,3))
'
################### Ex 5 ######################
'
setwd("C:/Users/BParracho/Documents/DataMiningExercises")
dados=read.table("imag-seg_Dataset.data")

pca = princomp(dados[,-1],scores=T,cor=T)
pca_sum=summary(pca)
dados1=data.frame(dados[,1],pca$scores[,1:7]) #variance proportion to comp 7 is about 85%
'
################### Ex 7 ######################
'
data(state)
data=state.x77
pca=princomp(data,cor=T)
biplot(pca,pc.biplot=T)
'
###################     ###########################








