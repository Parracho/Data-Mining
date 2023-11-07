library(MASS)
library(lattice)
library(psych) 
############# Ex17 ###############
"
rm(list=ls())
data(crabs)
sp_crabs=splom(log(crabs[,4:8]),col=as.numeric(crabs[,1]),pch=as.numeric(crabs[,2]),
main='circle/triangle is gender, black/red is species')
cl= kmeans( log(crabs[,4:8]), 2, nstart=1, iter.max=10)
sp_crabs1=splom(log(crabs[,4:8]),col=cl$cluster+2, 
main='blue/green is cluster finds big/small')
"
################## Ex da aula ###############
"
mu2=c(3.86,3.10,0.84,0.84,1.64,1.08,0.26,0.01)
sigma2=diag(c(8.41,12.06,0.12,0.22,1.49,1.77,0.35,2.73))

mu1=c(0,0,0,0,0,0,0,0)
sigma1=diag(c(1,1,1,1,1,1,1,1)) #diag(1,8,8)
n=99999999
for (p in 1:10){
data1=mvrnorm(100,mu1,sigma1)
data2=mvrnorm(100,mu2,sigma2)
data=rbind(data1,data2)
pairs(data,col=c(rep('blue',100),rep('red',100)))
cl=kmeans(data,2,nstart=1,iter.max=15)
tbl_cl=table(cl$cluster,col=c(rep(1,100),rep(2,100)))
i=tr(tbl_cl)
if (i<n)
{n=i
tbl_min=tbl_cl
}
}
"
################## Ex18 ########################
"
setwd('C:/Users/BParracho/Documents/DataMiningExercises')
dados=read.table('leukemia.data',sep=',')
s=c(rep(0,7129))
for (i in 1:7129){
s[i]=t.test(dados[dados[,7129]=='AML',i],dados[dados[,7129]=='ALL',i],var.equal=TRUE)$statistic} #matrix of the data that is aml and all
b=order(abs(s))
X=dados[,b[6730:7129]] #400
X=scale(X)
"
##################### Ex da aula ####################
n=100
mu=c(0,0)
sigma1=matrix(c(1,-0.7,-0.7,1),2,2,byrow=T)
sigma2=matrix(c(1,0.7,0.7,1),2,2,byrow=T)
data1=mvrnorm(n,mu,sigma1)
data2=mvrnorm(n,mu,sigma2)
amostra=rbind(data1,data2)
pairs(amostra,col=c(rep('red',100),rep('blue',100)))
library(mclust)
model=Mclust(amostra)
sum=summary(model)
plot(model,"classification")
