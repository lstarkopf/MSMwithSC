## .libPaths(c(.libPaths(),"~/R/library64/"))
library(prodlim)
library(lava)
library(parallel)
library(scam)
library(ggplot2)

mccores <- 1

## Working directory
setwd("~/Dropbox/PhD FINAL/MSIII/Simulations/")

## Load functions needed
f <- list.files(path="R",full.names=TRUE,pattern="\\.R$")
ff <- lapply(f,function(i)source(i))

#####################################################################
## Setting 1 - true relationship between Y(a) and Z monotone
####################################################################
## Set effect size for data generation
gamma <- c(0,0,0)
alpha <- c(1.5,0.1,-0.03)
beta <- c(2,1.3,-0.9,0.2,-0.04)

## Compute the truth
set.seed(17)
d <- generate_data(n=100000,alpha=alpha,beta=beta,gamma=gamma)
## Generate counterfactual data
d1 <- d[,c("C1","C2","Z","m0","A1","set.Y1")]
d1$a <- 1
d0 <- d[,c("C1","C2","Z","m0","A0","set.Y0")]
d0$a <- 0
colnames(d1) <- colnames(d0) <- c("C1","C2","Z","m0","A","Y","a")
dCF <- rbind(d1,d0)
dCF$a <- factor(dCF$a)
## Fit the MSM with correct function of Z
msm <-glm(formula=Y~a+m0,data=dCF,family=binomial("logit"))
## Predict true counterfactual probabilities
newdata <- data.frame(expand.grid(a=factor(c(0,1)),Z=c(0:20)))
newdata$m0 <- log(newdata$Z+1)
pp <-predict(msm,newdata=newdata,type="response")
pp.yes <- pp[newdata$a==1]
pp.no <- pp[newdata$a==0]
truth <- list(pp.yes=pp.yes,pp.no=pp.no)
## Save true counterfactual probabilities
save(truth,file=paste0("Results/truth-mono-",Sys.Date(),".RData"))

## Simulations (without bootstrap)
s <- 2000
n <- 500
set.seed(12)
seeds <- sample(1:10000000,s,replace=FALSE)
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,formula1="Y~A+C1+C2+s(Z,bs='mpd',k=25,m=2)",formula3="Y~A+C1+C2+m0",formula2="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",y.formula="Y~A+s(Z,bs='mpd',k=25,m=2)",mccores=mccores,z=0:20)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=500
save(output,file=paste0("Results/resultsn",n,"-mono-",Sys.Date(),".RData"))

n <- 5000
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,formula1="Y~A+C1+C2+s(Z,bs='mpd',k=25,m=2)",formula3="Y~A+C1+C2+m0",formula2="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",y.formula="Y~A+s(Z,bs='mpd',k=25,m=2)",mccores=mccores,z=0:20)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=5000
save(output,file=paste0("Results/resultsn",n,"-mono-",Sys.Date(),".RData"))

#######################################################################################
## Setting 2 - true relationship between Y(a) and Z monotone, Z had uniform distribution
#######################################################################################
## Set effect size for data generation
gamma <- c(0,0,0)
alpha <- c(1.5,0.1,-0.03)
beta <- c(2,1.3,-0.9,0.2,-0.04)

## Compute the truth
set.seed(17)
d <- generate_data(n=1000000,alpha=alpha,beta=beta,gamma=gamma,unif=TRUE)
## Generate counterfactual data
d1 <- d[,c("C1","C2","Z","m0","A1","set.Y1")]
d1$a <- 1
d0 <- d[,c("C1","C2","Z","m0","A0","set.Y0")]
d0$a <- 0
colnames(d1) <- colnames(d0) <- c("C1","C2","Z","m0","A","Y","a")
dCF <- rbind(d1,d0)
dCF$a <- factor(dCF$a)
## Fit the MSM with correct function of Z
msm <-glm(formula=Y~a+m0,data=dCF,family=binomial("logit"))
## Predict true counterfactual probabilities
newdata <- data.frame(expand.grid(a=factor(c(0,1)),Z=c(0:20)))
newdata$m0 <- log(newdata$Z+1)
pp <-predict(msm,newdata=newdata,type="response")
pp.yes <- pp[newdata$a==1]
pp.no <- pp[newdata$a==0]
truth <- list(pp.yes=pp.yes,pp.no=pp.no)
## Save true counterfactual probabilities
save(truth,file=paste0("Results/truth-mono-unif-",Sys.Date(),".RData"))

## Simulations (without bootstrap)
s <- 2000
n <- 500
set.seed(12)
seeds <- sample(1:10000000,s,replace=FALSE)
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,formula1="Y~A+C1+C2+s(Z,bs='mpd',k=25,m=2)",formula2="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",formula3="Y~A+C1+C2+m0",y.formula="Y~A+s(Z,bs='mpd',k=25,m=2)",mccores=mccores,z=0:20,unif=TRUE)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=500
save(output,file=paste0("Results/resultsn",n,"-mono-unif-",Sys.Date(),".RData"))


## Simulations (without bootstrap)
s <- 2000
n <- 5000
set.seed(12)
seeds <- sample(1:10000000,s,replace=FALSE)
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,formula1="Y~A+C1+C2+s(Z,bs='mpd',k=25,m=2)",formula2="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",formula3="Y~A+C1+C2+m0",y.formula="Y~A+s(Z,bs='mpd',k=25,m=2)",mccores=mccores,z=0:20,unif=TRUE)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=5000
save(output,file=paste0("Results/resultsn",n,"-mono-unif-",Sys.Date(),".RData"))
