## .libPaths(c(.libPaths(),"~/R/library64/"))
library(prodlim)
library(lava)
library(parallel)
library(scam)
library(data.table)

mccores <- 1

## Working directory
setwd("~/Dropbox/PhD FINAL/MSIII/Simulations/")

## Load functions needed
f <- list.files(path="R",full.names=TRUE,pattern="\\.R$")
ff <- lapply(f,function(i)source(i))

#####################################################################
## Setting 1 - true relationship between Y(a) and Z monotone
####################################################################
## Compute the truth
alpha <-c(1.5,0.1,-0.03)
beta <- c(2,1.3,-0.9,0.2,-0.04)
gamma=c(0,0,0)

truth <- generateTruth(alpha=alpha,
              beta=beta,
              gamma=gamma,
              seed=17,
              sample.size=100000,
              unif=FALSE)
## Save true counterfactual probabilities
save(truth,file=paste0("Results/truth-mono-",Sys.Date(),".RData"))

## Simulations (without bootstrap)
s <- 2000
n <- 500
set.seed(12)
seeds <- sample(1:10000000,s,replace=FALSE)
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,outcome="Y",treatment="A",covariates=c("C1","C2"),smooth="s(Z,bs='mpd',k=25,m=2)",Gformula="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",mccores=mccores,grid=0:20)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=500
save(output,file=paste0("Results/resultsn",n,"-mono-",Sys.Date(),".RData"))

n <- 5000
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,outcome="Y",treatment="A",covariates=c("C1","C2"),smooth="s(Z,bs='mpd',k=25,m=2)",Gformula="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",mccores=mccores,grid=0:20)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=5000
save(output,file=paste0("Results/resultsn",n,"-mono-",Sys.Date(),".RData"))

#######################################################################################
## Setting 2 - true relationship between Y(a) and Z monotone, Z had uniform distribution
#######################################################################################
## Compute the truth
alpha <- c(1.5,0.1,-0.03)
beta <- c(2,1.3,-0.9,0.2,-0.04)
gamma <- c(0,0,0)
truth <- generateTruth(alpha=alpha,
              beta=beta,
              gamma=gamma,
              seed=17,
              sample.size=100000,
              unif=TRUE)
## Save true counterfactual probabilities
save(truth,file=paste0("Results/truth-mono-unif-",Sys.Date(),".RData"))

## Simulations (without bootstrap)
s <- 2000
n <- 500
set.seed(12)
seeds <- sample(1:10000000,s,replace=FALSE)
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,outcome="Y",treatment="A",covariates=c("C1","C2"),smooth="s(Z,bs='mpd',k=25,m=2)",Gformula="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",mccores=mccores,grid=0:20,unif=TRUE)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=500
save(output,file=paste0("Results/resultsn",n,"-mono-unif-",Sys.Date(),".RData"))


## Simulations (without bootstrap)
s <- 2000
n <- 5000
set.seed(12)
seeds <- sample(1:10000000,s,replace=FALSE)
out <- sim.loop.MSM(n=n,s=s,seeds=seeds,alpha=alpha,beta=beta,gamma=gamma,outcome="Y",treatment="A",covariates=c("C1","C2"),smooth="s(Z,bs='mpd',k=25,m=2)",Gformula="Y~A+C1+C2+rms:::rcs(Z,c(5,10,15,20))",mccores=mccores,grid=0:20,unif=TRUE)
output <- list(out=out,truth=truth)
class(output) <- "simloop"
## Save results for n=5000
save(output,file=paste0("Results/resultsn",n,"-mono-unif-",Sys.Date(),".RData"))
