## Working directory
setwd("~/Dropbox/PhD FINAL/MSIII/Simulations/")
library(ggplot2)
library(scales)
library(grid)

## Load functions needed
f <- list.files(path="R",full.names=TRUE,pattern="\\.R$")
ff <- lapply(f,function(i)source(i))

###################################################################################################################
## Setting 1 - true relationship monotone, n=500
###################################################################################################################
output <- get(load("Results/resultsn500-mono-2022-10-02.RData"))

## Summary plot - distribution of the estimated probabilities, mean curve min and max curves over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-500",Sys.Date(),".pdf"),width=9,height=4)
plot(output,n=500,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Bias over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-500-bias-",Sys.Date(),".pdf"),width=9,height=4)
plot(output,which="bias",ylim1=c(-0.2,0.1),ylim2=c(-0.2,0.1),n=500,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Variance over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-500-variance-",Sys.Date(),".pdf"),width=9,height=4)
plot(output,which="variance",ylim1=c(0,0.02),ylim2=c(0,0.02),n=500,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()


###################################################################################################################
## Setting 1 - true relationship monotone, n=5000
###################################################################################################################

output2 <- get(load("Results/resultsn5000-mono-2022-10-02.RData"))
## Summary plot - distribution of the estimated probabilities, mean curve min and max curves over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-5000",Sys.Date(),".pdf"),width=9,height=4)
plot(output2,n=5000,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Bias over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-5000-bias-",Sys.Date(),".pdf"),width=9,height=4)
plot(output2,which="bias",ylim1=c(-0.2,0.1),ylim2=c(-0.2,0.1),n=5000,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Variance over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-5000-variance-",Sys.Date(),".pdf"),width=9,height=4)
plot(output2,which="variance",ylim1=c(0,0.006),ylim2=c(0,0.006),n=5000,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()


###################################################################################################################
## Setting 2 - true relationship monotone, Z uniform, n=500
###################################################################################################################
output <- get(load("Results/resultsn500-mono-unif-2022-10-02.RData"))

## Summary plot - distribution of the estimated probabilities, mean curve min and max curves over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-unif-500",Sys.Date(),".pdf"),width=9,height=4)
plot(output,n=500,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Bias over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-unif-500-bias-",Sys.Date(),".pdf"),width=9,height=4)
plot(output,which="bias",ylim1=c(-0.2,0.1),ylim2=c(-0.2,0.1),n=500,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Variance over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-unif-500-variance-",Sys.Date(),".pdf"),width=9,height=4)
plot(output,which="variance",ylim1=c(0,0.02),ylim2=c(0,0.02),n=500,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

###################################################################################################################







###################################################################################################################
## Setting 2 - true relationship monotone, Z uniform, n=5000
###################################################################################################################
output <- get(load("Results/resultsn5000-mono-unif-2022-10-03.RData"))

## Summary plot - distribution of the estimated probabilities, mean curve min and max curves over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-unif-5000",Sys.Date(),".pdf"),width=9,height=4)
plot(output,n=5000,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Bias over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-unif-5000-bias-",Sys.Date(),".pdf"),width=9,height=4)
plot(output,which="bias",ylim1=c(-0.2,0.1),ylim2=c(-0.2,0.1),n=5000,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

## Variance over 2000 simulations
pdf(file=paste0("Figures/simulation-mono-unif-5000-variance-",Sys.Date(),".pdf"),width=9,height=4)
plot(output,which="variance",ylim1=c(0,0.003),ylim2=c(0,0.003),n=5000,subtitle=c("Bystander CPR","No bystander CPR"))
dev.off()

###################################################################################################################
