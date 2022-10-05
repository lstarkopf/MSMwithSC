### generateTruth.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 10 2022 (12:59)
## Version:
## Last-Updated: August 27 2022 (15:16)
##           By: Liis Starkopf
##     Update #: 3
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log: added parameter unif
#----------------------------------------------------------------------
##
### Code:

## Set effect size for data generation
generateTruth <- function(alpha,beta,gamma,seed,sample.size,unif=FALSE){
    ## Generate data (with fixed seed) for setting 1
    set.seed(seed)
    d <- generate_data(n=sample.size,alpha=alpha,beta=beta,gamma=gamma,unif=unif)
    ## Counterfactual data under intervention where everybody gets bystander CPR
    d1 <- d[,c("C1","C2","Z","m0","A1","set.Y1")]
    ## Counterfactual data under intervention where nobody gets bystander CPR
    d0 <- d[,c("C1","C2","Z","m0","A0","set.Y0")]
    colnames(d1) <- colnames(d0) <- c("C1","C2","Z","m0","A","Y")
    ## Combined counterfactual data
    dCF <- rbind(d1,d0)
    dCF$A <- factor(dCF$A)
    ## Fit the MSM to the counterfactual data with correct function of Z
    msm <-glm(formula=Y~A+m0,data=dCF,family=binomial("logit"))
    ## Predict true counterfactual probabilities
    newdata <- data.frame(expand.grid(A=factor(c(0,1)),Z=c(0:20)))
    ## True function of Z
    newdata$m0 <- log(newdata$Z+1)
    ## Predicted counterfactual 30-day survival probabilities
    pp <-predict(msm,newdata=newdata,type="response")
    pp.yes <- pp[newdata$A==1]
    pp.no <- pp[newdata$A==0]
    truth <- list(pp.yes=pp.yes,pp.no=pp.no)
    truth
}

######################################################################
### generateTruth.R ends here
