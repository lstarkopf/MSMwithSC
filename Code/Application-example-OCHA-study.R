## The OHCA data set (Rajan et al. 2016) used in the manuscript consists of 7623 observations and the
## following variables:
##
## CPR - Bystander CPR status (0: no bystander CPR, 1: bystander CPR).
## responset -  Ambulance response time in minutes (min. 0, mean 8.18, max. 30).
## surv30 - 30-day survival after cardiac arrest (0: dead, 1: alive).
## age - Age in years (18-65) divided into 10 groups.
## sex - Sex (0: female, 1: male).
## location - Location of arrest (1: private home, 2: public).
## witness -  Witnessed status (1: not witnessed, 2: witnessed by a bystander or EMS).
## year -  Year of arrest (min. 2005, max. 2011).
## ihd -  Ischemic heart disease (0: no, 1: yes).
## premi - Previous miocardial infarction (0: no, 1: yes).
## chf - Heart failure (0: no, 1: yes).
## copd -  Chronic obstructive pulmonary disease (0: no, 1: yes).

## For further details about the data, see Rajan et al. 2016..

## Implementation
## Step 1. Fit a suitable regression model for the outcome. We fit a logistic regression model by using the scam function from the scam package.


library(scam)
ymod <- scam(formula=surv30~cpr+age+sex+location+witness+ihd+chf+preami+copd+year+s(responset,by=cpr,bs="ps",k=25,m=2),data=ohca, family=binomial("logit"))

## Here the arguments formula, data and family of the function scam are similar to the arguments required by the glm function. The term s(responset,by=cpr,bs="ps",k=25,m=2)
## in the formula is similar to the smooth term in the formula of the generalized additive models implemented in the mgcv package. Specifically, we have specified that the smooth
## term is a function of responset and cpr is the variable by which the smooth term is multiplied (effect modifier). Setting bs="ps" results in using penalized B-spline basis with
## dimension \(\tt{k=25}\) and \(\tt{m}+1\) is the order of the basis functions.


## Step 2. Compute the predictions by applying the fitted model from the previous step once using the exposure value cpr=0 for everyone in the population and once
## using the exposure value cpr=1 for everyone in the population thereby setting the ambulance response time and the patient and OHCA characteristics to their observed values.

data.yes <- data.no <-ohca
data.yes$cpr <- 1
data.no$cpr <- 0
data.msm <- rbind(data.yes,data.no)
data.msm$survpred <- predict(ymod,newdata=data.msm,type="response")

## Here we first expand the observed data such that for each subject we have two replications. For the first replication the bystander CPR status is set to equal 1 and for the
## second replication the bystander CPR status is set to equal 0. The ambulance response time and other patient and OHCA characteristics are kept at their observed values for
## both replications. The predictions are obtained from the new expanded data set data.msm thereafter.


## Step 3. Fit a penalized logistic regression model for the predicted values from the previous step according to the marginal structural model \eqref{eq:MSMp} by using the
## scam function from the scam package and the expanded data set data.msm.

msm.fit <- scam(formula=survpred~cpr+s(responstid,by=cpr,bs="mpd",k=25,m=2),data=data.msm,family=binomial("logit"))

## The argument bs="mpd" in the formula of scam results in using penalized monotone decreasing B-splines for the smooth term. The smoothing parameter selection is done simultaneously with
## the estimation of the marginal structural model parameters by the scam function.  By default the prediction error criterion used for the smoothing parameter selection is the Un-Biased
## Risk Estimator for logistic regression models.


## Step 4. Generate a prediction of the counterfactual 30-day probability for each combination of bystander CPR status and ambulance response time of interest by evaluating the
## fitted marginal structural model from the precious step.

newdata <- data.frame(expand.grid(cpr=c(0,1),responst=c(0:16,20,30)))
p <- predict(msm.fit,newdata=newdata,type="response")


## Step 5.  The 95% confidence intervals can be obtained by a bootstrap procedure. The bootstrap procedure is done by the function boot.msm.cpr given below. The function boot.msm.cpr takes
## a formula for the outcome regression, a formula for the marginal structural model, a data set, a seed, number of bootstrap samples and a number of cores for parallel computing as arguments.
## The function boot.msm.cpr returns a list with the predicted counterfactual probabilities based on the marginal structural model together with 95% confidence intervals computed from the 2.5%
## and 97.5% quantiles of the resulting N bootstrap estimates. We apply the function boot.msm.cpr to N=2000 bootstrap samples of the observed data.

## A function that does step 1. - 4.
msm.cpr <- function(formula,msm.formula,data){
    ymod <- scam(formula=formula,data=data, family=binomial("logit"))
    data.yes <- data.no <-data
    data.yes$cpr <- 1
    data.no$cpr <- 0
    data.msm <- rbind(data.yes,data.no)
    data.msm$survpred <- predict(ymod,newdata=data.msm,type="response")
    msm.formula <- update(formula(msm.formula),survpred~.)
    msm.fit <- scam(formula=msm.formula,data=data.msm,family=binomial("logit"))
    newdata <- data.frame(expand.grid(cpr=c(0,1),responst=c(0:16,20,30)))
    pp <- predict(msm.fit,newdata=newdata,type="response")
    pp.yes <- p[newdata$cpr==1]
    pp.no <- p[newdata$cpr==0]
    return(list(surv.yes=pp.yes,surv.no=pp.no))
}


boot.msm.cpr <- function(formula,msm.formula,data,seed,B=20,cores=20){
    result <- msm.cpr(formula=formula,msm.formula=msm.formula,data=data)
    set.seed(seed)
    bootseeds <- sample(1:1000000,size=B,replace=FALSE)
    require(doParallel)
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    boot <- foreach(b=1:B,.packages="scam",.export="msm.cpr") %dopar% {
        set.seed(bootseeds[b])
        data.b <- data[sample(1:nrow(data),replace=TRUE),]
        result.b <- msm.cpr(formula=formula,msm.formula=msm.formula,data=data.b)
    }
    stopCluster(cl)
    gc()
    ## Confindence limits for surv.yes
    surv.yes <- do.call("cbind",lapply(boot,function(b)b$surv.yes))
    surv.yes.ci <- t(apply(surv.yes,1,quantile,c(0.025,0.975)))
    colnames(surv.yes.ci) <- c("surv.yes.lower","surv.yes.upper")
    ## Confindence limits for surv.no
    surv.no <- do.call("cbind",lapply(boot,function(b)b$surv.no))
    surv.no.ci <- t(apply(surv.no,1,quantile,c(0.025,0.975)))
    colnames(surv.no.ci) <- c("surv.no.lower","surv.no.upper")
    return(list(surv.yes=cbind(result$surv.yes,surv.yes.ci),surv.no=cbind(result$surv.no,surv.no.ci)))
}

## Use more cores for parallel computing (e.g., 50)
bootEst <- boot.msm.cpr(formula="surv30~cpr+age+sex+location+witness+ihd+chf+preami+copd+year+s(responset,by=cpr,bs="ps",k=25,m=2)", msm.formula="survpred~cpr+s(responset,by=cpr,bs="mpd",k=25,m=2)",seed=79,B=2000,cores=1)


