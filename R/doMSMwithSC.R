doMSMwithSC <- function(formula1,formula2,formula3,y.formula,data,z){
    fit1 <- scam(formula=formula(formula1),data=data,family=binomial(link="logit"))
    fit2 <- glm(formula=formula(formula2),data=data,family=binomial(link="logit"))
    fit3 <- glm(formula=formula(formula3),data=data,family=binomial(link="logit"))
    data.yes <- data.no <- data
    data.yes$A <- factor(1,levels=c(0,1))
    data.no$A <- factor(0,levels=c(0,1))
    ##g-formula
    pp2 <- sapply(c(0:20),function(z){data.yes$Z <- data.no$Z <- z;
                                     pp2.yes <- predict(fit2,newdata=data.yes,type="response")
                                     pp2.no <- predict(fit2,newdata=data.no,type="response")
                                     return(c(mean(pp2.yes),mean(pp2.no)))})
    ##scam
    data.yes$Z <- data.no$Z <- data$Z
    data.yes$Y <- predict(fit1,newdata=data.yes,type="response")
    data.no$Y <- predict(fit1,newdata=data.no,type="response")
    data.msm <- rbind(data.yes,data.no)
    msm.fit <- scam(formula=formula(y.formula),data=data.msm,family=binomial(link="logit"))
    data.yes <- data.table(A=factor(1,levels=c(0,1)),Z=z)
    data.no <- data.table(A=factor(0,levels=c(0,1)),Z=z)
    pp1.yes <- predict(msm.fit,newdata=data.yes,type="response")
    pp1.no <- predict(msm.fit,newdata=data.no,type="response")
    ##scam2
    data.yes <- data.no <- data
    data.yes$A <- factor(1,levels=c(0,1))
    data.no$A <- factor(0,levels=c(0,1))
    data.yes$Y <- predict(fit3,newdata=data.yes,type="response")
    data.no$Y <- predict(fit3,newdata=data.no,type="response")
    data.msm <- rbind(data.yes,data.no)
    msm.fit3 <- scam(formula=formula(y.formula),data=data.msm,family=binomial(link="logit"))
    data.yes <- data.table(A=factor(1,levels=c(0,1)),Z=z)
    data.no <- data.table(A=factor(0,levels=c(0,1)),Z=z)
    pp3.yes <- predict(msm.fit3,newdata=data.yes,type="response")
    pp3.no <- predict(msm.fit3,newdata=data.no,type="response")
    ## browser()
    return(list(SCAM=list(surv.yes=pp1.yes,surv.no=pp1.no),SCAM2=list(surv.yes=pp3.yes,surv.no=pp3.no),Gform=list(surv.yes=pp2[1,],surv.no=pp2[2,])))
}



