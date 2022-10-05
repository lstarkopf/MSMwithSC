doMSMwithSC <- function(outcome,
                        treatment,
                        smooth,
                        covariates,
                        Gformula,
                        data,
                        grid){
    theCall <- match.call()
    if (missing(grid)) grid = 1:20
    # data preparation
    data.yes <- data.no <- data
    data.yes$A <- factor(1,levels=c(0,1))
    data.no$A <- factor(0,levels=c(0,1))
    # ----------------------------------------------------------------------
    ## g-formula
    # ----------------------------------------------------------------------
    g <- glm(formula=formula(Gformula),
             data=data,
             family=binomial(link="logit"))
    pp2 <- sapply(grid,function(z){
        data.yes$Z <- data.no$Z <- z
        pp2.yes <- predict(g,newdata=data.yes,type="response")
        pp2.no <- predict(g,newdata=data.no,type="response")
        return(c(mean(pp2.yes),mean(pp2.no)))})
    # ----------------------------------------------------------------------
    # fit MSM in stacked data using scam::scam
    # ----------------------------------------------------------------------
    # B-splines
    form1 <- formula(paste0(outcome,"~",treatment,"+",smooth,"+",paste(covariates,collapse = "+")))
    # data generating formula
    form2 <- formula(paste0(outcome,"~",treatment,"+","m0","+",paste(covariates,collapse = "+")))
    # marginal structural model
    msm.formula = formula(paste0(outcome,"~",treatment,"+",smooth))
    out = lapply(c(form1,form2),function(ff){
        if(ff==form1){scfit <- scam(formula=ff,data=data,family=binomial(link="logit"))}
        if(ff==form2){scfit <- glm(formula=ff,data=data,family=binomial(link="logit"))}
        data.yes$Z <- data.no$Z <- data$Z
        data.yes$Y <- predict(scfit,newdata=data.yes,type="response")
        data.no$Y <- predict(scfit,newdata=data.no,type="response")
        data.msm <- rbind(data.yes,data.no)
        suppressWarnings(msm.fit <- scam(formula=formula(msm.formula),data=data.msm,family=binomial(link="logit")))
        data.yes2 <- data.table(A=factor(1,levels=c(0,1)),Z=grid)
        data.no2 <- data.table(A=factor(0,levels=c(0,1)),Z=grid)
        pp.yes <- predict(msm.fit,newdata=data.yes2,type="response")
        pp.no <- predict(msm.fit,newdata=data.no2,type="response")
        list(surv.yes=pp.yes,surv.no=pp.no)
    })
    names(out) = c("B_splines","known_shape")
    out = c(out,list(Gform=list(surv.yes=pp2[1,],surv.no=pp2[2,]),
                     grid = eval(grid),call=theCall))
    class(out) = "MSMwithSC"
    out
}



