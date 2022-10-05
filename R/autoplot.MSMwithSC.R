### autoplot.MSMwithSC.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 10 2022 (09:21)
## Version:
## Last-Updated: May 10 2022 (12:29)
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
autoplot.MSMwithSC <- function(object,estimator,...){
    ## initialize and check
    estimator <- match.arg(estimator, choices =  names(object)[1:(length(object)-2)], several.ok = TRUE)
    name.treatment <- eval(object$call[["treatment"]])
    dd = object[[estimator]]
    nn = length(dd$surv.yes)
    dd = data.frame(
        Covariate = rep(eval(object$call$grid),2),
        Treatment = rep(c("Treated","Untreated"),rep(nn,2)),
                    SurvivalProbability = c(dd$surv.yes,dd$surv.no))
    g = ggplot(dd,aes(Covariate,SurvivalProbability,color = Treatment,group = Treatment))+geom_line()
    g+ylim(c(0,1))
}


######################################################################
### autoplot.MSMwithSC.R ends here
