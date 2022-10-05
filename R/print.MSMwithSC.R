### summary.MSMwithSC.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 10 2022 (09:09)
## Version:
## Last-Updated: May 10 2022 (12:22)
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
summary.MSMwithSC <- function(x,print=FALSE,...){
    tab = do.call("rbind",lapply(names(x)[1:(length(x)-2)],function(method){
        data.table(Method = method,
                   Covariate = eval(x$grid),
                   Survival.treated = x[[method]][["surv.yes"]],
                   Survival.untreated = x[[method]][["surv.no"]])
    }))
    if (print){print(tab)}
    tab
}


######################################################################
### summary.MSMwithSC.R ends here
