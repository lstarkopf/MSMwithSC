### print.MSMwithSC.R --- 
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
print.MSMwithSC <- function(x,...){
    tab = do.call("rbind",lapply(names(x)[-length(x)],function(method){
        data.table(Method = method,
                   Exposure = eval(x$call[["grid"]]),
                   Survival.treated = x[[method]][["surv.yes"]],
                   Survival.untreated = x[[method]][["surv.no"]])
    }))
    print(tab)
    invisible(tab)
}


######################################################################
### print.MSMwithSC.R ends here
