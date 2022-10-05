sim.loop.MSM <- function(s,n,alpha,beta,gamma,seeds,outcome,treatment,smooth,covariates,Gformula,grid,mccores,unif=FALSE){
    sim.out<- mclapply(1:s,function(i){
                           set.seed(seeds[i])
                           suppressMessages(d<-generate_data(n=n,alpha=alpha,beta=beta,gamma=gamma,unif=unif))
                           message(i)
                           set.seed(seeds[i])
                           est <- try(doMSMwithSC(outcome=outcome,treatment=treatment,covariates=covariates,smooth=smooth,Gformula=Gformula, grid=grid,data=d),silent=TRUE)
                           if ("try-error"%in%class(est)){est <- NULL}
                           return(summary(est))
                       },mc.cores=mccores)
    return(sim.out)
}
