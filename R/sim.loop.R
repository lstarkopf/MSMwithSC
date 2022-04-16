sim.loop.MSM <- function(s,n,alpha,beta,gamma,seeds,formula1,formula2,formula3,y.formula=NULL,mccores,z,unif=FALSE){
    sim.out<- mclapply(1:s,function(i){
                           set.seed(seeds[i])
                           suppressMessages(d<-generate_data(n=n,alpha=alpha,beta=beta,gamma=gamma,unif=unif))
                           message(i)
                           d$A <- factor(d$A)
                           set.seed(seeds[i])
                           est <- try(doMSMwithSC(y.formula=y.formula,formula1=formula1,formula2=formula2,formula3=formula3,data=d,z=z),silent=TRUE)
                           if ("try-error"%in%class(est)){est <- NULL}
                          est
    },mc.cores=mccores)
    sim.out
}
