summary.simloop <- function(x,which="raw",times=c(0:20)){
    s <- length(x$out)
    if (which=="raw"){
        scam.mes.yes <- data.table(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="B_splines",Survival.treated])))
        scam2.mes.yes <- data.table(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="known_shape",Survival.treated])))
        gform.mes.yes <- data.table(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="Gform",Survival.treated])))
        s1 <- ncol(scam.mes.yes)
        s3 <- ncol(gform.mes.yes)
        s2 <- ncol(scam2.mes.yes)
        setnames(scam.mes.yes,paste0(c(1:s1)))
        setnames(scam2.mes.yes,paste0(c(1:s2)))
        setnames(gform.mes.yes,paste0(c(1:s3)))
        scam.mes.yes[,z:=times]
        scam2.mes.yes[,z:=times]
        gform.mes.yes[,z:=times]
        scam.mes.yes <- melt(scam.mes.yes,measure.vars=paste0(1:s1))
        scam.mes.yes[,Method:="MSM with constraints"]
        scam2.mes.yes <- melt(scam2.mes.yes,measure.vars=paste0(1:s2))
        scam2.mes.yes[,Method:="MSM with constraints (function known)"]
        gform.mes.yes <- melt(gform.mes.yes,measure.vars=paste0(1:s3))
        gform.mes.yes[,Method:="G-formula"]
        res.yes <- rbind(scam.mes.yes,scam2.mes.yes,gform.mes.yes)
        truth.yes <- data.table(z=times,truth=x$truth$pp.yes)
        res.yes <- truth.yes[res.yes,,on="z"]
        setorder(res.yes,z)
        res.yes[,mean.value:=median(value),by=.(z,Method)]
        res.yes[,min.value:=quantile(value,probs=0.25),by=.(z,Method)]
        res.yes[,max.value:=quantile(value,probs=0.75),by=.(z,Method)]
        res.yes[,CPR:=factor("Bystander CPR",levels=c("Bystander CPR","No bystander CPR"))]
        scam.mes.no <- data.table(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="B_splines",Survival.untreated])))
        scam2.mes.no <- data.table(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="known_shape",Survival.untreated])))
        gform.mes.no <- data.table(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="Gform",Survival.untreated])))
        setnames(scam.mes.no,paste0(c(1:s1)))
        setnames(scam2.mes.no,paste0(c(1:s2)))
        setnames(gform.mes.no,paste0(c(1:s3)))
        scam.mes.no[,z:=times]
        scam2.mes.no[,z:=times]
        gform.mes.no[,z:=times]
        scam.mes.no <- melt(scam.mes.no,measure.vars=paste0(1:s1))
        scam.mes.no[,Method:="MSM with constraints"]
        scam2.mes.no <- melt(scam2.mes.no,measure.vars=paste0(1:s2))
        scam2.mes.no[,Method:="MSM with constraints (function known)"]
        gform.mes.no <- melt(gform.mes.no,measure.vars=paste0(1:s3))
        gform.mes.no[,Method:="G-formula"]
        res.no <- rbind(scam.mes.no,scam2.mes.no,gform.mes.no)
        truth.no <- data.table(z=times,truth=x$truth$pp.no)
        res.no <- truth.no[res.no,,on="z"]
        setorder(res.no,z)
        res.no[,mean.value:=median(value),by=.(z,Method)]
        res.no[,min.value:=quantile(value,probs=0.25),by=.(z,Method)]
        res.no[,max.value:=quantile(value,probs=0.75),by=.(z,Method)]
        res.no[,CPR:=factor("No bystander CPR",levels=c("Bystander CPR","No bystander CPR"))]
        res <- rbind(res.yes,res.no)
        return(res)
    } else if (which=="bias"){
          bias.sc.yes <- data.table(rowMeans(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="B_splines",Survival.treated]-x$truth$pp.yes)),na.rm=TRUE))
          bias.sc2.yes <- data.table(rowMeans(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="known_shape",Survival.treated]-x$truth$pp.yes)),na.rm=TRUE))
          bias.gf.yes <- data.table(rowMeans(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="Gform",Survival.treated]-x$truth$pp.yes)),na.rm=TRUE))
          setnames(bias.sc.yes,"bias")
          setnames(bias.sc2.yes,"bias")
          setnames(bias.gf.yes,"bias")
          bias.sc.yes[,z:=factor(times,labels=times)]
          bias.sc2.yes[,z:=factor(times,labels=times)]
          bias.gf.yes[,z:=factor(times,labels=times)]
          bias.sc.yes[,Method:="MSM with constraints"]
          bias.sc2.yes[,Method:="MSM with constraints (function known)"]
          bias.gf.yes[,Method:="G-formula"]
          bias.yes <- rbind(bias.sc.yes,bias.sc2.yes,bias.gf.yes)
          bias.sc.no <- data.table(rowMeans(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="B_splines",Survival.untreated]-x$truth$pp.no)),na.rm=TRUE))
          bias.sc2.no <- data.table(rowMeans(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="known_shape",Survival.untreated]-x$truth$pp.no)),na.rm=TRUE))
          bias.gf.no <- data.table(rowMeans(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="Gform",Survival.untreated]-x$truth$pp.no)),na.rm=TRUE))
          setnames(bias.sc.no,"bias")
          setnames(bias.sc2.no,"bias")
          setnames(bias.gf.no,"bias")
          bias.sc.no[,z:=factor(times,labels=times)]
          bias.sc2.no[,z:=factor(times,labels=times)]
          bias.gf.no[,z:=factor(times,labels=times)]
          bias.sc.no[,Method:="MSM with constraints"]
          bias.sc2.no[,Method:="MSM with constraints (function known)"]
          bias.gf.no[,Method:="G-formula"]
          bias.no <- rbind(bias.sc.no,bias.sc2.no,bias.gf.no)
          return(list(bias.yes=bias.yes,bias.no=bias.no))
      } else if (which=="variance"){
            var.yes <- data.table(apply(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="B_splines",Survival.treated])),1,var))
            var2.yes <- data.table(apply(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="known_shape",Survival.treated])),1,var))
            var3.yes <- data.table(apply(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="Gform",Survival.treated])),1,var))
            setnames(var.yes,"var")
            setnames(var2.yes,"var")
            setnames(var3.yes,"var")
            var.yes[,z:=factor(times,labels=times)]
            var2.yes[,z:=factor(times,labels=times)]
            var3.yes[,z:=factor(times,labels=times)]
            var.yes[,Method:="MSM with constraints"]
            var2.yes[,Method:="MSM with constraints (function known)"]
            var3.yes[,Method:="G-formula"]
            var.yes <- rbind(var.yes,var2.yes,var3.yes)
            var.no <- data.table(apply(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="B_splines",Survival.untreated])),1,var))
            var2.no <- data.table(apply(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="known_shape",Survival.untreated])),1,var))
            var3.no <- data.table(apply(do.call("cbind",lapply(1:s,function(y)x$out[[y]][Method=="Gform",Survival.untreated])),1,var))
            setnames(var.no,"var")
            setnames(var2.no,"var")
            setnames(var3.no,"var")
            var.no[,z:=factor(times,labels=times)]
            var2.no[,z:=factor(times,labels=times)]
            var3.no[,z:=factor(times,labels=times)]
            var.no[,Method:="MSM with constraints"]
            var2.no[,Method:="MSM with constraints (function known)"]
            var3.no[,Method:="G-formula"]
            var.no <- rbind(var.no,var2.no,var3.no)
            return(list(var.yes=var.yes,var.no=var.no))
        }
}
