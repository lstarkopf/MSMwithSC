library(ggplot2)
library(scales)

theme_4 <- function(...)
    theme_classic()+theme(plot.margin=unit(c(1,1,1,0),"lines"),strip.background=element_blank(),strip.text.x=element_text(size=12,family="Times"),legend.position="top",legend.title=element_blank())+theme(axis.title.x=element_text(family="Times", size=14,vjust=-1),plot.subtitle=element_text(family="Times", size=14,hjust=0.5),plot.title=element_text(family="Times", size=14,hjust=-0.15),axis.title.y=element_text(family="Times",size=14))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot.simloop <- function(out,which="raw",ylim1=c(0,0.75),ylim2=c(0,0.45),n=5000,labs=c("A)","B)"),subtitle){
    ## browser()
    res <- summary(out,times=0:20,which=which)
    if (which=="raw"){
        d <- res[CPR=="Bystander CPR",.SD[1],by=.(z,Method)]
        p1 <- ggplot(d) +geom_ribbon(aes(x=z,ymin=min.value,ymax=max.value,group=Method,fill=Method),position=position_dodge(width=0),alpha=0.2,show.legend=FALSE)+ scale_fill_grey(start=0.8,end=0.2)+
            geom_line(aes(x=z,y=truth,linetype="True value"),size=0.5,color="black",show.legend=FALSE)+
                geom_line(aes(x=z,y=mean.value,group=Method,linetype=Method),size=0.5,color="black",show.legend=TRUE)+
                    xlab("Ambulance response time (minutes)")+ylab("Counterfactual 30-day survival")+ggtitle(labs[1],subtitle=paste0(subtitle[1],", n=",n))+
                        scale_linetype_manual(values=c("longdash","twodash","dotted","solid"))+scale_y_continuous(labels=percent,limits=ylim1)+theme_4()+guides(linetype=guide_legend(ncol=1))+guides(color=FALSE,fill=FALSE)+theme(legend.position=c(0.7,0.8))
        d <- res[CPR=="No bystander CPR",.SD[1],by=.(z,Method)]
        p2 <- ggplot(d) +geom_ribbon(aes(x=z,ymin=min.value,ymax=max.value,group=Method,fill=Method),position=position_dodge(width=0),alpha=0.2,linetype=0,show.legend=FALSE)+scale_fill_grey(start=0.8,end=0.2)+
            geom_line(aes(x=z,y=truth,group=1,linetype="True value"),size=0.5,color="black",show.legend=FALSE)+
                geom_line(aes(x=z,y=mean.value,group=Method,linetype=Method),size=0.5,color="black",show.legend=TRUE)+
                    xlab("Ambulance response time (minutes)")+ylab("Counterfactual 30-day survival")+ggtitle(labs[2],subtitle=paste0(subtitle[2],", n=",n))+
                        scale_linetype_manual(values=c("longdash","twodash","dotted","solid"))+scale_y_continuous(limits=ylim2,labels=percent)+theme_4()+guides(linetype=guide_legend(ncol=1))+guides(color=FALSE,fill=FALSE)+theme(legend.position=c(0.7,0.8))
        return(multiplot(p1,p2,cols=2))
    }
    if (which=="bias"){
        d <- res$bias.yes
        p1 <- ggplot(d) +geom_line(aes(x=z,y=bias,group=Method,linetype=Method),show.legend=TRUE)+
            geom_hline(yintercept=0,color="gray90",size=0.5)+
                    xlab("Ambulance response time (minutes)")+ylab("Bias")+ggtitle(labs[1],subtitle=paste0(subtitle[1],", n=",n))+
                        scale_linetype_manual(values=c("longdash","twodash","dotted"))+scale_x_discrete(breaks=c(0,5,10,15,20))+scale_y_continuous(labels=percent,limits=ylim1)+theme_4()+guides(linetype=guide_legend(ncol=1))+guides(color=FALSE,fill=FALSE)+theme(legend.position=c(0.5,0.2))
        d <- res$bias.no
        p2 <- ggplot(d) +geom_line(aes(x=z,y=bias,group=Method,linetype=Method),show.legend=TRUE)+
            geom_hline(yintercept=0,color="gray90",size=0.5)+
                    xlab("Ambulance response time (minutes)")+ylab("Bias")+ggtitle(labs[2],subtitle=paste0(subtitle[2],", n=",n))+
                        scale_linetype_manual(values=c("longdash","twodash","dotted"))+scale_x_discrete(breaks=c(0,5,10,15,20))+scale_y_continuous(limits=ylim2,labels=percent)+theme_4()+guides(linetype=guide_legend(ncol=1))+guides(color=FALSE,fill=FALSE)+theme(legend.position=c(0.5,0.2))
        return(multiplot(p1,p2,cols=2))
    }
    if(which=="variance"){
                d <- res$var.yes
                p1 <- ggplot(d) +geom_line(aes(x=z,y=var,group=Method,linetype=Method),show.legend=TRUE)+
            #geom_hline(yintercept=0,color="gray90",size=0.5)+
                    xlab("Ambulance response time (minutes)")+ylab("Variance")+ggtitle(labs[1],subtitle=paste0(subtitle[1],", n=",n))+
                        scale_linetype_manual(values=c("longdash","twodash","dotted"))+scale_x_discrete(breaks=c(0,5,10,15,20))+scale_y_continuous(labels=percent,limits=ylim1)+theme_4()+guides(linetype=guide_legend(ncol=1))+guides(color=FALSE,fill=FALSE)+theme(legend.position=c(0.5,0.8))
                d <- res$var.no
                p2 <- ggplot(d) +geom_line(aes(x=z,y=var,group=Method,linetype=Method),show.legend=TRUE)+
            #geom_hline(yintercept=0,color="gray90",size=0.5)+
                    xlab("Ambulance response time (minutes)")+ylab("Variance")+ggtitle(labs[2],subtitle=paste0(subtitle[2],", n=",n))+
                        scale_linetype_manual(values=c("longdash","twodash","dotted"))+scale_x_discrete(breaks=c(0,5,10,15,20))+scale_y_continuous(limits=ylim2,labels=percent)+theme_4()+guides(linetype=guide_legend(ncol=1))+guides(color=FALSE,fill=FALSE)+theme(legend.position=c(0.5,0.8))
        return(multiplot(p1,p2,cols=2))
            }
}






