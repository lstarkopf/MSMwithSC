generate_data<- function(n,alpha,beta,gamma,unif=FALSE){
    #browser()
    require(lava)
    m <- lvm()
    distribution(m,~C1) <- binomial.lvm(p=0.7)
    distribution(m,~C2) <- gaussian.lvm(mean=65,sd=2)
    if (unif){ distribution(m,~Z) <- uniform.lvm(0,25)
    }else{
        distribution(m,~Z) <- Gamma.lvm(shape=2.8,rate=0.38)
    }
    distribution(m,~A) <- binomial.lvm()
    distribution(m,~A0) <- binomial.lvm(p=0)
    distribution(m,~A1) <- binomial.lvm(p=1)
    distribution(m,~set.Y0) <- binomial.lvm()
    distribution(m,~set.Y1) <- binomial.lvm()
    regression(m,A~C1+C2) <- alpha[2:3]
    intercept(m,"A") <- alpha[1]
    regression(m,Z~C1+C2) <- gamma[2:3]
    intercept(m,"Z") <- gamma[1]
    transform(m,m0~Z) <- function(x){
                     m0 <- log(x[["Z"]]+1)
                     m0
                 }
    regression(m,set.Y0~A0+m0+C1+C2) <- beta[2:5]
    regression(m,set.Y1~A1+m0+C1+C2) <- beta[2:5]
    intercept(m,"set.Y0") <- beta[1]
    intercept(m,"set.Y1") <- beta[1]
    transform(m,Y~A+set.Y0+set.Y1) <- function(x){
        Y <- x[["set.Y0"]]
        Y[x[["A"]]==1] <- x[["set.Y1"]][x[["A"]]==1]
        Y
    }
    d <- sim(m,n)
    return(d)
}

