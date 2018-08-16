# This file is for the functions used in the optimal experimental design project
ellipsoid<-function(x1,x2){
  R<-c()
  for(i in 1:length(x1)){
    o<-c(x1[i],x2[i])
    R[i]<-(t(o-mu)%*%solve(mlvar)%*%(o-mu))
  }
  return(R)
}
# Equal weights desgins
asint_aprox_phi<-function(dt,sim){
  information<-c()
  theta<-matrix(c(rep(1,length(dt)),c(dt)),ncol=2)
  print(theta)
  for(i in 1:sim){
    x<-rmvnorm(1,mu,mlvar)
    x<-t(x)
    information[i]<-log(det(matrix(c((theta[,1])%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2)),
                                     (theta[,2])%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2)),
                                     (theta[,2])%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2)),
                                     ((theta[,2]^2))%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2))),
                                   ncol=2,nrow=2,byrow=T)))
  }
  if(is.nan(mean(information))){
    information=-999
  }
  return(-mean(information))
}
aprox_phi_int<-function(x,dt){
  theta<-matrix(c(rep(1,length(dt)),c(dt)),ncol=2)
  information<-log(det(matrix(c((theta[,1])%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2)),
                                (theta[,2])%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2)),
                                (theta[,2])%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2)),
                                ((theta[,2]^2))%*%(exp(theta%*%x)/((1+exp(theta%*%x))^2))),
                              ncol=2,nrow=2,byrow=T)))*dmvnorm(t(x),mu,mlvar)
  if(is.nan(information)){
    information=-Inf
  }
  return(information)
}
# Unequal wights design
fisherI<-function(theta,eta,n.p,joint=FALSE,parbeta,parmu,center,sigma){
  x<-c(eta[1:n.p])
  n<-eta[(n.p+1):length(eta)]
  n<-n/sum(n)
  p<-1/(1+exp(-theta[1]*(x-theta[2])))
  w<-p*(1-p)
  t<-sum(n*w)
  xbar<-(1/t)*sum(n*w*x)
  s<-sum(n*w*(x-xbar)^2)
  if(joint==TRUE){
    mlmean<-center
    varcov<-sigma
    information<-log(det(matrix(c(theta[1]^2*t,
                                  -theta[1]*t*(xbar-theta[2]),
                                  -theta[1]*t*(xbar-theta[2]),
                                  s+t*(xbar-theta[2])^2),
                                ncol=2,nrow=2,byrow=T)))*
      dmvnorm(theta,mean=center,sigma=sigma)
    return(information)
  }
  else{
    information<-log(det(matrix(c(theta[1]^2*t,
                                  -theta[1]*t*(xbar-theta[2]),
                                  -theta[1]*t*(xbar-theta[2]),
                                  s+t*(xbar-theta[2])^2),
                                ncol=2,nrow=2,byrow=T)))*
      dunif(theta[1],parbeta[1],parbeta[2])*dunif(theta[2],parmu[1],parmu[2])
    return(information)
  }
}
phi<-function(eta,n.p,joint=FALSE,beta,mu,mean,var){
  if(joint==TRUE){
    integrando<-cuhre(2,1,integrand=fisherI,
                      lower=c(-0.5,0),upper=c(30,0.6),rel.tol = 0.01,
                      eta=eta,n.p=design.points,
                      joint=joint,center=mean,sigma=var)$value  
    if(is.nan(integrando)){
      integrando<- -999
    }
    return(-integrando)
  }
  else{
    integrando<-cuhre(2,1,integrand=fisherI,rel.tol = 0.01,
                      lower=c(beta[1],mu[1]),upper=c(beta[2],mu[2]),
                      eta=eta,n.p=design.points,
                      parbeta=c(beta),parmu=c(mu))$value
    if(is.nan(integrando)){
      integrando<- -999
    }
    return(-integrando)
  }
}