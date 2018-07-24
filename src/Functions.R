# This file is for the functions used in the optimal experimental design project
ellipsoid<-function(x1,x2){
  R<-c()
  for(i in 1:length(x1)){
    o<-c(x1[i],x2[i])
    R[i]<-(t(o-mu)%*%solve(mlvar)%*%(o-mu))
  }
  return(R)
}