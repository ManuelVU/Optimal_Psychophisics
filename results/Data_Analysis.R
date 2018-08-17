# This is the Data analysis for the optimal experimental design in psychophisics
#### Load Data, Functions and Packages ####
load("results/CR_model.Rdata")
source("src/Functions.R")
library("R2Cuba")
library("mvtnorm")
#### ML estimation for prior distribution ####
deltas<-seq(0.05,0.7,0.04)
k<-70
slope<-c()
ld50<-c()
for(i in 1:length(HFA[1,1,1,])){
  print(i)
  result<-optim(c(1,1),fn=sigmoid,data=HFA[1,,1,i],x=deltas,k=70)
  slope[i]<-result$par[1]
  ld50[i]<-result$par[2]
}
#### Parameters: prior distributions for optimization ####
beta_p1<-c(0,10)
beta_p2<-c(20,30)
mu_p1<-c(0,1)
mu_p2<-c(0,0.5)
joint_mu<-c(mean(slope),mean(ld50))
joint_sigma<-as.matrix(var(cbind(slope,ld50)))

#### Number of optimal points by prior ####
# Uniform prior 1,1
design.points<-4
d1<-optim(par=c(seq(.01,.9,length.out = design.points),
                rep(1/design.points,design.points)),
          fn=phi,method="L-BFGS-B",
          n.p=design.points,
          beta=beta_p1,mu=mu_p1,
          lower=c(rep(0,design.points),rep(0,design.points)),
          upper=c(rep(1,design.points),rep(1,design.points)))
diseno<-c(d1$par[1:4],d1$par[5:8]/sum(d1$par[5:8]))
evaluaciones<-seq(0,1,0.001)
pruebas<-matrix(NA,nrow=length(evaluaciones),ncol=2)
for(i in 1:length(evaluaciones)){
  pruebas[i,1]<-evaluaciones[i]
  pruebas[i,2]<-cuhre(2,1,integrand=deta_dx,
                      n.p=design.points,eta_0=diseno,
                      evaluando=evaluaciones[i],
                      parbeta=beta_p1,
                      parmu=mu_p1,
                      rel.tol = 0.0001,
                      lower=c(beta_p1[1],mu_p1[1]),
                      upper=c(beta_p1[2],mu_p1[2]))$value
    
}
plot(pruebas[,1],pruebas[,2]-2,type="l")
points(diseno[1:4],rep(0,4))
