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
beta_p1<-c(2,8)
beta_p2<-c(15,18)
mu_p1<-c(0,1)
mu_p2<-c(0.1,0.6)
joint_mu<-c(mean(slope),mean(ld50))
joint_sigma<-as.matrix(var(cbind(slope,ld50)))

#### Number of optimal points by prior ####
#### Uniform prior 1,1 ####
design.points<-4
od1<-4
d1<-optim(par=c(seq(.01,.9,length.out = design.points),
                rep(1/design.points,design.points)),
          fn=phi,method="L-BFGS-B",
          n.p=design.points,
          beta=beta_p1,mu=mu_p1,
          lower=c(rep(0,design.points),rep(0,design.points)),
          upper=c(rep(1,design.points),rep(1,design.points)))
diseno1<-c(d1$par[1:design.points],
          d1$par[(design.points+1):(2*design.points)]/
            sum(d1$par[(design.points+1):(2*design.points)]))
evaluaciones<-seq(0,1,0.001)
pruebas1<-matrix(NA,nrow=length(evaluaciones),ncol=3)
for(i in 1:length(evaluaciones)){
  pruebas1[i,1]<-evaluaciones[i]
  integrada<-cuhre(2,1,integrand=deta_dx,
                   n.p=design.points,eta_0=diseno1,
                   evaluando=evaluaciones[i],
                   parbeta=beta_p1,
                   parmu=mu_p1,
                   rel.tol = 0.0001,
                   lower=c(beta_p1[1],mu_p1[1]),
                   upper=c(beta_p1[2],mu_p1[2]))
  pruebas1[i,2]<-integrada$value
  pruebas1[i,3]<-integrada$abs.error
    
}
#### Uniform prior 1,2 ####
design.points<-3
od2<-3
d2<-optim(par=c(seq(.01,.9,length.out = design.points),
                rep(1/design.points,design.points)),
          fn=phi,method="L-BFGS-B",
          n.p=design.points,
          beta=beta_p1,mu=mu_p2,
          lower=c(rep(0,design.points),rep(0,design.points)),
          upper=c(rep(1,design.points),rep(1,design.points)))
diseno2<-c(d2$par[1:design.points],
           d2$par[(design.points+1):(2*design.points)]/
             sum(d2$par[(design.points+1):(2*design.points)]))
evaluaciones<-seq(0,1,0.001)
pruebas2<-matrix(NA,nrow=length(evaluaciones),ncol=3)
for(i in 1:length(evaluaciones)){
  pruebas2[i,1]<-evaluaciones[i]
  integrada<-cuhre(2,1,integrand=deta_dx,
                   n.p=design.points,eta_0=diseno2,
                   evaluando=evaluaciones[i],
                   parbeta=beta_p1,
                   parmu=mu_p2,
                   rel.tol = 0.0001,
                   lower=c(beta_p1[1],mu_p2[1]),
                   upper=c(beta_p1[2],mu_p2[2]))
  pruebas2[i,2]<-integrada$value
  pruebas2[i,3]<-integrada$abs.error
  
}
#### Uniform prior 2,1 ####
design.points<-8
od3<-8
d3<-optim(par=c(seq(.01,.9,length.out = design.points),
                rep(1/design.points,design.points)),
          fn=phi,method="L-BFGS-B",
          n.p=design.points,
          beta=beta_p2,mu=mu_p1,
          lower=c(rep(0,design.points),rep(0,design.points)),
          upper=c(rep(1,design.points),rep(1,design.points)))
diseno3<-c(d3$par[1:design.points],
           d3$par[(design.points+1):(2*design.points)]/
           sum(d3$par[(design.points+1):(2*design.points)]))
evaluaciones<-seq(0,1,0.0001)
pruebas3<-matrix(NA,nrow=length(evaluaciones),ncol=3)
for(i in 1:length(evaluaciones)){
  pruebas3[i,1]<-evaluaciones[i]
  integrada<-cuhre(2,1,integrand=deta_dx,
                   n.p=design.points,eta_0=diseno3,
                   evaluando=evaluaciones[i],
                   parbeta=beta_p2,
                   parmu=mu_p1,
                   rel.tol = 0.0001,
                   lower=c(beta_p2[1],mu_p1[1]),
                   upper=c(beta_p2[2],mu_p1[2]))
  pruebas3[i,2]<-integrada$value
  pruebas3[i,3]<-integrada$abs.error
  
}
#### Uniform prior 2,2 ####
design.points<-4
od4<-4
d4<-optim(par=c(seq(.01,.9,length.out = design.points),
                rep(1/design.points,design.points)),
          fn=phi,method="L-BFGS-B",
          n.p=design.points,
          beta=beta_p2,mu=mu_p2,
          lower=c(rep(0,design.points),rep(0,design.points)),
          upper=c(rep(1,design.points),rep(1,design.points)))
diseno4<-c(d4$par[1:design.points],
           d4$par[(design.points+1):(2*design.points)]/
             sum(d4$par[(design.points+1):(2*design.points)]))
evaluaciones<-seq(0,1,0.001)
pruebas4<-matrix(NA,nrow=length(evaluaciones),ncol=3)
for(i in 1:length(evaluaciones)){
  pruebas4[i,1]<-evaluaciones[i]
  integrada<-cuhre(2,1,integrand=deta_dx,
                   n.p=design.points,eta_0=diseno4,
                   evaluando=evaluaciones[i],
                   parbeta=beta_p2,
                   parmu=mu_p2,
                   rel.tol = 0.0001,
                   lower=c(beta_p2[1],mu_p2[1]),
                   upper=c(beta_p2[2],mu_p2[2]))
  pruebas4[i,2]<-integrada$value
  pruebas4[i,3]<-integrada$abs.error
}
#### Joint Normal ####
design.points<-2
odj<-2
dj<-optim(par=c(seq(.01,.9,length.out = design.points),
                rep(1/design.points,design.points)),
          fn=phi,method="L-BFGS-B",
          n.p=design.points,joint=TRUE,
          mean=joint_mu,var=joint_sigma,
          lower=c(rep(0,design.points),rep(0,design.points)),
          upper=c(rep(1,design.points),rep(1,design.points)))
disenoj<-c(dj$par[1:design.points],
           dj$par[(design.points+1):(2*design.points)]/
             sum(dj$par[(design.points+1):(2*design.points)]))
evaluaciones<-seq(0,1,0.001)
pruebasj<-matrix(NA,nrow=length(evaluaciones),ncol=3)
for(i in 1:length(evaluaciones)){
  pruebasj[i,1]<-evaluaciones[i]
  integrada<-cuhre(2,1,integrand=deta_dx,
                   n.p=design.points,eta_0=disenoj,
                   evaluando=evaluaciones[i],
                   joint=TRUE,
                   center=joint_mu,
                   sigma=joint_sigma,
                   rel.tol = 0.0001,
                   lower=c(-0.5,0),
                   upper=c(30,0.6))
  pruebasj[i,2]<-integrada$value
  pruebasj[i,3]<-integrada$abs.error
}