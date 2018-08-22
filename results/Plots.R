# Plots for paper
#### Prior and Prior Predictive Distributions UNIFORM ####
pdf(file="results/Prior_and_Predictive.pdf",width=8,height = 4/7*11)
par(oma=c(2,2,0.1,0.1))
par(fig=c(0,0.52,0.47,1),
    mai=c(0.5,0.5,0.1,0.1))
plot(0,0,type="n",ylim=c(0,1),xlim=c(0,1),ann=F,axes=F)
b<-runif(300,beta_p1[1],beta_p1[2])
m<-runif(300,mu_p1[1],mu_p1[2])
col.curve<-"#8e9b9733"
for(i in 1:length(b)){
  curve(1/(1+exp(-b[i]*(x-m[i]))),from=0,to=1,add=T,col=col.curve)
}
box()
axis(1,labels=F)
axis(2,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"),las=2)

par(fig=c(0.28,0.51,0.52,0.75),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,axes=F,xlim=mu_p1,ylim=c(beta_p1[1],beta_p2[2]))
rect(mu_p1[1],beta_p1[1],mu_p1[2],beta_p2[2],border="black",col="white")
points(m,b,cex=0.5,pch=16,col="#07575b")
axis(1,at=mu_p1,labels=mu_p1,line=-0.15,padj = -1.1,cex.axis=0.8)
axis(2,at=c(beta_p1[1],beta_p2[2]),labels=c(beta_p1[1],beta_p2[2]),line=-0.2,las=2,cex.axis=0.8,hadj=0.5)

par(fig=c(0.47,1,0.47,1),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(0,0,type="n",ylim=c(0,1),xlim=c(0,1),ann=F,axes=F)
b<-runif(300,beta_p1[1],beta_p1[2])
m<-runif(300,mu_p2[1],mu_p2[2])
for(i in 1:length(b)){
  curve(1/(1+exp(-b[i]*(x-m[i]))),from=0,to=1,add=T,col=col.curve)
}
box()
axis(1,labels=F)
axis(2,labels=F)

par(fig=c(0.76,0.99,0.52,0.75),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,axes=F,xlim=mu_p1,ylim=c(beta_p1[1],beta_p2[2]))
rect(mu_p1[1],beta_p1[1],mu_p1[2],beta_p2[2],border="black",col="white")
points(m,b,cex=0.5,pch=16,col="#07575b")
axis(1,at=mu_p2,labels=mu_p2,line=-0.15,padj = -1.1,cex.axis=0.8)
axis(2,at=c(beta_p1[1],beta_p2[2]),labels=c(beta_p1[1],beta_p2[2]),line=-0.2,las=2,cex.axis=0.8,hadj=0.5)

par(fig=c(0,0.52,0,0.52),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(0,0,type="n",ylim=c(0,1),xlim=c(0,1),ann=F,axes=F)
b<-runif(200,beta_p2[1],beta_p2[2])
m<-runif(200,mu_p1[1],mu_p1[2])
for(i in 1:length(b)){
  curve(1/(1+exp(-b[i]*(x-m[i]))),from=0,to=1,add=T,col=col.curve)
}
box()
axis(1,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"))
axis(2,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"),las=2)

par(fig=c(0.28,0.51,0.05,0.28),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,axes=F,xlim=mu_p1,ylim=c(beta_p1[1],beta_p2[2]))
rect(mu_p1[1],beta_p1[1],mu_p1[2],beta_p2[2],border="black",col="white")
points(m,b,cex=0.5,pch=16,col="#07575b")
axis(1,at=mu_p1,labels=mu_p1,line=-0.15,padj = -1.1,cex.axis=0.8)
axis(2,at=c(beta_p1[1],beta_p2[2]),labels=c(beta_p1[1],beta_p2[2]),line=-0.2,las=2,cex.axis=0.8,hadj=0.5)

par(fig=c(0.47,1,0,0.52),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(0,0,type="n",ylim=c(0,1),xlim=c(0,1),ann=F,axes=F)
b<-runif(300,beta_p2[1],beta_p2[2])
m<-runif(300,mu_p2[1],mu_p2[2])
col.curve<-"#8e9b9733"
for(i in 1:length(b)){
  curve(1/(1+exp(-b[i]*(x-m[i]))),from=0,to=1,add=T,col=col.curve)
}
box()
axis(1,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"))
axis(2,labels=F)
par(fig=c(0.76,0.99,0.05,0.28),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,axes=F,xlim=mu_p1,ylim=c(beta_p1[1],beta_p2[2]))
rect(mu_p1[1],beta_p1[1],mu_p1[2],beta_p2[2],border="black",col="white")
points(m,b,cex=0.5,pch=16,col="#07575b")
axis(1,at=mu_p1,labels=mu_p1,line=-0.15,padj = -1.1,cex.axis=0.8)
axis(2,at=c(beta_p1[1],beta_p2[2]),labels=c(beta_p1[1],beta_p2[2]),line=-0.2,las=2,cex.axis=0.8,hadj=0.5)

mtext(expression(paste(beta,"~ U (2, 8)")),side=2,outer=T,at=0.78,cex=1.6)
mtext(expression(paste(beta,"~ U (15, 18)")),side=2,outer=T,at=0.28,cex=1.6)
mtext(expression(paste(mu,"~ U (0.1, 0.6)")),side=1,outer=T,at=0.78,cex=1.6,
      line=0.5)
mtext(expression(paste(mu,"~ U (0, 1)")),side=1,outer=T,at=0.28,cex=1.6,
      line=0.5)
dev.off()

#### Support for optimal design and directiona dx ####
pdf(file="results/Support_detadx.pdf",width=8,height = 4/7*11)
par(oma=c(2,2,0.1,0.1))
par(fig=c(0,0.52,0.46,1),
    mai=c(0.5,0.5,0.1,0.1))
plot(d1$par[1:od1],d1$par[(od1+1):(od1*2)]/sum(d1$par[(od1+1):(od1*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0),labels=F,at=seq(0,1,0.2))
axis(2,line=-0.45,at=seq(0,0.4,0.1),labels=c("0",seq(0.1,0.4,0.1)),
     las=2)

par(fig=c(0.47,1,0.46,1),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(d2$par[1:od2],d2$par[(od2+1):(od2*2)]/sum(d2$par[(od2+1):(od2*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0),labels=F,at=seq(0,1,0.2))
axis(2,line=-0.45,at=seq(0,0.4,0.1),labels=F)

par(fig=c(0,0.52,0,0.53),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(d3$par[1:od3],d3$par[(od3+1):(od3*2)]/sum(d3$par[(od3+1):(od3*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0))
axis(2,line=-0.45,las=2,at=seq(0,0.4,0.1),labels=c("0",seq(0.1,0.4,0.1)))

par(fig=c(0.47,1,0,0.53),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(d4$par[1:od4],d1$par[(od4+1):(od4*2)]/sum(d4$par[(od4+1):(od4*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0),at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"))
axis(2,line=-0.45,labels=F,at=seq(0,0.4,0.1))

dev.off()