# Plots for paper
#### Prior and Prior Predictive Distributions UNIFORM ####
pdf(file="doc/Prior_and_Predictive.pdf",width=8,height = 4/7*11)
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
pdf(file="doc/Support_detadx.pdf",width=8,height = 4/7*11)
col.sup<-"#8e9b97"
par(oma=c(2,2,0.1,0.1))
par(fig=c(0,0.52,0.46,1),
    mai=c(0.5,0.5,0.1,0.1))
plot(d1$par[1:od1],d1$par[(od1+1):(od1*2)]/sum(d1$par[(od1+1):(od1*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0),labels=F,at=seq(0,1,0.2))
axis(2,line=-0.45,at=seq(0,0.4,0.1),labels=c("0",seq(0.1,0.4,0.1)),
     las=2)
par(fig=c(0.15,0.38,0.72,0.97),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,xlim=c(0,1),axes=F,ann=F,type="n",ylim=c(-0.05,0))
segments(x0=d1$par[1:od1],x1=d1$par[1:od1],y0=0,y1=-0.05,
         lty=3,col=col.sup)
lines(pruebas1[,1],pruebas1[,2]-2,col="#07575b")
rect(-0.015,-0.0515,1.015,0)
axis(1,at=c(0,1),labels=c("0","1"),line=-0.03,cex.axis=.7,padj = -1.3)
axis(2,at=c(0,-0.05),labels=c("0","-0.05"),las=2,cex.axis=.7,line=-0.13,hadj=0.8)
mtext(expression(d(eta,x)),side=2,cex=0.8)

par(fig=c(0.47,1,0.46,1),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(d2$par[1:od2],d2$par[(od2+1):(od2*2)]/sum(d2$par[(od2+1):(od2*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0),labels=F,at=seq(0,1,0.2))
axis(2,line=-0.45,at=seq(0,0.4,0.1),labels=F)
par(fig=c(0.62,0.85,0.72,0.97),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,xlim=c(0,1),axes=F,ann=F,type="n",ylim=c(-0.05,0))
segments(x0=d2$par[1:od2],x1=d2$par[1:od2],y0=0,y1=-0.05,
         lty=3,col=col.sup)
lines(pruebas2[,1],pruebas2[,2]-2,col="#07575b")
rect(-0.015,-0.0515,1,0)
axis(1,at=c(0,1),labels=c("0","1"),line=-0.03,cex.axis=.7,padj = -1.3)
axis(2,at=c(0,-0.05),labels=c("0","-0.05"),las=2,cex.axis=.7,line=-0.13,hadj=0.8)
mtext(expression(d(eta,x)),side=2,cex=0.8)


par(fig=c(0,0.52,0,0.53),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(d3$par[1:od3],d3$par[(od3+1):(od3*2)]/sum(d3$par[(od3+1):(od3*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0))
axis(2,line=-0.45,las=2,at=seq(0,0.4,0.1),labels=c("0",seq(0.1,0.4,0.1)))
par(fig=c(0.15,0.38,0.26,0.51),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,xlim=c(0,1),axes=F,ann=F,type="n",ylim=c(-0.05,0))
segments(x0=d3$par[1:od3],x1=d3$par[1:od3],y0=0,y1=-0.05,
         lty=3,col=col.sup)
lines(pruebas3[,1],pruebas3[,2]-2,col="#07575b")
rect(0,-0.0515,1,0.0003)
axis(1,at=c(0,1),labels=c("0","1"),line=-0.03,cex.axis=.7,padj = -1.3)
axis(2,at=c(0,-0.05),labels=c("0","-0.05"),las=2,cex.axis=.7,line=-0.2,hadj=0.8)
mtext(expression(d(eta,x)),side=2,cex=0.8)

par(fig=c(0.47,1,0,0.53),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(d4$par[1:od4],d1$par[(od4+1):(od4*2)]/sum(d4$par[(od4+1):(od4*2)]),
     ylim=c(0,0.46),type="h",lwd=2,axes=F,ann=F,xlim=c(0,1))
rect(-0.01,0,1.01,0.46)
axis(1,pos=c(0,0),at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"))
axis(2,line=-0.45,labels=F,at=seq(0,0.4,0.1))
par(fig=c(0.62,0.85,0.26,0.51),
    mai=c(0.5,0.5,0.1,0.1),
    new=T)
plot(0,0,xlim=c(0,1),axes=F,ann=F,type="n",ylim=c(-0.05,0))
segments(x0=d4$par[1:od4],x1=d4$par[1:od4],y0=0,y1=-0.05,
         lty=3,col=col.sup)
lines(pruebas4[,1],pruebas4[,2]-2,col="#07575b")
rect(0,-0.0515,1,0)
axis(1,at=c(0,1),labels=c("0","1"),line=-0.03,cex.axis=.7,padj = -1.3)
axis(2,at=c(0,-0.05),labels=c("0","-0.05"),las=2,cex.axis=.7,line=-0.2,hadj=0.8)
mtext(expression(d(eta,x)),side=2,cex=0.8)

mtext(expression(paste(beta,"~ U (2, 8)")),side=2,outer=T,at=0.78,cex=1.2)
mtext(expression(paste(beta,"~ U (15, 18)")),side=2,outer=T,at=0.28,cex=1.2)
mtext(expression(paste(mu,"~ U (0.1, 0.6)")),side=1,outer=T,at=0.78,cex=1.2,
      line=0.5)
mtext(expression(paste(mu,"~ U (0, 1)")),side=1,outer=T,at=0.28,cex=1.2,
      line=0.5)
dev.off()
#### Joint Normal Prior Distribution ####
pdf("doc/Joint_Normal.pdf",width=8,height = 4/7*11)
par(oma=c(2,2,0.1,0.1))
par(fig=c(0,0.5,0.47,1),
    mai=c(0.5,0.5,0.1,0.1))
plot(0,0,type="n",ylim=c(0,1),xlim=c(0,1),ann=F,axes=F)
parameters<-rmvnorm(300,joint_mu,joint_sigma)
col.curve<-"#8e9b9733"
col.bar<-"#8e9b97"
for(i in 1:length(b)){
  curve(1/(1+exp(-b[i]*(x-m[i]))),from=0,to=1,add=T,col=col.curve)
}
box()
axis(1,labels=F)
axis(2,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"),las=2)
mtext(expression(paste(P,"(change)")),side=2,cex=1.4,line=2.6)
par(fig=c(0.55,0.9,0.794,1),
    new=T)
hist(parameters[,2],axes=F,xlim=c(0,0.6),ann=F,border=col.bar,
     col=col.bar,freq=F)
curve(dnorm(x,joint_mu[2],sqrt(joint_sigma[2,2])),from=0,to=.6,
      col="#07575b",add=T)
par(fig=c(0.817,1,0.5,0.9),
    new=T)
Hist(parameters[,1],horizontal = T,xax = c(0,max(parameters[,1])),
     lbreaks = length(seq(min(parameters[,1]),max(parameters[,1]),1)),
     color=col.bar)
lines(dnorm(seq(0,max(parameters[,1]),0.1),joint_mu[1],sqrt(joint_sigma[1,1])),seq(0,max(parameters[,1]),0.1),
      col="#07575b")
par(fig=c(0.55,0.9,0.5,0.9),
    new=T)
plot(parameters[,2],parameters[,1],cex=1,pch=21,bg="#07575b",
     xlim=c(0,0.6),ylim=c(0,max(parameters[,1])),col="black",axes=F,ann=F)
box()
axis(1,at=seq(0,0.6,0.2),labels=c("0",seq(0.2,0.6,0.2)),padj=-1)
axis(2,las=2,at=seq(0,25,5),labels=c("0",seq(5,25,5)),hadj = 0.6)
mtext(expression(beta),side=2,line=2.6,las=2,cex=1.6)
mtext(expression(mu),side=1,line=1.7,cex=1.6)

par(fig=c(0,0.52,0,0.52),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(disenoj[1:odj],
     disenoj[(odj+1):(2*odj)]/sum(disenoj[(odj+1):(2*odj)]),type="h",lwd=3,
     xlim=c(0,1),ylim=c(0,1),axes=F,ann=F)
axis(1,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"),padj=-1,
     line=-0.45)
axis(2,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"),
     padj=0.3,las=2,line=0)
text(0.7,0.7,labels=bquote(paste(phi," = - ",.(round(dj$value,2)))),
     cex=1.6)
rect(-0.0395,0,1,1)
mtext(expression(paste(x)),side=1,cex=1.6,line=2)
mtext(expression(paste(eta)),side=2,cex=1.6,line=2.9,las=2)

par(fig=c(0.5,1,0.0122,0.5196),
    mai=c(0.5,0.5,0.1,0.1),new=T)
plot(0,0,xlim=c(0,1),ylim=c(-0.08,0),type="n",
     axes=F,ann=F)
segments(x0=dj$par[1:odj],x1=dj$par[1:odj],y0=0,y1=-0.085,
         lty=3,col=col.sup,lwd=2)
lines(pruebasj[,1],pruebasj[,2]-2,col="#07575b",lwd=2)
rect(0,-0.0825,1,0)
axis(1,at=seq(0,1,0.2),labels=c("0",seq(0.2,0.8,0.2),"1"),padj=-1,
     line=-0.09)
axis(2,las=2,at=seq(-0.08,0,length.out = 2),line=-0.58,labels=c(-0.08,"0"),hadj=0.8)
mtext(expression(paste(x)),side=1,cex=1.6,line=2)
mtext(expression(d(eta,x)),side=2,cex=1.6)
dev.off()
