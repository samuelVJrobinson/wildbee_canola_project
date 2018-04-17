# Effect decomposition for model B

# Define a function to compute the potential outcomes for percentage of pods
# affected by Conopomorpha cramerella (Cc) conditional on the presence and absence
# of the ant Philidris cordata and whether it affects Cc directly, indirectly, both,
# or not at all.
# Pc.dir is the value of Pc serving as input to Cc
# Pc.indir is the value of Pc serving as input for Hs

Cc.calc<-function(Pc.dir,Pc.indir){

	# Create empty matrices/arrays to accommodate the posteriors of the parameters
	
	M=1002 # Number of samples of the posteriors of the parameters
	p.Hs=matrix(NA,nrow=M,ncol=n.tree)
	a.Hs.subplot.hat=matrix(NA,nrow=M,ncol=n.subplot)
	a.Hs.plot.hat=matrix(NA,nrow=M,ncol=n.plot)

	# Create an empty array for the simulated outcomes of Hs.hat
	# This is done because expected values for Hs.hat are continuous, 
	# while the realised values to be used in the calculation of outcomes
	# of Cc are discrete,

	Q=50 # Number of simulations
	Hs.hat=array(NA,c(M,n.tree,Q)) 

	# Filling the matrices/arrays

	for(h in 1:n.plot){
		a.Hs.plot.coef<-matrix(
		c(mB$sims.list$a.Hs, mB$sims.list$b.temp_Hs),
		nrow=M,ncol=2,byrow=F)
		a.Hs.plot.contrast<-as.vector(c(1,temp[h]))
		a.Hs.plot.hat[,h]<-a.Hs.plot.coef%*%a.Hs.plot.contrast
	}

	for(k in 1:n.subplot){
		a.Hs.subplot.coef<-matrix(    
		c(a.Hs.plot.hat[,plot[k]],mB$sims.list$b.Nfert_Hs),nrow=M,ncol=2,byrow=F)
		a.Hs.subplot.contrast<-as.vector(c(1,Nfert[k]))
		a.Hs.subplot.hat[,k]<-a.Hs.subplot.coef%*%a.Hs.subplot.contrast
	}

	for(q in 1:Q){
		for(j in 1:n.tree){
			p.Hs.coef<-matrix(    
			c(a.Hs.subplot.hat[,subplot[j]],mB$sims.list$b.Pc_Hs),nrow=M,ncol=2,byrow=F)
			p.Hs.contrast<-as.vector(c(1,Pc.indir[j]))
			p.Hs[,j]<-invlogit(p.Hs.coef%*%p.Hs.contrast)
			Hs.hat[,j,q]<-rbinom(1002,new.Npodh[j],p.Hs[,j]) 
		}
	}

	# Empty the memory

	gc()

	# Repeat as above, for Cc, drawing upon the simulated values of Hs.

	p.Cc=matrix(NA,nrow=M,ncol=n.tree)
	a.Cc.subplot.hat=matrix(NA,nrow=M,ncol=n.subplot)
	a.Cc.plot.hat=matrix(NA,nrow=M,ncol=n.plot)
	Cc.hat=array(NA,c(M,n.tree,Q))

	a.Cc.plot.coef<-matrix(mB$sims.list$a.Cc,nrow=M,ncol=1,byrow=F)

	for(h in 1:n.plot){
		a.Cc.plot.coef<-matrix(
		c(mB$sims.list$a.Cc, mB$sims.list$b.temp_Cc),
		nrow=M,ncol=2,byrow=F)
		a.Cc.plot.contrast<-as.vector(c(1,temp[h]))
		a.Cc.plot.hat[,h]<-a.Cc.plot.coef%*%a.Cc.plot.contrast
	}

	for(k in 1:n.subplot){
		a.Cc.subplot.coef<-matrix(
		c(a.Cc.plot.hat[,plot[k]],mB$sims.list$b.Nfert_Cc),nrow=M,ncol=2,byrow=F)
		a.Cc.subplot.contrast<-as.vector(c(1,Nfert[k]))
		a.Cc.subplot.hat[,k]<-a.Cc.subplot.coef%*%a.Cc.subplot.contrast
	}

	# Replace zero values of Npodh by 1 to avoid zero denominator issues
	new.Npodh=Npodh+ifelse(Npodh==0,1,0) 
	
	for(q in 1:Q){
		for(j in 1:n.tree){
			p.Cc.coef<-matrix(    
			c(a.Cc.subplot.hat[,subplot[j]],
			mB$sims.list$b.Pc_Cc,
			mB$sims.list$b.Hs_Cc*(Hs.hat[,j,q]/new.Npodh[j])),nrow=M,ncol=3,byrow=F)
			p.Cc.contrast<-as.vector(c(1,Pc.dir[j],1))
			p.Cc[,j]<-invlogit(p.Cc.coef%*%p.Cc.contrast)
			Cc.hat[,j,q]<-rbinom(1002,new.Npodh[j],p.Cc[,j])
		}
	}
return(Cc.hat)
}

new.Npodh=Npodh+ifelse(Npodh==0,1,0) 

# Define the inverse logit function used in the function above.
invlogit <- function(x){return(1/(1+exp(-x)))}



# Apply the function to calculate the potential outcomes

Pc0=rep(0,430)
Pc1=rep(1,430)


# Potential outcome if the ant (Pc) is absent

Cc_Pc0_HsPc0=Cc.calc(Pc.dir=Pc0,Pc.indir=Pc0)
Cc_Pc0_HsPc0m=apply(Cc_Pc0_HsPc0,c(1,2),mean)
rm(Cc_Pc0_HsPc0)
gc()


# Potential outcome if the ant (Pc) is present and affects Cc directly, and indirectly via Hs

Cc_Pc1_HsPc1=Cc.calc(Pc.dir=Pc1,Pc.indir=Pc1)
Cc_Pc1_HsPc1m=apply(Cc_Pc1_HsPc1,c(1,2),mean)
rm(Cc_Pc1_HsPc1)
gc()

# Potential outcome if the ant (Pc) is present and affects Cc directly but not indirectly

Cc_Pc1_HsPc0=Cc.calc(Pc.dir=Pc1,Pc.indir=Pc0)
Cc_Pc1_HsPc0m=apply(Cc_Pc1_HsPc0,c(1,2),mean)
rm(Cc_Pc1_HsPc0)
gc()

# Potential outcome if the ant (Pc) is present and affects Cc only indirectly via Hs

Cc_Pc0_HsPc1=Cc.calc(Pc.dir=Pc0,Pc.indir=Pc1)
Cc_Pc0_HsPc1m=apply(Cc_Pc0_HsPc1,c(1,2),mean)
rm(Cc_Pc0_HsPc1)
gc()

# Average effects

total=rowMeans(
t(t(Cc_Pc1_HsPc1m)/new.Npodh)-
t(t(Cc_Pc0_HsPc0m)/new.Npodh))
direct=rowMeans(
t(t(Cc_Pc1_HsPc0m)/new.Npodh)-
t(t(Cc_Pc0_HsPc0m)/new.Npodh))
indirect=rowMeans(
t(t(Cc_Pc0_HsPc1m)/new.Npodh)-
t(t(Cc_Pc0_HsPc0m)/new.Npodh))
indirect.perc=indirect*100/total

# Define function to plot the posterior distributions of the average effects

plotpost=function(x,main,xlim=NULL,ylim=NULL,at=NULL,labels=NULL,panelid=""){
plot(density(x),xlab="",ylab="",main="",axes=F,xlim=xlim,ylim=ylim)
axis(1)
axis(2,at=at,labels=labels,las=1)
box()
abline(v=mean(x),col="red")
abline(v=sort(x)[25],col="red",lty=2)
abline(v=sort(x)[975],col="red",lty=2)
mtext(main,side=1,line=3)
mtext("Density",side=2,line=4)
mtext(panelid,side=3,line=0.8,cex.main=1.5,font=1,adj=-0.3)
}

# Plot the results

par(mfrow=c(1,4))
plotpost(x=total*100,main="Total effect(%)",panelid="A")
plotpost(x=direct*100,main="Direct effect(%)",panelid="B")
plotpost(x=indirect*100,main="Indirect effect(%)",panelid="C")
plotpost(x=indirect.perc,main="Indirect effect (% of total)",xlim=c(-100,300),at=c(0,0.01,0.02),labels=c("0","0.01","0.02"),panelid="D")

