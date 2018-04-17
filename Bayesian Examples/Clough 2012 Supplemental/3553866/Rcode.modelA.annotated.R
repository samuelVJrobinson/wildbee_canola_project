# R - code for model A

# Read in the data

getwd() #shows current working directory
# Use setwd() to set your working directory to where the datafiles are stored
setwd("C:/workingdirectory/")

data.plot=read.table("data.plot.txt",h=T,dec=".")
data.subplot=read.table("data.subplot.txt",h=T,dec=".")
data.tree=read.table("data.tree.txt",h=T,dec=".")

# Defining tree-level variable vectors

Pc=data.tree$Pc
Hs=data.tree$Hs
Cc=data.tree$Cc
Npodh=data.tree$npodh
subplot=data.tree$subplot

# Defining subplot-level variable vectors

Nfert=data.subplot$Nfert
plot=data.subplot$plot

# Defining plot-level variable vectors

temp=data.plot$temp
age=data.plot$age

# Number of observations at each level

n.tree=430
n.subplot=86
n.plot=43

# Defining the required data

mA.data<-list("n.tree","n.plot","n.subplot",
"Pc","Hs","Cc","Npodh","subplot", 	
"Nfert","plot",
"temp","age")

# Defining the initial values for the MCMC chains

mA.inits<-function (){list(
	sigma.eps.Npodh=runif(1,1,2),
	b.Pc_Hs=runif(1,-1,1),sigma.eps.Hs=runif(1,1,2),
	b.Pc_Cc=runif(1,-1,1),sigma.eps.Cc=runif(1,1,2),

	b.Nfert_Npodh=runif(1,-1,1),sigma.Npodh.subplot=runif(1,1,2),
	b.Nfert_Pc=runif(1,-1,1),
	#sigma.Pc.subplot=runif(1,1,2),
	b.Nfert_Hs=runif(1,-1,1),sigma.Hs.subplot=runif(1,1,2),
	b.Nfert_Cc=runif(1,-1,1),sigma.Cc.subplot=runif(1,1,2),

	a.Npodh=runif(1,-1,1),b.temp_Npodh=runif(1,-1,1),sigma.Npodh.plot=runif(1,1,2),
	a.Pc=runif(1,-1,1),b.temp_Pc=runif(1,-1,1),b.age_Pc=runif(1,-1,1),sigma.Pc.plot=runif(1,1,2),
	a.Hs=runif(1,-1,1),b.temp_Hs=runif(1,-1,1),sigma.Hs.plot=runif(1,1,2),
	a.Cc=runif(1,-1,1),b.temp_Cc=runif(1,-1,1),sigma.Cc.plot=runif(1,1,2),

	a.Npodh.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Hs.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Cc.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Npodh.plot=rep(runif(1,-1,1),times=n.plot),
	a.Pc.plot=rep(runif(1,-1,1),times=n.plot),
	a.Hs.plot=rep(runif(1,-1,1),times=n.plot),
	a.Cc.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

mA.parameters<-c(names(mA.inits())[1:25],
"fit.Npodh",
"fit.Npodh.rep",
"fit.Pc",
"fit.Pc.rep",
"fit.Hs",
"fit.Hs.rep",
"fit.Cc",
"fit.Cc.rep",
"Npodh.fr",
"p.Pc.fr",
"p.Hs.fr",
"p.Cc.fr"
)

# Loading the R2WinBUGS package and running the model

library(R2WinBUGS)

mA <- bugs (mA.data, mA.inits, mA.parameters,  n.chains=3, 
 "BUGS.mA.annotated.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, DIC=FALSE,n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)

# Cleaning up the memory

memory.size()
gc()

# Getting an overview of the output, i.e. the posterior samples

str(mA$sims.list)
names(mA$sims.list)
print(mA$summary[c(1:25),],digits=3)

# Writing Posterior predictive checking function

ppc.plot=function(variable1,model){
	x=eval(parse(text=paste(model,"$sims.list$fit.",variable1,sep="")))
	y=eval(parse(text=paste(model,"$sims.list$fit.",variable1,".rep",sep="")))
	plot(x,y,
	main=variable1 , 
	xlab="Discrepancy for actual data set",
	ylab="Discrepancy for perfect data sets", las=1)
	abline(0,1, lwd=2)
	print(mean(y > x))
}

# Produce PPC diagnostic plots and statistics for all response variables

par(mfrow=c(2,2))
ppc.plot("Npodh","mA")
ppc.plot("Pc","mA")
ppc.plot("Hs","mA")
ppc.plot("Cc","mA")


# Prepare data for the residual plots

# calculate the posterior of the expected values
Npodh.hat=apply(mA$sims.list$Npodh.fr,2,mean)
p.Pc.hat=apply(mA$sims.list$p.Pc.fr,2,mean)
p.Hs.hat=apply(mA$sims.list$p.Hs.fr,2,mean)
p.Cc.hat=apply(mA$sims.list$p.Cc.fr,2,mean)

# calculate the residuals
Npodh.res=Npodh-Npodh.hat
Pc.res=Pc-p.Pc.hat
Hs.res=Hs-p.Hs.hat*Npodh
Cc.res=Cc-p.Cc.hat*Npodh

# Plot the residuals

plot(data.frame(Cc.res,Hs.res,Pc.res,Npodh.res,temp=rep(temp,each=10),age=rep(age,each=10)))
cor(data.frame(Cc.res,Hs.res,Pc.res,Npodh.res,temp=rep(temp,each=10),age=rep(age,each=10),Nfert[subplot]))

plot(Npodh.res~Npodh.hat);abline(h=0)
plot(Pc.res~p.Pc.hat);abline(h=0)
plot(Hs.res~p.Hs.hat);abline(h=0)
plot(Cc.res~p.Cc.hat);abline(h=0)

