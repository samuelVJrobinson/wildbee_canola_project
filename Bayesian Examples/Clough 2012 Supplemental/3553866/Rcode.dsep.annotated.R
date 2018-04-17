# Testing the independence claims of model A and model B

library(R2WinBUGS)
## Data preparation

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



## Model to test independence claim 1

m1.data<-list("n.plot","temp","age")

# Defining the initial values for the MCMC chains

m1.inits<-function (){list(
a.temp=runif(1,-1,1),
b.age_temp=runif(1,-1,1),
sigma=runif(1,1,2))
}

# Defining the parameters for which I want to save the posterior samples

m1.parameters=c(names(m1.inits()))
library(R2WinBUGS)
setwd("C:/yanndata/transfer/SEM_methods/revision/code_rev_1/claim_tests")
m1 <- bugs (m1.data, m1.inits, m1.parameters,  n.chains=3, 
 "claim1_bugs.txt", bugs.directory="c:/Programme/WinBUGS14",
 working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000,codaPkg=F,debug=TRUE)
print(m1)

## Define the function to get the largest CI that does not include zero

fun.CI=function(x){
w=sum(x<0)
w2=(1002-(abs(501-w)*2))/1002
return(w2)
}

# Apply the function to the posterior distribution of b.age_temp

fun.CI(m1$sims.list$b.age_temp)
# This shows the independence claim 1 to be substantiated.



## Model to test independence claim 2

m2.data<-list("n.tree","n.plot","n.subplot",
"Pc","Hs","Npodh","subplot", 	
"Nfert","plot",
"temp","age")

# Defining the initial values for the MCMC chains

m2.inits<-function (){list(
	b.Pc_Hs=runif(1,-1,1), sigma.eps.Hs=runif(1,1,2),
	b.Nfert_Hs=runif(1,-1,1),sigma.Hs.subplot=runif(1,1,2),
	a.Hs=runif(1,-1,1),b.temp_Hs=runif(1,-1,1),b.age_Hs=runif(1,-1,1),
	sigma.Hs.plot=runif(1,1,2),
	a.Hs.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Hs.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m2.parameters<-c(names(m2.inits())[1:9],
"fit.Hs",
"fit.Hs.rep",
"p.Hs.fr"
)

# Run the model

m2 <- bugs (m2.data, m2.inits, m2.parameters,  n.chains=3, 
 "claim2_bugs.txt", bugs.directory="c:/Programme/WinBUGS14",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000,codaPkg=F,debug=TRUE)
print(m2$summary[c(1:9),],digits=3)

fun.CI(m2$sims.list$b.age_Hs)
#shows the independence claim 2 to be substantiated.


## Model to test independence claim 3 A

m3A.data<-list("n.tree","n.plot","n.subplot",
"Pc","Cc","Npodh","subplot", 	
"Nfert","plot",
"temp","age")

# Defining the initial values for the MCMC chains

m3A.inits<-function (){list(
	b.Pc_Cc=runif(1,-1,1),sigma.eps.Cc=runif(1,1,2),
	b.Nfert_Cc=runif(1,-1,1),sigma.Cc.subplot=runif(1,1,2),
	a.Cc=runif(1,-1,1),b.temp_Cc=runif(1,-1,1),b.age_Cc=runif(1,-1,1),
	sigma.Cc.plot=runif(1,1,2),
	a.Cc.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Cc.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m3A.parameters<-c(names(m3A.inits())[1:9],
"fit.Cc",
"fit.Cc.rep",
"p.Cc.fr"
)

# Run the model

m3A <- bugs (m3A.data, m3A.inits, m3A.parameters,  n.chains=3, 
 "claim3A_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)
print(m3A$summary[c(1:8),],digits=3)

fun.CI(m3A$sims.list$b.age_Cc)
# Shows the independence claim to be substantiated.



# Model to test independence claim 3 B

m3B.data<-list("n.tree","n.plot","n.subplot",
"Pc","Hs","Cc","Npodh","subplot", 	
"Nfert","plot",
"temp","age")

# Defining the initial values for the MCMC chains

m3B.inits<-function (){list(
	b.Pc_Cc=runif(1,-1,1),b.Hs_Cc=runif(1,-1,1),sigma.eps.Cc=runif(1,1,2),
	b.Nfert_Cc=runif(1,-1,1),sigma.Cc.subplot=runif(1,1,2),
	a.Cc=runif(1,-1,1),b.temp_Cc=runif(1,-1,1),b.age_Cc=runif(1,-1,1),
	sigma.Cc.plot=runif(1,1,2),
	a.Cc.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Cc.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m3B.parameters<-c(names(m3B.inits())[1:10],
"fit.Cc",
"fit.Cc.rep",
"p.Cc.fr"
)

# Run the model

m3B <- bugs (m3B.data, m3B.inits, m3B.parameters,  n.chains=3, 
 "claim3B_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)
print(m3B$summary[c(1:10),],digits=3)
fun.CI(m3B$sims.list$b.age_Cc)
#shows the independence claim to be substantiated.



# Model to test independence claim 4

m4.data<-list("n.tree","n.plot","n.subplot",
"Npodh","subplot", 	
"Nfert","plot",
"temp","age")

# Defining the initial values for the MCMC chains

m4.inits<-function (){list(
	sigma.eps.Npodh=runif(1,1,2),
	b.Nfert_Npodh=runif(1,-1,1),sigma.Npodh.subplot=runif(1,1,2),
	a.Npodh=runif(1,-1,1),b.temp_Npodh=runif(1,-1,1),b.age_Npodh=runif(1,-1,1),
	a.Npodh.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Npodh.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m4.parameters<-c(names(m4.inits())[1:6],
"fit.Npodh",
"fit.Npodh.rep",
"Npodh.fr"
)

# Run the model

m4 <- bugs (m4.data, m4.inits, m4.parameters,  n.chains=3, 
 "claim4_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)
print(m4$summary[c(1:8),],digits=3)

fun.CI(m4$sims.list$b.age_Npodh)
#independence claim is supported 

# Model to test independence claim 5

m5.data<-list("n.tree","n.plot","n.subplot",
"Pc","Npodh","subplot", 	
"Nfert","plot",
"temp","age")

# Defining the initial values for the MCMC chains

m5.inits<-function (){list(
	sigma.eps.Npodh=runif(1,1,2),b.Pc_Npodh=runif(1,-1,1),
	b.Nfert_Npodh=runif(1,-1,1),sigma.Npodh.subplot=runif(1,1,2),
	a.Npodh=runif(1,-1,1),b.temp_Npodh=runif(1,-1,1),b.age_Npodh=runif(1,-1,1),
	a.Npodh.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Npodh.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m5.parameters<-c(names(m5.inits())[1:7],
"fit.Npodh",
"fit.Npodh.rep",
"Npodh.fr"
)

m5 <- bugs (m5.data, m5.inits, m5.parameters,  n.chains=3, 
 "claim5_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)
print(m5$summary[c(1:8),],digits=3)
fun.CI(m5$sims.list$b.Pc_Npodh)
#independence claim is supported 



# Model to test independence claim 6

m6.data<-list("n.tree","n.plot","n.subplot",
"Pc","Hs","Npodh","subplot", 	
"Nfert","plot",
"temp")

# Defining the initial values for the MCMC chains

m6.inits<-function (){list(
	b.Pc_Hs=runif(1,-1,1), b.Npodh_Hs=runif(1,-1,1), sigma.eps.Hs=runif(1,1,2),
	b.Nfert_Hs=runif(1,-1,1),sigma.Hs.subplot=runif(1,1,2),
	a.Hs=runif(1,-1,1),b.temp_Hs=runif(1,-1,1),
	sigma.Hs.plot=runif(1,1,2),
	a.Hs.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Hs.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m6.parameters<-c(names(m6.inits())[1:10],
"fit.Hs",
"fit.Hs.rep",
"p.Hs.fr"
)

# Run the model

m6 <- bugs (m6.data, m6.inits, m6.parameters,  n.chains=3, 
 "claim6_bugs.txt", bugs.directory="c:/Programme/WinBUGS14",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000,codaPkg=F,debug=TRUE)
print(m6$summary[c(1:10),],digits=3)

fun.CI(m6$sims.list$b.Npodh_Hs)



# Model to test independence claim 7A

m7A.data<-list("n.tree","n.plot","n.subplot",
"Pc","Cc","Npodh","subplot", 	
"Nfert","plot","temp")

# Defining the initial values for the MCMC chains

m7A.inits<-function (){list(
	b.Pc_Cc=runif(1,-1,1),b.Npodh_Cc=runif(1,-1,1),sigma.eps.Cc=runif(1,1,2),
	b.Nfert_Cc=runif(1,-1,1),sigma.Cc.subplot=runif(1,1,2),
	a.Cc=runif(1,-1,1),b.temp_Cc=runif(1,-1,1),
	sigma.Cc.plot=runif(1,1,2),
	a.Cc.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Cc.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m7A.parameters<-c(names(m7A.inits())[1:10],
"fit.Cc",
"fit.Cc.rep",
"p.Cc.fr"
)

# Run the model

m7A <- bugs (m7A.data, m7A.inits, m7A.parameters,  n.chains=3, 
 "claim7A_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)
print(m7A$summary[c(1:10),],digits=3)

fun.CI(m7A$sims.list$b.Npodh_Cc)



# Model to test independence claim 7B

m7B.data<-list("n.tree","n.plot","n.subplot",
"Pc","Hs","Cc","Npodh","subplot", 	
"Nfert","plot","temp")

# Defining the initial values for the MCMC chains

m7B.inits<-function (){list(
	b.Pc_Cc=runif(1,-1,1),b.Hs_Cc=runif(1,-1,1),b.Npodh_Cc=runif(1,-1,1),
	sigma.eps.Cc=runif(1,1,2),
	b.Nfert_Cc=runif(1,-1,1),sigma.Cc.subplot=runif(1,1,2),
	a.Cc=runif(1,-1,1),b.temp_Cc=runif(1,-1,1),
	sigma.Cc.plot=runif(1,1,2),
	a.Cc.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Cc.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples

m7B.parameters<-c(names(m7B.inits())[1:10],
"fit.Cc",
"fit.Cc.rep",
"p.Cc.fr"
)

# Run the model

m7B <- bugs (m7B.data, m7B.inits, m7B.parameters,  n.chains=3, 
 "claim7B_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=10000, codaPkg=F,debug=TRUE)
print(m7B$summary[c(1:8),],digits=3)

fun.CI(m7B$sims.list$b.Npodh_Cc)
#



# Model to test independence claim 8A

m8A.data<-list("n.tree","n.plot","n.subplot",
"Pc","Hs","Cc","Npodh","subplot", 	
"Nfert","plot",
"temp")

# Defining the initial values for the MCMC chains

m8A.inits<-function (){list(
	b.Pc_Cc=runif(1,-1,1),b.Hs_Cc=runif(1,-1,1),sigma.eps.Cc=runif(1,1,2),
	b.Nfert_Cc=runif(1,-1,1),sigma.Cc.subplot=runif(1,1,2),
	a.Cc=runif(1,-1,1),b.temp_Cc=runif(1,-1,1),
	sigma.Cc.plot=runif(1,1,2),
	a.Cc.subplot=rep(runif(1,-1,1),times=n.subplot),
	a.Cc.plot=rep(runif(1,-1,1),times=n.plot)
)}

# Defining the parameters for which I want to save the posterior samples





m8A.parameters<-c(names(m8A.inits())[1:8],
"fit.Cc",
"fit.Cc.rep",
"p.Cc.fr"
)

m8A <- bugs (m8A.data, m8A.inits, m8A.parameters,  n.chains=3, 
 "claim8A_bugs.txt", bugs.directory="c:/Programme/WinBUGS14/",
    working.directory=getwd(), clearWD=FALSE, n.iter=50000, n.sims=1000, codaPkg=F,debug=TRUE)
print(m8A$summary[c(1:8),],digits=3)

fun.CI(m8A$sims.list$b.Hs_Cc)
# Shows that this independence claim is not supported (P<0.001)

# Joining all claims 

p.A=c(
fun.CI(m1$sims.list$b.age_temp),
fun.CI(m2$sims.list$b.age_Hs),
fun.CI(m3A$sims.list$b.age_Cc),
fun.CI(m4$sims.list$b.age_Npodh),
fun.CI(m5$sims.list$b.Pc_Npodh),
fun.CI(m6$sims.list$b.Npodh_Hs),
fun.CI(m7A$sims.list$b.Npodh_Cc),
fun.CI(m8A$sims.list$b.Hs_Cc)+1e-09)
#I add a small constant to avoid a zero value,
#even though all of the 1000 samples of the posterior were negative.
#The distribution of the parameter b.Hs_Cc suggests a minute P-value 
#approaching zero (<0.0001).

C.A=-2*sum(log(p.A))
k.A=k.B=length(p.A)

overall_P.A=1-pchisq(C.A,2*k.A)

p.B=c(
fun.CI(m1$sims.list$b.age_temp),
fun.CI(m2$sims.list$b.age_Hs),
fun.CI(m3B$sims.list$b.age_Cc),
fun.CI(m4$sims.list$b.age_Npodh),
fun.CI(m5$sims.list$b.Pc_Npodh),
fun.CI(m6$sims.list$b.Npodh_Hs),
fun.CI(m7B$sims.list$b.Npodh_Cc))

C.B=-2*sum(log(p.B))
k.B=length(p.B)

overall_P.B=1-pchisq(C.B,2*k.B)

# write results to table

write.table(cbind(as.character(c(1:8,"k","C","P")),c(p.A,k.A,C.A,overall_P.A),c(p.B,0,k.B,C.B,overall_P.B)),"Ctest_out.txt",row.names=F)
