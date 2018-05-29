#Simple path analysis example
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 3)

#Generate some data:
set.seed(123)
nsamp <- 100 #Number of samples
a <- runif(nsamp,-1,1) #a = random between -1 and 1
b <- a*-0.5 + rnorm(nsamp,0,0.2) # b = a*-0.5 + error
c <- a*(1)+b*0.5+rnorm(nsamp,0,0.2) #c = a*1 + b*0.5 + error
pairs(cbind(a,b,c),pch=19)
#Parameters to recover:
#slopeAB = -0.5
#slopeAC = 1
#slopeBC = 0.5
#sigmaB = sigmaC = 0.2

#MLE linear models 
summary(lm(b~a-1)) #OK
summary(lm(c~a+b-1)) #OK

#Data into list structure for Stan
paData <- list(N=nsamp,a=a,b=b,c=c)

#Stan model
paMod <- '
data {
  int N;
  vector[N] a;
  vector[N] b;
  vector[N] c;
}
parameters {
  //Slope terms
  real slopeAB; 
  real slopeBC;
  real slopeAC;
  //Variance terms
  real<lower=0> sigmaB; 
  real<lower=0> sigmaC;
}
model {
  //Expected values
  vector[N] predB = a*slopeAB; 
  vector[N] predC = a*slopeAC + b*slopeBC;
  //Priors
  slopeAB ~ normal(0,3);
  slopeBC ~ normal(0,3);
  slopeAC ~ normal(0,3);
  sigmaB ~ gamma(1,1);
  sigmaC ~ gamma(1,1);
  //Likelihood
  b~normal(predB,sigmaB);
  c~normal(predC,sigmaC);
}
'
#Run model
fit <- stan(model_code=paMod,data=paData,iter=2000, chains = 3)

#Get parameter estimates
pars <- c('slopeAB','slopeBC','slopeAC','sigmaB','sigmaC')
print(fit,pars=pars)
traceplot(fit,pars=pars) #Chains have mixed well
pairs(fit,pars=pars) #Correlation between slopeBC & slope AC
plot(fit,pars=pars)

fitChains <- extract(fit) #Extract values from model

#Results
par(mfrow=c(2,1))
#slopeAB - OK
curve(dnorm(x,0,3),min(fitChains$slopeAB),max(fitChains$slopeAB),main='Prior slopeAB')
hist(fitChains$slopeAB,main='Posterior slopeAB',breaks=20)
abline(v=quantile(fitChains$slopeAB,c(0.025,0.5,0.975)),lty=c(2,1,2),lwd=c(1,2,1))
abline(v=-0.5,col='red')
#slopeBC - OK
curve(dnorm(x,0,3),min(fitChains$slopeBC),max(fitChains$slopeBC),main='Prior slopeBC')
hist(fitChains$slopeBC,main='Posterior slopeBC',breaks=20)
abline(v=quantile(fitChains$slopeBC,c(0.025,0.5,0.975)),lty=c(2,1,2),lwd=c(1,2,1))
abline(v=0.5,col='red')
#slopeAC - OK
curve(dnorm(x,0,3),min(fitChains$slopeBC),max(fitChains$slopeBC),main='Prior slopeAC')
hist(fitChains$slopeAC,main='Posterior slopeAC',breaks=20)
abline(v=quantile(fitChains$slopeAC,c(0.025,0.5,0.975)),lty=c(2,1,2),lwd=c(1,2,1))
abline(v=1,col='red')
#sigmaB - OK
curve(dgamma(x,1,3),min(fitChains$sigmaB),max(fitChains$sigmaB),main='Prior sigmaB')
hist(fitChains$sigmaB,main='Posterior sigmaB',breaks=20)
abline(v=quantile(fitChains$sigmaB,c(0.025,0.5,0.975)),lty=c(2,1,2),lwd=c(1,2,1))
abline(v=0.2,col='red')
#sigmaC
curve(dgamma(x,1,3),min(fitChains$sigmaC),max(fitChains$sigmaC),main='Prior sigmaC')
hist(fitChains$sigmaC,main='Posterior sigmaC',breaks=20)
abline(v=quantile(fitChains$sigmaC,c(0.025,0.5,0.975)),lty=c(2,1,2),lwd=c(1,2,1))
abline(v=0.2,col='red')
par(mfrow=c(1,1))

#Predicted values
getVals <- function(x) quantile(x,c(0.5,0.025,0.975)) #Extracts median, lower, and upper 95% quantiles

predB <- apply(outer(fitChains$slopeAB,a),2,getVals)
# predC <- apply(outer(fitChains$slopeAC,a)+outer(fitChains$slopeBC,b),2,function(x) quantile(x,c(0.5,0.025,0.975)))

#Predicted values for b 
plot(a,b,pch=19,main='Effect of a on b') 
lines(a[order(a)],predB[1,order(a)],col='red',lwd=2) #Median
lines(a[order(a)],predB[2,order(a)],col='red',lwd=1,lty=2) #Lower
lines(a[order(a)],predB[3,order(a)],col='red',lwd=1,lty=2) #Upper

#Predicted values for c
effBC <- apply(outer(fitChains$slopeBC,b),2,getVals) #Effect of b on c
effAC <- apply(outer(fitChains$slopeAC,a),2,getVals) #Effect of a on c

plot(a,c-effBC[1,],pch=19,ylab='c|b',main='Partial effect of a on c') #Partial effect of a on c
lines(a[order(a)],effAC[1,order(a)],col='red',lwd=2) #Median
lines(a[order(a)],effAC[2,order(a)],col='red',lwd=1,lty=2) #Lower
lines(a[order(a)],effAC[3,order(a)],col='red',lwd=1,lty=2) #Upper

plot(b,c-effAC[1,],pch=19,ylab='c|a',main='Partial effect of b on c') #Partial effect of b on c
lines(b[order(b)],effBC[1,order(b)],col='red',lwd=2) #Median
lines(b[order(b)],effBC[2,order(b)],col='red',lwd=1,lty=2) #Lower
lines(b[order(b)],effBC[3,order(b)],col='red',lwd=1,lty=2) #Upper

#Standardized path coefficients:
coefs <- sapply(fitChains,median)
coefs <- coefs[1:3]

coefs[1] <- coefs[1]*(sd(a)/sd(b)) #a -> b
coefs[2] <- coefs[2]*(sd(b)/sd(c)) #b -> c
coefs[3] <- coefs[3]*(sd(a)/sd(c)) #a -> c

#Plot of results:

#Empty plot
plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "",bg='white',xaxt='n',yaxt='n')
rect(10,70,30,90,col='white',border='black')
rect(40,10,60,30,col='white',border='black')
rect(70,70,90,90,col='white',border='black')
text(c(20,50,80),c(80,20,80),c('a','b','c'))
arrows(20,70,40,20,lwd=abs(coefs[1]*3))
arrows(60,20,80,70,lwd=abs(coefs[2]*3))
arrows(30,80,70,80,lwd=abs(coefs[3]*3))

rect(20,40,30,55,col='white',border='white')
rect(70,40,80,55,col='white',border='white')
rect(45,70,55,90,col='white',border='white')
text(c(28,72,50),c(50,50,80),round(coefs,2))


