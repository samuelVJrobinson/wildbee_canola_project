#Experimental treatment of "average" % SNL in landscape using distance-decaying weighted average.
#Idea: the usual treatment of landscape parameters is to identify some critical distance from a point, at which the likelihood/R^2 is maximized, and use that distance for everything. This likely doesn't work for smaller-bodied creatures, and may be different for different landscape features. This allows the average contribution from different distances away from a point to vary as a function of rho, a distance decay parameter which is estimated using ML or HMC.

str(landscape)

#Temporary version using 10092
ldat <- landscape %>% 
  filter(year==2016) %>% select(BLID,contains('Seminatural')) %>% 
  gather('Radius','Percent',contains('Seminatural')) %>% 
  separate(Radius,c('Type','Radius'),sep='_') %>% 
  mutate(Radius=as.numeric(Radius)) %>% 
  filter(BLID==10092) %>%
  arrange(Radius) %>% select(Radius,Percent)

#Create % cover within annuli, using data averaged over entire radius
diffNums <- function(vals,dists){
  N <- length(vals) #Length of vector
  percArea <-  (dists^2-c(0,dists[1:N-1])^2)/max(dists^2) #Proportion area within each annulus
  ret <- numeric(N) #Return value vector
  ret[1] <- vals[1] #First value is %cover, by definition
  for(i in 2:length(vals)){
    ret[i] <- (vals[i]*percArea[i]+(vals[i]-vals[i-1])*sum(percArea[1:i-1]))/percArea[i]
  }
  names(ret) <- dists
  return(ret)
} 

ldat$PercAnn <- with(ldat,diffNums(Percent,Radius)) #Percentage SNL within each annulus
  
#Error function
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

#Indefinite integral of negative exponential function (alpha^2)*exp(-0.5*(D^2)/(rho^2)) is:
# (sqrt(pi)*alpha^2*erf(D/(sqrt(2)*rho))*rho)/sqrt(2)
negExpFun <- function(D,alpha,rho) (alpha^2)*exp(-0.5*(D^2)/(rho^2))
int_negExpFun <- function(D,alpha,rho) (sqrt(pi)*alpha^2*erf(D/(sqrt(2)*rho))*rho)/sqrt(2)

curve(negExpFun(x,1,2),0,5) #alpha = 1, rho =2
curve(int_negExpFun(x,1,2),0,5)

#If integral of negExpFun dD from 0 to inf = 1, and rho is positive, then alpha must be:
alphaFun <- function(rho) sqrt(22619537)/(2*sqrt(3998607)*pi^(1/4)*sqrt(rho))

#Weighted average, using distance decay function


