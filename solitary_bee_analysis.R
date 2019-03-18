#ANALYSIS FOR PAUL, INVESTIGATING SOLITARY BEE - CANOLA OVERLAP, AND HOW THIS INFLUENCES YEAR-TO-YEAR PERSISTANCE

# Load everything ----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(beepr)

#ggplot theme
prestheme <- theme(legend.position='right',
                legend.text=element_text(size=15),
                axis.text=element_text(size=15), 
                axis.title=element_text(size=20),
                title=element_text(size=20),
                panel.grid.major=element_line(size=0.5,colour='black',linetype='dotted'),
                panel.grid.minor=element_blank(),
                panel.border=element_rect(size=1,colour='black'),
                strip.text=element_text(size=15))
theme_set(theme_bw()+prestheme) #Sets graph theme to B/Ws + prestheme
rm(prestheme)

setwd('~/Projects/UofC/wildbee_canola_project')
#load('arthropod.Rdata') #Has 793 more records; not sure why... 
load('arthropodJoinLandscape.Rdata')
load('trap.Rdata')
#load('flower.Rdata') #Precise flower data doesn't exist for 2015
trap=df3
rm(df3)

year2year <- mutate(df2,year=paste0('y',year)) %>% #Traps that were used in both years
  group_by(year,BLID) %>%
  summarize(count=n()) %>%
  spread(year,count,fill=NA) %>%
  filter(!is.na(y2015)&!is.na(y2016)) %>%
  select(BLID) %>%
  .$BLID

#Known solitary cleptoparasites:
parasitoids=c('Coelioxys','Epeolus','Holcopasites','Melecta','Nomada','Sphecodes','Stelis','Triepeolus')
#Agricultural bees
agricultural=c('Apis mellifera','Megachile rotundata')

df2 <- df2 %>%
  mutate_at(vars(family:species),funs(as.character)) %>% #Convert to character
  mutate_at(vars(family:species),funs(sub(' ','',.))) %>% #Strips white space
  mutate_at(vars(family:species),funs(sub('Megachildae','Megachilidae',.))) %>% #Fixes spelling
  mutate_at(vars(family:species),funs(sub('Colleditae','Colletidae',.))) %>% 
  mutate_at(vars(family:species),funs(sub('Agopostemon','Agapostemon',.))) %>% 
  filter(genus!='Bombus') %>% #Get rid of bumblebees
  mutate_at(vars(family:species),funs(sub('variolosus','variolosa',.))) %>% #NOT SURE IF THESE ARE 2 SEPARATE SPP.
  mutate_at(vars(family:species),funs(factor(.))) %>% #Convert to factor
  select(-length_mm,-caste,-intertegular_mm) %>%
  filter(BLID %in% year2year) 

bees=select(df2,BLID:latTrap) %>% #Bees/trapping data
  unite(genSpp,genus,species,sep=' ',remove=F) %>%
  mutate(parasitoid=ifelse(genus %in% parasitoids,T,F)) %>%
  mutate(agricultural=ifelse(genSpp %in% agricultural,T,F)) %>%
  #Aggregates dates
  mutate(startDate=paste(year,startJulian,sep='-'),endDate=paste(year,endJulian,sep='-')) %>%
  mutate(startDate=as.POSIXct(startDate,format='%Y-%j'),endDate=as.POSIXct(endDate,format='%Y-%j')) %>%
  select(-startMonth,-startJulian,-endMonth,-endJulian)

landscape=select(df2,BLID,lcRuralDeveloped_100m:aafcOtherCrop_2016_4000m) %>% #Landscape data
  distinct()

trap=trap %>%
  filter(BLID %in% year2year) %>%
  mutate(year=startYear) %>%
  select(-startNESWPhoto_DSC,-endNESWPhoto_DSC,-lonTrap,-latTrap,-elevTrap) %>%
  select(-startHour,-startMinute,-endHour,-endMinute) %>%
  unite(startDate,startYear:startDay,sep='-') %>%
  unite(endDate,endYear:endDay,sep='-') %>%
  mutate(startDate=as.POSIXct(startDate,format='%Y-%m-%d'),endDate=as.POSIXct(endDate,format='%Y-%m-%d'))

text2perc<-function(intext){
  if(is.na(intext)|intext=='NA'|intext==''){
    return(NA)
  }
  if(grepl(',',intext)|grepl('-',intext)) { #If there is a comma or dash
    intext=strsplit(unlist(strsplit(intext,',')),'-') #Split into pieces
  }
  intext=lapply(intext,FUN=function(x) mean(as.numeric(x),na.rm=T))
  return(mean(unlist(intext)))
}

#Fixes all the measurements of canola bloom %
temp2015=trap %>%
  filter(year==2015) %>%
  rename(canolaBloom=percentCanolaBloom) %>%
  mutate(canolaAdjacent=as.character(canolaAdjacent),
            canolaBloom=as.character(canolaBloom)) %>%
  mutate(canolaAdjacent=grepl('yes',canolaAdjacent)) %>%
  mutate(canolaBloom=ifelse(grepl('<',canolaBloom),2,canolaBloom)) %>%
  mutate(canolaBloom=gsub('PP','',canolaBloom)) %>%
  mutate(canolaBloom=gsub('NotMeasured',NA,canolaBloom)) %>%
  mutate(canolaBloom=ifelse(grepl('(to [sS]eed|past bloom)',canolaBloom),0,canolaBloom)) %>%
  mutate(canolaBloom=gsub('[NESW]{1,2}:','',canolaBloom)) %>%
  mutate(canolaBloom=gsub(' ','',canolaBloom)) %>%
  mutate(canolaBloom=ifelse(canolaBloom=='','0',canolaBloom)) %>%
  rowwise() %>%
  mutate(canolaBloom=text2perc(canolaBloom)) %>%  #Canola perc bloom for 2015
  ungroup() %>% 
  mutate(canolaBloom=(100/max(canolaBloom))*canolaBloom) %>%  #Scales by maximum measured bloom (makes max==100%)
  select(BTID,BLID,pass,replicate,startDate:endDate,year,canolaBloom)

temp2016=trap %>%
  filter(year==2016) %>%
  filter(pass!=0) %>%
  select(-canolaAdjacent:-deployedNotes,-locationType) %>%
  select(-adjMowed,-oppMowed) %>%
  unite(adjCrop,adjCrop,adjCropBloom) %>%
  unite(oppCrop,oppCrop,oppCropBloom) %>%
  gather('crop','bloom',adjCrop:oppCrop) %>%
  separate(bloom,c('croptype','perc'),sep='_') %>%
  mutate(perc=ifelse(croptype!='canola',NA,perc)) %>%
  rowwise() %>%
  mutate(perc=text2perc(perc)) %>%
  unite(croptype,croptype,perc) %>%
  spread(crop,croptype) %>%
  separate(adjCrop,c('adjCrop','adjPerc'),sep='_') %>%
  separate(oppCrop,c('oppCrop','oppPerc'),sep='_') %>%
  mutate(adjPerc=as.numeric(adjPerc),oppPerc=as.numeric(oppPerc)) %>%
  rowwise() %>%
  mutate(canolaBloom=mean(c(adjPerc,oppPerc),na.rm=T)) %>%
  mutate(canolaBloom=ifelse(adjCrop=='canola'||oppCrop=='canola',canolaBloom,0)) %>%
  mutate(canolaBloom=ifelse(is.nan(canolaBloom),NA,canolaBloom)) %>%
  select(BTID,BLID,pass,replicate,startDate:endDate,year,canolaBloom)
  
trap=bind_rows(temp2015,temp2016) %>%
  mutate(traptime=as.numeric(difftime(endDate,startDate,units='days'))) %>%
  mutate(startDate=as.numeric(format(startDate,format='%j'))) %>%
  mutate(endDate=as.numeric(format(endDate,format='%j'))) %>%
  rowwise() %>%
  mutate(midDate=mean(c(startDate,endDate)))
rm(temp2015,temp2016)

#Landscape variables of interest (percent cover), at radii of 100,250,500m
landscape <- landscape %>%
  select(BLID,contains('Canola',ignore.case=T),contains('Forest'),
         contains('Shrubland'),contains('Grassland'),contains('Pasture')) %>%
  # select(BLID,contains('100m'),contains('250m'),contains('500m')) %>%
  select(BLID,contains('2015'),contains('2016')) %>%
  gather('class','area',-1) %>%
  separate(class,c('class','year','radius')) %>%
  mutate(class=gsub('aafc','',class),radius=gsub('m','',radius)) %>%
  spread(class,area) %>%
  mutate(Seminatural=Forest+NativeGrassland+Shrubland) %>% #All "seminatural" land
  rename(Pasture=PastureForageCrop) %>%
  # select(-Forest,-NativeGrassland,-Shrubland) %>%
  mutate(totalArea=(pi*as.numeric(radius)^2)) %>%
  mutate_at(vars(Canola:Seminatural),funs(./totalArea)) %>%
  select(-totalArea) %>%
  mutate_at(vars(Canola:Seminatural),funs(round(.,4))) %>%
  gather('class','proportion',Canola:Seminatural) %>%
  mutate(proportion=ifelse(proportion>1,1,proportion)) %>%
  unite(class,class,radius) %>%
  spread(class,proportion)

# #Checking to make sure that traps with 0% canola recorded actually had 0% canola around them
# #Answer: not really. There are occasional fields that have canola near them but the bloom wasn't recorded. Probably should do some kind of overall estimate, or random effect.
# landscape %>%
#   select(-contains('Pasture')) %>%
#   select(-contains('Seminatural')) %>%
#   unite(siteYear,BLID:year) %>%
#   left_join(select(trap,BLID,year,canolaBloom) %>%
#               unite(siteYear,BLID:year) %>%
#               group_by(siteYear) %>%
#               summarize(sumPercBloom=as.numeric(sum(canolaBloom,na.rm=T)>0)),by='siteYear') %>%
#   gather('Radius','Measured',contains('Canola')) %>%
#   mutate(Radius=paste('Radius',gsub('Canola_','',Radius),'m')) %>%
#   separate(siteYear,c('site','year'),sep='_') %>%
#   ggplot(aes(Measured,sumPercBloom,col=year))+geom_point(position=position_jitter(width=0.01,height=0.01))+facet_wrap(~Radius,ncol=1)+
#   geom_smooth(method='glm',method.args=list(family='binomial'))+
#   labs(y='%Bloom Measured?',x='Actual amount of canola in landscape')


# #Canola abundance with pass. Looks like bloom may have been missed at:
# #12000, 12754, 14025, 15208, 20001 in 2015 - first observation only
# #10781, 2nd obs?
# #14376 in 2016 - 1st and 3rd obs.
# ggplot(trap,aes(midDate,canolaBloom,col=factor(year)))+
#   geom_line()+geom_point()+
#   facet_wrap(~BLID)+
#   scale_colour_manual(values=c('red','blue'))+labs(col='Year')+
#   theme(axis.text=element_text(size=7),strip.text=element_text(size=10))

#Sets "suspicious" 0 bloom values to NA
trap[trap$BLID==12000 & trap$year==2015 & trap$pass==1,'canolaBloom'] <- NA
trap[trap$BLID==12754 & trap$year==2015 & trap$pass==1,'canolaBloom'] <- NA
trap[trap$BLID==14025 & trap$year==2015 & trap$pass==1,'canolaBloom'] <- NA
trap[trap$BLID==15208 & trap$year==2015 & trap$pass==1,'canolaBloom'] <- NA
trap[trap$BLID==20001 & trap$year==2015 & trap$pass==1,'canolaBloom'] <- NA
trap[trap$BLID==10781 & trap$year==2015 & trap$pass==2,'canolaBloom'] <- NA
trap[trap$BLID==14376 & trap$year==2016 & (trap$pass==1|trap$pass==3),'canolaBloom'] <- NA

#Get distance matrix from trap latitude and longitude
library(sp)
library(maptools)
library(rgdal)
library(rgeos)

trapCoords <- df2 %>% select(BLID:lat) %>% group_by(BLID) %>%  #Get lat and lon
  summarize(lon=first(lon),lat=first(lat)) %>% 
  data.frame()

coordinates(trapCoords)=~lon+lat 
proj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #Initial projection (latlon) to set
proj4string(trapCoords) <- proj #Set projection
newproj <- CRS("+init=epsg:32612") #CRS for UTM 12
trapCoords <- spTransform(trapCoords,newproj) #Transform to UTM 12
distMat <- gDistance(trapCoords,byid=T) #Get distance matrix between sites
colnames(distMat) <- rownames(distMat) <- trapCoords$BLID #Rename rows and columns
distMat <- distMat/10000 #Scales distances to 10s of kms

#Detach spatial stats packages
detach("package:rgdal", unload=TRUE)
detach("package:sp", unload=TRUE)
detach("package:maptools", unload=TRUE)
detach("package:rgeos", unload=TRUE)

#Convenience functions
logit <- function(x){
  return(log(x/(1-x)))
}
invLogit <- function(x){ 
  i <- exp(x)/(1+exp(x)) 
  i[is.nan(i)] <- 1 #If x is large enough, becomes Inf/Inf = NaN, but Lim(invLogit) x->Inf = 1
  return(i)
}
#Posterior predictive plots 
PPplots <- function(resid,predResid,actual,pred,main=NULL){
  par(mfrow=c(2,1))
  plot(resid~predResid,ylab='Sum actual residuals',xlab='Sum simulated residuals',main=main)
  x <- sum(resid<predResid)/length(resid)
  legend('topleft',paste('p =',round(min(x,1-x),3)))
  abline(0,1,col='red') #PP plot
  plot(actual~pred, #Predicted vs Actual
       xlab=paste('Predicted',main),ylab=paste('Actual',main)) 
  abline(0,1,col='red')
  abline(lm(actual~pred),col='red',lty=2)
  par(mfrow=c(1,1))
}
#Faster pair plots, using extracted lists
fastPairs <- function(l){ #List l
  pairs(l,lower.panel=function(x,y){
    par(usr=c(0,1,0,1))
    text(0.5, 0.5, round(cor(x,y),2), cex = 1 * exp(abs(cor(x,y))))})
}
#Fix names of morphospecies (if subgenus listed) 
#eg. "Lasioglossum dialictus_sp17" -> "Lasioglossum (Dialictus) spp.17"
#eg. "Lasioglossum_dialictus_sp17" -> "Lasioglossum (Dialictus) spp.17"
fixNames <- function(a){ 
  if(length(a)>1) stop('Name not specified correctly')
  if(!grepl('_',a)) stop('No underscores found')
  
  #Extract substrings
  #If more than 1 underscore, use underscore as splitting character rather than space
  splitchar <- ifelse(nchar(a)-nchar(gsub('_','',a))>1,'_',' ') #Splitting character
  pt1 <- strsplit(a,splitchar)[[1]][1] #Genus name
  pt2 <- strsplit(strsplit(a,splitchar)[[1]][2],'_')[[1]][1] #Subgenus name (non-capitalized)
  pt3 <- strsplit(a,'_')[[1]][length(strsplit(a,'_')[[1]])] #Spp + Number
  pt2 <- paste0(toupper(substr(pt2,0,1)),substr(pt2,2,nchar(pt2))) #Capitalize subgenus name
  pt3 <- paste0('spp.',substr(pt3,3,nchar(pt3))) #Add spp. to pt3
  return(paste0(pt1,' (',pt2,') ',pt3)) #Paste parts 1-3 together
}
#Posterior p-values (percent overlapping zero)
postP <- function(a){
  upr <- sum(mod1$SNLslope[,1]>0)/length(a)
  lwr <- sum(mod1$SNLslope[,1]<0)/length(a)
  both <- c(upr,lwr); names(both) <- c('upr','lwr')
  return(both)
}


# Basic abundance plots ---------------------------------------------------

#Summary of species
p1 <- group_by(bees,genSpp) %>%
  filter(agricultural==F) %>% #Filter agricultural
  summarize(number=n(),parasitoid=first(parasitoid)) %>%
  filter(number>5) %>%  arrange(desc(number)) %>% 
  rowwise() %>% 
  mutate(genSpp=ifelse(grepl('_',genSpp),fixNames(genSpp),genSpp)) %>%
  mutate(genSpp=factor(genSpp,levels=.$genSpp)) %>% ungroup() %>% 
  arrange(genSpp) %>% 
  mutate(parasitoid=factor(parasitoid,labels=c('Non-parasitic','Nest parasite'))) %>% 
  ggplot(aes(genSpp,number,fill=parasitoid))+geom_col()+
  theme(axis.text.x=element_text(angle=90,vjust=0.25,size=7))+
  labs(x='Species',y='Count',fill='Cleptoparasite')+scale_fill_manual(values=c('black','red'))+
  # scale_y_sqrt()+
  theme(panel.grid.major=element_line(size=0.5,colour='grey90',linetype='solid'),
         panel.grid.minor=element_blank(),axis.text.x=element_text(hjust=1))+
  theme(legend.position=c(0.8,0.9),legend.background=element_rect(fill='white',colour='black',linetype='solid'),
        legend.title=element_blank(),legend.text=element_text(size=12))
  
ggsave('./Figures/countsSpp.png',p1,width=10,height=6)

#Broken down by family
group_by(bees,genSpp) %>%
  filter(agricultural==F) %>% #Filter agricultural
  summarize(number=n(),parasitoid=first(parasitoid),family=first(family)) %>%
  filter(number>5) %>%
  arrange(desc(number)) %>%
  mutate(genSpp=factor(genSpp,levels=.$genSpp)) %>%
  ggplot(aes(genSpp,number,fill=parasitoid))+geom_col()+
  theme(axis.text.x=element_text(angle=90,vjust=0.25,size=7),
        strip.text=element_text(size=7))+
  labs(x='Spp',y='Count')+scale_fill_manual(values=c('black','red'))+
  facet_wrap(~family,scales='free_x',nrow=1)

#Summary of genera
group_by(bees,genus) %>%
  filter(agricultural==F) %>% #Filter agricultural
  summarize(number=n(),parasitoid=first(parasitoid)) %>%
  #filter(number>5) %>%
  arrange(desc(number)) %>%
  mutate(genus=factor(genus,levels=.$genus)) %>%
  ggplot(aes(genus,number,fill=parasitoid))+geom_col()+
  theme(axis.text.x=element_text(angle=90,vjust=0.25,size=7))+
  labs(x='Genus',y='Count')+scale_fill_manual(values=c('black','red'))

#Genus counts broken down by family & year
p1 <- group_by(bees,genus,year) %>%
  filter(agricultural==F) %>% #Filter agricultural
  summarize(number=n(),parasitoid=first(parasitoid),family=first(family)) %>%
  ungroup() %>% arrange(desc(number)) %>%
  mutate(parasitoid=factor(parasitoid,labels=c('Non-parasitic','Nest parasite'))) %>% 
  mutate(genus=factor(genus,levels=unique(genus))) %>%
  ggplot(aes(genus,number,fill=parasitoid))+geom_col()+
  theme(axis.text.x=element_text(angle=90,vjust=0.25,size=7))+
  labs(x='Genus',y='Count',fill='Cleptoparasite')+scale_fill_manual(values=c('black','red'))+
  facet_grid(year~family,scales='free_x')+
  theme(panel.grid.major=element_line(size=0.5,colour='grey90',linetype='solid'),
        panel.grid.minor=element_blank(),axis.text.x=element_text(hjust=1,size=10))+
  theme(legend.position=c(0.9,0.9),legend.background=element_rect(fill='white',colour='black',linetype='solid'),
        legend.title=element_blank(),legend.text=element_text(size=12))
ggsave('./Figures/countsFamilyYear.png',p1,width=10,height=8)

#Summary statistics
bees %>% filter(!parasitoid,!agricultural) %>% 
  group_by(BLID,year) %>% 
  summarize(count=n(),weeks=sum(deployedHours/(24*7)),catchRate=count/weeks) %>% 
  select(BLID,year,catchRate) %>% mutate(year=paste0('y',year)) %>% 
  spread(year,catchRate) %>% data.frame() %>% 
  with(.,lm(y2016~y2015)) %>% summary()
  
  ggplot(aes(y2015,y2016))+geom_point()+geom_abline(intercept=0,slope=1)+
  labs(x='Catches/week 2015',y='Catches/week 2016')

#Catches per week - similar across years
bees %>% filter(!parasitoid,!agricultural) %>% 
  group_by(BLID,year) %>% 
  summarize(count=n(),weeks=sum(deployedHours/(24*7)),catchRate=count/weeks) %>% 
  group_by(year) %>% summarize(meanCatch=mean(catchRate),sdCatch=sd(catchRate))


# Other plots -------------------------------------------------------------

#Shows year-to-year transitions of canola (lots), pastures (not a lot), and SNL (not a lot)
landscape %>%  
  #select(BLID,year,contains('Canola')) %>%
  gather('TypeRadius','Proportion',contains('_')) %>%
  mutate(year=paste0('y',year)) %>%
  spread(year,Proportion) %>%
  separate(TypeRadius,c('Type','Radius'),convert=T) %>%
  ggplot(aes(y2015,y2016))+geom_jitter()+geom_smooth(method='lm',se=F)+facet_wrap(Type~Radius)+
  geom_abline(intercept=0,slope=1,linetype='dashed')+labs(y='2016',x='2015')+
  theme(axis.text=element_text(size=10))


trap %>% #canolaBloom on centered endDate
  filter(BLID %in% unique(trap$BLID)[with(trap,tapply(canolaBloom,BLID,mean,na.rm=T))>0]) %>%
  ggplot(aes(endDate-206,canolaBloom,group=BLID))+geom_line()+facet_wrap(~year)+
  theme(axis.text=element_text(size=7),strip.text=element_text(size=10))+
  labs(x='Centered bloom date',y='Percent Bloom')



# Hypotheses --------------------------------------------------------------

#Spp that use canola will see a boost in populations in the year after. This will be dependent on:
# a) Overlap in flight time and canola bloom period, as well as population in that year
# b) Body size/tongue length? (Kind of Jen's question...) - likely related to usage of canola

#Specific tests:

#Standardized overlap (area under curve using similar flight time)


# Test with top 4 spp caught in year-to-year traps -------------------------------------------

#Names of top4 wild spp
top4=filter(bees,BLID %in% year2year,!agricultural)  %>%
  mutate(year=paste0('y',year)) %>%
  group_by(year,genSpp) %>%
  summarize(number=n()) %>%
  spread(year,number,fill=0) %>%
  mutate(notZero=(y2015>0&y2016>0),diff=abs(y2015-y2016),total=y2015+y2016) %>%
  filter(notZero) %>% #Filter years with zero in one year
  filter((diff/total)<0.5) %>% #Filter sp where magnitude of yearly difference is less than 50% of total count
  arrange(desc(total)) %>%
  top_n(4,total) %>%
  .$genSpp

#Occurance data for top 4 bees
top4bees=filter(bees,BLID %in% year2year) %>%
  filter(genSpp %in% top4)

#Trapping occurance data (start and end of passes)
passes=trap %>%
  filter(BLID %in% year2year) %>%
  select(BLID,pass,year,startDate,midDate,endDate,canolaBloom) %>%
  group_by(BLID,pass,year) %>%
  summarize(start=first(startDate),midDate=first(midDate),end=first(endDate)) %>%
  unite(ID,BLID:year,remove=F) %>%
  arrange(year,BLID,pass)

temp=group_by(top4bees,genSpp,BLID,pass,year) %>% #Abundance by date & trap
  summarize(count=n()) %>%
  unite(ID,BLID:year) %>%
  spread(genSpp,count) %>%
  full_join(passes,by='ID') %>% 
  gather('genSpp','count',2:5) %>%
  mutate(count=ifelse(is.na(count),0,count)) %>% #Changes NAs to zeros
  select(-ID) %>%
  mutate(BLID=factor(BLID)) %>%
  arrange(genSpp,year,BLID,pass)

#Absence/presence plot
temp %>%
  mutate(status=ifelse(count==0,'Absent','Present')) %>%
  mutate(status=factor(status,levels=c('Present','Absent'))) %>%
  ggplot(aes(midDate,BLID))+
  geom_point(aes(fill=status),shape=21)+
  theme(axis.text.y=element_text(size=8))+
  geom_point(aes(start,BLID,size=NULL),col='black',shape=124)+
  geom_point(aes(end,BLID,size=NULL),col='black',shape=124)+
  facet_grid(year~genSpp)+
  scale_fill_manual(values=c('black','white')) +
  labs(shape='Empty',x='Day of year',fill='Status')

#Abundance plot
temp %>%
  mutate(status=ifelse(count==0,'Absent','Present')) %>%
  mutate(status=factor(status,levels=c('Present','Absent'))) %>%
  ggplot(aes(midDate,BLID))+
  geom_point(aes(fill=status,size=(count/(end-start))),shape=21)+
  theme(axis.text.y=element_text(size=8))+
  geom_point(aes(start,BLID,size=NULL),col='black',shape=124,show.legend=F)+
  geom_point(aes(end,BLID,size=NULL),col='black',shape=124,show.legend=F)+
  facet_grid(year~genSpp)+
  scale_fill_manual(values=c('black','white')) +
  labs(shape='Empty',x='Day of year',fill='Status',size='Count')

group_by(top4bees,genSpp,year,midJulian) %>% #Abundance by date and year
  summarize(count=n()) %>%
  ggplot(aes(midJulian,count))+geom_point()+
  geom_smooth(method='gam',formula=y~s(x),method.args=list(family='nb'))+
  facet_grid(year~genSpp)

mutate(top4bees,year=paste0('y',year)) %>% #Do populations in year 2015 relate to year 2016?
  group_by(genSpp,year,BLID) %>%
  summarize(count=n()) %>%
  spread(year,count,fill=0) %>%
  filter(!is.na(y2015)&!is.na(y2016)) %>%
  ggplot(aes(y2015,y2016))+geom_jitter()+geom_abline(slope=1,intercept=0)+
  facet_wrap(~genSpp,scales='free')+geom_smooth(method='lm')+
  #xlim(0,100)+ylim(0,100)+
  labs(x='Number in 2015',y='Number in 2016')
  #Weird outlier patterns...looks like 2016 was a bad year for some spp, but it might have just been because trapping was done at a different time. Should compare shortest overlapping time period.

# Single-year model of wild bees - test using JAGS ---------------  

#Names of top4 wild spp
top4=filter(bees,BLID %in% year2year,!agricultural)  %>%
  mutate(year=paste0('y',year)) %>%
  group_by(year,genSpp) %>%
  summarize(number=n()) %>%
  spread(year,number,fill=0) %>%
  mutate(notZero=(y2015>0&y2016>0),diff=abs(y2015-y2016),total=y2015+y2016) %>%
  filter(notZero) %>% #Filter years with zero in one year
  filter((diff/total)<0.5) %>% #Filter sp where magnitude of yearly difference is less than 50% of total count
  arrange(desc(total)) %>%
  top_n(4,total) %>%
  .$genSpp

#Occurance data for top 4 bees
top4bees=filter(bees,BLID %in% year2year) %>%
  filter(genSpp %in% top4)

#Trapping occurance data (start and end of passes)
passes=trap %>%
  select(BLID,pass,replicate,year,startDate,midDate,endDate,canolaBloom) %>%
  distinct() %>%
  unite(ID,BLID:year,remove=F) %>%
  arrange(year,BLID,pass)

#Anthophora model from 2015 only
temp=group_by(top4bees,genSpp,BLID,pass,replicate,year) %>% #Abundance by date & trap
  summarize(count=n()) %>%
  unite(ID,BLID:year) %>%
  spread(genSpp,count) %>%
  full_join(passes,by='ID') %>% 
  gather('genSpp','count',2:5) %>%
  mutate(count=ifelse(is.na(count),0,count)) %>% #Changes NAs to zeros
  select(-ID) %>%
  mutate(BLID=factor(BLID)) %>%
  arrange(genSpp,year,BLID,pass) %>%
  filter(year==2015,genSpp=='Anthophora terminalis')
  
setwd("~/Projects/UofC/wildbee_canola_project/Models")

library(coda)
library(jagsUI)

datalist=with(temp, #Data to feed into JAGS
              list(
                N=nrow(temp), #Number of total samples
                Nsite=length(unique(BLID)), #Number of sites
                count=count, #Count of bees
                site=as.numeric(BLID), #site index
                traplength=(endDate-startDate)/7, #offset (in weeks)
                centDate=midDate-mean(midDate) #Centered midDate
              )
)

#Starting values for chains (GLMM version)
start=function() list(alpha.mean=dunif(1,-10,10),
                      alpha.sd=dunif(1,-10,10),
                      beta=rnorm(1,0,1)) 
mod1=jags(data=datalist,inits=start,c('alpha.mean','alpha','beta','fit','fit.new'),
          model.file='poissonGLMM_jags.txt',
          n.chains=3,n.adapt=2000,n.iter=10000,n.burnin=1000,n.thin=10,parallel=T)

summary(mod1)
#xyplot(mod1)
#traceplot(mod1) # Many plots...
densityplot(mod1)
pp.check(mod1,'fit','fit.new') #Bad fit... actual doesn't match predicted at all

mod1fit=mod1$samples[[1]] #Results from mod1

b0=mod1fit[,'alpha.mean']
b1=mod1fit[,'beta']
centDate=-23:25

res1=data.frame(date=centDate,fit=NA,upr=NA,lwr=NA)

for(i in 1:nrow(res1)){ #Predictions
  res1$fit[i]=exp(median(b0)+median(b1)*centDate[i])
  res1[i,3:4]=quantile(exp(b0+b1*centDate[i]),c(0.0275,0.975))
}

ggplot(res1,aes(centDate+mean(temp$midDate),fit))+
  geom_point(data=temp,aes(x=midDate,y=count*7/(endDate-startDate)))+
  #geom_line(data=temp,aes(x=midDate,y=count,group=BLID))+
  geom_line(col='red',size=1)+geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
  labs(x='Day of year',y='Predicted count',title='Poisson GLMM')+ylim(0,10)

#NB GLM model - takes longer, but has identical estimates, and better n.eff.

datalist=with(temp, #Data to feed into JAGS
              list(
                N=nrow(temp), #Number of total samples
                Nsite=length(unique(BLID)), #Number of sites
                count=count, #Count of bees
                site=as.numeric(BLID), #site index
                traplength=(endDate-startDate), #offset (in days)
                centDate=midDate-mean(midDate) #Centered midDate
              )
)

start=function() list(alpha.mean=rnorm(1,0,1),
                      alpha.sd=runif(1,0,10),
                      beta=rnorm(1,0,.1),
                      logtheta=rnorm(1,0,.1) #Dispersion
) 
mod2a=jags(data=datalist,inits=start,c('alpha.mean','alpha','beta','logtheta','theta','fit','fit.new'),
           model.file='nbGLMM_jags.txt',
           n.chains=3,n.adapt=2000,n.iter=10000,n.burnin=1000,n.thin=10,parallel=T)

summary(mod2a)
pp.check(mod2a,'fit','fit.new') #Model fits pretty well - p ~ 0.6ish

traceplot(mod2a) 


mod2afit=mod2a$samples[[1]] #Results from mod2a

b0=mod2afit[,'alpha.mean']
theta=mod2afit[,'theta']
b1=mod2afit[,'beta']
centDate=-23:25

res2=data.frame(date=centDate,fit=NA,upr=NA,lwr=NA)

for(i in 1:nrow(res2)){ #Predictions
  res2$fit[i]=exp(median(b0)+median(b1)*centDate[i])
  res2[i,3:4]=quantile(exp(b0*theta+b1*centDate[i]),c(0.0275,0.975))
}

#Overall
ggplot(res2,aes(centDate+mean(temp$midDate),fit))+
  geom_point(data=temp,aes(x=midDate,y=count*7/(endDate-startDate)),position=position_jitter())+
  #geom_line(data=temp,aes(x=midDate,y=count,group=BLID))+
  geom_line(col='red',size=1)+geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
  labs(x='Day of year',y='Predicted count',title='NB GLMM')+ylim(0,10)

#Gaussian process model - no assumptions based on time



  

# Single-year estimation of canola bloom ----------------------------------

setwd("~/Projects/UofC/wildbee_canola_project/Models")

library(coda)
library(jagsUI)

#Names of top4 wild spp
top4=filter(bees,BLID %in% year2year,!agricultural)  %>%
  mutate(year=paste0('y',year)) %>%
  group_by(year,genSpp) %>%
  summarize(number=n()) %>%
  spread(year,number,fill=0) %>%
  mutate(notZero=(y2015>0&y2016>0),diff=abs(y2015-y2016),total=y2015+y2016) %>%
  filter(notZero) %>% #Filter years with zero in one year
  filter((diff/total)<0.5) %>% #Filter sp where magnitude of yearly difference is less than 50% of total count
  arrange(desc(total)) %>%
  top_n(4,total) %>%
  .$genSpp

#Occurance data for top 4 bees
top4bees=filter(bees,BLID %in% year2year) %>%
  filter(genSpp %in% top4)

#Trapping occurance data (start and end of passes)
passes=trap %>%
  select(BLID,pass,replicate,year,startDate,midDate,endDate,canolaBloom) %>%
  distinct() %>%
  unite(ID,BLID:year,remove=F) %>%
  arrange(year,BLID,pass)

#Anthophora model from 2015 only
temp=group_by(top4bees,genSpp,BLID,pass,replicate,year) %>% #Abundance by date & trap
  summarize(count=n()) %>%
  unite(ID,BLID:year) %>%
  spread(genSpp,count) %>%
  full_join(passes,by='ID') %>%
  gather('genSpp','count',2:5) %>%
  mutate(count=ifelse(is.na(count),0,count)) %>% #Changes NAs to zeros
  select(-ID) %>%
  mutate(BLID=factor(BLID)) %>%
  arrange(genSpp,year,BLID,pass) %>%
  filter(year==2016,genSpp=='Anthophora terminalis')

# #Fixed effects: each site has mu, sigma
# datalist=with(temp, #Data to feed into JAGS
#               list(
#                 N=nrow(temp), #Number of total samples
#                 Nsite=length(unique(BLID)), #Number of sites
#                 # count=count, #Count of bees
#                 site=as.numeric(BLID), #site index
#                 # traplength=(endDate-startDate)/7, #offset (in weeks)
#                 centEndDate=endDate-mean(midDate), #Centered midDate (using mean of midDate)
#                 canolaBloom=canolaBloom, #Bloom
#                 nearCanola=as.numeric(with(temp,tapply(canolaBloom,BLID,sum,na.rm=T))>0), #Is field near canola?
#                 lwrRange=-40, #Range of dates to integrate over
#                 uprRange=30,
#                 intWidth=1 #Width of rectangles to integrate across
#               )
# )
# #Sets all fields with zero canola to NA
# datalist$canolaBloom[temp$BLID %in% with(temp,unique(BLID)[tapply(canolaBloom,BLID,sum)==0])]=NA
# 
# #Starting values
# start=function() list(mu.canola=rnorm(1,0,0.01),
#                       sigma.canola=runif(1,10,20),
#                       resid.canola=rgamma(1,0.1,0.1))
# mod3=jags(data=datalist,inits=start,c('mu.canola','sigma.canola','resid.canola'),
#           model.file='canolaGaussian.txt',
#           n.chains=3,n.adapt=500,n.iter=2000,n.burnin=500,n.thin=5,parallel=F)
# summary(mod3) #This model works OK - very small sigma, though
# xyplot(mod3)
# densityplot(mod3)
# 
# mod3fit=mod3$samples[[1]] #Results from mod3
# 
# mu.canola=mod3fit[,'mu.canola']
# sigma.canola=mod3fit[,'sigma.canola']
# centDate=-20:27
# 
# res3=data.frame(date=centDate,fit=NA,upr=NA,lwr=NA)
# 
# for(i in 1:nrow(res3)){ #Predictions
#   res3$fit[i]=100*exp(-0.5*((centDate[i]-median(mu.canola))/median(sigma.canola))^2)
#   res3[i,3:4]=quantile(100*exp(-0.5*((centDate[i]-mu.canola)/sigma.canola)^2),c(0.0275,0.975))
# }
# 
# #Looks OK, but lots of site-to-site variability
# ggplot(res3,aes(centDate+mean(temp$midDate),fit))+
#   geom_point(data=temp,aes(x=endDate,y=canolaBloom))+
#   geom_line(data=temp,aes(x=endDate,y=canolaBloom,group=BLID))+
#   geom_line(col='red',size=1)+geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
#   labs(x='Day of year',y='Predicted bloom')


#Mixed-effects: each site can have a different mu and sigma
datalist=with(temp, #Data to feed into JAGS
              list(
                N=nrow(temp), #Number of total samples
                Nsite=length(unique(BLID)), #Number of sites
                # count=count, #Count of bees
                site=as.numeric(BLID), #site index
                # traplength=(endDate-startDate)/7, #offset (in weeks)
                centStartDate=endDate-206, #Centered startDate
                centEndDate=endDate-206, #Centered midDate
                canolaBloom=canolaBloom, #Proportion Bloom
                nearCanola=as.numeric(with(temp,tapply(canolaBloom,BLID,sum,na.rm=T))>0), #Is field near canola?
                lwrRange=-40, #Range of dates to integrate over
                uprRange=30,
                intWidth=1, #Width of rectangles to integrate across (days)
                NintRange=30-(-40)/1, #Number of rectangles (days)
                BloomThresh=0.15 #Threshold value (15%), after which foragers can use canola
              )
)
#Sets all fields with zero canola to NA
datalist$canolaBloom[temp$BLID %in% with(temp,unique(BLID)[tapply(canolaBloom,BLID,sum)==0])]=NA

start=function() list(mu.canola=rnorm(1,-15,0.1),
                      sigma.mu.site=rgamma(1,1,0.5),
                      resid.canola=rgamma(1,0.1,0.1),
                      sigma.canola=rgamma(1,3,.1),
                      sigma.sigma.site=rgamma(1,3,1)
                      )
mod4=jags(data=datalist,
          inits=start,c('mu.canola','sigma.mu.site', #Overall
                        'resid.canola',
                        'sigma.canola','sigma.sigma.site',
                        'mu.site.canola','sigma.site.canola', #Site-level
                        'totalCanola',
                        'fit','fit.new'), #PP-checks
          model.file='canolaGaussianMixed.txt',
          n.chains=1,n.adapt=2000,n.iter=23000,n.burnin=3000,n.thin=20,parallel=T)
summary(mod4)

traceplot(mod4,parameters=c('mu.canola','sigma.mu.site',
                            'resid.canola','sigma.canola','sigma.sigma.site'))

pp.check(mod4,'fit','fit.new') #Looks OK once residuals near boundary conditions have been dealt with

mod4fit=as.mcmc(mod4$samples[[1]]) #1st chain

#Trace of mean of canola bloom (in fields near canola)
plot(mod4fit[,grepl('mu.site',colnames(mod4fit))][,which(datalist$nearCanola==1)]+mod4fit[,'mu.canola'])
#Trace of sigma of canola bloom (in fields near canola)
plot(mod4fit[,grepl('sigma.site',colnames(mod4fit))][,which(datalist$nearCanola==1)]+mod4fit[,'sigma.canola'])
#Trace of integral of canola (in fields near canola)
plot(mod4fit[,grepl('totalCanola',colnames(mod4fit))][,which(datalist$nearCanola==1)])

round(HPDinterval(mod4fit),2)

mod4fit=mod4$samples[[1]] #Results from 1st chain of mod4
pars=apply(mod4fit,2,median)
pars=pars[grepl('site.canola',names(pars))] #Strips out everything except site estimates
pars=matrix(pars,ncol=2,dimnames=list(unique(temp$BLID),c('mu','sigma')))
pars=pars[which(datalist$nearCanola>0),]
centDate=-20:27

#Cheapo way of binding dataframes
res4=matrix(NA,length(centDate),nrow(pars),dimnames=list(centDate,rownames(pars)))

#Predictions
for(i in 1:nrow(res4)){ #For each date
  for(j in 1:ncol(res4)){ #For each site
    res4[i,j]=round(100*exp(-0.5*((centDate[i]-pars[j,1])/pars[j,2])^2),2)
  }
}
res4=data.frame(centDate=centDate,res4)
res4=gather(res4,'BLID','fit',2:13)
res4$BLID=gsub('X','',res4$BLID)

#Looks OK
# ggplot(res4,aes(centDate+mean(temp$midDate),fit,group=BLID))+
ggplot(res4,aes(centDate,fit,group=BLID))+
  geom_point(data=temp,aes(x=endDate-mean(temp$midDate),y=canolaBloom,group=BLID))+
  #geom_line(data=temp,aes(x=endDate,y=canolaBloom,group=BLID))+
  geom_line(col='red',size=1)+
  facet_wrap(~BLID)+
  labs(x='Day of year',y='Percent bloom')+
  theme(axis.text=element_text(size=7),strip.text=element_text(size=10))

#Fixed effects: each field can have a different mu & sigma, and is estimated independently (no hyperprior)

#Which fields are near canola
whichNearCanola <- as.numeric(with(temp,tapply(canolaBloom,BLID,sum,na.rm=T))>0)
whichNearCanola[whichNearCanola==1] <- 1:sum(whichNearCanola) #Index of means to estimate

datalist=with(temp, #Data to feed into JAGS
              list(
                N=nrow(temp), #Number of total samples
              #  Nsite=length(unique(BLID)), #Number of sites
                # count=count, #Count of bees
                site=as.numeric(BLID), #site index
                # traplength=(endDate-startDate)/7, #offset (in weeks)
                centEndDate=endDate-206, #Centered midDate
                canolaBloom=canolaBloom, #Proportion Bloom
                whichNearCanola=whichNearCanola+1, #Which fields are near canola (>0), and what their order is
                muField=rep(-5,sum(whichNearCanola>0)+1), #Mean bloom time (for all site with canola)
                precMuField=diag(0.05,sum(whichNearCanola>0)+1), #Precision for mu
                # sigmaField=rep(1,sum(with(temp,tapply(canolaBloom,BLID,sum,na.rm=T))>0)), #Spread of bloom time
                # precSigmaField=diag(0.1,sum(with(temp,tapply(canolaBloom,BLID,sum,na.rm=T))>0)) #Precision for sigma
                sigmaField=2, #Spread of bloom time
                precSigmaField=0.1 #Precision for sigma
                # lwrRange=-40, #Range of dates to integrate over
                # uprRange=30,
                # intWidth=1, #Width of rectangles to integrate across
                # NintRange=30-(-40)/1 #Number of rectangles
              )
)
#Sets all fields with zero canola to NA
datalist$canolaBloom[temp$BLID %in% with(temp,unique(BLID)[tapply(canolaBloom,BLID,sum)==0])]=NA

start=function() list(mu.canola=rnorm(sum(whichNearCanola>0)+1,-5,0.1),
                      sigma.canola=rep(10,sum(whichNearCanola>0)+1))

mod4=jags(data=datalist,
          inits=start,c('mu.canola','sigma.canola', #Overall
                        'resid.canola'),
          model.file='canolaGaussianMixed3.txt',
          n.chains=1,n.adapt=2000,n.iter=21000,n.burnin=1000,n.thin=20,parallel=F)
summary(mod4)
plot(mod4)

#Get predictions from models
sampMod4 <- mod4$samples[[1]] #Chain 1
pred <- expand.grid(Date=c(-23:36),Field=c(1:8),fit=NA,upr=NA,lwr=NA) #Data frame for predictions
predfun <- function(day,mu,sigma) {
    return(round(quantile(100*exp(-0.5*((day-mu)/sigma)^2),c(0.5,0.05,0.95)),2))
}

for(i in 1:nrow(pred)){
  pred[i,c(3:5)] <- predfun(pred$Date[i],
          sampMod4[,grep('mu',colnames(sampMod4))[pred$Field[i]]],
          sampMod4[,grep('sigma',colnames(sampMod4))[pred$Field[i]]])
}

plotdata=filter(temp,as.numeric(temp$BLID) %in% which(whichNearCanola>0)) %>% 
  mutate(Field=droplevels(BLID))

filter(pred,Field!=1) %>% mutate(Field=factor(Field-1,labels=levels(plotdata$Field))) %>%  
  ggplot(aes(Date+206,fit))+geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
  geom_line()+facet_wrap(~Field)+
  geom_point(data=plotdata,aes(x=endDate,y=canolaBloom))+
  labs(y='Canola bloom',x='Day of Year')


# Main model run in JAGS ----------------------------------------------------------

setwd("~/Projects/UofC/Wild bee time project/Models")

library(coda)
library(jagsUI)

#Names of top4 wild spp
top4=filter(bees,BLID %in% year2year,!agricultural)  %>%
  mutate(year=paste0('y',year)) %>%
  group_by(year,genSpp) %>%
  summarize(number=n()) %>%
  spread(year,number,fill=0) %>%
  mutate(notZero=(y2015>0&y2016>0),diff=abs(y2015-y2016),total=y2015+y2016) %>%
  filter(notZero) %>% #Filter years with zero in one year
  filter((diff/total)<0.5) %>% #Filter sp where magnitude of yearly difference is less than 50% of total count
  arrange(desc(total)) %>%
  top_n(4,total) %>%
  .$genSpp

#Occurance data for top 4 bees
top4bees=filter(bees,BLID %in% year2year) %>%
  filter(genSpp %in% top4)

#Trapping occurance data (start and end of passes)
passes=trap %>%
  select(BLID,pass,replicate,year,startDate,midDate,endDate,canolaBloom) %>%
  distinct() %>%
  unite(ID,BLID:year,remove=F) %>%
  arrange(year,BLID,pass)

#Anthophora model from both years
temp=group_by(top4bees,genSpp,BLID,pass,replicate,year) %>% #Abundance by date & trap
  summarize(count=n()) %>%
  unite(ID,BLID:year) %>%
  spread(genSpp,count) %>%
  full_join(passes,by='ID') %>% 
  gather('genSpp','count',2:5) %>%
  mutate(count=ifelse(is.na(count),0,count)) %>% #Changes NAs to zeros
  select(-ID) %>%
  mutate(BLID=factor(BLID)) %>%
  arrange(genSpp,year,BLID,pass) %>%
  filter(genSpp=='Anthophora terminalis') %>%
  select(-genSpp)

#Landscape stuff at 250m radius, for each site
templandscape=landscape %>%
  gather('type','proportion',Canola_100:Seminatural_500) %>%
  filter(grepl('_250',type)) %>%
  unite(type,type,year) %>%
  spread(type,proportion) %>%
  rowwise() %>%
  mutate(Seminatural_250=mean(Seminatural_250_2015,Seminatural_250_2016)) %>%
  select(-Seminatural_250_2015,-Seminatural_250_2016) 

centDateOffset=median(temp$midDate) #Median day to use in all analysis

temp2015=temp %>% #Data from 2015
  filter(year==2015) %>%
  select(-year) %>%
  mutate(nearCanola=BLID %in% with(templandscape,BLID[Canola_250_2015>0])) %>%
  mutate(canolaBloom=ifelse(nearCanola,canolaBloom,NA)) %>%
  mutate(traplength=endDate-startDate)

temp2016=temp %>% #Data from 2016
  filter(year==2016) %>%
  select(-year) %>%
  mutate(nearCanola=BLID %in% with(templandscape,BLID[Canola_250_2016>0])) %>%
  mutate(canolaBloom=ifelse(nearCanola,canolaBloom,NA)) %>%
  mutate(traplength=endDate-startDate)

datalist=list(
  Nsite=nrow(templandscape), #Number of sites
  N2015=nrow(temp2015), #N samples
  N2016=nrow(temp2016), 
  sites2015=as.numeric(temp2015$BLID), #Site indices
  sites2016=as.numeric(temp2016$BLID),
  prop.canola2015=templandscape$Canola_250_2015, #Proportion of canola in landscape
  prop.canola2016=templandscape$Canola_250_2016,
  prop.SNL=templandscape$Seminatural_250, #Proportion of SNL
  centEndDate2015=temp2015$endDate-centDateOffset, #Date of collection
  centEndDate2016=temp2016$endDate-centDateOffset,
  centMidDate2015=temp2015$midDate-centDateOffset, #Midpoint b/w collections
  centMidDate2016=temp2016$midDate-centDateOffset,
  traplength2015=temp2015$traplength, #Trapping length (days)
  traplength2016=temp2016$traplength,
  canolaBloom2015=temp2015$canolaBloom, #Canola bloom
  canolaBloom2016=temp2016$canolaBloom,
  #canolaSites2015=unname(which(with(temp2015,tapply(nearCanola,BLID,sum)>0))), #Sites near canola
  #canolaSites2016=unname(which(with(temp2016,tapply(nearCanola,BLID,sum)>0))),
  count2015=temp2015$count, #Anthophora terminalis count
  count2016=temp2016$count,
  NintRange=max(temp$endDate-centDateOffset)-min(temp$startDate-centDateOffset), #Range of days
  lwrRange=min(temp$startDate-centDateOffset)#,
  #uprRange=max(temp$endDate-centDateOffset)
)

start=function() list(
  mu.canola2015 = rnorm(1,-12,0.5), #Prior for mean of bloom   
  sigma.mu.site2015 = rgamma(1,1,0.5), #Precision for mean of bloom (1/SD^2)
  sigma.canola2015 = rgamma(1,3,.1), #Shape factor for generating SD of bloom
  sigma.sigma.site2015 = rgamma(1,3,1), #Rate for generating SD
  
  resid.canola2015 = rgamma(1,0.1,0.1), #Precision for "Residual"  
  
  intN2015.mean = rnorm(1,0,0.1), #Intercept mean
  intN2015.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
  slopeN2015 = rnorm(1,0,0.1), #Slope of Count-Time relationship
  logDispN2015 = rnorm(1,0.5,0.1), #Dispersion parameter
  
  mu.canola2016 = rnorm(1,-6,0.5), #Prior for mean of bloom   
  sigma.mu.site2016 = rgamma(1,1,0.5), #Precision for mean of bloom (1/SD^2)
  sigma.canola2016 = rgamma(1,3,.1), #Shape factor for generating SD of bloom
  sigma.sigma.site2016 = rgamma(1,3,1), #Rate for generating SD
  
  resid.canola2016 = rgamma(1,0.1,0.1), #Precision for "Residual"  
  intN2016.mean = rnorm(1,0,0.1), #Intercept mean
  intN2016.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
  slopeN2016 = rnorm(1,0,0.1), #Slope of Count-Time relationship
  logDispN2016 = rnorm(1,0.5,0.1), #Dispersion parameter
  
  canola.slope = rnorm(1,0,0.1), #Slope of canola bloom on count (0 = neutral, + = repelling, - = attracting)  
  carryover.slope2015 = rnorm(1,1,0.1), #Slope of carryover due to last year's population
  SNL.slope2015 = rnorm(1,1,0.1), # Slope of SNL effect on Site-level Intercept
  SNL.slope2016 = rnorm(1,0,0.1), # Slope of SNL effect on Site-level Intercept 
  overlap.slope = rnorm(1,0,0.1) #Slope of total overlap (2015) on intercept (2016) 
) 

library(beepr)
mainMod=jags(data=datalist,
          inits=start,parameters.to.save=
          c('mu.canola2015','sigma.canola2015',
            'intN2015.mean','slopeN2015','dispN2015', #2015 variables
            'mu.canola2016','sigma.canola2016',
            'intN2016.mean','slopeN2016','dispN2016', #2016 variables
            'canola.slope', #Overall variables
            'carryover.slope2015',
            'SNL.slope2015','SNL.slope2016',
            'overlap.slope',
            'mu.site.canola2015','sigma.site.canola2015', #Site variables
            'mu.site.canola2016','sigma.site.canola2016',
            'intN2015.site','intN2016.site',
            'fit.canola2015','fitNew.canola2015','fit.count2015','fitNew.count2015', #post.pred. checks
            'fit.canola2016','fitNew.canola2016','fit.count2016','fitNew.count2016'),
          model.file='main_model.txt',
          n.chains=3,n.adapt=3000,n.iter=6500,n.burnin=500,n.thin=6,parallel=T)
beepr::beep(2)
save(mainMod,file='mainMod.Rdata') #Saves data for later
# load('mainMod.Rdata')

summary(mainMod) #Canola mean/SD estimates are diverging. #Tried using dunif priors for bloom SD, but was slow, and produced worse estimates

traceplot(mainMod,parameters=c('mu.canola2015','sigma.canola2015','intN2015.mean','slopeN2015', #2015 variables
                               'mu.canola2016','sigma.canola2016','intN2016.mean','slopeN2016', #2016 variables
                               'canola.slope', #Overall variables
                               'carryover.slope2015',
                               'SNL.slope2015','SNL.slope2016',
                               'overlap.slope'))
# xyplot(mainMod)
# densityplot(mainMod)

pp.check(mainMod,'fit.canola2015','fitNew.canola2015') #OK-ish
pp.check(mainMod,'fit.canola2016','fitNew.canola2016') #Not good, even with separate parameters
pp.check(mainMod,'fit.count2015','fitNew.count2015') #OK
pp.check(mainMod,'fit.count2016','fitNew.count2016') #OK

modfit=as.mcmc(mainMod$samples[[1]]) #1st chain
modfit2=data.frame(par=colnames(modfit),parMeans=colMeans(modfit),parSD=apply(modfit,2,sd),
          parMed=round(apply(modfit,2,median),3),
          parUpr=apply(modfit,2,function(x) quantile(x,0.975)),
          parLwr=apply(modfit,2,function(x) quantile(x,0.025))) %>%
  mutate(parRange=round(parUpr-parLwr,3),parEff=round(parMed/parRange,3)) %>%
  select(par,parMed,parRange,parEff)
  
head(modfit2,15)

# Trace of mean of canola bloom (in fields near canola)
plot(modfit[,grepl('mu.site',colnames(modfit))][,which(with(datalist,tapply(nearCanola2015,sites2015,sum)>0))])
#Trace of sigma of canola bloom (in fields near canola)
plot(modfit[,grepl('sigma.site',colnames(modfit))][,which(with(datalist,tapply(nearCanola2015,sites2015,sum)>0))])

# Trace of mean of canola bloom (in fields near canola)
plot(modfit[,grepl('mu.site',colnames(modfit))][,which(with(datalist,tapply(nearCanola2016,sites2016,sum)>0))])
#Trace of sigma of canola bloom (in fields near canola)
plot(modfit[,grepl('sigma.site',colnames(modfit))][,which(with(datalist,tapply(nearCanola2016,sites2016,sum)>0))])

round(HPDinterval(modfit),2)

pars=apply(modfit,2,median)
pars=pars[grepl('site.canola',names(pars))] #Strips out everything except site estimates
pars=matrix(pars,ncol=4,dimnames=list(unique(temp$BLID),c('mu','sigma','mu2016','sigma2016')))
pars=pars[with(temp,tapply(canolaBloom,BLID,sum,na.rm=T)>0),]
pars=cbind(rbind(pars[,1:2],pars[,3:4]),rep(c(2015,2016),each=nrow(pars)))
centDate=with(datalist,seq(lwrRange,lwrRange+NintRange,1))


#Alternate run, using same priors for canola bloom in each year

datalist=list(
  Nsite=nrow(templandscape), #Number of sites
  N2015=nrow(temp2015), #N samples
  N2016=nrow(temp2016), 
  sites2015=as.numeric(temp2015$BLID), #Site indices
  sites2016=as.numeric(temp2016$BLID),
  prop.canola2015=templandscape$Canola_250_2015, #Proportion of canola in landscape
  prop.canola2016=templandscape$Canola_250_2016,
  prop.SNL=templandscape$Seminatural_250, #Proportion of SNL
  centEndDate2015=temp2015$endDate-centDateOffset, #Date of collection
  centEndDate2016=temp2016$endDate-centDateOffset,
  centMidDate2015=temp2015$midDate-centDateOffset, #Midpoint b/w collections
  centMidDate2016=temp2016$midDate-centDateOffset,
  traplength2015=temp2015$traplength, #Trapping length (days)
  traplength2016=temp2016$traplength,
  canolaBloom2015=temp2015$canolaBloom, #Canola bloom
  canolaBloom2016=temp2016$canolaBloom,
  # nearCanola2015=as.numeric(temp2015$nearCanola), #Measurements near canola
  # nearCanola2016=as.numeric(temp2016$nearCanola),
  # canolaSites2015=as.numeric(with(temp2015,tapply(nearCanola,BLID,sum)>0)), #Sites near canola
  # canolaSites2016=as.numeric(with(temp2016,tapply(nearCanola,BLID,sum)>0)),
  count2015=temp2015$count, #Anthophora terminalis count
  count2016=temp2016$count,
  NintRange=max(temp$endDate-centDateOffset)-min(temp$startDate-centDateOffset), #Range of days
  lwrRange=min(temp$startDate-centDateOffset)#,
  #uprRange=max(temp$endDate-centDateOffset)
)


start=function() list(
  mu.canola = rnorm(1,-7,0.01), #Prior for mean of bloom   
  sigma.mu.site = rgamma(1,1,0.5), #SD for mean of bloom 
  #tau.mu.site = rgamma(1,0.1,0.1), #Precision for mean of bloom (1/SD^2)
  sigma.canola = rgamma(1,1,1), #Shape factor for generating SD of bloom
  sigma.sigma.site = rgamma(1,1,1), #Rate for generating SD of bloom
  
  resid.canola = rgamma(1,0.1,0.1), #Precision for "Residual"  
  
  intN2015.mean = rnorm(1,0,0.01), #Intercept mean
  intN2015.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
  slopeN2015 = rnorm(1,0,0.01), #Slope of Count-Time relationship
  logDispN2015 = rnorm(1,0.5,0.01), #Dispersion parameter
  
  intN2016.mean = rnorm(1,0,0.01), #Intercept mean
  intN2016.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
  slopeN2016 = rnorm(1,0,0.001), #Slope of Count-Time relationship
  logDispN2016 = rnorm(1,0.5,0.01), #Dispersion parameter
  
  canola.slope = rnorm(1,0,0.01), #Slope of canola bloom on count (0 = neutral, + = repelling, - = attracting)  
  SNL.slope2015 = rnorm(1,1,1), # Slope of SNL effect on Site-level Intercept
  SNL.slope2016 = rnorm(1,0,1), # Slope of SNL effect on Site-level Intercept 
  overlap.slope = rnorm(1,0,0.01) #Slope of total overlap (2015) on intercept (2016) 
)

# start=function() list(
#   
#   mu.canola = rnorm(1,-15,1), #Prior for mean of bloom   
#   tau.mu.site = rgamma(1,1,0.5), #Precision (1/sqrt(SD)) for mean of bloom
#   sigma.canola = rgamma(1,.1,.1), #Shape factor for generating SD of bloom
#   resid.canola = rgamma(1,0.1,0.1), #Precision for "Residual"  
#   
#   intN2015.mean = rnorm(1,0,0.01), #Intercept mean
#   intN2015.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
#   slopeN2015 = rnorm(1,0,1), #Slope of Count-Time relationship
#   logDispN2015 = rnorm(1,0.3,.1), #Dispersion parameter
#   
#   intN2016.mean = rnorm(1,0,1), #Intercept mean
#   intN2016.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
#   slopeN2016 = rnorm(1,0,1), #Slope of Count-Time relationship
#   logDispN2016 = rnorm(1,0.3,.1), #Dispersion parameter
#   
#   canola.slope = rnorm(1,0,1), #Slope of canola bloom on count (0 = neutral, + = repelling, - = attracting)  
#   SNL.slope = rnorm(1,0,1), # Slope of SNL effect on Site-level Intercept - SHOULD THIS BE THE SAME BETWEEN YEARS?  
#   overlap.slope = rnorm(1,0,1) #Slope of total overlap (2015) on intercept (2016) 
# ) 

mainMod2=jags(data=datalist,
             inits=start,parameters.to.save=
               c('mu.canola','sigma.mu.site',
                 'sigma.canola','sigma.sigma.site', #canola bloom variables
                 'resid.canola',
                 'intN2015.mean','slopeN2015', 
                 'intN2016.mean','slopeN2016',
                 'canola.slope','SNL.slope','overlap.slope', #Overall variables
                 'mu.site.canola2015','sigma.site.canola2015', #Site variables
                 'mu.site.canola2016','sigma.site.canola2016',
                 'intN2015.site','intN2016.site',
                 'dispN2015','dispN2016', #Dispersion parameters
                 'fit.canola2015','fitNew.canola2015','fit.count2015','fitNew.count2015', #post.pred. checks
                 'fit.canola2016','fitNew.canola2016','fit.count2016','fitNew.count2016'),
             model.file='main_model2.txt',
             n.chains=3,n.adapt=2000,n.iter=5200,n.burnin=200,n.thin=10,parallel=T)
beepr::beep(1)
save(mainMod2,file='mainMod2.Rdata') #Saves data for later
# load('mainMod2.Rdata')

traceplot(mainMod2,parameters=c('mu.canola','sigma.mu.site',
                                'sigma.canola','sigma.sigma.site', #canola bloom variables
                                'resid.canola',
                                'intN2015.mean','slopeN2015', 
                                'intN2016.mean','slopeN2016',
                                'canola.slope','SNL.slope','overlap.slope', #Overall variables
                                'dispN2015','dispN2016'
                                ))

summary(mainMod2) #Looks OK, but estimates for hyperparameters are still terrible

pp.check(mainMod2,'fit.canola2015','fitNew.canola2015') 
pp.check(mainMod2,'fit.canola2016','fitNew.canola2016') #No clusters, but simulated > actual
pp.check(mainMod2,'fit.count2015','fitNew.count2015') 
pp.check(mainMod2,'fit.count2016','fitNew.count2016') 

modfit=as.mcmc(mainMod2$samples[[1]]) #1st chain

round(HPDinterval(modfit),2)

#Alternate model only estimating bloom using fields next to canola (rather than using NAs)

datalist=list(
  Nsite=nrow(templandscape), #Number of sites
  N2015=nrow(temp2015), #N samples
  N2016=nrow(temp2016), 
  sites2015=as.numeric(temp2015$BLID), #Site indices
  sites2016=as.numeric(temp2016$BLID),
  canolaSites2015=unname(which(with(temp2015,tapply(nearCanola,BLID,sum)>0))), #Index of sites with canola near them
  canolaSites2016=unname(which(with(temp2016,tapply(nearCanola,BLID,sum)>0))),
  canolaSitesIndex2015=match(1:nrow(templandscape),
                             unname(which(with(temp2015,tapply(nearCanola,BLID,sum)>0)))), 
  canolaSitesIndex2016=match(1:nrow(templandscape),
                             unname(which(with(temp2016,tapply(nearCanola,BLID,sum)>0)))),
  # nearCanola2015=unname(with(temp2015,tapply(nearCanola,BLID,mean))), #Near canola
  # nearCanola2016=unname(with(temp2016,tapply(nearCanola,BLID,mean))),
  prop.canola2015=templandscape$Canola_250_2015, #Proportion of canola in landscape
  prop.canola2016=templandscape$Canola_250_2016,
  prop.SNL=templandscape$Seminatural_250, #Proportion of SNL
  centEndDate2015=temp2015$endDate-centDateOffset, #Date of collection
  centEndDate2016=temp2016$endDate-centDateOffset,
  centMidDate2015=temp2015$midDate-centDateOffset, #Midpoint b/w collections
  centMidDate2016=temp2016$midDate-centDateOffset,
  traplength2015=temp2015$traplength, #Trapping length (days)
  traplength2016=temp2016$traplength,
  canolaBloom2015=temp2015$canolaBloom, #Canola bloom
  canolaBloom2016=temp2016$canolaBloom,
  count2015=temp2015$count, #Anthophora terminalis count
  count2016=temp2016$count,
  NintRange=max(temp$endDate-centDateOffset)-min(temp$startDate-centDateOffset), #Range of days
  lwrRange=min(temp$startDate-centDateOffset)#,
  #uprRange=max(temp$endDate-centDateOffset)
)

start=function() list(
  mu.canola2015 = rnorm(1,-7,0.01), #Prior for mean of bloom   
  tau.mu.site2015 = rgamma(1,0.1,0.1), #Precision for mean of bloom (1/SD^2)
  sigma.canola2015 = rgamma(1,1,.1), #Shape factor for generating SD of bloom
  
  resid.canola2015 = rgamma(1,0.1,0.1), #Precision for "Residual"  
  intN2015.mean = rnorm(1,0,0.01), #Intercept mean
  intN2015.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
  slopeN2015 = rnorm(1,0,0.01), #Slope of Count-Time relationship
  logDispN2015 = rnorm(1,0,0.01), #Dispersion parameter
  
  mu.canola2016 = rnorm(1,-7,0.01), #Prior for mean of bloom   
  tau.mu.site2016 = rgamma(1,0.1,0.1), #Precision for mean of bloom (1/SD^2)
  sigma.canola2016 = rgamma(1,1,.1), #Shape factor for generating SD of bloom
  
  resid.canola2016 = rgamma(1,0.1,0.1), #Precision for "Residual"  
  intN2016.mean = rnorm(1,0,0.01), #Intercept mean
  intN2016.prec = rgamma(1,0.1,0.1), #Precision of Intercept SD
  slopeN2016 = rnorm(1,0,0.001), #Slope of Count-Time relationship
  logDispN2016 = rnorm(1,0,0.01), #Dispersion parameter
  
  canola.slope = rnorm(1,0,0.01), #Slope of canola bloom on count (0 = neutral, + = repelling, - = attracting)
  SNL.slope = rnorm(1,0,0.01), # Slope of SNL effect on Site-level Intercept 
  overlap.slope = rnorm(1,0,0.01) #Slope of total overlap (2015) on intercept (2016) 
)

mainMod3=jags(data=datalist,
              inits=start,parameters.to.save=
                c('mu.canola','sigma.canola', #canola bloom variables
                  'intN2015.mean','slopeN2015', 
                  'intN2016.mean','slopeN2016',
                  'canola.slope','SNL.slope','overlap.slope', #Overall variables
                  'mu.site.canola2015','sigma.site.canola2015', #Site variables
                  'mu.site.canola2016','sigma.site.canola2016',
                  'intN2015.site','intN2016.site',
                  'fit.canola2015','fitNew.canola2015','fit.count2015','fitNew.count2015', #post.pred. checks
                  'fit.canola2016','fitNew.canola2016','fit.count2016','fitNew.count2016'),
              model.file='main_model3.txt',
              n.chains=3,n.adapt=500,n.iter=2500,n.burnin=500,n.thin=2,parallel=T)



detach("package:jagsUI", unload=TRUE)

# Test using splines (Lawrence's idea) ------------------------------------



#Single-year model of wild bees - test using Stan ---------------
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
setwd("~/Projects/UofC/wildbee_canola_project/models")
library(shinystan)

#Names of wild spp
wildSpp <- filter(bees,BLID %in% year2year,!agricultural)  %>%
  mutate(year=paste0('y',year)) %>%
  group_by(year,genSpp) %>%
  summarize(number=n()) %>% ungroup() %>% 
  left_join(distinct(select(bees,genSpp,family)),by='genSpp') %>% 
  spread(year,number,fill=0) %>%
  mutate(notZero=(y2015>0&y2016>0),diff=abs(y2015-y2016),total=y2015+y2016) %>%
  filter(notZero) %>% #Filter years with zero in one year
  # filter((diff/total)<0.5) %>% #Filter sp where magnitude of yearly difference is less than 50% of total count
  arrange(desc(total)) %>% mutate(pathName=gsub(' ','_',genSpp)) %>% #Create path name for files
  rowwise() %>% mutate(labelName=ifelse(grepl('_',genSpp),fixNames(genSpp),genSpp)) %>% ungroup 
  
#Occurance data for top 20 bees
wildBees <- filter(bees,BLID %in% year2year) %>%
  filter(genSpp %in% wildSpp$genSpp)

#Trapping occurance data (start and end of passes)
passes <- trap %>%
  select(BLID,pass,replicate,year,startDate,midDate,endDate,canolaBloom) %>%
  distinct() %>%
  unite(ID,BLID:year,remove=F) %>%
  arrange(year,BLID,pass)

#Anthophora model 
temp <- wildBees %>% mutate(genSpp=factor(genSpp,levels=wildSpp$genSpp,labels=gsub(' ','_',wildSpp$genSpp))) %>% 
  group_by(genSpp,BLID,pass,replicate,year) %>% #Abundance by date & trap
  summarize(count=n()) %>% ungroup() %>% 
  unite(ID,BLID:year) %>% spread(genSpp,count) %>%  full_join(passes,by='ID') %>% 
  gather('genSpp','count',contains("_")) %>% 
  mutate(count=ifelse(is.na(count),0,count)) %>% #Changes NAs to zeros
  select(-ID) %>% mutate(BLID=factor(BLID)) %>% arrange(genSpp,year,BLID,pass) %>%
  spread(genSpp,count) %>%  
  mutate(traplength=(endDate-startDate)/7) %>% #Trapping period (in weeks)
  mutate(centDate=(midDate-mean(midDate))/7) %>%  #Centers midDate (around 206.41) and converts to week 
  mutate(centEndDate=(endDate-mean(midDate))/7) %>%  #Centers endDate and coverts to week
  group_by(year,BLID) %>% 
  mutate(nearCanola=any(canolaBloom>0)) %>%   #Was canola bloom recorded at that site?
  data.frame()
  
# Year-to-year landscape proportions (at different radii)
# landscape %>% 
#   select(-contains('Canola')) %>% 
#   unite(Forest,contains('Forest')) %>% 
#   unite(NatGrass,contains('Native')) %>% unite(Pasture,contains('Pasture')) %>% 
#   unite(SNL,contains('Seminatural')) %>% unite(Shrub,contains('Shrub')) %>% 
#   gather('class','numbers',-BLID,-year) %>% 
#   separate(numbers,c('Rad100','Rad250','Rad500'),sep='_') %>% 
#   gather('radius','perc',Rad100:Rad500) %>% 
#   mutate(year=paste0('y',year),perc=as.numeric(perc)) %>%
#   spread(year,perc) %>% 
#   filter(radius!='Rad100')
#   ggplot(aes(y2015,y2016))+geom_point()+
#   facet_grid(class~radius)+
#   geom_abline(slope=1,intercept=0)


#Gaussian process model
datalist <- with(temp, #Data to feed into STAN
              list(
                N=nrow(temp), #Number of total samples
                NperYear=unname(tapply(year,year,length)), #Number of samples per year
                Nsite=length(unique(BLID)), #Number of sites
                Ncanola=sum(!is.na(canolaBloom)), #Number of observed canola measurements
                site=as.numeric(BLID), #site index
                year=temp$year-min(temp$year)+1, #Year of observation
                NperSite=matrix(aggregate(pass~BLID+year,data=temp,length)$pass,ncol=2), #Number of obs per site per year
                count=Anthophora_terminalis, #Anthophora count
                # count=Dufourea_maura,
                # count=Melissodes_confusus,
                # count=Lasioglossum_leucozonium,
                traplength=traplength, #offset (in weeks)
                Ndates2015=length(unique(centDate[year==2015])),
                Ndates2016=length(unique(centDate[year==2016])),
                centDates2015=unique(sort(centDate[year==2015])), #unique dates for 2015
                centDates2016=sort(unique(centDate[year==2016])), #unique dates for 2016
                dateIndex2015=match(centDate[year==2015],sort(unique(centDate[year==2015]))), #Index for 2015 dates
                dateIndex2016=match(centDate[year==2016],sort(unique(centDate[year==2016]))), #Index for 2016 dates 
                centStartDate=centEndDate-traplength, #Start date
                centDate=centDate, #Centered date (between start and end)
                centEndDate=centEndDate, #End date
                centBloomDate=centEndDate[!is.na(canolaBloom)], #Centered end date (used for canola bloom)
                canolaBloom=canolaBloom[!is.na(canolaBloom)], #Observed canola bloom
                bloomIndex=as.numeric(which(!is.na(canolaBloom))), #Index for matching missing observed canola bloom
                nearCanola=as.numeric(nearCanola[!is.na(canolaBloom)]), #Was field during that year near canola?
                distMat=distMat #Distance matrix for sites
              )
)

datalist=c(datalist,with(landscape, #Adds landscape data
     list(
       percSNL=Seminatural_500[year==2015], #Seems to be a fairly good indicator of yearly SNL
       percCanola=cbind(Canola_250[year==2015],Canola_250[year==2016]) #Canola proportion in 2015/2016
     ))
)
str(datalist)

#Initial values
inits <- function() { with(datalist,
   list(rho=c(1.7,0.8),alpha=c(1.2,0.8),b0=c(-1.8,-2.1),b0_site=rep(0,Nsite*1),
        sigma_site=c(1),
        rhoDist=1,alphaDist=1,
        SNLslope=c(0.9,0),slopeLastYear=0.6,slopeCanolaOverlap=-0.3,
        canolaEffect=c(-0.15,0.13),phi=c(1.5,0.9),
        muDateCanola=c(-2,-1),sigmaDateCanola=c(1.5,1.5),ampCanola=c(75,97)
   ))
}

# with(datalist,data.frame(dates=centDate,count,year,site)) %>% 
#   ggplot(aes(dates,count))+geom_point()+geom_line(aes(group=site))+facet_wrap(~year)+
#   scale_y_log10()
# 
#Run Overall model
datalist$count <- rowSums(temp[,c(9:96)]) #Sum all of bee spp
modGP <- stan(file='gpMod3_spatial.stan',data=datalist,iter=3000,chains=3,control=list(adapt_delta=0.85),init=inits)
save(modGP,file='Overall.Rdata')

#Run model for top 20 bees
for(i in wildSpp$pathName[c(1:20)]){
  datalist$count <- temp[,i] 
  modGP <- stan(file='gpMod3_spatial.stan',data=datalist,iter=2000,chains=3,control=list(adapt_delta=0.85),init=inits)
  
  save(modGP,file=paste0(i,'.Rdata'))
  print(paste('Finished',i))
}

pars <- c('rho','alpha','b0','rhoDist','alphaDist','SNLslope','slopeLastYear','slopeCanolaOverlap','canolaEffect','phi','thetaZI')
# pars=c('muCanola','sigmaCanola','ampCanola','residCanola') #OK

#Make basic diagnostic plots for each model
for(i in c(wildSpp$pathName[1:20],'Overall')){
  load(paste0(i,'.Rdata'))
  p1 <- stan_hist(modGP,pars=pars)+labs(title=i)+geom_vline(xintercept=0,linetype='dashed') #Posterior distribution
  ggsave(paste0('../Figures/Diagnostic Plots/',i,'_dist.png'),p1,width=14,height=8)
  p2 <- traceplot(modGP,pars=pars) #Traceplot
  ggsave(paste0('../Figures/Diagnostic Plots/',i,'_trace.png'),p2,width=12,height=8)
  mod1 <- extract(modGP)
  
  #Actual counts
  if(i=='Overall') act <- rowSums(temp[,c(9:96)]) else act <- temp[,i]

  #PP plots
  png(file =paste0('../Figures/Diagnostic Plots/',i,'_PP.png'),
      width=6,height=12,units='in',res=100,bg = "white")
  with(mod1,PPplots(apply(count_resid,1,function(x) sum(abs(x))),
                    apply(predCount_resid,1,function(x) sum(abs(x))),
                    act,exp(apply(mu,2,median)),main=i))
  dev.off()
  
  #Spatial plots of random intercepts
  lenwidRat <- dist(range(trapCoords@coords[,2]))/dist(range(trapCoords@coords[,1])) #Ratio of lat to lon (height to dist)
  p1 <- data.frame(trapCoords@coords,intercept=apply(mod1$b0_site,2,mean)) %>% 
    ggplot(aes(x=lon,y=lat))+
    # geom_density_2d(colour='gray50')+
    geom_point(aes(col=intercept,size=abs(intercept)),show.legend=c('col'=T,'size'=F))+
    scale_colour_gradient(high='red',low='blue')+
    labs(x='UTM Easting',y='UTM Northing',col='Site\nIntercept',title=paste(i,'distribution'))
  ggsave(paste0('../Figures/Diagnostic Plots/',i,'_spatRanEf.png'),p1,width=10,height=10*lenwidRat)
  
  # png(file=paste0('../Figures/Diagnostic Plots/',i,'_spatRanEf.png'),
  #     width=10,height=10*lenwidRat,units='in',res=100,bg = "white")
  # plot(trapCoords@coords,cex=abs(apply(mod1$b0_site,2,mean)),pch=19,
  #      col=c(ifelse(sign(apply(mod1$b0_site,2,mean))==-1,'red','blue')),
  #      xlab='UTM E',ylab='UTM N',main=paste('Site-level intercept for',i)) 
  # dev.off()
  
  #Graph of spatial lag
  p2 <- data.frame(Dist=seq(0,max(distMat),length=20)*10,t(sapply(seq(0,max(distMat),length=20)^2,function(x){
    quantile(with(mod1,(alphaDist^2)*exp(-0.5*x/(rhoDist^2))),c(0.5,0.05,0.95))
  }))) %>% rename(lwr=X5.,med=X50.,upr=X95.) %>% 
    ggplot(aes(Dist,med))+geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
    geom_line(size=1)+labs(x='Distance(km)',y='Covariance',title=paste('Spatial autocorrelation for',i))
  ggsave(paste0('../Figures/Diagnostic Plots/',i,'_spatLag.png'),p2,width=8,height=6)
  rm(p1,p2); gc() #Cleanup
  print(paste('Plots made for',i))
}

#Notes: 
#Bad trace for Andrena_amphibola, Andrena_thaspii, Lasioglossum_dialictus_sp5, Osmia_melanosmia
#Separation for Overall
#Should probably use inverse gamma prior to reduce correlation at zero distance


#Plot of random intercepts - not really normal, so sigma2 has trouble centering.
t(apply(mod1$b0_site,2,function(x) quantile(x,c(0.5,0.975,0.025)))) %>%
  as.data.frame() %>% rename(median='50%',upr='97.5%',lwr='2.5%') %>% arrange(median) %>%
  mutate(row=1:nrow(.)) %>% 
  # ggplot(aes(row,median))+geom_pointrange(aes(ymax=upr,ymin=lwr))+geom_hline(yintercept=0,col='red')
  ggplot(aes(row,median))+geom_pointrange(aes(ymax=upr,ymin=lwr))+
    geom_hline(yintercept=0,col='red')
  # ggplot(aes(sample=log(median+abs(min(median)))))+geom_qq()+geom_qq_line() #Normal QQ plot
  # ggplot(aes(x=median))+geom_histogram()

#Partial regression coefficients for year-to-year populations
load('anthophora_terminalis_mod.Rdata')
mod1 <- extract(modGP)

tempDat <- with(mod1,data.frame(percSNL=datalist$percSNL,
                      canolaOverlap=apply(mod1$siteCanolaOverlap,2,median),
                      year1=apply(mu_site[,,1],2,median),b0site=apply(b0_site,2,median),
                      year2=apply(mu_site[,,2],2,median))) %>% 
  mutate(resY2=year2-(median(mod1$SNLslope[,2])*percSNL+ #Residual for year2
                              median(mod1$slopeCanolaOverlap)*canolaOverlap+
                              median(mod1$slopeLastYear)*year1)) 
# #Partial effect of Year 1
# mutate(tempDat,expY2=(median(mod1$slopeLastYear)*year1+median(mod1$SNLslope[,2])*mean(percSNL)+
#                                            median(mod1$slopeCanolaOverlap)*mean(canolaOverlap))) %>% 
mutate(tempDat,expY1=median(mod1$slopeLastYear)*year1) %>% 
  mutate(upr=sapply(year1,function(x) quantile(mod1$slopeLastYear*x,0.95))) %>%
  mutate(lwr=sapply(year1,function(x) quantile(mod1$slopeLastYear*x,0.05))) %>% 
  ggplot(aes(x=year1,y=expY1))+
  geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
  geom_line(size=1)+
  # geom_smooth(method='lm',col='red')+
  labs(x='Abundance in year 1',y='Abundance in year 2')

#Effect of only SNL
mutate(tempDat,expY1=median(mod1$SNLslope[,2])*percSNL) %>% 
  mutate(upr=sapply(percSNL,function(x) quantile(mod1$SNLslope[,2]*x,0.95))) %>%
  mutate(lwr=sapply(percSNL,function(x) quantile(mod1$SNLslope[,2]*x,0.05))) %>% 
  arrange(percSNL) %>% 
  ggplot(aes(x=percSNL,y=expY1))+
  geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
  geom_line(size=1)+
  # geom_smooth(method='lm',col='red')+
  labs(x='Percent Semi-Natural',y='Abundance in year 2')

#Effect of only canola overlap
mutate(tempDat,expY1=median(mod1$slopeCanolaOverlap)*canolaOverlap) %>% 
  mutate(upr=sapply(canolaOverlap,function(x) quantile(mod1$slopeCanolaOverlap*x,0.95))) %>%
  mutate(lwr=sapply(canolaOverlap,function(x) quantile(mod1$slopeCanolaOverlap*x,0.05))) %>% 
  arrange(canolaOverlap) %>% 
  ggplot(aes(x=canolaOverlap,y=expY1))+
  geom_ribbon(aes(ymax=upr,ymin=lwr),alpha=0.3)+
  geom_line(size=1)+
  # geom_smooth(method='lm',col='red')+
  labs(x='Canola overlap in year 1',y='Abundance in year 2')

#Plot of Gaussian processes for each year
gpResAll <- data.frame() #Empty data frame

for(i in c(wildSpp$pathName[1:20],'Overall')){
  load(paste0(i,'.Rdata'))
  mod1 <- extract(modGP)
  if(grepl('Overall',i)) act <- rowSums(temp[,c(9:96)]) else act <- temp[,i]
  
  #Working residuals: (actual-expected)/expected
  workRes <- (act/apply(with(mod1,exp(mu)*(1-thetaZI[,datalist$year])),2,median))-1 #Expected value
  #Marginalized effects from Y1
  margY1 <- median(mod1$b0[,1]) + #Intercept
    median(mean(datalist$percSNL)*mod1$SNLslope[,1]) + #SNL effect
    median(mod1$canolaEffect[,1] * mean(mod1$predCanolaPass[,datalist$year==1])) #Canola per pass
  
  margY2 <- median(mod1$b0[,2]) + #Intercept
    median(mean(datalist$percSNL)*mod1$SNLslope[,2]) + #SNL effect
    median(mod1$canolaEffect[,2] * mean(mod1$predCanolaPass[,datalist$year==2])) #Canola per pass
  
  #GP trend for each year
  y1 <- with(datalist,t(apply(mod1$gpTrend2015+margY1,2,function(x) quantile(x,c(0.05,0.5,0.95))))[dateIndex2015,c(1:3)])
  y2 <- with(datalist,t(apply(mod1$gpTrend2016+margY2,2,function(x) quantile(x,c(0.05,0.5,0.95))))[dateIndex2016,c(1:3)])
  
  #Assemble GP trend along with dates, year, residual
  gpResTemp <- with(datalist,data.frame(rbind(y1,y2),day=c(centDates2015[dateIndex2015],centDates2016[dateIndex2016]),
                                        year=c(rep(1,length(dateIndex2015)),rep(2,length(dateIndex2016))))) %>% 
                                        # resid=log(act/(apply(mod1$count_resid,2,median)+act)))) %>% #Log-residual
    rename(lwr=X5.,med=X50.,upr=X95.) %>%
    mutate(resid=(act/exp(med))-1, #Working residual (proportion difference from expected)
           day=(day*7+206.41),year=factor(year,labels=c('2015','2016')),spp=i)
  gpResAll <- rbind(gpResAll,gpResTemp) #Combine with others
  print(paste('Finished',i))
}

#Create overall figure of Gaussian process model
p1 <- gpResAll %>% mutate_at(vars(lwr:upr),exp) %>% 
  mutate(resid=med+(resid*med)) %>% 
  mutate(year=factor(year)) %>%
  mutate(spp=factor(spp,levels=c('Overall',wildSpp$pathName),labels=c('Overall',wildSpp$labelName))) %>% 
  group_by(spp) %>% filter(resid<=quantile(resid,0.97)) %>%
  ggplot(aes(day,group=year))+geom_ribbon(aes(ymax=upr,ymin=lwr,fill=year),alpha=0.3,show.legend=F)+
  geom_line(aes(y=med,col=year),size=1) + 
  facet_wrap(~spp,scales='free_y')+labs(y='Counts per week',x='Day of year',col='Year',main=i)+
  geom_point(aes(y=resid,col=year),alpha=0.5,size=0.75) +
  scale_colour_manual(values=c('red','blue'))+scale_fill_manual(values=c('red','blue'))+
  theme(strip.text=element_text(size=9),axis.text=element_text(size=12))+
  theme(legend.position=c(0.9,0.05),legend.background=element_rect(fill='white',colour='black',linetype='solid'),
        legend.title=element_text(size=15),legend.text=element_text(size=12))
ggsave('../Figures/GPtrends.png',p1,width=12,height=8.5)

#Plot canola bloom
p1 <- with(mod1,data.frame(canolaBloom=datalist$canolaBloom,centEndDate=round((datalist$centBloomDate)*7+206.41),
                     nearCanola=datalist$nearCanola,year=datalist$year[datalist$bloomIndex],
                     predBloom=apply(mod1$muCanola,2,median),uprBloom=apply(mod1$muCanola,2,quantile,0.95),
                     lwrBloom=apply(mod1$muCanola,2,quantile,0.05))) %>% 
  filter(nearCanola==1) %>% mutate(year=factor(year,labels=c('2015','2016'))) %>% 
  arrange(centEndDate,year) %>% 
  mutate(centEndDate=as.POSIXct(paste(centEndDate,2015),format='%j %Y')) %>% 
  ggplot(aes(x=centEndDate,col=year))+geom_line(aes(y=predBloom),size=1)+
  geom_ribbon(aes(ymax=uprBloom,ymin=lwrBloom,fill=year,col=NULL),alpha=0.3)+
  geom_point(aes(y=canolaBloom),show.legend=F)+
  labs(x='Day of Year',y='Percent Bloom',fill='Year',col='Year')+
  scale_colour_manual(values=c('red','blue'))+
  scale_fill_manual(values=c('red','blue'))
ggsave('../Figures/bloomModel.png',p1,width=8,height=6)


# # Get all main coefficients from models
# getCoefs <- function(path){
#   load(path)
#   mod1 <- extract(modGP)
#   a <- data.frame(spp=sub('.Rdata','',path),
#                   par=c('SNLslope1','SNLslope2','slopeLastYear','slopeCanolaOverlap'),
#                   meas=rbind(t(apply(mod1$SNLslope,2,function(x) quantile(x,c(0.025,0.25,0.5,0.75,0.975)))),
#                              quantile(mod1$slopeLastYear,c(0.025,0.25,0.5,0.75,0.975)),
#                              quantile(mod1$slopeCanolaOverlap,c(0.025,0.25,0.5,0.75,0.975))))
#   names(a)[3:7] <- c('lwr2','lwr1','med','upr1','upr2')
#   rm(mod1,modGP); gc()
#   return(a)
# }
# filepaths <- unname(sapply(c(wildSpp$pathName[1:20],'Overall'),function(x) {
#   # x[1] <- tolower(x[1])
#   x <- sub(' ','_',x)
#   x <- paste(x,'.Rdata',sep='')
#   return(x)
# }))
# coefs <- lapply(filepaths,getCoefs)
# coefs <- do.call('rbind',coefs)
# save(coefs,file='mainCoefs.Rdata')
load('mainCoefs.Rdata')

coefs <- coefs %>% mutate(spp=as.character(spp)) %>%
  mutate(spp=paste(toupper(substr(spp,0,1)),substr(spp,2,nchar(spp)),sep='')) %>% #capitalize
  mutate(spp=sub('_',' ',spp)) %>%
  left_join(distinct(select(bees,genSpp,family)),by=c('spp'='genSpp')) %>%  #Join in family names
  mutate(family=as.character(family)) %>%
  mutate(family=ifelse(spp=='Overall','Overall',family)) %>% #Fix names
  rowwise() %>%
  mutate(spp=ifelse(grepl('_',spp),paste(strsplit(spp,' ')[[1]][1],strsplit(spp,'_')[[1]][2]),spp)) %>%
  data.frame() %>%
  mutate(param=as.character(par)) %>% select(-par) %>%
  mutate(lwr2=ifelse(lwr2>-10,lwr2,-10)) %>% #Trim limits of x-axis
  # filter(param!='SNLslope1') %>% #Strip out SNL slopes from year 1
  mutate(param=factor(param,levels=c('SNLslope1','SNLslope2','slopeLastYear','slopeCanolaOverlap'),
                      labels=c('Semi-natural → Counts 2015','Semi-natural → Counts 2016',
                                     'Counts 2015 → Counts 2016 ','Canola Overlap 2015\n→ Counts 2016')))

nameOrder <- coefs %>% select(spp,family) %>% distinct() %>% 
  mutate(family=factor(family,levels=c("Overall","Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae"))) %>%
  arrange(desc(family),desc(spp)) %>% mutate(spp=as.character(spp)) %>% 
  slice(c(1:3,6,4,5,7:n()))

#Parameters for rectangles
rectParams1 <- nameOrder %>% mutate(family=factor(family,levels=levels(nameOrder$family)[length(levels(nameOrder$family)):1])) %>% 
  group_by(family) %>% summarize(n=n()) %>% ungroup %>%
  mutate(ymax=cumsum(n),ymin=lag(ymax)) %>% mutate(ymin=ifelse(is.na(ymin),0,ymin)) %>%
  mutate(ymax=ymax,ymin=ymin) %>% select(-n) %>% 
  unite(family,family:ymin)

rectParams2 <- coefs %>% 
  gather('est','quant',lwr2:upr2) %>% 
  group_by(param) %>% summarize(xmax=max(quant),xmin=min(quant)) %>% 
  unite(param,param:xmin)

rectParams <- expand.grid(param=rectParams2$param,family=rectParams1$family) %>% 
  separate(param,c('param','xmax','xmin'),sep='_',convert=T) %>% 
  separate(family,c('family','ymax','ymin'),sep='_',convert=T) %>% 
  mutate(param=factor(param,levels=levels(coefs$param))) %>% 
  mutate(family=factor(family,levels=levels(nameOrder$family)))
rm(rectParams1,rectParams2)

#Parameters for text labels
textParams <- rectParams %>% rowwise() %>% 
  mutate(x=mean(xmax,xmin)-abs(xmax-xmin)*0.99,y=mean(ymax,ymin)-abs(ymax-ymin)*0.5) %>% 
  select(-ymax,-ymin,-xmax,-xmin) %>% 
  mutate(family=as.character(family),keep=param=='Semi-natural → Counts 2015'&family!='Overall') %>% 
  mutate(family=ifelse(keep,family,NA),x=ifelse(keep,x,NA),y=ifelse(keep,y,NA)) %>% select(-keep)

p1 <- coefs %>% mutate(spp=factor(spp,levels=nameOrder$spp)) %>% rowwise() %>% 
  ggplot(aes(x=med,y=spp))+
  facet_wrap(~param,scales='free_x',ncol=4)+
  geom_rect(data=rectParams,aes(x=NULL,y=NULL,xmin=xmin,xmax=xmax,ymin=ymin+0.5,ymax=ymax+0.5,fill=family),alpha=0.5,show.legend=F)+
  geom_vline(xintercept=0,col='black',linetype='dashed')+#coord_flip()+
  geom_errorbarh(aes(xmin=lwr2,xmax=upr2),height=0)+
  geom_errorbarh(aes(xmin=lwr1,xmax=upr1),height=0,size=2)+
  geom_point(size=3)+
  geom_label(data=textParams,aes(x=x,y=y+0.5,label=family),fill='gray90',col='black',size=4,hjust='left')+
  labs(y='Bee species',x='Slope Parameter')+
  scale_fill_manual(values=c('white','gray70','gray50','gray70','gray50','gray70'))+
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10),strip.text=element_text(size=10))
ggsave('../Figures/coef_estimates.png',p1,width=11,height=8.5)


#Plots of canola attractiveness across spp.
setwd("~/Projects/UofC/wildbee_canola_project/Models")

# # Get intercepts and per-pass canola coefficients from models
# getCoefs <- function(path){
#   load(path)
#   mod1 <- extract(modGP)
#   a <- data.frame(spp=sub('.Rdata','',path),
#                   par=c('canolaEffect1','canolaEffect2','b0_2015','b0_2016'),
#                   meas=rbind(t(apply(cbind(mod1$canolaEffect,mod1$b0),2,function(x)
#                     quantile(x,c(0.025,0.25,0.5,0.75,0.975))))))
#   names(a)[3:7] <- c('lwr2','lwr1','med','upr1','upr2')
#   rm(mod1,modGP); gc()
#   return(a)
# }
# filepaths <- unname(sapply(c(wildSpp$pathName[1:20],'Overall'),function(x) {
#   # x[1] <- tolower(x[1])
#   x <- sub(' ','_',x)
#   x <- paste(x,'.Rdata',sep='')
#   return(x)
# }))
# coefs <- lapply(filepaths,getCoefs)
# coefs <- do.call('rbind',coefs)
# save(coefs,file='canolaCoefs.Rdata')
load('canolaCoefs.Rdata')

coefs <- coefs %>% mutate(spp=as.character(spp)) %>%
  mutate(spp=paste(toupper(substr(spp,0,1)),substr(spp,2,nchar(spp)),sep='')) %>% #capitalize
  mutate(spp=sub('_',' ',spp)) %>%
  left_join(distinct(select(bees,genSpp,family)),by=c('spp'='genSpp')) %>%  #Join in family names
  mutate(family=as.character(family)) %>%
  mutate(family=ifelse(spp=='Overall','Overall',family)) %>% #Fix names
  rowwise() %>%
  mutate(spp=ifelse(grepl('_',spp),paste(strsplit(spp,' ')[[1]][1],strsplit(spp,'_')[[1]][2]),spp)) %>%
  data.frame() %>%
  mutate(param=as.character(par)) %>% select(-par) %>% 
  # mutate(lwr2=ifelse(lwr2>-10,lwr2,-10)) %>% #Trim limits of x-axis
  # filter(param!='SNLslope1') %>% #Strip out SNL slopes from year 1
  mutate(param=factor(param,labels=c('Intercept 2015','Intercept 2016','Canola Abundance 2015\n→ Counts 2015','Canola Abundance 2016\n→ Counts 2016')))

nameOrder <- coefs %>% select(spp,family) %>% distinct() %>% 
  mutate(family=factor(family,levels=c("Overall","Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae"))) %>%
  arrange(desc(family),desc(spp)) %>% mutate(spp=as.character(spp)) %>% 
  slice(c(1:3,6,4,5,7:n()))

#Parameters for rectangles
rectParams1 <- nameOrder %>% mutate(family=factor(family,levels=levels(nameOrder$family)[length(levels(nameOrder$family)):1])) %>% 
  group_by(family) %>% summarize(n=n()) %>% ungroup %>%
  mutate(ymax=cumsum(n),ymin=lag(ymax)) %>% mutate(ymin=ifelse(is.na(ymin),0,ymin)) %>%
  mutate(ymax=ymax,ymin=ymin) %>% select(-n) %>% 
  unite(family,family:ymin)

rectParams2 <- coefs %>% 
  gather('est','quant',lwr2:upr2) %>% 
  group_by(param) %>% summarize(xmax=max(quant),xmin=min(quant)) %>% 
  unite(param,param:xmin)

rectParams <- expand.grid(param=rectParams2$param,family=rectParams1$family) %>% 
  separate(param,c('param','xmax','xmin'),sep='_',convert=T) %>% 
  separate(family,c('family','ymax','ymin'),sep='_',convert=T) %>% 
  mutate(param=factor(param,levels=levels(coefs$param))) %>% 
  mutate(family=factor(family,levels=levels(nameOrder$family)))
rm(rectParams1,rectParams2)

#Parameters for text labels
textParams <- rectParams %>% rowwise() %>% 
  mutate(x=mean(c(xmax,xmin))-abs(xmax-xmin)*.45,y=mean(ymax,ymin)-abs(ymax-ymin)*0.5) %>% 
  mutate(x=ifelse(family=='Colletidae',-0.5,x)) %>%
  select(-ymax,-ymin,-xmax,-xmin) %>% 
  mutate(family=as.character(family),keep=param=='Intercept 2015'&family!='Overall') %>% 
  mutate(family=ifelse(keep,family,NA),x=ifelse(keep,x,NA),y=ifelse(keep,y,NA)) %>% select(-keep) 

p1 <-
  coefs %>% mutate(spp=factor(spp,levels=nameOrder$spp)) %>% rowwise() %>% 
  ggplot(aes(x=med,y=spp))+
  facet_wrap(~param,scales='free_x',ncol=4)+
  geom_rect(data=rectParams,aes(x=NULL,y=NULL,xmin=xmin,xmax=xmax,ymin=ymin+0.5,ymax=ymax+0.5,fill=family),alpha=0.5,show.legend=F)+
  geom_vline(xintercept=0,col='black',linetype='dashed')+
  geom_errorbarh(aes(xmin=lwr2,xmax=upr2),height=0)+
  geom_errorbarh(aes(xmin=lwr1,xmax=upr1),height=0,size=2)+
  geom_point(size=3)+
  labs(y='Bee species',x='Slope Parameter')+
  geom_label(data=textParams,aes(x=x,y=y+0.5,label=family),fill='gray90',col='black',size=4,hjust='left')+
  scale_fill_manual(values=c('white','gray70','gray50','gray70','gray50','gray70'))+
  theme(axis.text.y=element_text(size=10),axis.text.x=element_text(size=10),strip.text=element_text(size=10))
ggsave('../Figures/canolaEstimates.png',p1,width=11,height=8.5)


#Megachile perihirta and Osmia sp1 appear to be positively & negatively (respectively) affected by adjacent canola bloom in Y1 only. Check that these parameters aren't correlated with other ones. No super obvious correlations for M.p. 

load(paste0(wildSpp$pathName[c(15,18)],'.Rdata')[2])
mod1 <- extract(modGP)
fastPairs(mod1[c('rho','alpha','b0','sigma_site','SNLslope','slopeCanolaOverlap','canolaEffect')])



#Plot of population between years (from data)
ggplot(temp,aes(midDate,Anthophora_terminalis,col=factor(year),group=paste(year,BLID,replicate)))+
  geom_point()+geom_line()+facet_wrap(~BLID)+
  scale_y_sqrt()+labs(col='Year')

ggplot(temp,aes(midDate,Dufourea_maura,col=factor(year),group=paste(year,BLID,replicate)))+
  geom_point(aes(shape=canolaBloom>0))+geom_line()+facet_wrap(~BLID)+
  scale_y_sqrt()+labs(col='Year')

ggplot(temp,aes(midDate,Dufourea_maura,group=paste(BLID,replicate)))+
  geom_point()+geom_line()+facet_wrap(~year,ncol=1)

modGP_results <- extract(modGP) #Extract values from model

#Posterior predictive checks
data.frame(actual=datalist$count,predicted=apply(modGP_results$predCounts,2,median)) %>% 
  ggplot(aes(predicted,actual))+
  geom_point(position=position_jitter(height=0.1,width=0.1))+
  geom_abline(intercept=0,slope=1)

with(modGP_results,{ #Prior vs Posterior for b0sd
  par(mfrow=c(1,1))
  hist(b0sd,main=NA)
  par(new=T)
  curve(MCMCpack::dinvgamma(x,2,1),min(b0sd),max(b0sd),main='Prior',ylab='Frequency',xlab='b0sd',col='red',axes=F)
})

with(modGP_results,{
  par(mfrow=c(3,1)); #Plot b0site results
  year2015 <- apply(b0site[,,1],2,median);
  
  year2016 <- apply(b0site[,,2],2,median);
  hist(year2015,main='b0 2015');
  hist(year2016,main='b0 2016');
  plot(year2015,year2016,xlab='b0 2015',ylab='b0 2016')
  abline(0,1,lty='dashed');
  par(mfrow=c(1,1));
})

#Plots of eta values
data.frame(year=datalist$year,eta=exp(apply(modGP_results$eta,2,median)),site=datalist$site,
           centDate=datalist$centDate,counts=datalist$count) %>% 
  gather('type','measurement',eta,counts) %>% 
  ggplot(aes(centDate,measurement))+geom_point()+geom_line(aes(group=site),alpha=0.5)+
  facet_grid(type~year,scales='free_y')


detach("package:rstan", unload=TRUE) #Detach rstan


# Community analysis using vegan ------------------------------------------
library(vegan)
# ord <- metaMDS(dune)
# plot(ord,type='n',disp='sites')
# ordihull(ord,dune.env$Management,col=1:4,lwd=3)
# points(ord,display='sites')

#Matrix of abundance values
beeMat <- bees %>% group_by(BLID,year,genSpp) %>%
  summarize(count=n()) %>% ungroup() %>% 
  spread(genSpp,count,fill=0) %>% select(-BLID,-year) %>% 
  as.matrix()

ordBees <- metaMDS(beeMat,k=3,trymax=300)

plot(ordBees)

# ord.fit <- with(landscape,envfit(ordBees~year+Canola_100+Forest_100+NativeGrassland_100+Pasture_100+Shrubland_100))
ord.fit <- with(landscape,envfit(ordBees~year))
print(ord.fit)
plot(ord.fit)

is2015 <- landscape$year=='2015'
is2016 <- landscape$year=='2016'

#Model of community from 2015. Looks like Forest and Shrubland are important
ord.fit2015 <- with(ordBees,envfit(points[is2015,] ~ landscape$Forest_100[is2015]+
                                   landscape$NativeGrassland_100[is2015]+
                                   landscape$Shrubland_100[is2015])
)
print(ord.fit2015)
plot(ordBees$points[is2015,])
plot(ord.fit2015)

#Model of community from 2016. Looks like MDS1 and 2 are important, and canola from 2015 changes very little
ord.fit2016 <- with(ordBees,envfit(points[is2016,]~points[is2015,1]+points[is2015,2]+points[is2015,3]+
                                     landscape$Canola_100[is2015]))

print(ord.fit2016)

plot(ordBees$points[is2016,])
plot(ord.fit)



# Forest+NativeGrassland+Shrubland




# Unused models -----------------------------------------------------------

# datalist=with(temp, #Data to feed into JAGS
#               list(
#                 N=nrow(temp), #Number of total samples
#                 Nsite=length(unique(BLID)), #Number of sites
#                 #                Nreps=5, #Replications at each site
#                 count=count, #Count of bees
#                 site=as.numeric(BLID), #site index
#                 traplength=(endDate-startDate)/7, #offset (in weeks)
#                 centDate=midDate-mean(midDate) #Centered date
#               )
# )
# 
# #Trying NB GLMM
# # start=function() list(alpha=rnorm(1,0,.1),beta=rnorm(1,0,.1),logtheta=dnorm(1,0,.1)) #GLM version
# start=function() list(alpha.mean=dunif(1,-10,10),alpha.sd=dunif(1,-10,10),
#                       beta=rnorm(1,0,1),logtheta=dnorm(1,0,1)) #Starting values for chains (GLMM version)
# mod2=jags(data=datalist,inits=start,c('alpha.mean','beta','theta','fit','fit.new'),
#           model.file='nbGLMM_jags.txt',n.chains=3,n.iter=5000,n.burnin=1000,n.thin=5,parallel=F)
# summary(mod2)
# xyplot(mod2)
# densityplot(mod2)
# pp.check(mod2,'fit','fit.new') #Model fits better...
# 
# #Alternative NB GLM model - takes longer, but has identical estimates, and better n.eff.
# 
# start=function() list(alpha=rnorm(1,0,.1),beta=rnorm(1,0,.1),r=dunif(1,0,50))
# mod2=jags(data=datalist,inits=start,c('alpha','beta','r','fit','fit.new'),
#           model.file='nb2GLMM_jags.txt',n.chains=3,n.iter=5000,n.burnin=1000,n.thin=5,parallel=F)
# summary(mod2)
# pp.check(mod2,'fit','fit.new') #Model fits better...

# Binomial-poisson mixture model, where phi is a function of trapping offset - model works when lambda doesn't change through time, but falls apart when changes in lambda are included... not enough data to estimate it?

# datalist=with(temp, #Data to feed into JAGS
#               list(
#                 N=nrow(temp), #Number of total samples
#                 Nsite=length(unique(BLID)), #Number of sites
#                 count=count, #Count of bees
#                 site=as.numeric(BLID),
#                 traplength=(endDate-startDate)/7 , #offset
#  #               Ndays=length(unique(midDate)), #number of days of trapping
#                 centDate=midDate-mean(midDate) #Days of trapping
#               )
# )
# start=function() list(
#   #trueCount=unname(with(datalist,tapply(count,site,max))),
#   trueCount=with(datalist,count)+1,
# #  lambda=rgamma(1,1,0.3),
#   phi=runif(1,0,0.1),
#   alpha.1=rnorm(1,0,1),
#   alpha.mean=rnorm(1,0,1),
#   alpha.prec=runif(1,0.001,2),
#   theta.zero=dunif(1,0,1)
#   ) #Starting values for chains 
# mod3=jags(data=datalist,inits=start,c('alpha.mean','alpha.1','phi','theta.zero'),
#           model.file='binPoissonMix_jags.txt',
#           n.chains=3,n.adapt=1000,n.iter=10000,n.burnin=200,n.thin=20,parallel=F)
# #Not converging. Not enough data to work with...
# summary(mod3) 
# xyplot(mod3)
# densityplot(mod3)

# #Second version of canola model, using IFELSE trick to get around estimating sigma and mu. 
# #This works OK, but is much slower and tends to drag the mean down with it
# datalist=with(temp, #Data to feed into JAGS
#               list(
#                 N=nrow(temp), #Number of total samples
#                 Nsite=length(unique(BLID)), #Number of sites
#                 site=as.numeric(BLID), #site index
#                 centEndDate=endDate-mean(midDate), #Centered midDate (using mean of midDate)
#                 canolaBloom=canolaBloom, #Bloom
#                 nearCanola=as.numeric(with(temp,tapply(canolaBloom,BLID,sum))>0) #Is field near canola?
#               )
# )
# 
# start=function() list(mu.canola=rnorm(1,-7,0.1),
#                       sigma.canola=runif(1,1,20),
#                       resid.canola=rgamma(1,0.1,0.1),
#                       sigma.mu.site=runif(1,0,20),
#                       sigma.sigma.site=runif(1,0,10)) 
# mod5=jags(data=datalist,inits=start,c('mu.canola','mu.site.canola','sigma.canola','sigma.site.canola'),
#           model.file='canolaGaussianMixed2.txt',
#           n.chains=3,n.adapt=2000,n.iter=14000,n.burnin=2000,n.thin=100,parallel=T)
# 
# summary(mod5)
# 
# mod5fit=mod5$samples[[1]] #Results from 1st chain of mod5
# pars=apply(mod5fit,2,median)
# pars=pars[c(-1,-55,-109)]
# pars=matrix(pars,ncol=2,dimnames=list(unique(temp$BLID),c('mu','sigma')))
# pars=pars[which(datalist$nearCanola>0),]
# centDate=-20:27
# 
# #Cheapo way of binding dataframes
# res5=matrix(NA,length(centDate),nrow(pars),dimnames=list(centDate,rownames(pars)))
# 
# #Predictions
# for(i in 1:nrow(res5)){ #For each date
#   for(j in 1:ncol(res5)){ #For each site
#     res5[i,j]=round(100*exp(-0.5*((centDate[i]-pars[j,1])/pars[j,2])^2),2)
#   }
# }
# res5=data.frame(centDate=centDate,res5) 
# res5=gather(res5,'BLID','fit',2:13)
# res5$BLID=gsub('X','',res5$BLID)
# 
# ggplot(res5,aes(centDate+mean(temp$midDate),fit,group=BLID))+
#   geom_point(data=temp,aes(x=endDate,y=canolaBloom,group=BLID))+
#   geom_line(col='red',size=1)+
#   geom_line(data=res4,col='blue',size=1)+ #Compare with NA version
#   facet_wrap(~BLID)+
#   labs(x='Day of year',y='Percent bloom')+
#   theme(axis.text=element_text(size=7),strip.text=element_text(size=10))
# 
# #Starting values for chains (GLMM version))
# start=function() list(alpha.mean=rnorm(1,0,1),
#                       alpha.sd=runif(1,0,10),
#                       beta=rnorm(1,0,.1),
#                       logtheta=rnorm(1,0,.1), #Dispersion
#                       #siteZero=as.numeric(with(datalist,tapply(count,site,sum))>1),
#                       theta.zero=runif(1,0,1) #Site occupancy param
# ) 
# mod2=jags(data=datalist,inits=start,c('alpha.mean','alpha','beta','theta','theta.zero','siteZero','fit','fit.new'),
#           model.file='ZInbGLMM_jags.txt',
#           n.chains=3,n.adapt=2000,n.iter=10000,n.burnin=1000,n.thin=10,parallel=T)
# 
# summary(mod2) #This only ends up fitting 2 sites as empty, so this doesn't really help much
# pp.check(mod2,'fit','fit.new') 
