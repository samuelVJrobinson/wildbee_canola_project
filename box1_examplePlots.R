#Example plots for separate box on year-to-year random effects

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_classic())

N <- 20
sigma <- 1
y1_ab <- rnorm(N)

y2_abPos2 <- y1_ab*2 + rnorm(N,0,sigma)
y2_abPos1 <- y1_ab*1 + rnorm(N,0,sigma)
y2_abNeut <- y1_ab*0 + rnorm(N,0,sigma)
y2_abNeg1 <- y1_ab*-1 + rnorm(N,0,sigma)
y2_abNeg2 <- y1_ab*-2 + rnorm(N,0,sigma)

dat <- data.frame(site=letters[1:N],y1_ab,y2_abPos2,y2_abPos1,y2_abNeut,y2_abNeg1,y2_abNeg2)

p1 <- dat %>% gather('gamma','y2_ab',y2_abPos2:y2_abNeg2) %>% 
  mutate(gamma=factor(gamma,labels=c('-2','-1','0','1','2'))) %>% 
  filter(gamma!='-2') %>% 
  rename('y1'='y1_ab','y2'='y2_ab') %>% 
  gather('year','meas',y1,y2) %>% 
  mutate(year=factor(year,labels=c('1','2'))) %>% 
  ggplot(aes(year,meas,group=site))+
  # geom_point(position=position_jitter(width=0.1))+
  geom_point()+geom_line()+
  facet_wrap(~gamma,ncol=4,labeller=label_bquote(gamma==.(as.character(gamma))))+
  # facet_wrap(~gamma,ncol=4)+
  labs(x='Year',y='Random effect')
ggsave('./Figures/exampleGammaPlot.png',p1,width=5,height=3)
