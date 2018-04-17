library(RODBC)
conn<-odbcConnect('ecologics',case='nochange') #ODBC connection must be set up beforehand

df1<-sqlQuery(conn,'SELECT * FROM arthropod') #Arthropod table
df2<-sqlQuery(conn,'SELECT * FROM arthropodJoinLandscape') #Arthropod table joined with landscape and temperature data
df3<-sqlQuery(conn,'SELECT * FROM trap')
flowers<-sqlQuery(conn,'SELECT * FROM floral')

#NOTE: THIS TAKES ABOUT 5 MINS TO DOWNLOAD

setwd("~/Projects/UofC/Wild bee time project")
save(df1,file='arthropod.Rdata')
save(df2,file='arthropodJoinLandscape.Rdata')
save(df3,file='trap.Rdata')
save(flowers,file='flower.Rdata')

#Other notes from Paul:
#GDD is a more relevant measure of time than Jday (this is what the bees would "feel" rather than DOY)




