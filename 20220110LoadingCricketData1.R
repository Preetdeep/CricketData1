library(ARTofR)
library(dplyr)
library(ggplot2)
library(BSDA) #For z test
library(dgof) #For z test
#Import
xxx_title1("Whether a Batsman gets overconfident after a boundary")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##            WHETHER A BATSMAN GETS OVERCONFIDENT AFTER A BOUNDARY         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###using CSV files   
library(readr)
df1 <- read_csv("deliveries.csv")
df1<-cbind(1:length(df1$match_id),df1)
colnames(df1)[1]<-"sno"


## Subset of wickets
dfw<-subset(df1,!is.na(player_dismissed)) #To get all previous balls
dfw<-subset(dfw, over+ball >2) #no first ball wickets here
df2<-data.frame()
for(i in 1:length(dfw$sno)){
  
  df2[i,1:22]<-df1[dfw$sno[i]-1,1:22]
}
df3<-rbind(dfw,df2)
df3<-df3[order(df3$sno,decreasing=FALSE),]
xxx_box("Now that we have this, we can see if they scored a boundary.")

#  Now that we have this, we can see if they scored a boundary.                 
## in df2, see the scores

df4<-df2 #For safety
xxx_box1("For Same Batsman")

#...............................................................................
#                                                                              .
#  For Same Batsman                                                            .
#                                                                              .
#...............................................................................

# df2a<-subset(df2, batsman_runs!=1) #To remove singles
# df2b<-subset(df2, ball==1) #To remove cases where there was a strike change. So previous ball was single and over change or new over.
# Strike change = 1,3,5 which si %% 2 != 0
df2d<-subset(df2, batsman_runs%%2==0) # Only cases where strike has not changed 

df2c<-subset(df2,ball==6) #Only previous over , so first ball wickets
df2c2<-subset(df2c, batsman_runs%%2!=0 )

df2e<-rbind(df2d,df2c2) # This is our file
df2<-df2e #more for nomenclature

df2$total_runs<-ifelse(is.na(df2$player_dismissed),df2$total_runs,"W")


#...............................................................................
#                                                                              .
#  Coming back to DF2                                                           .
#                                                                              .
#...............................................................................


prevballsummary<-df2%>%group_by(total_runs,inning)%>% tally() #To get a count of all things that happened prevoiously
prevballsummary1<-filter(prevballsummary,inning==1)
prevballsummary2<-filter(prevballsummary,inning==2)
prevballsummary<-select(prevballsummary,total_runs,n)
plot1<-ggplot(data=prevballsummary,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity") # Somehow this is not in order
plot1<-ggplot(data=prevballsummary,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("Before wicket")
plot1
plot2<-ggplot(data=prevballsummary1,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity") # Only singles are extras
plot3<-ggplot(data=prevballsummary2,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity") # Only singles are extras

# Match this with total number of whatever in the match. 
df1$total_runs<-ifelse(is.na(df1$player_dismissed),df1$total_runs,"W")
prevballsummaryall<-df1%>%group_by(total_runs,inning)%>% tally()
plot4<-ggplot(data=prevballsummaryall,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity") +title("Throughout the match") # Somehow this is not in order
plot4<-ggplot(data=prevballsummaryall,aes( x=total_runs,y=n))+geom_bar(stat="identity")  + ggtitle("Throughout the match")
plot4
## We should remove First balls also 

# If we were to look at df1 Vs df2, we would get the difference between just before the wicket and the rest of the match.

cor.test(df1$batsman_runs,df4$batsman_runs)
z.test(df1$batsman_runs,df4$batsman_runs, alternative = "two.sided",sigma.x = 0.5,sigma.y = 0.5)
ks.test(df1$batsman_runs,df4$batsman_runs)
xxx_divider1("Now we do this for reverse")
# We find balls that have a 6, we get the next ball and plot those

dfboundary<-subset(df1,total_runs>3)
dfboundary<-subset(dfboundary,total_runs!="W")
unique(dfboundary$total_runs)# This includes Wickets also, greater than 3 


dfemp1<-data.frame()
for(i in 1:length(dfboundary$sno)){
  
  dfemp1[i,1:22]<-df1[dfboundary$sno[i]+1,1:22]
}
dfafterboundary<-dfemp1 #Remove first ball of the match
dfafterboundary<-subset(dfafterboundary,ball+over>2)

dfzero<-subset(df1,total_runs==0)
unique(dfzero$total_runs)# This is only zero
dfemp2<-data.frame()
for(i in 1:length(dfzero$sno)){
  
  dfemp2[i,1:22]<-df1[dfzero$sno[i]+1,1:22]
}
dfafterzero<-dfemp2
dfzero<-subset(dfzero,ball+over>2)

# There is no difference between Innings 1 and 2. whereas there should have been.
# match this with total number of 0,s and Ws in the game 
1. Summary of Wicket balls, over and ball and innings
2. Summary of previous balls, over and all and innings
3. No first ball wickets here


#https://www.kaggle.com/ash316/let-s-play-cricket/data
#This is for the Deliveries File


#https://www.kaggle.com/patrickb1912/ipl-complete-dataset-20082020
#This is for IPL Ball by Ball

#https://www.kaggle.com/kalilurrahman/international-cricket-data-latest?select=ipl_csv2
#This might have more




xxx_divider1("Importing and Converting")
#....................Importing and Converting....................
install.packages("rjson")
library(rjson)
result <- fromJSON(file = "all_json/64071.json")
df1 <- as.data.frame(result)

library(jsonlite)
result<-fromJSON("all_json/64071.json", flatten=TRUE)
result1<-as.data.frame(result$innings)
result2<-as.data.frame(result1, flatten=TRUE)
install.packages("yaml")
library(yaml)
res1<-read_yaml("apl/1160569.yaml")
str(res1)
res2<-res1$innings

  
  
  
