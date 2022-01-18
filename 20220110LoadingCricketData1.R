library(ARTofR)
library(dplyr)
library(ggplot2)
#library(BSDA) #For z test
#library(dgof) #For z test
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
#for(i in 1:length(dfw$sno)){
  
#  df2[i,1:22]<-df1[dfw$sno[i]-1,1:22]
#}
#write.csv(df2,"dfbeforewicket.csv")
df2 <- read_csv("dfbeforewicket.csv")

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
prevballsummary1$perc<-prevballsummary1$n/sum(prevballsummary1$n)
prevballsummary2<-filter(prevballsummary,inning==2)
prevballsummary2$perc<-prevballsummary2$n/sum(prevballsummary2$n)
prevballsummary<-select(prevballsummary,total_runs,n)
plot1<-ggplot(data=prevballsummary,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity") # Somehow this is not in order
plot1<-ggplot(data=prevballsummary,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("Before wicket")
plot1
plot2<-ggplot(data=prevballsummary1,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity")+ggtitle("Before wicket in first innings") # Only singles are extras
plot2b<-ggplot(data=prevballsummary1,aes( x=reorder(total_runs,-n),y=perc))+geom_bar(stat="identity")+ggtitle("Before wicket in first innings") +
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)# Only singles are extras
plot2b
plot3<-ggplot(data=prevballsummary2,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity")+ggtitle("Before wicket in second innings") # Only singles are extras
plot3b<-ggplot(data=prevballsummary2,aes( x=reorder(total_runs,-n),y=perc))+geom_bar(stat="identity")+ggtitle("Before wicket in second innings") +
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)# Only singles are extras
plot3b

# Some difference in Same Batsman and Different

# Match this with total number of whatever in the match. 
df1$total_runs<-ifelse(is.na(df1$player_dismissed),df1$total_runs,"W")
prevballsummaryall<-df1%>%group_by(total_runs,inning)%>% tally()
prevballsummaryall1<-filter(prevballsummaryall,inning==1)
prevballsummaryall1$perc<-prevballsummaryall1$n/sum(prevballsummaryall1$n)
prevballsummaryall2<-filter(prevballsummaryall,inning==2)
prevballsummaryall2$perc<-prevballsummaryall2$n/sum(prevballsummaryall2$n)
plot4<-ggplot(data=prevballsummaryall,aes( x=reorder(total_runs,-n),y=n))+geom_bar(stat="identity") +title("Throughout the match") # Somehow this is not in order
plot4<-ggplot(data=prevballsummaryall,aes( x=total_runs,y=n))+geom_bar(stat="identity")  + ggtitle("Throughout the match")
plot4
plot5<-ggplot(data=prevballsummaryall1,aes( x=total_runs,y=perc))+geom_bar(stat="identity")  + ggtitle("Throughout the first innings in %")+
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)# Only singles are extras
plot5
plot6<-ggplot(data=prevballsummaryall2,aes( x=total_runs,y=perc))+geom_bar(stat="identity")  + ggtitle("Throughout the second innings in %")+
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)# Only singles are extras
plot6
# There are more runs in the first innings, fewer wickets also
## We should remove First balls also 

# If we were to look at df1 Vs df2, we would get the difference between just before the wicket and the rest of the match.

str(t.test(df1$batsman_runs,df4$batsman_runs)) ##This is significant

xxx_divider1("Now we do this for reverse")
# We find balls that have a 6, we get the next ball and plot those

dfboundary<-subset(df1,total_runs>3)
dfboundary<-subset(dfboundary,total_runs!="W")
unique(dfboundary$total_runs)# This includes Wickets also, greater than 3 


dfemp1<-data.frame()
#for(i in 1:length(dfboundary$sno)){
#  
#  dfemp1[i,1:22]<-df1[dfboundary$sno[i]+1,1:22]
#}
# write.csv(dfemp1,"dfafterboundary.csv")
dfemp1<-read.csv("dfafterboundary.csv")
dfafterboundary<-dfemp1 #Remove first ball of the match
dfafterboundary<-subset(dfafterboundary,ball+over>2)

dfafterboundary$total_runs<-ifelse(is.na(dfafterboundary$player_dismissed),dfafterboundary$total_runs,"W")
prevballsummaryafterboundary<-dfafterboundary%>%group_by(total_runs,inning)%>% tally()
prevballsummaryafterboundary1<-filter(prevballsummaryafterboundary,inning==1)
prevballsummaryafterboundary2<-filter(prevballsummaryafterboundary,inning==2)
plot7<-ggplot(data=prevballsummaryafterboundary,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("After Boundary")
plot7
plot8<-ggplot(data=prevballsummaryafterboundary1,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("After Boundary in First Innings")+ylim(0,5500)
plot8
plot9<-ggplot(data=prevballsummaryafterboundary2,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("After Boundary in Second Innings")+ylim(0,5500)
plot9

## There is a difference in the number of boundaries
dfzero<-subset(df1,total_runs==0)
unique(dfzero$total_runs)# This is only zero
#dfemp2<-data.frame()
#for(i in 1:length(dfzero$sno)){
#  
#  dfemp2[i,1:22]<-df1[dfzero$sno[i]+1,1:22]
#}
#write.csv(dfemp2,"dfafterzero.csv")
dfemp2<-read.csv("dfafterzero.csv")

dfafterzero<-dfemp2
dfafterzero<-subset(dfafterzero,ball+over>2)
prevballsummaryafterzero<-dfafterzero%>%group_by(total_runs,inning)%>% tally()
prevballsummaryafterzero1<-filter(prevballsummaryafterzero,inning==1)
prevballsummaryafterzero2<-filter(prevballsummaryafterzero,inning==2)


plot10<-ggplot(data=prevballsummaryafterzero,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("After Dot")
plot10
plot11<-ggplot(data=prevballsummaryafterzero1,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("After Dot in First Innings") +ylim(0,10000)
plot11
plot12<-ggplot(data=prevballsummaryafterzero2,aes( x=total_runs,y=n))+geom_bar(stat="identity") +ggtitle("After Dot in Second Innings")+ylim(0,10000)
plot12
 ## There is also a difference in number of dot balls 

## We will need to get Percentage

prevballsummaryafterzero1$perc<-prevballsummaryafterzero1$n/sum(prevballsummaryafterzero1$n)
prevballsummaryafterzero2$perc<-prevballsummaryafterzero2$n/sum(prevballsummaryafterzero2$n)
prevballsummaryafterboundary1$perc<-prevballsummaryafterboundary1$n/sum(prevballsummaryafterboundary1$n)
prevballsummaryafterboundary2$perc<-prevballsummaryafterboundary2$n/sum(prevballsummaryafterboundary2$n)


plot13<-ggplot(data=prevballsummaryafterboundary1,aes( x=total_runs,y=perc))+geom_bar(stat="identity") +ggtitle("After Boundary in First Innings in %") +
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)
plot13
plot14<-ggplot(data=prevballsummaryafterboundary2,aes( x=total_runs,y=perc))+geom_bar(stat="identity") +ggtitle("After Boundary in Second Innings in %") +
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)
plot14

plot15<-ggplot(data=prevballsummaryafterzero1,aes( x=total_runs,y=perc))+geom_bar(stat="identity") +ggtitle("After Dot in First Innings in %") +
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)
plot15
plot16<-ggplot(data=prevballsummaryafterzero2,aes( x=total_runs,y=perc))+geom_bar(stat="identity") +ggtitle("After Dot in Second Innings, in %")+
  geom_text(aes(label=round(perc,2)), position=position_dodge(width=0.9), vjust=-0.25)
plot16

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

  

# Waste -------------------------------------------------------------------



library(dplyr)
library(stargazer)
library(bookdown)
library(ARTofR)
library(ggplot2)
#library(BSDA) #For z test
#library(dgof) #For z test
library(readr)
df1 <- read_csv("deliveries.csv")
df1<-cbind(1:length(df1$match_id),df1)
colnames(df1)[1]<-"sno"
## Subset of wickets
dfw<-subset(df1,!is.na(player_dismissed)) #To get all previous balls
dfw<-subset(dfw, over+ball >2) #no first ball wickets here
#df2<-data.frame()
#for(i in 1:length(dfw$sno)){

#  df2[i,1:22]<-df1[dfw$sno[i]-1,1:22]
#}
#write.csv(df2,"dfbeforewicket.csv")
df2 <- read_csv("dfbeforewicket.csv")
df2<-df2[,2:23] # needed because the file gets an extra first column taht is a duplication of another column
df3<-rbind(dfw,df2)
df3<-df3[order(df3$sno,decreasing=FALSE),]
  
df2<-as.data.frame(df2)
summary(lm(df2$total_runsOrg~df2$match_id+df2$over+df2$ball+df2$inning))
