library(RcmdrMisc)
library(effects)
library(ggplot2)
library(reshape)
library(plyr)
library(rlang)
library(foreign)
library(Rcmdr)
library(splines)
library(tidyverse)
# loading the data inyo r console
odidata<- read.csv(file.choose(), header= TRUE) 
odidata
#total number of matches played by india since 1 jan , 2000
noofmatches<- nrow(odidata)
noofmatches
#renaming the collumn names of dataset
colnames(odidata)
colnames(odidata)<- c("team","match.result.for.india","margin","margin.in.wickets","margin.in.runs","toss.result.for.india","batting.turn","opposition","host.country","date","dn.match","day.match","home","away","neutral","sena.host","asian.host","sena.opposition","asian.opposition","no.of.batsmen","no.of.bowlers","no.of.spinners","no.of.pacers","total.allrounders","no.of.spin.allrounders","no.of.fast.allrounders","india.runs","india.wickets.lost","opposition.runs","opposition.wickets.lost","100.by.indian.batsmen","no.of.100s.by.india.batsmen","indian.bowlers.taking.5.plus.wickets")
colnames(odidata)
# number of matches won and lost by india since 1 jan 2000
matcheswon<-nrow(odidata[odidata$match.result.for.india=="won",])
matcheswon
#no of matches lost by india since 1 jan 2000
matcheslost<- nrow(odidata[odidata$match.result.for.india=="lost",])
matcheslost
# no of matches india tied since 1 jan 2000
matchestied<-nrow(odidata[odidata$match.result.for.india=="tied",])
matchestied
# percentage of matches won, lost and tied by india since 1 jan 2000
perwon<- matcheswon/noofmatches*100
perwon
perlost<-matcheslost/noofmatches*100
perlost
pertied<-matchestied/noofmatches*100
pertied
#barplot of performance in matches since 1 jan 2000
performancenumber<-c(perwon,perlost,pertied)
performancenumber
performanceparameter<-c("percentage won","percentage lost","percentage tied")
performanceparameter
performance<-data.frame(parameter=performanceparameter,value=performancenumber)
performance
names(performancenumber)<-performanceparameter
performancenumber
#creating the graph
gt<-ggplot(performance, aes(x = parameter, y = value, fill = parameter))+geom_col()+geom_text(aes(label = round(performancenumber,2)), vjust = 1.5, colour = "black")+labs(title = "overall performance")+theme(legend.position = "top") 
gt#this is the graph
#performance in daynight matches
noofdaynightmatch<-nrow(odidata[odidata$dn.match=="TRUE",])
noofdaynightmatch
noofdaynightmatchwon<-nrow(odidata[odidata$dn.match=="TRUE" & odidata$match.result.for.india=="won",])
noofdaynightmatchwon
noofdaynightmatchlost<-nrow(odidata[odidata$dn.match=="TRUE" & odidata$match.result.for.india=="lost",])
noofdaynightmatchlost
noofdaynightmatchtied<-nrow(odidata[odidata$dn.match=="TRUE" & odidata$match.result.for.india=="tied",])
noofdaynightmatchtied
# percentage of d/n matches won, lost and tied by india since 1 jan 2000
perdaynightwon<-noofdaynightmatchwon/noofdaynightmatch*100
perdaynightwon
perdaynightlost<-noofdaynightmatchlost/noofdaynightmatch*100
perdaynightlost
perdaynighttied<-noofdaynightmatchtied/noofdaynightmatch*100
perdaynighttied
performancedaynightnumber<-c(perdaynightwon,perdaynightlost,perdaynighttied)
performanceparameter<-c("percentage won","percentage lost","percentage tied")
names(performancedaynightnumber)<-performanceparameter
performancedaynightnumber
dnperformance<-data.frame(parameter=performanceparameter,value=performancedaynightnumber)
dnperformance
gt2<-ggplot(dnperformance, aes(x = parameter, y = value, fill = parameter))+geom_col()+geom_text(aes(label = round(performancedaynightnumber,2)), vjust = 1.5, colour = "black")+labs(title = "d/n performance")+theme(legend.position = "top") 
gt2
# performance in day matches
noofdaymatch<-nrow(odidata[odidata$day.match=="TRUE",])
noofdaymatch
noofdaymatchwon<-nrow(odidata[odidata$day.match=="TRUE" & odidata$match.result.for.india=="won",])
noofdaymatchwon
noofdaymatchlost<-nrow(odidata[odidata$day.match=="TRUE" & odidata$match.result.for.india=="lost",])
noofdaymatchlost
noofdaymatchtied<-nrow(odidata[odidata$day.match=="TRUE" & odidata$match.result.for.india=="tied",])
noofdaymatchtied
# percentage of day matches won, lost and tied by india since 1 jan 2000
perdaywon<-noofdaymatchwon/noofdaymatch*100
perdaywon
perdaylost<-noofdaymatchlost/noofdaymatch*100
perdaylost
perdaytied<-noofdaymatchtied/noofdaymatch*100
perdaytied
performancedaynumber<-c(perdaywon,perdaylost,perdaytied)
performancedaynumber
performanceparameter<-c("percentage won","percentage lost","percentage tied")
names(performancedaynumber)<-performanceparameter
performancedaynumber
dayperformance<-data.frame(parameter=performanceparameter,value=performancedaynumber)
dayperformance
gt3<-ggplot(dayperformance, aes(x = parameter, y = value, fill = parameter))+geom_col()+geom_text(aes(label = round(performancedaynumber,2)), vjust = 1.5, colour = "black")+labs(title = "day performance")+theme(legend.position = "top") 
gt3

#--------------------------------------------------------
#comparative graphical representation of overall day and d/n matches
matchtype.comp<-c("overall","overall","overall","dn","dn","dn","day","day","day")
resulttype.comp<-c("win%","loss%","tie%","win%","loss%","tie%","win%","loss%","tie%")
value.comp<-c(perwon,perlost,pertied,perdaynightwon,perdaynightlost,perdaynighttied,perdaywon,perdaylost,perdaytied)
pp1<-data.frame(matchtype.comp,resulttype.comp,value.comp)
pp1
# creating the y-position for adding labels to the bars
pp1 <- ddply(pp1, .(matchtype.comp),transform, pos = cumsum(value.comp) - (0.5 *value.comp))
pp1
pp1$resulttype.comp <- factor(pp1$resulttype.comp, levels = c("tie%","loss%","win%"))
pp1
#creating the graph
stack.ar<-ggplot(pp1, aes(fill=resulttype.comp, y=value.comp, x=matchtype.comp)) + geom_bar(position="fill", stat="identity")
stack.ar #this is the graph
# adding the labels to the graph
fill <- c("deeppink2", "royalblue2","goldenrod1")
stack.ar+geom_text(aes(label = paste(round((value.comp),2),"%"),y=pos/100,size = 4,fontface="bold"))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("comparison of performance in overall,d/n & day matches") + scale_fill_manual(values=fill)+labs(x = "type of match", y = "%  distribution of results")
#-----------------------------------------------------

# toss result for india
tosseswon<- nrow(odidata[odidata$toss.result.for.india=="won",])
tosseswon
tosseslost<-nrow(odidata[odidata$toss.result.for.india=="lost",])
tosseslost
pertosswon<- tosseswon/noofmatches*100
pertosswon
pertosslost<-tosseslost/noofmatches*100
pertosslost
pertossresult<-c(pertosswon,pertosslost)
tossresulttype<-c("won","lost")
tossresulttype
tossmatrix<-as.matrix(pertossresult)
tossmatrix
rownames(tossmatrix)<-tossresulttype
colnames(tossmatrix)<-c("percentage")
tossmatrix
tossdf<-as.data.frame(tossmatrix)
tossdf
colnames(tossdf)
# barplot for toss results
ggtoss<-ggplot(tossdf, aes(x = tossresulttype, y = pertossresult, fill = tossresulttype))+geom_col()+geom_text(aes(label = round(percentage,2)), vjust = 1.5, colour = "black")+labs(title = "toss result",x="toss result",y="percentage of outcomes")+theme(legend.position = "top")+scale_fill_manual("toss result type",values=c("green","pink")) 
ggtoss
# india's decision after winnin toss in overall matches
tosseswon<- nrow(odidata[odidata$toss.result.for.india=="won",])
tosseswon
tosswonbatfirst<-nrow(odidata[odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
tosswonbatfirst
tosswonchase<-nrow(odidata[odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
tosswonchase

# barplot for toss decisions
pertosswonbatfirst<-tosswonbatfirst/tosseswon*100
pertosswonbatfirst
pertosswonbatsecond<-tosswonchase/tosseswon*100
pertosswonbatsecond
decision.type.toss.won<-c("bat 1t","bowl 1st")
percentagetossresult<-c(pertosswonbatfirst,pertosswonbatsecond)
percentagetossresult
tossdecisiondf<- data.frame(decision.type.toss.won,percentagetossresult)
tossdecisiondf
ggtosswon<-ggplot(tossdecisiondf, aes(x = decision.type.toss.won, y = percentagetossresult, fill = decision.type.toss.won))+geom_col()+geom_text(aes(label = round(percentagetossresult,2)), vjust = 1.5, colour = "black")+labs(title = "toss decision percentages",x="toss result",y="percentage of decisions")+theme(legend.position = "top")+scale_fill_manual("toss result decision",values=c("green","pink")) 
ggtosswon

#----------------------------------------------------------------------------------------------------------
# comparison of toss decisions in day vs d/n matches
noofdaymatch<-nrow(odidata[odidata$day.match=="TRUE",])
noofdaymatch
noofdaynightmatch<-nrow(odidata[odidata$dn.match=="TRUE",])
noofdaynightmatch
tosswonindaymatches<-nrow(odidata[odidata$day.match=="TRUE" & odidata$toss.result.for.india=="won",])
tosswonindaymatches
tosswonindnmatches<-nrow(odidata[odidata$dn.match=="TRUE" & odidata$toss.result.for.india=="won",])
tosswonindnmatches
tosswondaydecisionbat<-nrow(odidata[odidata$day.match=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
tosswondaydecisionbat
tosswondaydecisionchase<-nrow(odidata[odidata$day.match=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
tosswondaydecisionchase
tosswondndecisionbat<-nrow(odidata[odidata$dn.match=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
tosswondndecisionbat
tosswondndecisionchase<-nrow(odidata[odidata$dn.match=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
tosswondndecisionchase
pertossdaybatfirst<-tosswondaydecisionbat/tosswonindaymatches*100
pertossdaybatfirst
pertossdnbatfirst<-tosswondndecisionbat/tosswonindnmatches*100
pertossdnbatfirst
pertossdaychase<-tosswondaydecisionchase/tosswonindaymatches*100
pertossdaychase
pertossdnchase<-tosswondndecisionchase/tosswonindnmatches*100
pertossdnchase

# graphical representation of day vs dn matches decision on winning the toss
type.of.match.decision <-c("day bat first","day chase","d-n bat first","d-n chase") 
type.of.match.decision
day.dn.toss.decision.per<-c(pertossdaybatfirst,pertossdaychase,pertossdnbatfirst,pertossdnchase)
day.dn.toss.decision.per
df.day.dn.toss.decision<-data.frame(type.of.match.decision,day.dn.toss.decision.per)
df.day.dn.toss.decision 
ggtosswon.day.dn<-ggplot(df.day.dn.toss.decision, aes(x = type.of.match.decision, y = day.dn.toss.decision.per, fill = type.of.match.decision))+geom_col()+geom_text(aes(label = round(day.dn.toss.decision.per,2)), vjust = 1.5, colour = "black")+labs(title = "toss decision percentages in day VS d/n matches",x="mactch type and decision",y="percentage of decisions")+theme(legend.position = "top")+scale_fill_manual("toss result decision",values=c("darkorange","goldenrod1","royalblue3","cyan")) 
ggtosswon.day.dn

#----------------------------------------------------------------------------
# toss decision depending on host country
# SENA stands for south africa, england(uk),new zealand, australia
# 1. tosses won in sena countries
tosswoninsena<-nrow(odidata[odidata$sena.host=="TRUE" & odidata$toss.result.for.india=="won",])
tosswoninsena
# 2. tosses lost in sena
tosslostsena<-nrow(odidata[odidata$sena.host=="TRUE" & odidata$toss.result.for.india=="lost",])
tosslostsena
#3. number of matches in sena
matchessena<-nrow(odidata[odidata$sena.host=="TRUE",])
matchessena
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#1. tosses won in asian countries
tosswonasia<-nrow(odidata[odidata$asian.host=="TRUE" & odidata$toss.result.for.india=="won",])
tosswonasia
#2. tosses lost in asian countries
tosslostasia<-nrow(odidata[odidata$asian.host=="TRUE" & odidata$toss.result.for.india=="lost",])
tosslostasia
#3. matches held in asian countries
matchesasia<-nrow(odidata[odidata$asian.host=="TRUE",])
matchesasia
#----------------------------------------------------------------------
# decision making depending upon host country
#1. no and percentage of times india decided to bat first in sena
# no of times
dx1<-nrow(odidata[odidata$sena.host=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
dx1
#percentage of times
perdx1<-dx1/tosswoninsena*100
perdx1
#2. no and percentage of times india decided to chase in sena
# no of times
dx2<-nrow(odidata[odidata$sena.host=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
dx2
#percentage of times
perdx2<-dx2/tosswoninsena*100
perdx2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#1. no and percentage of times india decided to bat first in asia
# no of times
dx3<-nrow(odidata[odidata$asian.host=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
dx3
#percentage of times
perdx3<-dx3/tosswonasia*100
perdx3
#2. no and percentage of times india decided to chase in asia
# no of times
dx4<-nrow(odidata[odidata$asian.host=="TRUE" & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
dx4
#percentage of times
perdx4<-dx4/tosswonasia*100
perdx4

#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders and decision
host.type<-c("SENA","SENA","ASIA","ASIA")
host.type
dec.host.type<-c("BAT 1ST","BAT 2ND","BAT 1ST","BAT 2ND")
dec.host.type
dec.value.host.type<-c(perdx1,perdx2,perdx3,perdx4)
dec.value.host.type
pp1.host.type<-data.frame(host.type,dec.host.type,dec.value.host.type)
pp1.host.type
# creating the y-position for adding labels to the bars
pp1.host.type <- ddply(pp1.host.type, .(host.type),transform, pos = cumsum(100-dec.value.host.type) - (0.5 *(100-dec.value.host.type)))
pp1.host.type
pp1.host.type$dec.host.type <- factor(pp1.host.type$dec.host.type, levels = c("BAT 1ST","BAT 2ND"))

#creating the graph
host.type.toss.decision.graph<-ggplot(pp1.host.type, aes(fill=dec.host.type, y=dec.value.host.type, x=host.type)) + geom_bar(position="fill", stat="identity")
host.type.toss.decision.graph #this is the graph
# adding the labels to the graph
fill <- c("deeppink2", "royalblue2")
host.type.toss.decision.graph+geom_text(aes(label = paste(round((100-dec.value.host.type),2),"%"),y=pos/100,size = 4))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("toss decision %s in ASIA vs SENA") + scale_fill_manual("toss decision type",values=fill)+labs(x = "host type", y = "% of decisions post winning toss")


#--------------------------------------------------------------------------------------
# toss decision depending upon the number of allrounders
# 1. checking the number of allrounder(s) india have played in matches since 1 jan, 2000
no.of.allrounders<-unique(odidata$total.allrounders)
no.of.allrounders
# thus india have played3,2,1 and no (0) alrrounders in all matches
# now we'll check india's decision on toss depending on the number of allrounders
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 3 allrounders and india won the toss
#1. no of such matches
ar3<-nrow(odidata[odidata$total.allrounders==3 & odidata$toss.result.for.india=="won",])
ar3
#2. no of times india decided to bat first
ar3.bat.first<-nrow(odidata[odidata$total.allrounders==3 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
ar3.bat.first
#3. no of times india decided to chase
ar3.chase<-nrow(odidata[odidata$total.allrounders==3 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
ar3.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.ar3.bat.first<-ar3.bat.first/ar3*100
per.ar3.bat.first        
#5. percentage batting second
per.ar3.chase<-ar3.chase/ar3*100
per.ar3.chase
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 2 allrounders and india won the toss
#1. no of such matches
ar2<-nrow(odidata[odidata$total.allrounders==2 & odidata$toss.result.for.india=="won",])
ar2
#2. no of times india decided to bat first
ar2.bat.first<-nrow(odidata[odidata$total.allrounders==2 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
ar2.bat.first
#3. no of times india decided to chase
ar2.chase<-nrow(odidata[odidata$total.allrounders==2 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
ar2.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.ar2.bat.first<-ar2.bat.first/ar2*100
per.ar2.bat.first        
#5. percentage batting second
per.ar2.chase<-ar2.chase/ar2*100
per.ar2.chase
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 1 allrounder and india won the toss
#1. no of such matches
ar1<-nrow(odidata[odidata$total.allrounders==1 & odidata$toss.result.for.india=="won",])
ar1
#2. no of times india decided to bat first
ar1.bat.first<-nrow(odidata[odidata$total.allrounders==1 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
ar1.bat.first
#3. no of times india decided to chase
ar1.chase<-nrow(odidata[odidata$total.allrounders==1 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
ar1.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.ar1.bat.first<-ar1.bat.first/ar1*100
per.ar1.bat.first        
#5. percentage batting second
per.ar1.chase<-ar1.chase/ar1*100
per.ar1.chase
# when india played no allrounder and india won the toss
#1. no of such matches
ar0<-nrow(odidata[odidata$total.allrounders==0 & odidata$toss.result.for.india=="won",])
ar0
#2. no of times india decided to bat first
ar0.bat.first<-nrow(odidata[odidata$total.allrounders==0 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
ar0.bat.first
#3. no of times india decided to chase
ar0.chase<-nrow(odidata[odidata$total.allrounders==0 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
ar0.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.ar0.bat.first<-ar0.bat.first/ar0*100
per.ar0.bat.first        
#5. percentage batting second
per.ar0.chase<-ar0.chase/ar0*100
per.ar0.chase
#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders and decisions
no.of.allrounders<-sort(no.of.allrounders,decreasing=TRUE)
no.of.allrounders
ar.no<-c(3,3,2,2,1,1,0,0)
ar.no
dec.type<-c("bat 1st","chase","bat 1st","chase","bat 1st","chase","bat 1st","chase")
dec.type
dec.value<-c(per.ar3.bat.first,per.ar3.chase,per.ar2.bat.first,per.ar2.chase,per.ar1.bat.first,per.ar1.chase,per.ar0.bat.first,per.ar0.chase)
dec.value
pp1<-data.frame(ar.no,dec.type,dec.value)
pp1
# creating the y-position for adding labels to the bars
pp1 <- ddply(pp1, .(ar.no),transform, pos = cumsum((100-dec.value)) - (0.5 * (100-dec.value)))
pp1
#creating the graph
stack.ar<-ggplot(pp1, aes(fill=dec.type, y=dec.value, x=ar.no)) + geom_bar(position="fill", stat="identity")
stack.ar #this is the graph
# adding the labels to the graph
fill <- c("deeppink2", "royalblue2")
stack.ar+geom_text(aes(label = paste(round((100-dec.value),2),"%"),y=pos/100,size = 4))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("decision %s according to no of allroumders") + scale_fill_manual(values=fill)+labs(x = "number of allrounders", y = "% of decisions post winning toss")


#------------------------------------------------------------------------------
# match result depending upon the number of bowlers
# 1. checking the number of bowler(s) india have played in matches since 1 jan, 2000
no.of.bowlers<-unique(odidata$no.of.bowlers)
no.of.bowlers
# thus india have played 5,4,3 and 2 bowlers in all matches since 1 jan, 2000
# now we'll check result of the match depending on the number of bowlers
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 5 bowlers and india won the toss
#1. no of such matches
bow5<-nrow(odidata[odidata$no.of.bowlers==5 & odidata$toss.result.for.india=="won",])
bow5
#2. no of times india decided to bat first
bow5.bat.first<-nrow(odidata[odidata$no.of.bowlers==5 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bow5.bat.first
#3. no of times india decided to chase
bow5.chase<-nrow(odidata[odidata$no.of.bowlers==5 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bow5.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bow5.bat.first<-bow5.bat.first/bow5*100
per.bow5.bat.first        
#5. percentage batting second
per.bow5.chase<-bow5.chase/bow5*100
per.bow5.chase
#--------------------------------------------------------------------------------------------------------------
# when india played 4 bowlers and india won the toss
#1. no of such matches
bow4<-nrow(odidata[odidata$no.of.bowlers==4 & odidata$toss.result.for.india=="won",])
bow4
#2. no of times india decided to bat first
bow4.bat.first<-nrow(odidata[odidata$no.of.bowlers==4 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bow4.bat.first
#3. no of times india decided to chase
bow4.chase<-nrow(odidata[odidata$no.of.bowlers==4 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bow4.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bow4.bat.first<-bow4.bat.first/bow4*100
per.bow4.bat.first        
#5. percentage batting second
per.bow4.chase<-bow4.chase/bow4*100
per.bow4.chase
#--------------------------------------------------------------------------------------------------------------
# when india played 3 bowlers and india won the toss
#1. no of such matches
bow3<-nrow(odidata[odidata$no.of.bowlers==3 & odidata$toss.result.for.india=="won",])
bow3
#2. no of times india decided to bat first
bow3.bat.first<-nrow(odidata[odidata$no.of.bowlers==3 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bow3.bat.first
#3. no of times india decided to chase
bow3.chase<-nrow(odidata[odidata$no.of.bowlers==3 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bow3.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bow3.bat.first<-bow3.bat.first/bow3*100
per.bow3.bat.first        
#5. percentage batting second
per.bow3.chase<-bow3.chase/bow3*100
per.bow3.chase
#--------------------------------------------------------------------------------------------------------------
# when india played 2 bowlers and india won the toss
#1. no of such matches
bow2<-nrow(odidata[odidata$no.of.bowlers==2 & odidata$toss.result.for.india=="won",])
bow2
#2. no of times india decided to bat first
bow2.bat.first<-nrow(odidata[odidata$no.of.bowlers==2 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bow2.bat.first
#3. no of times india decided to chase
bow2.chase<-nrow(odidata[odidata$no.of.bowlers==2 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bow2.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bow2.bat.first<-bow2.bat.first/bow2*100
per.bow2.bat.first        
#5. percentage batting second
per.bow2.chase<-bow2.chase/bow2*100
per.bow2.chase
#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders and decisions
no.of.bowlers<-sort(no.of.bowlers,decreasing=TRUE)
no.of.bowlers
bow.no<-c(5,5,4,4,3,3,2,2)
bow.no
dec.type<-c("bat 1st","chase","bat 1st","chase","bat 1st","chase","bat 1st","chase")
dec.type
dec.value.2<-c(per.bow5.bat.first,per.bow5.chase,per.bow4.bat.first,per.bow4.chase,per.bow3.bat.first,per.bow3.chase,per.bow2.bat.first,per.bow2.chase)
dec.value.2
pp1.bow<-data.frame(bow.no,dec.type,dec.value.2)
pp1.bow
# creating the y-position for adding labels to the bars
pp1.bow <- ddply(pp1.bow, .(bow.no),transform, pos = cumsum((100-dec.value.2)) - (0.5 * (100-dec.value.2)))
pp1.bow
#creating the graph
stack.bow<-ggplot(pp1.bow, aes(fill=dec.type, y=dec.value.2, x=bow.no)) + geom_bar(position="fill", stat="identity")
stack.bow#this is the graph
# adding the labels to the graph
fill <- c("lawngreen", "dodgerblue")
stack.bow+geom_text(aes(label = paste(round((100-dec.value.2),2),"%"),y=pos/100,size = 4))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("decision %s according to no of bowlers") + scale_fill_manual(values=fill)+labs(x = "number of bowlers", y = "% of decisions post winning toss")


#------------------------------------------------------------------------------
# toss decision depending upon the number of batsmen
# 1. checking the number of batsmen india have played in matches since 1 jan, 2000
no.of.batsmen<-unique(odidata$no.of.batsmen)
no.of.batsmen
# thus india have played 7,6,5 and 4 batsmen in all matches since 1 jan, 2000
# now we'll check india's decision on toss depending on the number of batsmen
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 7 batsmen and india won the toss
#1. no of such matches
bat7<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$toss.result.for.india=="won",])
bat7
#2. no of times india decided to bat first
bat7.bat.first<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bat7.bat.first
#3. no of times india decided to chase
bat7.chase<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bat7.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bat7.bat.first<-bat7.bat.first/bat7*100
per.bat7.bat.first        
#5. percentage batting second
per.bat7.chase<-bat7.chase/bat7*100
per.bat7.chase

#--------------------------------------------------------------------------------------------------------------
# when india played 6 batsmen and india won the toss
#1. no of such matches
bat6<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$toss.result.for.india=="won",])
bat6
#2. no of times india decided to bat first
bat6.bat.first<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bat6.bat.first
#3. no of times india decided to chase
bat6.chase<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bat6.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bat6.bat.first<-bat6.bat.first/bat6*100
per.bat6.bat.first        
#5. percentage batting second
per.bat6.chase<-bat6.chase/bat6*100
per.bat6.chase


#--------------------------------------------------------------------------------------------------------------
# when india played 5 batsmen and india won the toss
#1. no of such matches
bat5<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$toss.result.for.india=="won",])
bat5
#2. no of times india decided to bat first
bat5.bat.first<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bat5.bat.first
#3. no of times india decided to chase
bat5.chase<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bat5.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bat5.bat.first<-bat5.bat.first/bat5*100
per.bat5.bat.first        
#5. percentage batting second
per.bat5.chase<-bat5.chase/bat5*100
per.bat5.chase

#--------------------------------------------------------------------------------------------------------------
# when india played 4 batsmen and india won the toss
#1. no of such matches
bat4<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$toss.result.for.india=="won",])
bat4
#2. no of times india decided to bat first
bat4.bat.first<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="1st",])
bat4.bat.first
#3. no of times india decided to chase
bat4.chase<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$toss.result.for.india=="won" & odidata$batting.turn=="2nd",])
bat4.chase
# now we calculate the percentage of such instances of decision
#4. percentage batting first
per.bat4.bat.first<-bat4.bat.first/bat4*100
per.bat4.bat.first        
#5. percentage batting second
per.bat4.chase<-bat4.chase/bat4*100
per.bat4.chase

#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders and decisions
no.of.batsmen<-sort(no.of.batsmen,decreasing=TRUE)
no.of.batsmen
bat.no<-c(7,7,6,6,5,5,4,4)
bat.no
dec.type<-c("bat 1st","chase","bat 1st","chase","bat 1st","chase","bat 1st","chase")
dec.type
dec.value.3<-c(per.bat7.bat.first,per.bat7.chase,per.bat6.bat.first,per.bat6.chase,per.bat5.bat.first,per.bat5.chase,per.bat4.bat.first,per.bat4.chase)
dec.value.3
pp1.bat<-data.frame(bat.no,dec.type,dec.value.3)
pp1.bat
# creating the y-position for adding labels to the bars
pp1.bat <- ddply(pp1.bat, .(bat.no),transform, pos = cumsum((100-dec.value.3)) - (0.5 * (100-dec.value.3)))
pp1.bat
#creating the graph
stack.bat<-ggplot(pp1.bat, aes(fill=dec.type, y=dec.value.3, x=bat.no)) + geom_bar(position="fill", stat="identity")
stack.bat#this is the graph
# adding the labels to the graph
fill <- c("lawngreen", "dodgerblue")
stack.bat+geom_text(aes(label = paste(round((100-dec.value.3),2),"%"),y=pos/100,size = 4, fontface="bold"))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("decision %s according to no of batsmen") + scale_fill_manual(values=fill)+labs(x = "number of batsmen", y = "% of decisions post winning toss")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#How no of bowlers affect the result for india 
no.of.bowlers<-sort(no.of.bowlers,decreasing=TRUE)
no.of.bowlers # number of bowlers india has played
# india has played 5,4,3,2 bowlers since 1 jan,2000
# affect on result when india plays 5 bowlers 
#1. no of such matches
b5<-nrow(odidata[odidata$no.of.bowlers==5,])
b5
#2. no of matches won
b5.won<-nrow(odidata[odidata$no.of.bowlers==5 & odidata$match.result.for.india=="won",])
b5.won
#3. no of matches lost
b5.lost<-nrow(odidata[odidata$no.of.bowlers==5 & odidata$match.result.for.india=="lost",])
b5.lost
#3. no of matches tied
b5.tied<-nrow(odidata[odidata$no.of.bowlers==5 & odidata$match.result.for.india=="tied",])
b5.tied
#above results in percentages
# winning %
per.b5.won<-b5.won/b5*100
per.b5.won
# loss %
per.b5.lost<-b5.lost/b5*100
per.b5.lost
# tied %
per.b5.tied<-b5.tied/b5*100
per.b5.tied

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# affect on result when india plays 4 bowlers
#1. no of such matches
b4<-nrow(odidata[odidata$no.of.bowlers==4,])
b4
#2. no of matches won
b4.won<-nrow(odidata[odidata$no.of.bowlers==4 & odidata$match.result.for.india=="won",])
b4.won
#3. no of matches lost
b4.lost<-nrow(odidata[odidata$no.of.bowlers==4 & odidata$match.result.for.india=="lost",])
b4.lost
#3. no of matches tied
b4.tied<-nrow(odidata[odidata$no.of.bowlers==4 & odidata$match.result.for.india=="tied",])
b4.tied
#above results in percentages
# winning %
per.b4.won<-b4.won/b4*100
per.b4.won
# loss %
per.b4.lost<-b4.lost/b4*100
per.b4.lost
# tied %
per.b4.tied<-b4.tied/b4*100
per.b4.tied

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# affect on result when india plays 3 bowlers
#1. no of such matches
b3<-nrow(odidata[odidata$no.of.bowlers==3,])
b3
#2. no of matches won
b3.won<-nrow(odidata[odidata$no.of.bowlers==3 & odidata$match.result.for.india=="won",])
b3.won
#3. no of matches lost
b3.lost<-nrow(odidata[odidata$no.of.bowlers==3 & odidata$match.result.for.india=="lost",])
b3.lost
#3. no of matches tied
b3.tied<-nrow(odidata[odidata$no.of.bowlers==3 & odidata$match.result.for.india=="tied",])
b3.tied
#above results in percentages
# winning %
per.b3.won<-b3.won/b3*100
per.b3.won
# loss %
per.b3.lost<-b3.lost/b3*100
per.b3.lost
# tied %
per.b3.tied<-b3.tied/b3*100
per.b3.tied

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# affect on result when india plays 2 bowlers
#1. no of such matches
b2<-nrow(odidata[odidata$no.of.bowlers==2,])
b2
#2. no of matches won
b2.won<-nrow(odidata[odidata$no.of.bowlers==2 & odidata$match.result.for.india=="won",])
b2.won
#3. no of matches lost
b2.lost<-nrow(odidata[odidata$no.of.bowlers==2 & odidata$match.result.for.india=="lost",])
b2.lost
#3. no of matches tied
b2.tied<-nrow(odidata[odidata$no.of.bowlers==2 & odidata$match.result.for.india=="tied",])
b2.tied
#above results in percentages
# winning %
per.b2.won<-b2.won/b2*100
per.b2.won
# loss %
per.b2.lost<-b2.lost/b2*100
per.b2.lost
# tied %
per.b2.tied<-b2.tied/b2*100
per.b2.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of bowlers and decisions
no.of.bowlers<-sort(no.of.bowlers,decreasing=TRUE)
no.of.bowlers
bw.no<-c(5,5,5,4,4,4,3,3,3,2,2,2)
bw.no
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
result.per.bw<-c(per.b5.won,per.b5.lost,per.b5.tied,per.b4.won,per.b4.lost,per.b4.tied,per.b3.won,per.b3.lost,per.b3.tied,per.b2.won,per.b2.lost,per.b2.tied)
result.per.bw
bw.numb.result<-data.frame(bw.no,result.type,result.per.bw)
bw.numb.result
# creating the y-position for adding labels to the bars
pp1.bw.res <- ddply(bw.numb.result, .(bw.no),transform, pos = cumsum(result.per.bw) - (0.5 * result.per.bw))
pp1.bw.res
pp1.bw.res$result.type <- factor(pp1.bw.res$result.type, levels = c("tied","lost","won"))

#creating the graph
no.of.bw.res.graph<-ggplot(pp1.bw.res, aes(fill=result.type, y=result.per.bw, x=bw.no)) + geom_bar(position="fill", stat="identity")
no.of.bw.res.graph#this is the graph
# adding the labels to the graph
fill <- c("maroon1", "lawngreen","dodgerblue")
no.of.bw.res.graph+geom_text(aes(label = paste(round((result.per.bw),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s and number of bowlers india played") + scale_fill_manual(values=fill)+labs(x = "number of bowlers", y = "% of match results")

#---------------------------------------------------------------------------------------------------------------------------------------
#How no of batsmen affect the result for india 
no.of.batsmen<-unique(odidata$no.of.batsmen)
no.of.batsmen # number of batsmen india has played
# india has played 7,6,5,4 batsmen since 1 jan,2000
# affect on result when india plays 7 batsmen 
#1. no of such matches
bt7<-nrow(odidata[odidata$no.of.batsmen==7,])
bt7
#2. no of matches won
bt7.won<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$match.result.for.india=="won",])
bt7.won
#3. no of matches lost
bt7.lost<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$match.result.for.india=="lost",])
bt7.lost
#3. no of matches tied
bt7.tied<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$match.result.for.india=="tied",])
bt7.tied
#above results in percentages
# winning %
per.bt7.won<-bt7.won/bt7*100
per.bt7.won
# loss %
per.bt7.lost<-bt7.lost/bt7*100
per.bt7.lost
# tied %
per.bt7.tied<-bt7.tied/bt7*100
per.bt7.tied

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# affect on result when india plays 6 batsmen 
#1. no of such matches
bt6<-nrow(odidata[odidata$no.of.batsmen==6,])
bt6
#2. no of matches won
bt6.won<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$match.result.for.india=="won",])
bt6.won
#3. no of matches lost
bt6.lost<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$match.result.for.india=="lost",])
bt6.lost
#3. no of matches tied
bt6.tied<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$match.result.for.india=="tied",])
bt6.tied
#above results in percentages
# winning %
per.bt6.won<-bt6.won/bt6*100
per.bt6.won
# loss %
per.bt6.lost<-bt6.lost/bt6*100
per.bt6.lost
# tied %
per.bt6.tied<-bt6.tied/bt6*100
per.bt6.tied

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# affect on result when india plays 5 batsmen 
#1. no of such matches
bt5<-nrow(odidata[odidata$no.of.batsmen==5,])
bt5
#2. no of matches won
bt5.won<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$match.result.for.india=="won",])
bt5.won
#3. no of matches lost
bt5.lost<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$match.result.for.india=="lost",])
bt5.lost
#3. no of matches tied
bt5.tied<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$match.result.for.india=="tied",])
bt5.tied
#above results in percentages
# winning %
per.bt5.won<-bt5.won/bt5*100
per.bt5.won
# loss %
per.bt5.lost<-bt5.lost/bt5*100
per.bt5.lost
# tied %
per.bt5.tied<-bt5.tied/bt5*100
per.bt5.tied

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# affect on result when india plays 4 batsmen 
#1. no of such matches
bt4<-nrow(odidata[odidata$no.of.batsmen==4,])
bt4
#2. no of matches won
bt4.won<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$match.result.for.india=="won",])
bt4.won
#3. no of matches lost
bt4.lost<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$match.result.for.india=="lost",])
bt4.lost
#3. no of matches tied
bt4.tied<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$match.result.for.india=="tied",])
bt4.tied
#above results in percentages
# winning %
per.bt4.won<-bt4.won/bt4*100
per.bt4.won
# loss %
per.bt4.lost<-bt4.lost/bt4*100
per.bt4.lost
# tied %
per.bt4.tied<-bt4.tied/bt4*100
per.bt4.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of batsmen and decisions
no.of.batsmen<-sort(no.of.batsmen,decreasing=TRUE)
no.of.batsmen
bt.no<-c(7,7,7,6,6,6,5,5,5,4,4,4)
bt.no
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
result.per.bt<-c(per.bt7.won,per.bt7.lost,per.bt7.tied,per.bt6.won,per.bt6.lost,per.bt6.tied,per.bt5.won,per.bt5.lost,per.bt5.tied,per.bt4.won,per.bt4.lost,per.bt4.tied)
result.per.bt
bt.numb.result<-data.frame(bt.no,result.type,result.per.bt)
bt.numb.result
# creating the y-position for adding labels to the bars
pp1.bt.res <- ddply(bt.numb.result, .(bt.no),transform, pos = cumsum(result.per.bt) - (0.5 * result.per.bt))
pp1.bt.res
pp1.bt.res$result.type <- factor(pp1.bt.res$result.type, levels = c("tied","lost","won"))

#creating the graph
no.of.bt.res.graph<-ggplot(pp1.bt.res, aes(fill=result.type, y=result.per.bt, x=bt.no)) + geom_bar(position="fill", stat="identity")
no.of.bt.res.graph#this is the graph
# adding the labels to the graph
fill <- c("maroon1", "lawngreen","dodgerblue")
no.of.bt.res.graph+geom_text(aes(label = paste(round((result.per.bt),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s and number of batsmen india played") + scale_fill_manual(values=fill)+labs(x = "number of batsmen", y = "% of match results")

#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------IN ASIAN COUNTRIES--------------------------------------------------------------------
# match result depending upon the number of bowlers
# 1. checking the number of bowler(s) india have played in asia since 1 jan, 2000
odidata.asia<-odidata[odidata$asian.host=="TRUE",]
odidata.asia
no.of.bowlers.asia<-unique(odidata.asia$no.of.bowlers)
no.of.bowlers.asia
# thus india have played 5,4,3 and 2 bowlers in all matches in ASIA since 1 jan, 2000
# now we'll check result of the matches in ASIA depending on the number of bowlers
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 5 bowlers and india won the match
#1. no of such matches when india played 5 bowlers in asia
bow5.asia<-nrow(odidata.asia[odidata.asia$no.of.bowlers==5 & odidata.asia$asian.host=="TRUE",])
bow5.asia
#2. no of times india won
asia.bow5.won<-nrow(odidata.asia[odidata.asia$no.of.bowlers==5 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bow5.won
#3. no of times india lost
asia.bow5.lost<-nrow(odidata.asia[odidata.asia$no.of.bowlers==5 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bow5.lost
#4. no of times india tied the match
asia.bow5.tied<-nrow(odidata.asia[odidata.asia$no.of.bowlers==5 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bow5.tied
# now we calculate the percentage of such instances of decision
#4. percentage won
per.asia.bow5.won<-asia.bow5.won/bow5.asia*100
per.asia.bow5.won        
#5. percentage lost
per.asia.bow5.lost<-asia.bow5.lost/bow5.asia*100
per.asia.bow5.lost
#6. percentage tied
per.asia.bow5.tied<-asia.bow5.tied/bow5.asia*100
per.asia.bow5.tied
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 4 bowlers in asia 
#1. no of such matches
bow4.asia<-nrow(odidata.asia[odidata.asia$no.of.bowlers==4 & odidata.asia$asian.host=="TRUE",])
bow4.asia
#2. no of times india won
asia.bow4.won<-nrow(odidata.asia[odidata.asia$no.of.bowlers==4 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bow4.won
#3. no of times india lost
asia.bow4.lost<-nrow(odidata.asia[odidata.asia$no.of.bowlers==4 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bow4.lost
#4. no of times india tied the match
asia.bow4.tied<-nrow(odidata.asia[odidata.asia$no.of.bowlers==4 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bow4.tied
# now we calculate the percentage of such instances of decision
#4. percentage won
per.asia.bow4.won<-asia.bow4.won/bow4.asia*100
per.asia.bow4.won        
#5. percentage lost
per.asia.bow4.lost<-asia.bow4.lost/bow4.asia*100
per.asia.bow4.lost
#6. percentage tied
per.asia.bow4.tied<-asia.bow4.tied/bow4.asia*100
per.asia.bow4.tied
#-------------------------------------------------------------------------------------------------------------------------------------
# when india played 3 bowlers in asia 
#1. no of such matches
bow3.asia<-nrow(odidata.asia[odidata.asia$no.of.bowlers==3 & odidata.asia$asian.host=="TRUE",])
bow3.asia
#2. no of times india won
asia.bow3.won<-nrow(odidata.asia[odidata.asia$no.of.bowlers==3 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bow3.won
#3. no of times india lost
asia.bow3.lost<-nrow(odidata.asia[odidata.asia$no.of.bowlers==3 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bow3.lost
#4. no of times india tied the match
asia.bow3.tied<-nrow(odidata.asia[odidata.asia$no.of.bowlers==3 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bow3.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.bow3.won<-asia.bow3.won/bow3.asia*100
per.asia.bow3.won        
#6. percentage lost
per.asia.bow3.lost<-asia.bow3.lost/bow3.asia*100
per.asia.bow3.lost
#7. percentage tied
per.asia.bow3.tied<-asia.bow3.tied/bow3.asia*100
per.asia.bow3.tied
#-------------------------------------------------------------------------------------------------------------------------------------
# when india played 2 bowlers in asia 
#1. no of such matches
bow2.asia<-nrow(odidata.asia[odidata.asia$no.of.bowlers==2 & odidata.asia$asian.host=="TRUE",])
bow2.asia
#2. no of times india won
asia.bow2.won<-nrow(odidata.asia[odidata.asia$no.of.bowlers==2 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bow2.won
#3. no of times india lost
asia.bow2.lost<-nrow(odidata.asia[odidata.asia$no.of.bowlers==2 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bow2.lost
#4. no of times india tied the match
asia.bow2.tied<-nrow(odidata.asia[odidata.asia$no.of.bowlers==2 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bow2.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.bow2.won<-asia.bow2.won/bow2.asia*100
per.asia.bow2.won        
#6. percentage lost
per.asia.bow2.lost<-asia.bow2.lost/bow2.asia*100
per.asia.bow2.lost
#7. percentage tied
per.asia.bow2.tied<-asia.bow2.tied/bow2.asia*100
per.asia.bow2.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of bowlers in asia and match results for india
no.of.bowlers.asia<-sort(no.of.bowlers.asia,decreasing=TRUE)
no.of.bowlers.asia
bow.no.asia<-c(5,5,5,4,4,4,3,3,3,2,2,2)
bow.no.asia
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
result.value.2.asia<-c(per.asia.bow5.won,per.asia.bow5.lost,per.asia.bow5.tied,per.asia.bow4.won,per.asia.bow4.lost,per.asia.bow4.tied,per.asia.bow3.won,per.asia.bow3.lost,per.asia.bow3.tied,per.asia.bow2.won,per.asia.bow2.lost,per.asia.bow2.tied)
result.value.2.asia
pp1.bow.asia<-data.frame(bow.no.asia,result.type,result.value.2.asia)
pp1.bow.asia
pp1.bow.asia$result.type <- factor(pp1.bow.asia$result.type, levels = c("tied","lost","won"))
# creating the y-position for adding labels to the bars
pp1.bow.asia <- ddply(pp1.bow.asia, .(bow.no.asia),transform, pos = cumsum(result.value.2.asia) - (0.5 * result.value.2.asia))
pp1.bow.asia
#creating the graph
bw.result.asia<-ggplot(pp1.bow.asia, aes(fill=result.type, y=result.value.2.asia, x=bow.no.asia)) + geom_bar(position="fill", stat="identity")
bw.result.asia#this is the graph
# adding the labels to the graph
fill <- c("maroon1", "lawngreen","dodgerblue")
bw.result.asia+geom_text(aes(label = paste(round((result.value.2.asia),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s in ASIA VS no of bowlers") + scale_fill_manual(values=fill)+labs(x = "NUMBER OF BOWLERS", y = "PERCENTAGE OF MATCH RESULTS")

#-------------------------------------------------------------------------------------------------
#-----------------------------------IN SENA COUNTRIES--------------------------------------------------------------------
# match result depending upon the number of bowlers played in sena countries
# 1. checking the number of bowler(s) india have played in sena countries since 1 jan, 2000
odidata.sena<-odidata[odidata$sena.host=="TRUE",]
odidata.sena
no.of.bowlers.sena<-unique(odidata.sena$no.of.bowlers)
no.of.bowlers.sena
# thus india have played 5,4,3 and 2 bowlers in all matches in sena since 1 jan, 2000
# now we'll check result of the match in sena depending on the number of bowlers
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 5 bowlers in sena
#1. no of such matches
bow5.sena<-nrow(odidata.sena[odidata.sena$no.of.bowlers==5 & odidata.sena$sena.host=="TRUE",])
bow5.sena
#2. no of times india won
sena.bow5.won<-nrow(odidata.sena[odidata.sena$no.of.bowlers==5 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bow5.won
#3. no of times india lost
sena.bow5.lost<-nrow(odidata.sena[odidata.sena$no.of.bowlers==5 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bow5.lost
#4. no of times india tied the match
sena.bow5.tied<-nrow(odidata.sena[odidata.sena$no.of.bowlers==5 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bow5.tied
# now we calculate the percentage of such instances of decision
#4. percentage won
per.sena.bow5.won<-sena.bow5.won/bow5.sena*100
per.sena.bow5.won        
#5. percentage lost
per.sena.bow5.lost<-sena.bow5.lost/bow5.sena*100
per.sena.bow5.lost
#6. percentage tied
per.sena.bow5.tied<-sena.bow5.tied/bow5.sena*100
per.sena.bow5.tied
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 4 bowlers in sena
#1. no of such matches
bow4.sena<-nrow(odidata.sena[odidata.sena$no.of.bowlers==4 & odidata.sena$sena.host=="TRUE",])
bow4.sena
#2. no of times india won
sena.bow4.won<-nrow(odidata.sena[odidata.sena$no.of.bowlers==4 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bow4.won
#3. no of times india lost
sena.bow4.lost<-nrow(odidata.sena[odidata.sena$no.of.bowlers==4 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bow4.lost
#4. no of times india tied the match
sena.bow4.tied<-nrow(odidata.sena[odidata.sena$no.of.bowlers==4 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bow4.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bow4.won<-sena.bow4.won/bow4.sena*100
per.sena.bow4.won        
#6. percentage lost
per.sena.bow4.lost<-sena.bow4.lost/bow4.sena*100
per.sena.bow4.lost
#7. percentage tied
per.sena.bow4.tied<-sena.bow4.tied/bow4.sena*100
per.sena.bow4.tied

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 3 bowlers in sena
#1. no of such matches
bow3.sena<-nrow(odidata.sena[odidata.sena$no.of.bowlers==3 & odidata.sena$sena.host=="TRUE",])
bow3.sena
#2. no of times india won
sena.bow3.won<-nrow(odidata.sena[odidata.sena$no.of.bowlers==3 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bow3.won
#3. no of times india lost
sena.bow3.lost<-nrow(odidata.sena[odidata.sena$no.of.bowlers==3 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bow3.lost
#4. no of times india tied the match
sena.bow3.tied<-nrow(odidata.sena[odidata.sena$no.of.bowlers==3 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bow3.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bow3.won<-sena.bow3.won/bow3.sena*100
per.sena.bow3.won        
#6. percentage lost
per.sena.bow3.lost<-sena.bow3.lost/bow3.sena*100
per.sena.bow3.lost
#7. percentage tied
per.sena.bow3.tied<-sena.bow3.tied/bow3.sena*100
per.sena.bow3.tied

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 2 bowlers in sena
#1. no of such matches
bow2.sena<-nrow(odidata.sena[odidata.sena$no.of.bowlers==2 & odidata.sena$sena.host=="TRUE",])
bow2.sena
#2. no of times india won
sena.bow2.won<-nrow(odidata.sena[odidata.sena$no.of.bowlers==2 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bow2.won
#3. no of times india lost
sena.bow2.lost<-nrow(odidata.sena[odidata.sena$no.of.bowlers==2 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bow2.lost
#4. no of times india tied the match
sena.bow2.tied<-nrow(odidata.sena[odidata.sena$no.of.bowlers==2 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bow2.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bow2.won<-sena.bow2.won/bow2.sena*100
per.sena.bow2.won        
#6. percentage lost
per.sena.bow2.lost<-sena.bow2.lost/bow2.sena*100
per.sena.bow2.lost
#7. percentage tied
per.sena.bow2.tied<-sena.bow2.tied/bow2.sena*100
per.sena.bow2.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of bowlers in sena and match results for india
no.of.bowlers.sena<-sort(no.of.bowlers.sena,decreasing=TRUE)
no.of.bowlers.sena
bow.no.sena<-c(5,5,5,4,4,4,3,3,3,2,2,2)
bow.no.sena
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
bow.result.value.2.sena<-c(per.sena.bow5.won,per.sena.bow5.lost,per.sena.bow5.tied,per.sena.bow4.won,per.sena.bow4.lost,per.sena.bow4.tied,per.sena.bow3.won,per.sena.bow3.lost,per.sena.bow3.tied,per.sena.bow2.won,per.sena.bow2.lost,per.sena.bow2.tied)
bow.result.value.2.sena
pp1.bow.sena<-data.frame(bow.no.sena,result.type,bow.result.value.2.sena)
pp1.bow.sena
pp1.bow.sena$result.type <- factor(pp1.bow.sena$result.type, levels = c("tied","lost","won"))
# creating the y-position for adding labels to the bars
pp1.bow.sena <- ddply(pp1.bow.sena, .(bow.no.sena),transform, pos = cumsum(bow.result.value.2.sena) - (0.5 * bow.result.value.2.sena))
pp1.bow.sena
#creating the graph
bw.result.sena<-ggplot(pp1.bow.sena, aes(fill=result.type, y=bow.result.value.2.sena, x=bow.no.sena)) + geom_bar(position="fill", stat="identity")
bw.result.sena#this is the graph
# adding the labels to the graph
fill <- c("maroon1", "lawngreen","dodgerblue")
bw.result.sena+geom_text(aes(label = paste(round((bow.result.value.2.sena),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s in SENA VS no of bowlers") + scale_fill_manual(values=fill)+labs(x = "NUMBER OF BOWLERS", y = "PERCENTAGE OF MATCH RESULTS")

#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------IN ASIAN COUNTRIES--------------------------------------------------------------------
# match result depending upon the number of batsmen
# 1. checking the number of batsmen india have played in asia since 1 jan, 2000
odidata.asia<-odidata[odidata$asian.host=="TRUE",]
odidata.asia
no.of.batsmen.asia<-unique(odidata.asia$no.of.batsmen)
no.of.batsmen.asia
# thus india have played 7,6,5 and 4 batsmen in all matches in ASIA since 1 jan, 2000
# now we'll check result of the match in ASIA depending on the number of batsmen
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 7 batsmen in asia
#1. no of such matches
bt7.asia<-nrow(odidata.asia[odidata.asia$no.of.batsmen==7 & odidata.asia$asian.host=="TRUE",])
bt7.asia
#2. no of times india won
asia.bt7.won<-nrow(odidata.asia[odidata.asia$no.of.batsmen==7 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bt7.won
#3. no of times india lost
asia.bt7.lost<-nrow(odidata.asia[odidata.asia$no.of.batsmen==7 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bt7.lost
#4. no of times india tied the match
asia.bt7.tied<-nrow(odidata.asia[odidata.asia$no.of.batsmen==7 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bt7.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.bt7.won<-asia.bt7.won/bt7.asia*100
per.asia.bt7.won        
#6. percentage lost
per.asia.bt7.lost<-asia.bt7.lost/bt7.asia*100
per.asia.bt7.lost
#7. percentage tied
per.asia.bt7.tied<-asia.bt7.tied/bt7.asia*100
per.asia.bt7.tied

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 6 batsmen in asia
#1. no of such matches
bt6.asia<-nrow(odidata.asia[odidata.asia$no.of.batsmen==6 & odidata.asia$asian.host=="TRUE",])
bt6.asia
#2. no of times india won
asia.bt6.won<-nrow(odidata.asia[odidata.asia$no.of.batsmen==6 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bt6.won
#3. no of times india lost
asia.bt6.lost<-nrow(odidata.asia[odidata.asia$no.of.batsmen==6 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bt6.lost
#4. no of times india tied the match
asia.bt6.tied<-nrow(odidata.asia[odidata.asia$no.of.batsmen==6 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bt6.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.bt6.won<-asia.bt6.won/bt6.asia*100
per.asia.bt6.won        
#6. percentage lost
per.asia.bt6.lost<-asia.bt6.lost/bt6.asia*100
per.asia.bt6.lost
#7. percentage tied
per.asia.bt6.tied<-asia.bt6.tied/bt6.asia*100
per.asia.bt6.tied
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 5 batsmen in asia
#1. no of such matches
bt5.asia<-nrow(odidata.asia[odidata.asia$no.of.batsmen==5 & odidata.asia$asian.host=="TRUE",])
bt5.asia
#2. no of times india won
asia.bt5.won<-nrow(odidata.asia[odidata.asia$no.of.batsmen==5 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bt5.won
#3. no of times india lost
asia.bt5.lost<-nrow(odidata.asia[odidata.asia$no.of.batsmen==5 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bt5.lost
#4. no of times india tied the match
asia.bt5.tied<-nrow(odidata.asia[odidata.asia$no.of.batsmen==5 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bt5.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.bt5.won<-asia.bt5.won/bt5.asia*100
per.asia.bt5.won        
#6. percentage lost
per.asia.bt5.lost<-asia.bt5.lost/bt5.asia*100
per.asia.bt5.lost
#7. percentage tied
per.asia.bt5.tied<-asia.bt5.tied/bt5.asia*100
per.asia.bt5.tied
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 4 batsmen in asia
#1. no of such matches
bt4.asia<-nrow(odidata.asia[odidata.asia$no.of.batsmen==4 & odidata.asia$asian.host=="TRUE",])
bt4.asia
#2. no of times india won
asia.bt4.won<-nrow(odidata.asia[odidata.asia$no.of.batsmen==4 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.bt4.won
#3. no of times india lost
asia.bt4.lost<-nrow(odidata.asia[odidata.asia$no.of.batsmen==4 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.bt4.lost
#4. no of times india tied the match
asia.bt4.tied<-nrow(odidata.asia[odidata.asia$no.of.batsmen==4 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.bt4.tied
# now we calculate the percentage of such instances of decision
#4. percentage won
per.asia.bt4.won<-asia.bt4.won/bt4.asia*100
per.asia.bt4.won        
#6. percentage lost
per.asia.bt4.lost<-asia.bt4.lost/bt4.asia*100
per.asia.bt4.lost
#7. percentage tied
per.asia.bt4.tied<-asia.bt4.tied/bt4.asia*100
per.asia.bt4.tied
#----------------------------------------------------------------------------------------------
#comparative graphical representation of batsmen in asia and match results for india
no.of.batsmen.asia<-sort(no.of.batsmen.asia,decreasing=TRUE)
no.of.batsmen.asia
bt.no.asia<-c(7,7,7,6,6,6,5,5,5,4,4,4)
bt.no.asia
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
bt.result.value.2.asia<-c(per.asia.bt7.won,per.asia.bt7.lost,per.asia.bt7.tied,per.asia.bt6.won,per.asia.bt6.lost,per.asia.bt6.tied,per.asia.bt5.won,per.asia.bt5.lost,per.asia.bt5.tied,per.asia.bt4.won,per.asia.bt4.lost,per.asia.bt4.tied)
bt.result.value.2.asia
pp1.bt.asia<-data.frame(bt.no.asia,result.type,bt.result.value.2.asia)
pp1.bt.asia
pp1.bt.asia$result.type <- factor(pp1.bt.asia$result.type, levels = c("tied","lost","won"))
# creating the y-position for adding labels to the bars
pp1.bt.asia <- ddply(pp1.bt.asia, .(bt.no.asia),transform, pos = cumsum(bt.result.value.2.asia) - (0.5 * bt.result.value.2.asia))
pp1.bt.asia
#creating the graph
bt.result.asia<-ggplot(pp1.bt.asia, aes(fill=result.type, y=bt.result.value.2.asia, x=bt.no.asia)) + geom_bar(position="fill", stat="identity")
bt.result.asia#this is the graph
# adding the labels to the graph
fill <- c("maroon1", "lawngreen","dodgerblue")
bt.result.asia+geom_text(aes(label = paste(round((bt.result.value.2.asia),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s in ASIA VS no of batsmen") + scale_fill_manual(values=fill)+labs(x = "NUMBER OF BATSMEN", y = "PERCENTAGE OF MATCH RESULTS")
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------IN SENA COUNTRIES--------------------------------------------------------------------
# match result depending upon the number of batsmen
# 1. checking the number of batsmen(s) india have played in SENA since 1 jan, 2000
odidata.sena<-odidata[odidata$sena.host=="TRUE",]
odidata.sena
no.of.batsmen.sena<-unique(odidata.asia$no.of.batsmen)
no.of.batsmen.sena
# thus india have played 7,6,5 and 4 batsmen in all matches in SENA since 1 jan, 2000
# now we'll check result of the match in SENA depending on the number of batsmen
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 5 batsmen and india won the match
#1. no of such matches
# thus india have played 7,6,5 and 4batsmen in matches in SENA since 1 jan, 2000
# now we'll check result of the match depending on the number of batsmen
# when india played 7 batsmen in SENA
#1. no of such matches
bt7.sena<-nrow(odidata.sena[odidata.sena$no.of.batsmen==7 & odidata.sena$sena.host=="TRUE",])
bt7.sena
#2. no of times india won
sena.bt7.won<-nrow(odidata.sena[odidata.sena$no.of.batsmen==7 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bt7.won
#3. no of times india lost
sena.bt7.lost<-nrow(odidata.sena[odidata.sena$no.of.batsmen==7 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bt7.lost
#4. no of times india tied the match
sena.bt7.tied<-nrow(odidata.sena[odidata.sena$no.of.batsmen==7 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bt7.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bt7.won<-sena.bt7.won/bt7.sena*100
per.sena.bt7.won        
#6. percentage lost
per.sena.bt7.lost<-sena.bt7.lost/bt7.sena*100
per.sena.bt7.lost
#7. percentage tied
per.sena.bt7.tied<-sena.bt7.tied/bt7.sena*100
per.sena.bt7.tied
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#1. no of such matches
bt6.sena<-nrow(odidata.sena[odidata.sena$no.of.batsmen==6 & odidata.sena$sena.host=="TRUE",])
bt6.sena
#2. no of times india won
sena.bt6.won<-nrow(odidata.sena[odidata.sena$no.of.batsmen==6 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bt6.won
#3. no of times india lost
sena.bt6.lost<-nrow(odidata.sena[odidata.sena$no.of.batsmen==6 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bt6.lost
#4. no of times india tied the match
sena.bt6.tied<-nrow(odidata.sena[odidata.sena$no.of.batsmen==6 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bt6.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bt6.won<-sena.bt6.won/bt6.sena*100
per.sena.bt6.won        
#6. percentage lost
per.sena.bt6.lost<-sena.bt6.lost/bt6.sena*100
per.sena.bt6.lost
#7. percentage tied
per.sena.bt6.tied<-sena.bt6.tied/bt6.sena*100
per.sena.bt6.tied
#------------------------------------------------------------------------------------
#1. no of such matches
bt5.sena<-nrow(odidata.sena[odidata.sena$no.of.batsmen==5 & odidata.sena$sena.host=="TRUE",])
bt5.sena
#2. no of times india won
sena.bt5.won<-nrow(odidata.sena[odidata.sena$no.of.batsmen==5 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bt5.won
#3. no of times india lost
sena.bt5.lost<-nrow(odidata.sena[odidata.sena$no.of.batsmen==5 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bt5.lost
#4. no of times india tied the match
sena.bt5.tied<-nrow(odidata.sena[odidata.sena$no.of.batsmen==5 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bt5.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bt5.won<-sena.bt5.won/bt5.sena*100
per.sena.bt5.won        
#6. percentage lost
per.sena.bt5.lost<-sena.bt5.lost/bt5.sena*100
per.sena.bt5.lost
#7. percentage tied
per.sena.bt5.tied<-sena.bt5.tied/bt5.sena*100
per.sena.bt5.tied
#------------------------------------------------------------------------------------
#1. no of such matches
bt4.sena<-nrow(odidata.sena[odidata.sena$no.of.batsmen==4 & odidata.sena$sena.host=="TRUE",])
bt4.sena
#2. no of times india won
sena.bt4.won<-nrow(odidata.sena[odidata.sena$no.of.batsmen==4 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.bt4.won
#3. no of times india lost
sena.bt4.lost<-nrow(odidata.sena[odidata.sena$no.of.batsmen==4 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.bt4.lost
#4. no of times india tied the match
sena.bt4.tied<-nrow(odidata.sena[odidata.sena$no.of.batsmen==4 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.bt4.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.bt4.won<-sena.bt4.won/bt4.sena*100
per.sena.bt4.won        
#6. percentage lost
per.sena.bt4.lost<-sena.bt4.lost/bt4.sena*100
per.sena.bt4.lost
#7. percentage tied
per.sena.bt4.tied<-sena.bt4.tied/bt4.sena*100
per.sena.bt4.tied
#----------------------------------------------------------------------------------------------
#comparative graphical representation of batsmen in sena and match results for india
no.of.batsmen.sena<-sort(no.of.batsmen.sena,decreasing=TRUE)
no.of.batsmen.sena
bt.no.sena<-c(7,7,7,6,6,6,5,5,5,4,4,4)
bt.no.sena
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
bt.result.value.2.sena<-c(per.sena.bt7.won,per.sena.bt7.lost,per.sena.bt7.tied,per.sena.bt6.won,per.sena.bt6.lost,per.sena.bt6.tied,per.sena.bt5.won,per.sena.bt5.lost,per.sena.bt5.tied,per.sena.bt4.won,per.sena.bt4.lost,per.sena.bt4.tied)
bt.result.value.2.sena
pp1.bt.sena<-data.frame(bt.no.sena,result.type,bt.result.value.2.sena)
pp1.bt.sena
pp1.bt.sena$result.type <- factor(pp1.bt.sena$result.type, levels = c("tied","lost","won"))
# creating the y-position for adding labels to the bars
pp1.bt.sena <- ddply(pp1.bt.sena, .(bt.no.sena),transform, pos = cumsum(bt.result.value.2.sena) - (0.5 * bt.result.value.2.sena))
pp1.bt.sena
#creating the graph
bt.result.sena<-ggplot(pp1.bt.sena, aes(fill=result.type, y=bt.result.value.2.sena, x=bt.no.sena)) + geom_bar(position="fill", stat="identity")
bt.result.sena#this is the graph
# adding the labels to the graph
fill <- c("seagreen3", "orange1","dodgerblue2")
bt.result.sena+geom_text(aes(label = paste(round((bt.result.value.2.sena),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s in SENA VS no of batsmen") + scale_fill_manual(values=fill)+labs(x = "NUMBER OF BATSMEN", y = "PERCENTAGE OF MATCH RESULTS")


#---------------------------------------------------------------------------------------------------------------------------------------
#How no of allrounders affect the result for india 
total.allrounders<-unique(odidata$total.allrounders)
total.allrounders # number of allrounders india has played
# india has played 2,1,3 and 0 allrounders since 1 jan,2000
# affect on result when india plays 3 allrounders 
#1. no of such matches
ar3<-nrow(odidata[odidata$total.allrounders==3,])
ar3
#2. no of matches won
ar3.won<-nrow(odidata[odidata$total.allrounders==3 & odidata$match.result.for.india=="won",])
ar3.won
#3. no of matches lost
ar3.lost<-nrow(odidata[odidata$total.allrounders==3 & odidata$match.result.for.india=="lost",])
ar3.lost
#4. no of matches tied
ar3.tied<-nrow(odidata[odidata$total.allrounders==3 & odidata$match.result.for.india=="tied",])
ar3.tied
#above results in percentages
# winning %
per.ar3.won<-ar3.won/ar3*100
per.ar3.won
# loss %
per.ar3.lost<-ar3.lost/ar3*100
per.ar3.lost
# tied %
per.ar3.tied<-ar3.tied/ar3*100
per.ar3.tied
# affect on result when india plays 2 allrounders 
#1. no of such matches
ar2<-nrow(odidata[odidata$total.allrounders==2,])
ar2
#2. no of matches won
ar2.won<-nrow(odidata[odidata$total.allrounders==2 & odidata$match.result.for.india=="won",])
ar2.won
#3. no of matches lost
ar2.lost<-nrow(odidata[odidata$total.allrounders==2 & odidata$match.result.for.india=="lost",])
ar2.lost
#4. no of matches tied
ar2.tied<-nrow(odidata[odidata$total.allrounders==2 & odidata$match.result.for.india=="tied",])
ar2.tied
#above results in percentages
# winning %
per.ar2.won<-ar2.won/ar2*100
per.ar2.won
# loss %
per.ar2.lost<-ar2.lost/ar2*100
per.ar2.lost
# tied %
per.ar2.tied<-ar2.tied/ar2*100
per.ar2.tied
# affect on result when india plays 1 allrounders 
#1. no of such matches
ar1<-nrow(odidata[odidata$total.allrounders==1,])
ar1
#2. no of matches won
ar1.won<-nrow(odidata[odidata$total.allrounders==1 & odidata$match.result.for.india=="won",])
ar1.won
#3. no of matches lost
ar1.lost<-nrow(odidata[odidata$total.allrounders==1 & odidata$match.result.for.india=="lost",])
ar1.lost
#4. no of matches tied
ar1.tied<-nrow(odidata[odidata$total.allrounders==1 & odidata$match.result.for.india=="tied",])
ar1.tied
#above results in percentages
# winning %
per.ar1.won<-ar1.won/ar1*100
per.ar1.won
# loss %
per.ar1.lost<-ar1.lost/ar1*100
per.ar1.lost
# tied %
per.ar1.tied<-ar1.tied/ar1*100
per.ar1.tied

# affect on result when india plays 0 allrounders 
#1. no of such matches
ar0<-nrow(odidata[odidata$total.allrounders==0,])
ar0
#2. no of matches won
ar0.won<-nrow(odidata[odidata$total.allrounders==0 & odidata$match.result.for.india=="won",])
ar0.won
#3. no of matches lost
ar0.lost<-nrow(odidata[odidata$total.allrounders==0 & odidata$match.result.for.india=="lost",])
ar0.lost
#4. no of matches tied
ar0.tied<-nrow(odidata[odidata$total.allrounders==0 & odidata$match.result.for.india=="tied",])
ar0.tied
#above results in percentages
# winning %
per.ar0.won<-ar0.won/ar0*100
per.ar0.won
# loss %
per.ar0.lost<-ar0.lost/ar0*100
per.ar0.lost
# tied %
per.ar0.tied<-ar0.tied/ar0*100
per.ar0.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders and decisions
no.of.allrounders<-sort(no.of.allrounders,decreasing=TRUE)
no.of.allrounders
ar.no<-c(3,3,3,2,2,2,1,1,1,0,0,0)
ar.no
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
result.per.ar<-c(per.ar3.won,per.ar3.lost,per.ar3.tied,per.ar2.won,per.ar2.lost,per.ar2.tied,per.ar1.won,per.ar1.lost,per.ar1.tied,per.ar0.won,per.ar0.lost,per.ar0.tied)
result.per.ar
ar.numb.result<-data.frame(ar.no,result.type,result.per.ar)
ar.numb.result
# creating the y-position for adding labels to the bars
pp1.ar.res <- ddply(ar.numb.result, .(ar.no),transform, pos = cumsum(result.per.ar) - (0.5 * result.per.ar))
pp1.ar.res
pp1.ar.res$result.type <- factor(pp1.ar.res$result.type, levels = c("tied","lost","won"))

#creating the graph
no.of.ar.res.graph<-ggplot(pp1.ar.res, aes(fill=result.type, y=result.per.ar, x=ar.no)) + geom_bar(position="fill", stat="identity")
no.of.ar.res.graph#this is the graph
# adding the labels to the graph
fill <- c("gold1", "turquoise3","deeppink")
no.of.ar.res.graph+geom_text(aes(label = paste(round((result.per.ar),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s and number of allrounders india played") + scale_fill_manual(values=fill)+labs(x = "number of allrounders", y = "% of match results")
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------IN ASIAN COUNTRIES--------------------------------------------------------------------
# match result depending upon the number of allrounders
# 1. checking the number of allrounders(s) india have played in asia since 1 jan, 2000
odidata.asia<-odidata[odidata$asian.host=="TRUE",]
odidata.asia
total.allrounders.asia<-unique(odidata.asia$total.allrounders)
total.allrounders.asia
# thus india have played 3,2,1,0 allrounders in all matches in ASIA since 1 jan, 2000
# now we'll check result of the match in ASIA depending on the number of allrounders
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 3 allrounders and india won the match
#1. no of such matches
ar3.asia<-nrow(odidata.asia[odidata.asia$total.allrounders==3 & odidata.asia$asian.host=="TRUE",])
ar3.asia
#2. no of times india won
asia.ar3.won<-nrow(odidata.asia[odidata.asia$total.allrounders==3 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.ar3.won
#3. no of times india lost
asia.ar3.lost<-nrow(odidata.asia[odidata.asia$total.allrounders==3 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.ar3.lost
#4. no of times india tied the match
asia.ar3.tied<-nrow(odidata.asia[odidata.asia$total.allrounders==3 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.ar3.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.ar3.won<-asia.ar3.won/ar3.asia*100
per.asia.ar3.won        
#6. percentage lost
per.asia.ar3.lost<-asia.ar3.lost/ar3.asia*100
per.asia.ar3.lost
#7. percentage tied
per.asia.ar3.tied<-asia.ar3.tied/ar3.asia*100
per.asia.ar3.tied

# when india played 2 allrounders and india won the match
#1. no of such matches
ar2.asia<-nrow(odidata.asia[odidata.asia$total.allrounders==2 & odidata.asia$asian.host=="TRUE",])
ar2.asia
#2. no of times india won
asia.ar2.won<-nrow(odidata.asia[odidata.asia$total.allrounders==2 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.ar2.won
#3. no of times india lost
asia.ar2.lost<-nrow(odidata.asia[odidata.asia$total.allrounders==2 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.ar2.lost
#4. no of times india tied the match
asia.ar2.tied<-nrow(odidata.asia[odidata.asia$total.allrounders==2 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.ar2.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.ar2.won<-asia.ar2.won/ar2.asia*100
per.asia.ar2.won        
#6. percentage lost
per.asia.ar2.lost<-asia.ar2.lost/ar2.asia*100
per.asia.ar2.lost
#7. percentage tied
per.asia.ar2.tied<-asia.ar2.tied/ar2.asia*100
per.asia.ar2.tied

# when india played 1 allrounders and india won the match
#1. no of such matches
ar1.asia<-nrow(odidata.asia[odidata.asia$total.allrounders==1 & odidata.asia$asian.host=="TRUE",])
ar1.asia
#2. no of times india won
asia.ar1.won<-nrow(odidata.asia[odidata.asia$total.allrounders==1 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.ar1.won
#3. no of times india lost
asia.ar1.lost<-nrow(odidata.asia[odidata.asia$total.allrounders==1 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.ar1.lost
#4. no of times india tied the match
asia.ar1.tied<-nrow(odidata.asia[odidata.asia$total.allrounders==1 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.ar1.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.ar1.won<-asia.ar1.won/ar1.asia*100
per.asia.ar1.won        
#6. percentage lost
per.asia.ar1.lost<-asia.ar1.lost/ar1.asia*100
per.asia.ar1.lost
#7. percentage tied
per.asia.ar1.tied<-asia.ar1.tied/ar1.asia*100
per.asia.ar1.tied

# when india played 0 allrounders and india won the match
#1. no of such matches
ar0.asia<-nrow(odidata.asia[odidata.asia$total.allrounders==0 & odidata.asia$asian.host=="TRUE",])
ar0.asia
#2. no of times india won
asia.ar0.won<-nrow(odidata.asia[odidata.asia$total.allrounders==0 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="won",])
asia.ar0.won
#3. no of times india lost
asia.ar0.lost<-nrow(odidata.asia[odidata.asia$total.allrounders==0 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="lost",])
asia.ar0.lost
#4. no of times india tied the match
asia.ar0.tied<-nrow(odidata.asia[odidata.asia$total.allrounders==0 & odidata.asia$asian.host=="TRUE" & odidata.asia$match.result.for.india=="tied",])
asia.ar0.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.asia.ar0.won<-asia.ar0.won/ar0.asia*100
per.asia.ar0.won        
#6. percentage lost
per.asia.ar0.lost<-asia.ar0.lost/ar0.asia*100
per.asia.ar0.lost
#7. percentage tied
per.asia.ar0.tied<-asia.ar0.tied/ar0.asia*100
per.asia.ar0.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders in asia and match results for india
no.of.allrounders<-sort(no.of.allrounders,decreasing=TRUE)
no.of.allrounders
ar.no<-c(3,3,3,2,2,2,1,1,1,0,0,0)
ar.no
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
ar.result.value.2.asia<-c(per.asia.ar3.won,per.asia.ar3.lost,per.asia.ar3.tied,per.asia.ar2.won,per.asia.ar2.lost,per.asia.ar2.tied,per.asia.ar1.won,per.asia.ar1.lost,per.asia.ar1.tied,per.asia.ar0.won,per.asia.ar0.lost,per.asia.ar0.tied)
ar.result.value.2.asia
pp1.ar.asia<-data.frame(ar.no,result.type,ar.result.value.2.asia)
pp1.ar.asia
pp1.ar.asia$result.type <- factor(pp1.ar.asia$result.type, levels = c("tied","lost","won"))
# creating the y-position for adding labels to the bars
pp1.ar.asia <- ddply(pp1.ar.asia, .(ar.no),transform, pos = cumsum(ar.result.value.2.asia) - (0.5 * ar.result.value.2.asia))
pp1.ar.asia
#creating the graph
ar.result.asia<-ggplot(pp1.ar.asia, aes(fill=result.type, y=ar.result.value.2.asia, x=ar.no)) + geom_bar(position="fill", stat="identity")
ar.result.asia#this is the graph
# adding the labels to the graph
fill <- c("seagreen3", "orange1","dodgerblue2")
ar.result.asia+geom_text(aes(label = paste(round((ar.result.value.2.asia),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s in ASIA VS no of allrounders") + scale_fill_manual(values=fill)+labs(x = "NUMBER OF ALLROUNDERS", y = "PERCENTAGE OF MATCH RESULTS")

#-----------------------------------IN SENA COUNTRIES--------------------------------------------------------------------
# match result depending upon the number of allrounders
# 1. checking the number of allrounders(s) india have played in sena since 1 jan, 2000
odidata.sena<-odidata[odidata$sena.host=="TRUE",]
odidata.sena
total.allrounders.sena<-unique(odidata.sena$total.allrounders)
total.allrounders.sena
# thus india have played 3,2,1,0 allrounders in all matches in SENA since 1 jan, 2000
# now we'll check result of the match in SENA depending on the number of allrounders
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# when india played 3 allrounders and india won the match
#1. no of such matches
ar3.sena<-nrow(odidata.sena[odidata.sena$total.allrounders==3 & odidata.sena$sena.host=="TRUE",])
ar3.sena
#2. no of times india won
sena.ar3.won<-nrow(odidata.sena[odidata.sena$total.allrounders==3 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.ar3.won
#3. no of times india lost
sena.ar3.lost<-nrow(odidata.sena[odidata.sena$total.allrounders==3 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.ar3.lost
#4. no of times india tied the match
sena.ar3.tied<-nrow(odidata.sena[odidata.sena$total.allrounders==3 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.ar3.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.ar3.won<-sena.ar3.won/ar3.sena*100
per.sena.ar3.won        
#6. percentage lost
per.sena.ar3.lost<-sena.ar3.lost/ar3.sena*100
per.sena.ar3.lost
#7. percentage tied
per.sena.ar3.tied<-sena.ar3.tied/ar3.sena*100
per.sena.ar3.tied

# when india played 2 allrounders and india won the match
#1. no of such matches
ar2.sena<-nrow(odidata.sena[odidata.sena$total.allrounders==2 & odidata.sena$sena.host=="TRUE",])
ar2.sena
#2. no of times india won
sena.ar2.won<-nrow(odidata.sena[odidata.sena$total.allrounders==2 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.ar2.won
#3. no of times india lost
sena.ar2.lost<-nrow(odidata.sena[odidata.sena$total.allrounders==2 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.ar2.lost
#4. no of times india tied the match
sena.ar2.tied<-nrow(odidata.sena[odidata.sena$total.allrounders==2 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.ar2.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.ar2.won<-sena.ar2.won/ar2.sena*100
per.sena.ar2.won        
#6. percentage lost
per.sena.ar2.lost<-sena.ar2.lost/ar2.sena*100
per.sena.ar2.lost
#7. percentage tied
per.sena.ar2.tied<-sena.ar2.tied/ar2.sena*100
per.sena.ar2.tied

# when india played 1 allrounders and india won the match
#1. no of such matches
ar1.sena<-nrow(odidata.sena[odidata.sena$total.allrounders==1 & odidata.sena$sena.host=="TRUE",])
ar1.sena
#2. no of times india won
sena.ar1.won<-nrow(odidata.sena[odidata.sena$total.allrounders==1 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.ar1.won
#3. no of times india lost
sena.ar1.lost<-nrow(odidata.sena[odidata.sena$total.allrounders==1 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.ar1.lost
#4. no of times india tied the match
sena.ar1.tied<-nrow(odidata.sena[odidata.sena$total.allrounders==1 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.ar1.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.ar1.won<-sena.ar1.won/ar1.sena*100
per.sena.ar1.won        
#6. percentage lost
per.sena.ar1.lost<-sena.ar1.lost/ar1.sena*100
per.sena.ar1.lost
#7. percentage tied
per.sena.ar1.tied<-sena.ar1.tied/ar1.sena*100
per.sena.ar1.tied

# when india played 0 allrounders and india won the match
#1. no of such matches
ar0.sena<-nrow(odidata.sena[odidata.sena$total.allrounders==0 & odidata.sena$sena.host=="TRUE",])
ar0.sena
#2. no of times india won
sena.ar0.won<-nrow(odidata.sena[odidata.sena$total.allrounders==0 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="won",])
sena.ar0.won
#3. no of times india lost
sena.ar0.lost<-nrow(odidata.sena[odidata.sena$total.allrounders==0 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="lost",])
sena.ar0.lost
#4. no of times india tied the match
sena.ar0.tied<-nrow(odidata.sena[odidata.sena$total.allrounders==0 & odidata.sena$sena.host=="TRUE" & odidata.sena$match.result.for.india=="tied",])
sena.ar0.tied
# now we calculate the percentage of such instances of decision
#5. percentage won
per.sena.ar0.won<-sena.ar0.won/ar0.sena*100
per.sena.ar0.won        
#6. percentage lost
per.sena.ar0.lost<-sena.ar0.lost/ar0.sena*100
per.sena.ar0.lost
#7. percentage tied
per.sena.ar0.tied<-sena.ar0.tied/ar0.sena*100
per.sena.ar0.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of allrounders in sena and match results for india
no.of.allrounders<-sort(no.of.allrounders,decreasing=TRUE)
no.of.allrounders
ar.no<-c(3,3,3,2,2,2,1,1,1,0,0,0)
ar.no
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
ar.result.value.2.sena<-c(per.sena.ar3.won,per.sena.ar3.lost,per.sena.ar3.tied,per.sena.ar2.won,per.sena.ar2.lost,per.sena.ar2.tied,per.sena.ar1.won,per.sena.ar1.lost,per.sena.ar1.tied,per.sena.ar0.won,per.sena.ar0.lost,per.sena.ar0.tied)
ar.result.value.2.sena
pp1.ar.sena<-data.frame(ar.no,result.type,ar.result.value.2.sena)
pp1.ar.sena
pp1.ar.sena$result.type <- factor(pp1.ar.sena$result.type, levels = c("tied","lost","won"))
# creating the y-position for adding labels to the bars
pp1.ar.sena <- ddply(pp1.ar.sena, .(ar.no),transform, pos = cumsum(ar.result.value.2.sena) - (0.5 * ar.result.value.2.sena))
pp1.ar.sena
#creating the graph
ar.result.sena<-ggplot(pp1.ar.sena, aes(fill=result.type, y=ar.result.value.2.sena, x=ar.no)) + geom_bar(position="fill", stat="identity")
ar.result.sena#this is the graph
# adding the labels to the graph
fill <- c("seagreen3", "orange1","dodgerblue2")
ar.result.sena+geom_text(aes(label = paste(round((ar.result.value.2.sena),2),"%"),y=pos/100,size = 4),colour="black",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s in SENA VS no of allrounders") + scale_fill_manual(values=fill)+labs(x = "NUMBER OF ALLROUNDERS", y = "PERCENTAGE OF MATCH RESULTS")

#---------------------------------------------------------------------------
#5.How no of bowlers affect the score/wickets of the opposition
# no of bowlers
no.of.bowlers<-sort(no.of.bowlers,decreasing=TRUE)
no.of.bowlers
# average wickets of opposition (we take only opposition batting)
# (1st instances bcz opposition score batting 2nd is limited)
#(by  indian teams score)
# bowlers=5
wkts.avg.bow5<-mean(odidata[odidata$no.of.bowlers==5 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
wkts.avg.bow5
# bowlers=4
wkts.avg.bow4<-mean(odidata[odidata$no.of.bowlers==4 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
wkts.avg.bow4
# bowlers=3
wkts.avg.bow3<-mean(odidata[odidata$no.of.bowlers==3 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
wkts.avg.bow3
# bowlers=2
wkts.avg.bow2<-mean(odidata[odidata$no.of.bowlers==2 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
wkts.avg.bow2

# average runs of opposition (we take only opposition batting)
# (1st instances bcz opposition score batting 2nd is limited)
#(by  indian teams score)
# bowlers=5
runs.avg.bow5<-mean(odidata[odidata$no.of.bowlers==5 & odidata$batting.turn=="2nd","opposition.runs"])
runs.avg.bow5
# bowlers=4
runs.avg.bow4<-mean(odidata[odidata$no.of.bowlers==4 & odidata$batting.turn=="2nd","opposition.runs"])
runs.avg.bow4
# bowlers=3
runs.avg.bow3<-mean(odidata[odidata$no.of.bowlers==3 & odidata$batting.turn=="2nd","opposition.runs"])
runs.avg.bow3
# bowlers=2
runs.avg.bow2<-mean(odidata[odidata$no.of.bowlers==2 & odidata$batting.turn=="2nd","opposition.runs"])
runs.avg.bow2
#----------------------------------------------------------------------------------------------
#comparative graphical representation of opposition
#depending on no of indian bowlers playing the match
wicket.opp<-c(wkts.avg.bow5,wkts.avg.bow4,wkts.avg.bow3,wkts.avg.bow2)
wicket.opp
runs.opp<-c(runs.avg.bow5,runs.avg.bow4,runs.avg.bow3,runs.avg.bow2)
runs.opp
no.indian.bowlers<-c("5","4","3","2")
no.indian.bowlers
opp.scorecard<-data.frame(no.indian.bowlers,runs.opp,wicket.opp)
opp.scorecard
opp.score.graph<-ggplot(opp.scorecard, aes(x = wicket.opp, y = runs.opp)) +geom_point(aes(color = factor(no.indian.bowlers)),shape=16,size=5)+labs(title = "no of indian bowlers & opposition score")+labs(x = "average opposition wickets to fall",y = "average opposition runs",color = "no of IND bowlers playing",title = "NO OF IND BOWLERS PLAYING VS OPPOSITION SCORE",caption = "when opp. bats 1st")+scale_x_continuous(breaks = seq(5, 10, by = 1)) +scale_y_continuous(breaks = seq(150,350, by = 1))+theme_dark()+geom_text(aes(label=no.indian.bowlers),hjust=0, vjust=0)
opp.score.graph
#---------------------------------------------------------------------------
#How no of batsmen affect the score/wickets of the indian team when they bat 1st
# no of batsmen
no.of.batsmen<-sort(no.of.batsmen,decreasing=TRUE)
no.of.batsmen
# average wickets of indian team (we take only indian team batting)
# (1st instances bcz indian team score batting 2nd is limited)
#(by  opposition teams score)
# batsmen=7
wkts.avg.bat7<-mean(odidata[odidata$no.of.batsmen==7 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.bat7
# batsmen=6
wkts.avg.bat6<-mean(odidata[odidata$no.of.batsmen==6 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.bat6
# batsmen=5
wkts.avg.bat5<-mean(odidata[odidata$no.of.batsmen==5 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.bat5
# batsmen=4
wkts.avg.bat4<-mean(odidata[odidata$no.of.batsmen==4 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.bat4

# average runs of india (we take only india batting)
# (1st instances bcz india score batting 1st is limited)
#(by  opposition teams score)
# batsmen=7
runs.avg.bat7<-mean(odidata[odidata$no.of.batsmen==7 & odidata$batting.turn=="1st","india.runs"])
runs.avg.bat7
# batsmen=6
runs.avg.bat6<-mean(odidata[odidata$no.of.batsmen==6 & odidata$batting.turn=="1st","india.runs"])
runs.avg.bat6
# batsmen=5
runs.avg.bat5<-mean(odidata[odidata$no.of.batsmen==5 & odidata$batting.turn=="1st","india.runs"])
runs.avg.bat5
# batsmen=4
runs.avg.bat4<-mean(odidata[odidata$no.of.batsmen==4 & odidata$batting.turn=="1st","india.runs"])
runs.avg.bat4

#----------------------------------------------------------------------------------------------
#comparative graphical representation of opposition
#depending on no of indian batsmen playing the match
wicket.ind<-c(wkts.avg.bat7,wkts.avg.bat6,wkts.avg.bat5,wkts.avg.bat4)
wicket.ind
runs.ind<-c(runs.avg.bat7,runs.avg.bat6,runs.avg.bat5,runs.avg.bat4)
runs.ind
no.indian.batsmen<-c("7","6","5","4")
no.indian.batsmen
ind.scorecard<-data.frame(no.indian.batsmen,runs.ind,wicket.ind)
ind.scorecard
ind.score.graph<-ggplot(ind.scorecard, aes(x = wicket.ind, y = runs.ind)) +geom_point(aes(color = factor(no.indian.batsmen)),shape=16,size=5)+labs(title = "no of indian batsmen & indian score")+labs(x = "average indian wickets to fall",y = "average indian runs",color = "no of IND batsmen playing",title = "NO OF IND BATSMEN PLAYING VS INDIAN SCORE",caption = "when INDIA bats 1st")+scale_x_continuous(breaks = seq(5, 10, by = 1)) +scale_y_continuous(breaks = seq(150,350, by = 1))+theme_dark()+geom_text(aes(label=no.indian.batsmen),hjust=0, vjust=0)
ind.score.graph

#---------------------------------------------------------------------------
#How no of allrounders affect the score/wickets of the indian team
# no of batsmen
no.of.allrounders<-sort(no.of.allrounders,decreasing=TRUE)
no.of.allrounders
# average wickets of indian team (we take only indian team batting)
# (1st instances bcz indian team score batting 2nd is limited)
#(by  opposition teams score)
# allrounders=3
wkts.avg.ar3<-mean(odidata[odidata$total.allrounders==3 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.ar3
# allrounders=2
wkts.avg.ar2<-mean(odidata[odidata$total.allrounders==2 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.ar2
# allrounders=1
wkts.avg.ar1<-mean(odidata[odidata$total.allrounders==1 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.ar1
# allrounders=0
wkts.avg.ar0<-mean(odidata[odidata$total.allrounders==0 & odidata$batting.turn=="1st","india.wickets.lost"])
wkts.avg.ar0

# average runs of indian team (we take only indian team batting)
# (1st instances bcz indian team score batting 2nd is limited)
#(by  opposition teams score)
# allrounders=3
runs.avg.ar3<-mean(odidata[odidata$total.allrounders==3 & odidata$batting.turn=="1st","india.runs"])
runs.avg.ar3
# allrounders=2
runs.avg.ar2<-mean(odidata[odidata$total.allrounders==2 & odidata$batting.turn=="1st","india.runs"])
runs.avg.ar2
# allrounders=1
runs.avg.ar1<-mean(odidata[odidata$total.allrounders==1 & odidata$batting.turn=="1st","india.runs"])
runs.avg.ar1
# allrounders=0
runs.avg.ar0<-mean(odidata[odidata$total.allrounders==0 & odidata$batting.turn=="1st","india.runs"])
runs.avg.ar0
#----------------------------------------------------------------------------------------------
#comparative graphical representation of indian score
#depending on no of indian allrounders playing the match
wicket.ind.ar<-c(wkts.avg.ar3,wkts.avg.ar2,wkts.avg.ar1,wkts.avg.ar0)
wicket.ind.ar
runs.ind.ar<-c(runs.avg.ar3,runs.avg.ar2,runs.avg.ar1,runs.avg.ar0)
runs.ind.ar
no.indian.allrounders<-c("3","2","1","0")
no.indian.allrounders
ind.scorecard.ar<-data.frame(no.indian.allrounders,runs.ind.ar,wicket.ind.ar)
ind.scorecard.ar
ind.score.graph.ar<-ggplot(ind.scorecard.ar, aes(x = wicket.ind.ar, y = runs.ind.ar)) +geom_point(aes(color = factor(no.indian.allrounders)),shape=16,size=5)+labs(title = "no of indian allrounders & indian score")+labs(x = "average indian wickets to fall",y = "average indian runs",color = "no of IND allrounders playing",title = "NO OF IND ALLROUNDERS PLAYING VS INDIAN SCORE",caption = "when INDIA bats 1st")+scale_x_continuous(breaks = seq(5, 10, by = 1)) +scale_y_continuous(breaks = seq(150,350, by = 1))+theme_dark()+geom_text(aes(label=no.indian.allrounders),hjust=0, vjust=0)
ind.score.graph.ar

#---------------------------------------------------------------------------
#How no of allrounders affect the score/wickets of the opposition team
# no of batsmen
no.of.allrounders<-sort(no.of.allrounders,decreasing=TRUE)
no.of.allrounders
# allrounders=3
opp.wkts.avg.ar3<-mean(odidata[odidata$total.allrounders==3 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
opp.wkts.avg.ar3
# allrounders=2
opp.wkts.avg.ar2<-mean(odidata[odidata$total.allrounders==2 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
opp.wkts.avg.ar2
# allrounders=1
opp.wkts.avg.ar1<-mean(odidata[odidata$total.allrounders==1 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
opp.wkts.avg.ar1
# allrounders=0
opp.wkts.avg.ar0<-mean(odidata[odidata$total.allrounders==0 & odidata$batting.turn=="2nd","opposition.wickets.lost"])
opp.wkts.avg.ar0

# allrounders=3
opp.runs.avg.ar3<-mean(odidata[odidata$total.allrounders==3 & odidata$batting.turn=="2nd","opposition.runs"])
opp.runs.avg.ar3
# allrounders=2
opp.runs.avg.ar2<-mean(odidata[odidata$total.allrounders==2 & odidata$batting.turn=="2nd","opposition.runs"])
opp.runs.avg.ar2
# allrounders=1
opp.runs.avg.ar1<-mean(odidata[odidata$total.allrounders==1 & odidata$batting.turn=="2nd","opposition.runs"])
opp.runs.avg.ar1
# allrounders=0
opp.runs.avg.ar0<-mean(odidata[odidata$total.allrounders==0 & odidata$batting.turn=="2nd","opposition.runs"])
opp.runs.avg.ar0
#----------------------------------------------------------------------------------------------
#comparative graphical representation of indian score
#depending on no of indian allrounders playing the match
wicket.opp.ar<-c(opp.wkts.avg.ar3,opp.wkts.avg.ar2,opp.wkts.avg.ar1,opp.wkts.avg.ar0)
wicket.opp.ar
opp.runs.ar<-c(opp.runs.avg.ar3,opp.runs.avg.ar2,opp.runs.avg.ar1,opp.runs.avg.ar0)
opp.runs.ar
no.indian.allrounders<-c("3","2","1","0")
no.indian.allrounders
opp.scorecard.ar<-data.frame(no.indian.allrounders,opp.runs.ar,wicket.opp.ar)
opp.scorecard.ar
opp.score.graph.ar<-ggplot(opp.scorecard.ar, aes(x = wicket.opp.ar, y = opp.runs.ar)) +geom_point(aes(color = factor(no.indian.allrounders)),shape=16,size=5)+labs(title = "no of indian allrounders & opposition score")+labs(x = "average opposition wickets to fall",y = "average opposition runs",color = "no of IND allrounders playing",title = "NO OF IND ALLROUNDERS PLAYING VS OPPOSITION SCORE",caption = "when OPPOSITION bats 1st")+scale_x_continuous(breaks = seq(5, 10, by = 1)) +scale_y_continuous(breaks = seq(150,350, by = 1))+theme_dark()+geom_text(aes(label=no.indian.allrounders),hjust=0, vjust=0)
opp.score.graph.ar

#------------------------------------------------------------------------
#8.How no of batsmen affect the result when india is chasing?
no.of.batsmen<-unique(odidata$no.of.batsmen)
no.of.batsmen # number of batsmen india has played
# india has played 7,6,5,4 batsmen since 1 jan,2000
# affect on result when india plays 7 batsmen 
#1. no of such matches
chase.bt7<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$batting.turn=="2nd",])
chase.bt7
#2. no of matches won
chase.bt7.won<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$match.result.for.india=="won" & odidata$batting.turn=="2nd",])
chase.bt7.won
#3. no of matches lost
chase.bt7.lost<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$match.result.for.india=="lost" & odidata$batting.turn=="2nd",])
chase.bt7.lost
#3. no of matches tied
chase.bt7.tied<-nrow(odidata[odidata$no.of.batsmen==7 & odidata$match.result.for.india=="tied" & odidata$batting.turn=="2nd",])
chase.bt7.tied
#above results in percentages
# winning %
per.chase.bt7.won<-chase.bt7.won/chase.bt7*100
per.chase.bt7.won
# loss %
per.chase.bt7.lost<-chase.bt7.lost/chase.bt7*100
per.chase.bt7.lost
# tied %
per.chase.bt7.tied<-chase.bt7.tied/chase.bt7*100
per.chase.bt7.tied
# affect on result when india plays 6 batsmen 
#1. no of such matches
chase.bt6<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$batting.turn=="2nd",])
chase.bt6
#2. no of matches won
chase.bt6.won<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$match.result.for.india=="won" & odidata$batting.turn=="2nd",])
chase.bt6.won
#3. no of matches lost
chase.bt6.lost<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$match.result.for.india=="lost" & odidata$batting.turn=="2nd",])
chase.bt6.lost
#3. no of matches tied
chase.bt6.tied<-nrow(odidata[odidata$no.of.batsmen==6 & odidata$match.result.for.india=="tied" & odidata$batting.turn=="2nd",])
chase.bt6.tied
#above results in percentages
# winning %
per.chase.bt6.won<-chase.bt6.won/chase.bt6*100
per.chase.bt6.won
# loss %
per.chase.bt6.lost<-chase.bt6.lost/chase.bt6*100
per.chase.bt6.lost
# tied %
per.chase.bt6.tied<-chase.bt6.tied/chase.bt6*100
per.chase.bt6.tied

# affect on result when india plays 5 batsmen 
#1. no of such matches
chase.bt5<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$batting.turn=="2nd",])
chase.bt5
#2. no of matches won
chase.bt5.won<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$match.result.for.india=="won" & odidata$batting.turn=="2nd",])
chase.bt5.won
#3. no of matches lost
chase.bt5.lost<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$match.result.for.india=="lost" & odidata$batting.turn=="2nd",])
chase.bt5.lost
#3. no of matches tied
chase.bt5.tied<-nrow(odidata[odidata$no.of.batsmen==5 & odidata$match.result.for.india=="tied" & odidata$batting.turn=="2nd",])
chase.bt5.tied
#above results in percentages
# winning %
per.chase.bt5.won<-chase.bt5.won/chase.bt5*100
per.chase.bt5.won
# loss %
per.chase.bt5.lost<-chase.bt5.lost/chase.bt5*100
per.chase.bt5.lost
# tied %
per.chase.bt5.tied<-chase.bt5.tied/chase.bt5*100
per.chase.bt5.tied

# affect on result when india plays 4 batsmen 
#1. no of such matches
chase.bt4<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$batting.turn=="2nd",])
chase.bt4
#2. no of matches won
chase.bt4.won<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$match.result.for.india=="won" & odidata$batting.turn=="2nd",])
chase.bt4.won
#3. no of matches lost
chase.bt4.lost<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$match.result.for.india=="lost" & odidata$batting.turn=="2nd",])
chase.bt4.lost
#4. no of matches tied
chase.bt4.tied<-nrow(odidata[odidata$no.of.batsmen==4 & odidata$match.result.for.india=="tied" & odidata$batting.turn=="2nd",])
chase.bt4.tied
#above results in percentages
# winning %
per.chase.bt4.won<-chase.bt4.won/chase.bt4*100
per.chase.bt4.won
# loss %
per.chase.bt4.lost<-chase.bt4.lost/chase.bt4*100
per.chase.bt4.lost
# tied %
per.chase.bt4.tied<-chase.bt4.tied/chase.bt4*100
per.chase.bt4.tied

#----------------------------------------------------------------------------------------------
#comparative graphical representation of batsmen and decisions
no.of.batsmen<-sort(no.of.batsmen,decreasing=TRUE)
no.of.batsmen
chase.bt.no<-c(7,7,7,6,6,6,5,5,5,4,4,4)
chase.bt.no
result.type<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
result.type
result.per.chase.bt<-c(per.chase.bt7.won,per.chase.bt7.lost,per.chase.bt7.tied,per.chase.bt6.won,per.chase.bt6.lost,per.chase.bt6.tied,per.chase.bt5.won,per.chase.bt5.lost,per.chase.bt5.tied,per.chase.bt4.won,per.chase.bt4.lost,per.chase.bt4.tied)
result.per.chase.bt
chase.bt.numb.result<-data.frame(chase.bt.no,result.type,result.per.chase.bt)
chase.bt.numb.result
# creating the y-position for adding labels to the bars
pp1.chase.bt.res <- ddply(chase.bt.numb.result, .(chase.bt.no),transform, pos = cumsum(result.per.chase.bt) - (0.5 * result.per.chase.bt))
pp1.chase.bt.res
pp1.chase.bt.res$result.type <- factor(pp1.chase.bt.res$result.type, levels = c("tied","lost","won"))

#creating the graph
no.of.chase.bt.res.graph<-ggplot(pp1.chase.bt.res, aes(fill=result.type, y=result.per.chase.bt, x=chase.bt.no)) + geom_bar(position="fill", stat="identity")
no.of.chase.bt.res.graph#this is the graph
# adding the labels to the graph
fill <- c("gray34", "darkred","deepskyblue4")
no.of.chase.bt.res.graph+geom_text(aes(label = paste(round((result.per.chase.bt),2),"%"),y=pos/100,size = 4),colour="greenyellow",fontface="bold")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("result %s and number of batsmen when india chases") + scale_fill_manual(values=fill)+labs(x = "number of batsmen", y = "% of match results")



#-------------------------------------------------------------------------------

#Type of bowlers India plays against Asian and Sena countries.
# average number of bowlers india plays against asian countries
aver.bw.asia<-mean(odidata[odidata$asian.opposition=="TRUE","no.of.bowlers"])
aver.bw.asia
# average number of bowlers india plays against sena countries
aver.bw.sena<-mean(odidata[odidata$sena.opposition=="TRUE","no.of.bowlers"])
aver.bw.sena
# average number of spin bowlers india plays against sena countries
aver.bw.sena.spin<-mean(odidata[odidata$sena.opposition=="TRUE","no.of.spinners"])
aver.bw.sena.spin
# average number of spin bowlers india plays aginst asian countries
aver.bw.asia.spin<-mean(odidata[odidata$asian.opposition=="TRUE","no.of.spinners"])
aver.bw.asia.spin
# average number of fast bowlers india plays against sena countries
aver.bw.sena.fast<-mean(odidata[odidata$sena.opposition=="TRUE","no.of.pacers"])
aver.bw.sena.fast
# average number of fast bowlers india plays aginst asian countries
aver.bw.asia.fast<-mean(odidata[odidata$asian.opposition=="TRUE","no.of.pacers"])
aver.bw.asia.fast
#graphical representation of above data
#depending on no of indian allrounders playing the match
bw.host.region<-c("asia","asia","asia","sena","sena","sena")
bw.host.region
bw.type.sena.asia<-c("average no of bowlers","average no of spin bowlers","average no of fast bowlers","average no of bowlers","average no of spin bowlers","average no of fast bowlers")
bw.type.sena.asia
bw.value.sena.asia<- c(aver.bw.asia,aver.bw.asia.spin,aver.bw.asia.fast,aver.bw.sena,aver.bw.sena.spin,aver.bw.sena.fast)
bw.value.sena.asia
bw.asia.sena<- data.frame(bw.host.region,bw.type.sena.asia,bw.value.sena.asia)
bw.asia.sena
# creating the y-position for adding labels to the bars
bw.asia.sena <- ddply(bw.asia.sena, .(bw.host.region),transform, pos =bw.value.sena.asia/2)
bw.asia.sena
bw.asia.sena$bw.type.sena.asia <- factor(bw.asia.sena$bw.type.sena.asia, levels = c("average no of bowlers","average no of spin bowlers","average no of fast bowlers"))
#creating the graph
bw.result.sena<-ggplot(bw.asia.sena, aes(fill=bw.type.sena.asia, y=bw.value.sena.asia, x=bw.host.region)) + geom_bar(position=position_dodge(), stat="identity")
bw.result.sena#this is the graph
# adding the labels to the graph
fill <- c("deeppink1", "darkturquoise","chartreuse1")
bw.result.sena+geom_text(aes(label = paste(round((bw.value.sena.asia),2)),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("average no bowlers and their types vs sena and asian opponents") + scale_fill_manual(values=fill)+labs(x = "OPPONENT TYPE", y = "AVERAGE NUMBER OF BOWLERS")

#-----------------------------------------------------------------------------
#Type of bowlers India plays in Asian and Sena countries.
# average number of bowlers india plays in asian countries
aver.bw.asia.host<-mean(odidata[odidata$asian.host..Y.N.=="TRUE","no.of.bowlers"])
aver.bw.asia.host
# average number of bowlers india plays in sena countries
aver.bw.sena.host<-mean(odidata[odidata$SENA.host..Y.N.=="TRUE","no.of.bowlers"])
aver.bw.sena.host
# average number of spin bowlers india in against sena countries
aver.bw.sena.spin.host<-mean(odidata[odidata$SENA.host..Y.N.=="TRUE","no.of.spinners"])
aver.bw.sena.spin.host
# average number of spin bowlers india plays in asian countries
aver.bw.asia.spin.host<-mean(odidata[odidata$asian.host..Y.N.=="TRUE","no.of.spinners"])
aver.bw.asia.spin.host
# average number of fast bowlers india plays in sena countries
aver.bw.sena.fast.host<-mean(odidata[odidata$SENA.host..Y.N.=="TRUE","no.of.pacers"])
aver.bw.sena.fast.host
# average number of fast bowlers india plays in asian countries
aver.bw.asia.fast.host<-mean(odidata[odidata$asian.host..Y.N.=="TRUE","no.of.pacers"])
aver.bw.asia.fast.host
#graphical representation of above data
#depending on no of indian allrounders playing the match
bw.host.region<-c("asia","asia","asia","sena","sena","sena")
bw.host.region
bw.type.sena.asia<-c("average no of bowlers","average no of spin bowlers","average no of fast bowlers","average no of bowlers","average no of spin bowlers","average no of fast bowlers")
bw.type.sena.asia
bw.value.sena.asia.host<- c(aver.bw.asia.host,aver.bw.asia.spin.host,aver.bw.asia.fast.host,aver.bw.sena.host,aver.bw.sena.spin.host,aver.bw.sena.fast.host)
bw.value.sena.asia.host
bw.asia.sena.host<- data.frame(bw.host.region,bw.type.sena.asia,bw.value.sena.asia.host)
bw.asia.sena.host
# creating the y-position for adding labels to the bars
bw.asia.sena.host <- ddply(bw.asia.sena.host, .(bw.host.region),transform, pos =bw.value.sena.asia.host/2)
bw.asia.sena.host
bw.asia.sena.host$bw.type.sena.asia <- factor(bw.asia.sena.host$bw.type.sena.asia, levels = c("average no of bowlers","average no of spin bowlers","average no of fast bowlers"))
#creating the graph
bw.result.sena.host<-ggplot(bw.asia.sena.host, aes(fill=bw.type.sena.asia, y=bw.value.sena.asia.host, x=bw.host.region)) + geom_bar(position=position_dodge(), stat="identity")
bw.result.sena.host#this is the graph
# adding the labels to the graph
fill <- c("deeppink1", "darkturquoise","chartreuse1")
bw.result.sena.host+geom_text(aes(label = paste(round((bw.value.sena.asia.host),2)),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("average no bowlers and their types in sena and asian countries") + scale_fill_manual(values=fill)+labs(x = "HOST TYPE", y = "AVERAGE NUMBER OF BOWLERS")


#-----------------------------------------------------------------------------
#How type of bowlers in Asian and Sena countries affect the result?
# number of matches for various combinations of bowlers,spinners and pacers
# in overall matches
n404<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4,])
n404
n514<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4,])
n514
n303<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3,])
n303
n413<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3,])
n413
n523<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3,])
n523
n312<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2,])
n312
n422<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2,])
n422
n532<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2,])
n532
n211<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1,])
n211
n321<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1,])
n321
n330<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0,])
n330
n202<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2,])
n202
#graphical representation of above data
comb.bw<-c("4bw 0sp 4pa","5bw 1sp 4pa","3bw 0sp 3pa","4bw 1sp 3pa","5bw 2sp 3pa","3bw 1sp 2pa","4bw 2sp 2pa","5bw 3sp 2pa","2bo 1sp 1pa","3bw 2sp 1pa","3bw 3sp 0pa","2bw 0sp 2pa")
comb.bw
no.comb.matches<-c(n404,n514,n303,n413,n523,n312,n422,n532,n211,n321,n330,n202)
no.comb.matches
bow.comb.matches<-data.frame(comb.bw,no.comb.matches)
bow.comb.matches
# creating the y-position for adding labels to the bars
bow.comb.matches <- ddply(bow.comb.matches, .(comb.bw),transform, pos =no.comb.matches/2)
bow.comb.matches
bow.comb.matches$comb.bw <- factor(bow.comb.matches$comb.bw, levels = c("4bw 0sp 4pa","5bw 1sp 4pa","3bw 0sp 3pa","4bw 1sp 3pa","5bw 2sp 3pa","3bw 1sp 2pa","4bw 2sp 2pa","5bw 3sp 2pa","2bo 1sp 1pa","3bw 2sp 1pa"))
#creating the graph
graph.bow.comb.matches<-ggplot(bow.comb.matches, aes(fill=comb.bw, y=no.comb.matches, x=comb.bw)) + geom_bar(position=position_dodge(), stat="identity")
graph.bow.comb.matches#this is the graph
# adding the labels to the graph
fill <- c("gold1", "rosybrown1","olivedrab3","magenta1","darkcyan","tan4","turquoise3","cornsilk4","mediumorchid3","springgreen3","red","navy")
graph.bow.comb.matches+geom_text(aes(label = paste(round((no.comb.matches),2)),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_text()) + ggtitle("combination of bowlers and matches played") + scale_fill_manual(values=fill)+labs(x = "BOWLING COMBINATION", y = "NUMBER OF MATCHES PLAYED")

#------------------------------------------------------------------------------------------
#13.	How type of bowlers in overall matches affect the result?
n404<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4,])
n404
n514<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4,])
n514
n303<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3,])
n303
n413<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3,])
n413
n523<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3,])
n523
n312<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2,])
n312
n422<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2,])
n422
n532<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2,])
n532
n211<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1,])
n211
n321<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1,])
n321
n330<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0,])
n330
n202<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2,])
n202
#now we calculate no of matches for these combinations, win,loss & tied percentages
#number of wins for these combinations
n404.win<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$match.result.for.india=="won",])
n404.win
n514.win<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$match.result.for.india=="won",])
n514.win
n303.win<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$match.result.for.india=="won",])
n303.win
n413.win<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$match.result.for.india=="won",])
n413.win
n523.win<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$match.result.for.india=="won",])
n523.win
n312.win<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$match.result.for.india=="won",])
n312.win
n422.win<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$match.result.for.india=="won",])
n422.win
n532.win<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$match.result.for.india=="won",])
n532.win
n211.win<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$match.result.for.india=="won",])
n211.win
n321.win<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$match.result.for.india=="won",])
n321.win
n330.win<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$match.result.for.india=="won",])
n330.win
n202.win<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$match.result.for.india=="won",])
n202.win
#number of losses for these combinations
n404.loss<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$match.result.for.india=="lost",])
n404.loss
n514.loss<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$match.result.for.india=="lost",])
n514.loss
n303.loss<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$match.result.for.india=="lost",])
n303.loss
n413.loss<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$match.result.for.india=="lost",])
n413.loss
n523.loss<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$match.result.for.india=="lost",])
n523.loss
n312.loss<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$match.result.for.india=="lost",])
n312.loss
n422.loss<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$match.result.for.india=="lost",])
n422.loss
n532.loss<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$match.result.for.india=="lost",])
n532.loss
n211.loss<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$match.result.for.india=="lost",])
n211.loss
n321.loss<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$match.result.for.india=="lost",])
n321.loss
n330.loss<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$match.result.for.india=="lost",])
n330.loss
n202.loss<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$match.result.for.india=="lost",])
n202.loss
#number of tie for these combinations
n404.tie<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$match.result.for.india=="tied",])
n404.tie
n514.tie<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$match.result.for.india=="tied",])
n514.tie
n303.tie<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$match.result.for.india=="tied",])
n303.tie
n413.tie<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$match.result.for.india=="tied",])
n413.tie
n523.tie<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$match.result.for.india=="tied",])
n523.tie
n312.tie<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$match.result.for.india=="tied",])
n312.tie
n422.tie<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$match.result.for.india=="tied",])
n422.tie
n532.tie<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$match.result.for.india=="tied",])
n532.tie
n211.tie<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$match.result.for.india=="tied",])
n211.tie
n321.tie<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$match.result.for.india=="tied",])
n321.tie
n330.tie<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$match.result.for.india=="tied",])
n330.tie
n202.tie<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$match.result.for.india=="tied",])
n202.tie
#percentage of win,loss and tie for these combinations

#wins
per.won.404<-n404.win/n404*100
per.won.404
per.won.514<-n514.win/n514*100
per.won.514
per.won.303<-n303.win/n303*100
per.won.303
per.won.413<-n413.win/n413*100
per.won.413
per.won.523<-n523.win/n523*100
per.won.523
per.won.312<-n312.win/n312*100
per.won.312
per.won.422<-n422.win/n422*100
per.won.422
per.won.532<-n532.win/n532*100
per.won.532
per.won.211<-n211.win/n211*100
per.won.211
per.won.321<-n321.win/n321*100
per.won.321
per.won.330<-n330.win/n330*100
per.won.330
per.won.202<-n202.win/n202*100
per.won.202
#losses
per.loss.404<-n404.loss/n404*100
per.loss.404
per.loss.514<-n514.loss/n514*100
per.loss.514
per.loss.303<-n303.loss/n303*100
per.loss.303
per.loss.413<-n413.loss/n413*100
per.loss.413
per.loss.523<-n523.loss/n523*100
per.loss.523
per.loss.312<-n312.loss/n312*100
per.loss.312
per.loss.422<-n422.loss/n422*100
per.loss.422
per.loss.532<-n532.loss/n532*100
per.loss.532
per.loss.211<-n211.loss/n211*100
per.loss.211
per.loss.321<-n321.loss/n321*100
per.loss.321
per.loss.330<-n330.loss/n330*100
per.loss.330
per.loss.202<-n202.loss/n202*100
per.loss.202
#tie                        
per.tie.404<-n404.tie/n404*100
per.tie.404
per.tie.514<-n514.tie/n514*100
per.tie.514
per.tie.303<-n303.tie/n303*100
per.tie.303
per.tie.413<-n413.tie/n413*100
per.tie.413
per.tie.523<-n523.tie/n523*100
per.tie.523
per.tie.312<-n312.tie/n312*100
per.tie.312
per.tie.422<-n422.tie/n422*100
per.tie.422
per.tie.532<-n532.tie/n532*100
per.tie.532
per.tie.211<-n211.tie/n211*100
per.tie.211
per.tie.321<-n321.tie/n321*100
per.tie.321
per.tie.330<-n330.tie/n330*100
per.tie.330
per.tie.202<-n202.tie/n202*100
per.tie.202
#graphical representation of this data
#number of matches for every combination
no.comb.matches
no.comb.matches.2<-c(n404,n404,n404,n514,n514,n514,n303,n303,n303,n413,n413,n413,n523,n523,n523,n312,n312,n312,n422,n422,n422,n532,n532,n532,n211,n211,n211,n321,n321,n321,n330,n330,n330,n202,n202,n202)
no.comb.matches.2
comp.comb.bw<-c("4bw 0sp 4pa","4bw 0sp 4pa","4bw 0sp 4pa","5bw 1sp 4pa","5bw 1sp 4pa","5bw 1sp 4pa","3bw 0sp 3pa","3bw 0sp 3pa","3bw 0sp 3pa","4bw 1sp 3pa","4bw 1sp 3pa","4bw 1sp 3pa","5bw 2sp 3pa","5bw 2sp 3pa","5bw 2sp 3pa","3bw 1sp 2pa","3bw 1sp 2pa","3bw 1sp 2pa","4bw 2sp 2pa","4bw 2sp 2pa","4bw 2sp 2pa","5bw 3sp 2pa","5bw 3sp 2pa","5bw 3sp 2pa","2bo 1sp 1pa","2bo 1sp 1pa","2bo 1sp 1pa","3bw 2sp 1pa","3bw 2sp 1pa","3bw 2sp 1pa","3bw 3pa 0sp","3bw 3pa 0sp","3bw 3pa 0sp","2bw 0pa 2sp","2bw 0pa 2sp","2bw 0pa 2sp")
comp.comb.bw
comb.bw.result.type<-c("win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie")
comb.bw.result.type
comp.bw.comb.value<-c(per.won.404,per.loss.404,per.tie.404,per.won.514,per.loss.514,per.tie.514,per.won.303,per.loss.303,per.tie.303,per.won.413,per.loss.413,per.tie.413,per.won.523,per.loss.523,per.tie.523,per.won.312,per.loss.312,per.tie.312,per.won.422,per.loss.422,per.tie.422,per.won.532,per.loss.532,per.tie.532,per.won.211,per.loss.211,per.tie.211,per.won.321,per.loss.321,per.tie.321,per.won.330,per.loss.330,per.tie.330,per.won.202,per.loss.202,per.tie.202)
comp.bw.comb.value
bw.comb.data<-data.frame(comp.comb.bw,no.comb.matches.2,comb.bw.result.type,comp.bw.comb.value)
bw.comb.data#dataframe created
# fileterin out cases where number of matches>10
bw.comb.data2<-bw.comb.data[bw.comb.data$no.comb.matches.2>10,]
bw.comb.data2#final dataframe

# creating the y-position for adding labels to the bars
bw.comb.data2 <- ddply(bw.comb.data2, .(comp.comb.bw),transform, pos =comp.bw.comb.value/2)
bw.comb.data2
bw.comb.data2$comb.bw.result.type <- factor(bw.comb.data2$comb.bw.result.type, levels = c("tie","loss","win"))
#creating the graph
graph.bw.comb.data2<-ggplot(bw.comb.data2, aes(fill=comb.bw.result.type, y=comp.bw.comb.value, x=comp.comb.bw)) + geom_bar(position=position_dodge(), stat="identity")
graph.bw.comb.data2#this is the graph
# adding the labels to the graph
fill <- c("maroon1","deepskyblue","lawngreen")
graph.bw.comb.data2+geom_text(aes(label = paste(round((comp.bw.comb.value),2),"%"),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("combination of bowlers and % results") + scale_fill_manual(values=fill)+labs(x = "BOWLING COMBINATION-->bw=total bowlers,sp=spinners,pa=pacers", y = "percentage of results")

#-----------------------------------------------------------------------------------------
#13.How type of bowlers in Asian and Sena countries affect the result?
#----------------------ASIA----------------------------------------------------------
#number of matches for these combinations
n404.asia<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE",])
n404.asia
n514.asia<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE",])
n514.asia
n303.asia<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE",])
n303.asia
n413.asia<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE",])
n413.asia
n523.asia<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE",])
n523.asia
n312.asia<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE",])
n312.asia
n422.asia<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE",])
n422.asia
n532.asia<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE",])
n532.asia
n211.asia<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE",])
n211.asia
n321.asia<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE",])
n321.asia

#number of matches won for these combinations
n404.asia.won<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n404.asia.won
n514.asia.won<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n514.asia.won
n303.asia.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n303.asia.won
n413.asia.won<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n413.asia.won
n523.asia.won<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n523.asia.won
n312.asia.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n312.asia.won
n422.asia.won<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n422.asia.won
n532.asia.won<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n532.asia.won
n211.asia.won<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n211.asia.won
n321.asia.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n321.asia.won

#number of matches lost for these combinations
n404.asia.lost<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n404.asia.lost
n514.asia.lost<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n514.asia.lost
n303.asia.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n303.asia.lost
n413.asia.lost<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n413.asia.lost
n523.asia.lost<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n523.asia.lost
n312.asia.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n312.asia.lost
n422.asia.lost<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n422.asia.lost
n532.asia.lost<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n532.asia.lost
n211.asia.lost<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n211.asia.lost
n321.asia.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n321.asia.lost

#number of matches tied for these combinations
n404.asia.tied<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n404.asia.tied
n514.asia.tied<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==1&odidata$no.of.pacers==4&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n514.asia.tied
n303.asia.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n303.asia.tied
n413.asia.tied<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n413.asia.tied
n523.asia.tied<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n523.asia.tied
n312.asia.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n312.asia.tied
n422.asia.tied<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n422.asia.tied
n532.asia.tied<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==3&odidata$no.of.pacers==2&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n532.asia.tied
n211.asia.tied<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n211.asia.tied
n321.asia.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$asian.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n321.asia.tied

#percentage of these results
#won
per.n404.asia.won<-n404.asia.won/n404.asia*100
per.n404.asia.won
per.n514.asia.won<-n514.asia.won/n514.asia*100
per.n514.asia.won
per.n303.asia.won<-n303.asia.won/n303.asia*100
per.n303.asia.won
per.n413.asia.won<-n413.asia.won/n413.asia*100
per.n413.asia.won
per.n523.asia.won<-n523.asia.won/n523.asia*100
per.n523.asia.won
per.n312.asia.won<-n312.asia.won/n312.asia*100
per.n312.asia.won
per.n422.asia.won<-n422.asia.won/n422.asia*100
per.n422.asia.won
per.n532.asia.won<-n532.asia.won/n532.asia*100
per.n532.asia.won
per.n211.asia.won<-n211.asia.won/n211.asia*100
per.n211.asia.won
per.n321.asia.won<-n321.asia.won/n321.asia*100
per.n321.asia.won

#lost
per.n404.asia.lost<-n404.asia.lost/n404.asia*100
per.n404.asia.lost
per.n514.asia.lost<-n514.asia.lost/n514.asia*100
per.n514.asia.lost
per.n303.asia.lost<-n303.asia.lost/n303.asia*100
per.n303.asia.lost
per.n413.asia.lost<-n413.asia.lost/n413.asia*100
per.n413.asia.lost
per.n523.asia.lost<-n523.asia.lost/n523.asia*100
per.n523.asia.lost
per.n312.asia.lost<-n312.asia.lost/n312.asia*100
per.n312.asia.lost
per.n422.asia.lost<-n422.asia.lost/n422.asia*100
per.n422.asia.lost
per.n532.asia.lost<-n532.asia.lost/n532.asia*100
per.n532.asia.lost
per.n211.asia.lost<-n211.asia.lost/n211.asia*100
per.n211.asia.lost
per.n321.asia.lost<-n321.asia.lost/n321.asia*100
per.n321.asia.lost

#tied
per.n404.asia.tied<-n404.asia.tied/n404.asia*100
per.n404.asia.tied
per.n514.asia.tied<-n514.asia.tied/n514.asia*100
per.n514.asia.tied
per.n303.asia.tied<-n303.asia.tied/n303.asia*100
per.n303.asia.tied
per.n413.asia.tied<-n413.asia.tied/n413.asia*100
per.n413.asia.tied
per.n523.asia.tied<-n523.asia.tied/n523.asia*100
per.n523.asia.tied
per.n312.asia.tied<-n312.asia.tied/n312.asia*100
per.n312.asia.tied
per.n422.asia.tied<-n422.asia.tied/n422.asia*100
per.n422.asia.tied
per.n532.asia.tied<-n532.asia.tied/n532.asia*100
per.n532.asia.tied
per.n211.asia.tied<-n211.asia.tied/n211.asia*100
per.n211.asia.tied
per.n321.asia.tied<-n321.asia.tied/n321.asia*100
per.n321.asia.tied

#graphical representation of this data
#number of matches for every combination
no.comb.matches.asia
no.comb.matches.asia<-c(n404.asia,n404.asia,n404.asia,n514.asia,n514.asia,n514.asia,n303.asia,n303.asia,n303.asia,n413.asia,n413.asia,n413.asia,n523.asia,n523.asia,n523.asia,n312.asia,n312.asia,n312.asia,n422.asia,n422.asia,n422.asia,n532.asia,n532.asia,n532.asia,n211.asia,n211.asia,n211.asia,n321.asia,n321.asia,n321.asia)
comp.comb.bw.asia<-c("4bw 0sp 4pa","4bw 0sp 4pa","4bw 0sp 4pa","5bw 1sp 4pa","5bw 1sp 4pa","5bw 1sp 4pa","3bw 0sp 3pa","3bw 0sp 3pa","3bw 0sp 3pa","4bw 1sp 3pa","4bw 1sp 3pa","4bw 1sp 3pa","5bw 2sp 3pa","5bw 2sp 3pa","5bw 2sp 3pa","3bw 1sp 2pa","3bw 1sp 2pa","3bw 1sp 2pa","4bw 2sp 2pa","4bw 2sp 2pa","4bw 2sp 2pa","5bw 3sp 2pa","5bw 3sp 2pa","5bw 3sp 2pa","2bo 1sp 1pa","2bo 1sp 1pa","2bo 1sp 1pa","3bw 2sp 1pa","3bw 2sp 1pa","3bw 2sp 1pa")
comp.comb.bw.asia
comb.bw.result.type<-c("win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie")
comb.bw.result.type
comp.bw.comb.value.asia<-c(per.n404.asia.won,per.n404.asia.lost,per.n404.asia.tied,per.n514.asia.won,per.n514.asia.lost,per.n514.asia.tied,per.n303.asia.won,per.n303.asia.lost,per.n303.asia.tied,per.n413.asia.won,per.n413.asia.lost,per.n413.asia.tied,per.n523.asia.won,per.n523.asia.lost,per.n523.asia.tied,per.n312.asia.won,per.n312.asia.lost,per.n312.asia.tied,per.n422.asia.won,per.n422.asia.lost,per.n422.asia.tied,per.n532.asia.won,per.n532.asia.lost,per.n532.asia.tied,per.n211.asia.won,per.n211.asia.lost,per.n211.asia.tied,per.n321.asia.won,per.n321.asia.lost,per.n321.asia.tied)
comp.bw.comb.value.asia
bw.comb.data.asia.2<-data.frame(comp.comb.bw.asia,comb.bw.result.type,comp.bw.comb.value.asia)
bw.comb.data.asia.2#dataframe created
#final dataframe

# creating the y-position for adding labels to the bars
bw.comb.data.asia.2 <- ddply(bw.comb.data.asia.2, .(comp.comb.bw.asia),transform, pos =comp.bw.comb.value.asia/2)
bw.comb.data.asia.2
bw.comb.data.asia.2$comb.bw.result.type <- factor(bw.comb.data.asia.2$comb.bw.result.type, levels = c("tie","loss","win"))

#creating the graph
graph.bw.comb.data.asia.2<-ggplot(bw.comb.data.asia.2, aes(fill=comb.bw.result.type, y=comp.bw.comb.value.asia, x=comp.comb.bw.asia)) + geom_bar(position=position_dodge(), stat="identity") 
graph.bw.comb.data.asia.2#this is the graph
# adding the labels to the graph
fill <- c("gold","turquoise2","hotpink2")
graph.bw.comb.data.asia.2+geom_text(aes(label = paste(round((comp.bw.comb.value.asia),2),"%"),y=pos,size = 2),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("combination of bowlers and % results in ASIA") + scale_fill_manual(values=fill)+labs(x = "BOWLING COMBINATION-->bw=total bowlers,sp=spinners,pa=pacers", y = "percentage of results")

#-----------------------------------------------------------------------------------------
#13.How type of bowlers in SENA countries affect the result?
#----------------------SENA----------------------------------------------------------
#number of matches for these combinations
n404.sena<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$SENA.host..Y.N.=="TRUE",])
n404.sena
n303.sena<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE",])
n303.sena
n413.sena<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE",])
n413.sena
n523.sena<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE",])
n523.sena
n312.sena<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE",])
n312.sena
n422.sena<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE",])
n422.sena
n211.sena<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE",])
n211.sena
n321.sena<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE",])
n321.sena
n330.sena<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$SENA.host..Y.N.=="TRUE",])
n330.sena
n202.sena<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE",])
n202.sena
#number of matches won for these combinations
n404.sena.won<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n404.sena.won
n303.sena.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n303.sena.won
n413.sena.won<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n413.sena.won
n523.sena.won<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n523.sena.won
n312.sena.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n312.sena.won
n422.sena.won<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n422.sena.won
n211.sena.won<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n211.sena.won
n321.sena.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n321.sena.won
n330.sena.won<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n330.sena.won
n202.sena.won<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="won",])
n202.sena.won
#number of matches lost for these combinations
n404.sena.lost<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n404.sena.lost
n303.sena.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n303.sena.lost
n413.sena.lost<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n413.sena.lost
n523.sena.lost<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n523.sena.lost
n312.sena.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n312.sena.lost
n422.sena.lost<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n422.sena.lost
n211.sena.lost<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n211.sena.lost
n321.sena.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n321.sena.lost
n330.sena.lost<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n330.sena.lost
n202.sena.lost<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="lost",])
n202.sena.lost
#number of matches tied for these combinations
n404.sena.tied<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==0&odidata$no.of.pacers==4&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n404.sena.tied
n303.sena.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==0&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n303.sena.tied
n413.sena.tied<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==1&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n413.sena.tied
n523.sena.tied<-nrow(odidata[odidata$no.of.bowlers==5&odidata$no.of.spinners==2&odidata$no.of.pacers==3&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n523.sena.tied
n312.sena.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==1&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n312.sena.tied
n422.sena.tied<-nrow(odidata[odidata$no.of.bowlers==4&odidata$no.of.spinners==2&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n422.sena.tied
n211.sena.tied<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==1&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n211.sena.tied
n321.sena.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==2&odidata$no.of.pacers==1&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n321.sena.tied
n330.sena.tied<-nrow(odidata[odidata$no.of.bowlers==3&odidata$no.of.spinners==3&odidata$no.of.pacers==0&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n330.sena.tied
n202.sena.tied<-nrow(odidata[odidata$no.of.bowlers==2&odidata$no.of.spinners==0&odidata$no.of.pacers==2&odidata$SENA.host..Y.N.=="TRUE"&odidata$match.result.for.india=="tied",])
n202.sena.tied
#percentage of these results
#won
per.n404.sena.won<-n404.sena.won/n404.sena*100
per.n404.sena.won
per.n303.sena.won<-n303.sena.won/n303.sena*100
per.n303.sena.won
per.n413.sena.won<-n413.sena.won/n413.sena*100
per.n413.sena.won
per.n523.sena.won<-n523.sena.won/n523.sena*100
per.n523.sena.won
per.n312.sena.won<-n312.sena.won/n312.sena*100
per.n312.sena.won
per.n422.sena.won<-n422.sena.won/n422.sena*100
per.n422.sena.won
per.n211.sena.won<-n211.sena.won/n211.sena*100
per.n211.sena.won
per.n321.sena.won<-n321.sena.won/n321.sena*100
per.n321.sena.won
per.n330.sena.won<-n330.sena.won/n330.sena*100
per.n330.sena.won
per.n202.sena.won<-n202.sena.won/n202.sena*100
per.n202.sena.won

#lost
per.n404.sena.lost<-n404.sena.lost/n404.sena*100
per.n404.sena.lost
per.n303.sena.lost<-n303.sena.lost/n303.sena*100
per.n303.sena.lost
per.n413.sena.lost<-n413.sena.lost/n413.sena*100
per.n413.sena.lost
per.n523.sena.lost<-n523.sena.lost/n523.sena*100
per.n523.sena.lost
per.n312.sena.lost<-n312.sena.lost/n312.sena*100
per.n312.sena.lost
per.n422.sena.lost<-n422.sena.lost/n422.sena*100
per.n422.sena.lost
per.n211.sena.lost<-n211.sena.lost/n211.sena*100
per.n211.sena.lost
per.n321.sena.lost<-n321.sena.lost/n321.sena*100
per.n321.sena.lost
per.n330.sena.lost<-n330.sena.lost/n330.sena*100
per.n330.sena.lost
per.n202.sena.lost<-n202.sena.lost/n202.sena*100
per.n202.sena.lost
#tied
per.n404.sena.tied<-n404.sena.tied/n404.sena*100
per.n404.sena.tied
per.n303.sena.tied<-n303.sena.tied/n303.sena*100
per.n303.sena.tied
per.n413.sena.tied<-n413.sena.tied/n413.sena*100
per.n413.sena.tied
per.n523.sena.tied<-n523.sena.tied/n523.sena*100
per.n523.sena.tied
per.n312.sena.tied<-n312.sena.tied/n312.sena*100
per.n312.sena.tied
per.n422.sena.tied<-n422.sena.tied/n422.sena*100
per.n422.sena.tied
per.n211.sena.tied<-n211.sena.tied/n211.sena*100
per.n211.sena.tied
per.n321.sena.tied<-n321.sena.tied/n321.sena*100
per.n321.sena.tied
per.n330.sena.tied<-n330.sena.tied/n330.sena*100
per.n330.sena.tied
per.n202.sena.tied<-n202.sena.tied/n202.sena*100
per.n202.sena.tied

#graphical representation of this data
#number of matches for every combination
no.comb.matches.sena<-c(n404.sena,n404.sena,n404.sena,n303.sena,n303.sena,n303.sena,n413.sena,n413.sena,n413.sena,n523.sena,n523.sena,n523.sena,n312.sena,n312.sena,n312.sena,n422.sena,n422.sena,n422.sena,n211.sena,n211.sena,n211.sena,n321.sena,n321.sena,n321.sena,n330.sena,n330.sena,n330.sena,n202.sena,n202.sena,n202.sena)
no.comb.matches.sena
comp.comb.bw.sena<-c("4bw 0sp 4pa","4bw 0sp 4pa","4bw 0sp 4pa","3bw 0sp 3pa","3bw 0sp 3pa","3bw 0sp 3pa","4bw 1sp 3pa","4bw 1sp 3pa","4bw 1sp 3pa","5bw 2sp 3pa","5bw 2sp 3pa","5bw 2sp 3pa","3bw 1sp 2pa","3bw 1sp 2pa","3bw 1sp 2pa","4bw 2sp 2pa","4bw 2sp 2pa","4bw 2sp 2pa","2bo 1sp 1pa","2bo 1sp 1pa","2bo 1sp 1pa","3bw 2sp 1pa","3bw 2sp 1pa","3bw 2sp 1pa","3bw 0sp 3pa","3bw 0sp 3pa","3bw 0sp 3pa","2bw 0sp 2pa","2bw 0sp 2pa","2bw 0sp 2pa")
comp.comb.bw.sena
comb.bw.result.type<-c("win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie","win","loss","tie")
comb.bw.result.type
comp.bw.comb.value.sena<-c(per.n404.sena.won,per.n404.sena.lost,per.n404.sena.tied,per.n303.sena.won,per.n303.sena.lost,per.n303.sena.tied,per.n413.sena.won,per.n413.sena.lost,per.n413.sena.tied,per.n523.sena.won,per.n523.sena.lost,per.n523.sena.tied,per.n312.sena.won,per.n312.sena.lost,per.n312.sena.tied,per.n422.sena.won,per.n422.sena.lost,per.n422.sena.tied,per.n211.sena.won,per.n211.sena.lost,per.n211.sena.tied,per.n321.sena.won,per.n321.sena.lost,per.n321.sena.tied,per.n330.sena.won,per.n330.sena.lost,per.n330.sena.tied,per.n202.sena.won,per.n202.sena.lost,per.n202.sena.tied)
comp.bw.comb.value.sena
bw.comb.data.sena<-data.frame(comp.comb.bw.sena,comb.bw.result.type,comp.bw.comb.value.sena)
bw.comb.data.sena#dataframe created
# fileterin out cases where number of matches>10
bw.comb.data.sena.2<-bw.comb.data.sena[bw.comb.data.sena$no.comb.matches.sena>10,]
bw.comb.data.sena.2#final dataframe

# creating the y-position for adding labels to the bars
bw.comb.data.sena <- ddply(bw.comb.data.sena, .(comp.comb.bw.sena),transform, pos =comp.bw.comb.value.sena/2)
bw.comb.data.sena
bw.comb.data.sena$comb.bw.result.type <- factor(bw.comb.data.sena$comb.bw.result.type, levels = c("tie","loss","win"))
#creating the graph
graph.bw.comb.data.sena<-ggplot(bw.comb.data.sena, aes(fill=comb.bw.result.type, y=comp.bw.comb.value.sena, x=comp.comb.bw.sena)) + geom_bar(position=position_dodge(), stat="identity")
graph.bw.comb.data.sena#this is the graph
# adding the labels to the graph
fill <- c("gold","turquoise2","hotpink2")
graph.bw.comb.data.sena+geom_text(aes(label = paste(round((comp.bw.comb.value.sena),2),"%"),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle("combination of bowlers and % results in SENA") + scale_fill_manual(values=fill)+labs(x = "BOWLING COMBINATION-->bw=total bowlers,sp=spinners,pa=pacers", y = "percentage of results")

#-------------------------------------------------------------------------------------------------------
#30.What is India's overall record against test playing nations since 1 January, 2000 in ODI cricket?
#overall
australia.overall<-nrow(odidata[odidata$against.australia == "TRUE" ,])
australia.overall
australia.overall.win<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="won" ,])
australia.overall.win
australia.overall.lost<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="lost" ,])
australia.overall.lost
australia.overall.tied<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="tied" ,])
australia.overall.tied
bangladesh.overall<-nrow(odidata[odidata$against.bangladesh == "TRUE" ,])
bangladesh.overall
bangladesh.overall.win<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="won" ,])
bangladesh.overall.win
bangladesh.overall.lost<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="lost" ,])
bangladesh.overall.lost
bangladesh.overall.tied<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="tied" ,])
bangladesh.overall.tied
england.overall<-nrow(odidata[odidata$against.england == "TRUE" ,])
england.overall
england.overall.win<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="won" ,])
england.overall.win
england.overall.lost<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="lost" ,])
england.overall.lost
england.overall.tied<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="tied" ,])
england.overall.tied
newzealand.overall<-nrow(odidata[odidata$against.newzealand == "TRUE" ,])
newzealand.overall
newzealand.overall.win<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="won" ,])
newzealand.overall.win
newzealand.overall.lost<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="lost" ,])
newzealand.overall.lost
newzealand.overall.tied<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="tied" ,])
newzealand.overall.tied
pakistan.overall<-nrow(odidata[odidata$against.pakistan == "TRUE" ,])
pakistan.overall
pakistan.overall.win<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="won" ,])
pakistan.overall.win
pakistan.overall.lost<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="lost" ,])
pakistan.overall.lost
pakistan.overall.tied<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="tied" ,])
pakistan.overall.tied
southafrica.overall<-nrow(odidata[odidata$against.southafrica == "TRUE" ,])
southafrica.overall
southafrica.overall.win<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="won" ,])
southafrica.overall.win
southafrica.overall.lost<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="lost" ,])
southafrica.overall.lost
southafrica.overall.tied<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="tied" ,])
southafrica.overall.tied
srilanka.overall<-nrow(odidata[odidata$against.srilanka == "TRUE" ,])
srilanka.overall
srilanka.overall.win<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="won" ,])
srilanka.overall.win
srilanka.overall.lost<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="lost" ,])
srilanka.overall.lost
srilanka.overall.tied<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="tied" ,])
srilanka.overall.tied
westindies.overall<-nrow(odidata[odidata$against.westindies == "TRUE" ,])
westindies.overall
westindies.overall.win<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="won" ,])
westindies.overall.win
westindies.overall.lost<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="lost" ,])
westindies.overall.lost
westindies.overall.tied<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="tied" ,])
westindies.overall.tied
zimbabwe.overall<-nrow(odidata[odidata$against.zimbabwe == "TRUE" ,])
zimbabwe.overall
zimbabwe.overall.win<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="won" ,])
zimbabwe.overall.win
zimbabwe.overall.lost<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="lost" ,])
zimbabwe.overall.lost
zimbabwe.overall.tied<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="tied" ,])
zimbabwe.overall.tied
team<-c("Australia","Australia","Australia","Australia","Bangladesh","Bangladesh","Bangladesh","Bangladesh","England","England","England","England","New Zealand","New Zealand","New Zealand","New Zealand","Pakistan","Pakistan","Pakistan","Pakistan","South Africa","South Africa","South Africa","South Africa","Sri Lanka","Sri Lanka","Sri Lanka","Sri Lanka","West Indies","West Indies","West Indies","West Indies","Zimbabwe","Zimbabwe","Zimbabwe","Zimbabwe")
team
match.stats<-c("played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied")
match.stats
value.match.stats<-c(australia.overall,australia.overall.win,australia.overall.lost,australia.overall.tied,bangladesh.overall,bangladesh.overall.win,bangladesh.overall.lost,bangladesh.overall.tied,england.overall,england.overall.win,england.overall.lost,england.overall.tied,newzealand.overall,newzealand.overall.win,newzealand.overall.lost,newzealand.overall.tied,pakistan.overall,pakistan.overall.win,pakistan.overall.lost,pakistan.overall.tied,southafrica.overall,southafrica.overall.win,southafrica.overall.lost,southafrica.overall.tied,srilanka.overall,srilanka.overall.win,srilanka.overall.lost,srilanka.overall.tied,westindies.overall,westindies.overall.win,westindies.overall.lost,westindies.overall.tied,zimbabwe.overall,zimbabwe.overall.win,zimbabwe.overall.lost,zimbabwe.overall.tied)
value.match.stats
india.vs.opp<-data.frame(team,match.stats,value.match.stats)
india.vs.opp

# creating the y-position for adding labels to the bars
india.vs.opp <- ddply(india.vs.opp, .(team),transform, pos =value.match.stats/2)
india.vs.opp
india.vs.opp$match.stats <- factor(india.vs.opp$match.stats, levels = c("played","won","lost","tied"))
#creating the graph
graph.india.vs.opp<-ggplot(india.vs.opp, aes(fill=match.stats, y=value.match.stats, x=team)) + geom_bar(position=position_dodge(), stat="identity")
graph.india.vs.opp#this is the graph
# adding the labels to the graph
fill <- c("gold","lawngreen","turquoise2","hotpink2")
graph.india.vs.opp+geom_text(aes(label = paste(value.match.stats),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD AGAINST TOP 10 TEST NATIONS  ") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")

#converting these into percentage value
aus.win<- australia.overall.win/australia.overall*100
aus.loss<-australia.overall.lost/australia.overall*100
aus.tie<-australia.overall.tied/australia.overall*100
ban.win<-bangladesh.overall.win/bangladesh.overall*100
ban.loss<-bangladesh.overall.lost/bangladesh.overall*100
ban.tie<-bangladesh.overall.tied/bangladesh.overall*100
eng.win<-england.overall.win/england.overall*100
eng.loss<-england.overall.lost/england.overall*100
eng.tie<-england.overall.tied/england.overall*100
nz.win<-newzealand.overall.win/newzealand.overall*100
nz.loss<-newzealand.overall.lost/newzealand.overall*100
nz.tie<-newzealand.overall.tied/newzealand.overall*100
pak.win<-pakistan.overall.win/pakistan.overall*100
pak.loss<-pakistan.overall.lost/pakistan.overall*100
pak.tie<-pakistan.overall.tied/pakistan.overall*100
sa.win<-southafrica.overall.win/southafrica.overall*100
sa.loss<-southafrica.overall.lost/southafrica.overall*100
sa.tie<-southafrica.overall.tied/southafrica.overall*100
slk.win<-srilanka.overall.win/srilanka.overall*100
slk.loss<-srilanka.overall.lost/srilanka.overall*100
slk.tie<-srilanka.overall.tied/srilanka.overall*100
wi.win<-westindies.overall.win/westindies.overall*100
wi.loss<-westindies.overall.lost/westindies.overall*100
wi.tie<-westindies.overall.tied/westindies.overall*100
zim.win<-zimbabwe.overall.win/zimbabwe.overall*100
zim.loss<-zimbabwe.overall.lost/zimbabwe.overall*100
zim.tie<-zimbabwe.overall.tied/zimbabwe.overall*100

per.opp.stats<-c(aus.win,aus.loss,aus.tie,ban.win,ban.loss,ban.tie,eng.win,eng.loss,eng.tie,nz.win,nz.loss,nz.tie,pak.win,pak.loss,pak.tie,sa.win,sa.loss,sa.tie,slk.win,slk.loss,slk.tie,wi.win,wi.loss,wi.tie,zim.win,zim.loss,zim.tie)
team.3<-c("Australia","Australia","Australia","Bangladesh","Bangladesh","Bangladesh","England","England","England","New Zealand","New Zealand","New Zealand","Pakistan","Pakistan","Pakistan","South Africa","South Africa","South Africa","Sri Lanka","Sri Lanka","Sri Lanka","West Indies","West Indies","West Indies","Zimbabwe","Zimbabwe","Zimbabwe")
team.3
match.stats.3<-c("won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied","won","lost","tied")
match.stats.3
india.vs.opp.per<-data.frame(team.3,match.stats.3,per.opp.stats)
india.vs.opp.per

# creating the y-position for adding labels to the bars
india.vs.opp.per <- ddply(india.vs.opp.per, .(team.3),transform, pos =cumsum(per.opp.stats)- (0.5*per.opp.stats))
india.vs.opp.per
india.vs.opp.per$match.stats.3 <- factor(india.vs.opp.per$match.stats.3, levels = c("tied","lost","won"))
#creating the graph
graph.india.vs.opp.per<-ggplot(india.vs.opp.per, aes(fill=match.stats.3, y=per.opp.stats, x=team.3)) + geom_bar(position="fill", stat="identity")
graph.india.vs.opp.per#this is the graph
# adding the labels to the graph
fill <- c("gold","lawngreen","turquoise2")
graph.india.vs.opp.per+geom_text(aes(label = paste(round(per.opp.stats),2),y=pos,size = 4),colour="black",fontface="bold",position = "fill")+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD IN %s AGAINST TOP 10 TEST NATIONS  ") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")

# at home
australia.overall.home<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$home=="TRUE" ,])
australia.overall.home
australia.overall.win.home<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="won" &odidata$home=="TRUE",])
australia.overall.win.home
australia.overall.lost.home<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
australia.overall.lost.home
australia.overall.tied.home<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="tied"&odidata$home=="TRUE" ,])
australia.overall.tied.home
bangladesh.overall.home<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$home=="TRUE" ,])
bangladesh.overall.home
bangladesh.overall.win.home<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE"&odidata$home=="TRUE" ,])
bangladesh.overall.win.home
bangladesh.overall.lost.home<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
bangladesh.overall.lost.home
bangladesh.overall.tied.home<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="tied" &odidata$home=="TRUE",])
bangladesh.overall.tied.home
england.overall.home<-nrow(odidata[odidata$against.england == "TRUE" &odidata$home=="TRUE",])
england.overall.home
england.overall.win.home<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE" ,])
england.overall.win.home
england.overall.lost.home<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
england.overall.lost.home
england.overall.tied.home<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="tied"&odidata$home=="TRUE" ,])
england.overall.tied.home
newzealand.overall.home<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$home=="TRUE" ,])
newzealand.overall.home
newzealand.overall.win.home<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE" ,])
newzealand.overall.win.home
newzealand.overall.lost.home<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
newzealand.overall.lost.home
newzealand.overall.tied.home<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="tied" &odidata$home=="TRUE",])
newzealand.overall.tied.home
pakistan.overall.home<-nrow(odidata[odidata$against.pakistan == "TRUE" &odidata$home=="TRUE",])
pakistan.overall.home
pakistan.overall.win.home<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="won" &odidata$home=="TRUE",])
pakistan.overall.win.home
pakistan.overall.lost.home<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="lost" &odidata$home=="TRUE",])
pakistan.overall.lost.home
pakistan.overall.tied.home<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="tied"&odidata$home=="TRUE" ,])
pakistan.overall.tied.home
southafrica.overall.home<-nrow(odidata[odidata$against.southafrica == "TRUE" &odidata$home=="TRUE",])
southafrica.overall.home
southafrica.overall.win.home<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE" ,])
southafrica.overall.win.home
southafrica.overall.lost.home<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
southafrica.overall.lost.home
southafrica.overall.tied.home<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="tied" &odidata$home=="TRUE",])
southafrica.overall.tied.home
srilanka.overall.home<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$home=="TRUE" ,])
srilanka.overall.home
srilanka.overall.win.home<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE" ,])
srilanka.overall.win.home
srilanka.overall.lost.home<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
srilanka.overall.lost.home
srilanka.overall.tied.home<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="tied"&odidata$home=="TRUE" ,])
srilanka.overall.tied.home
westindies.overall.home<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$home=="TRUE" ,])
westindies.overall.home
westindies.overall.win.home<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE" ,])
westindies.overall.win.home
westindies.overall.lost.home<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="lost" &odidata$home=="TRUE",])
westindies.overall.lost.home
westindies.overall.tied.home<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="tied"&odidata$home=="TRUE" ,])
westindies.overall.tied.home
zimbabwe.overall.home<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$home=="TRUE" ,])
zimbabwe.overall.home
zimbabwe.overall.win.home<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="won"&odidata$home=="TRUE" ,])
zimbabwe.overall.win.home
zimbabwe.overall.lost.home<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="lost"&odidata$home=="TRUE" ,])
zimbabwe.overall.lost.home
zimbabwe.overall.tied.home<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="tied"&odidata$home=="TRUE" ,])
zimbabwe.overall.tied.home
team<-c("Australia","Australia","Australia","Australia","Bangladesh","Bangladesh","Bangladesh","Bangladesh","England","England","England","England","New Zealand","New Zealand","New Zealand","New Zealand","Pakistan","Pakistan","Pakistan","Pakistan","South Africa","South Africa","South Africa","South Africa","Sri Lanka","Sri Lanka","Sri Lanka","Sri Lanka","West Indies","West Indies","West Indies","West Indies","Zimbabwe","Zimbabwe","Zimbabwe","Zimbabwe")
team
match.stats<-c("played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied")
match.stats
value.match.stats.home<-c(australia.overall.home,australia.overall.win.home,australia.overall.lost.home,australia.overall.tied.home,bangladesh.overall.home,bangladesh.overall.win.home,bangladesh.overall.lost.home,bangladesh.overall.tied.home,england.overall.home,england.overall.win.home,england.overall.lost.home,england.overall.tied.home,newzealand.overall.home,newzealand.overall.win.home,newzealand.overall.lost.home,newzealand.overall.tied.home,pakistan.overall.home,pakistan.overall.win.home,pakistan.overall.lost.home,pakistan.overall.tied.home,southafrica.overall.home,southafrica.overall.win.home,southafrica.overall.lost.home,southafrica.overall.tied.home,srilanka.overall.home,srilanka.overall.win.home,srilanka.overall.lost.home,srilanka.overall.tied.home,westindies.overall.home,westindies.overall.win.home,westindies.overall.lost.home,westindies.overall.tied.home,zimbabwe.overall.home,zimbabwe.overall.win.home,zimbabwe.overall.lost.home,zimbabwe.overall.tied.home)
value.match.stats.home
india.vs.opp.home<-data.frame(team,match.stats,value.match.stats.home)
india.vs.opp.home

# creating the y-position for adding labels to the bars
india.vs.opp.home <- ddply(india.vs.opp.home, .(team),transform, pos =value.match.stats.home/2)
india.vs.opp.home
india.vs.opp.home$match.stats <- factor(india.vs.opp.home$match.stats, levels = c("played","won","lost","tied"))
#creating the graph
graph.india.vs.opp.home<-ggplot(india.vs.opp.home, aes(fill=match.stats, y=value.match.stats.home, x=team)) + geom_bar(position=position_dodge(), stat="identity")
graph.india.vs.opp.home#this is the graph
# adding the labels to the graph
fill <- c("gold","lawngreen","turquoise2","hotpink2")
graph.india.vs.opp.home+geom_text(aes(label = paste(value.match.stats.home),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD AGAINST TOP 10 TEST NATIONS AT HOME ") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")


#1.	What is India's away record against test playing nations since 1 January, 2000 in ODI cricket?
australia.overall.away<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$away=="TRUE" ,])
australia.overall.away
australia.overall.win.away<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="won" &odidata$away=="TRUE",])
australia.overall.win.away
australia.overall.lost.away<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
australia.overall.lost.away
australia.overall.tied.away<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="tied"&odidata$away=="TRUE" ,])
australia.overall.tied.away
bangladesh.overall.away<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$away=="TRUE" ,])
bangladesh.overall.away
bangladesh.overall.win.away<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE"&odidata$away=="TRUE" ,])
bangladesh.overall.win.away
bangladesh.overall.lost.away<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
bangladesh.overall.lost.away
bangladesh.overall.tied.away<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="tied" &odidata$away=="TRUE",])
bangladesh.overall.tied.away
england.overall.away<-nrow(odidata[odidata$against.england == "TRUE" &odidata$away=="TRUE",])
england.overall.away
england.overall.win.away<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE" ,])
england.overall.win.away
england.overall.lost.away<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
england.overall.lost.away
england.overall.tied.away<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="tied"&odidata$away=="TRUE" ,])
england.overall.tied.away
newzealand.overall.away<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$away=="TRUE" ,])
newzealand.overall.away
newzealand.overall.win.away<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE" ,])
newzealand.overall.win.away
newzealand.overall.lost.away<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
newzealand.overall.lost.away
newzealand.overall.tied.away<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="tied" &odidata$away=="TRUE",])
newzealand.overall.tied.away
pakistan.overall.away<-nrow(odidata[odidata$against.pakistan == "TRUE" &odidata$away=="TRUE",])
pakistan.overall.away
pakistan.overall.win.away<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="won" &odidata$away=="TRUE",])
pakistan.overall.win.away
pakistan.overall.lost.away<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="lost" &odidata$away=="TRUE",])
pakistan.overall.lost.away
pakistan.overall.tied.away<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="tied"&odidata$away=="TRUE" ,])
pakistan.overall.tied.away
southafrica.overall.away<-nrow(odidata[odidata$against.southafrica == "TRUE" &odidata$away=="TRUE",])
southafrica.overall.away
southafrica.overall.win.away<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE" ,])
southafrica.overall.win.away
southafrica.overall.lost.away<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
southafrica.overall.lost.away
southafrica.overall.tied.away<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="tied" &odidata$away=="TRUE",])
southafrica.overall.tied.away
srilanka.overall.away<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$away=="TRUE" ,])
srilanka.overall.away
srilanka.overall.win.away<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE" ,])
srilanka.overall.win.away
srilanka.overall.lost.away<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
srilanka.overall.lost.away
srilanka.overall.tied.away<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="tied"&odidata$away=="TRUE" ,])
srilanka.overall.tied.away
westindies.overall.away<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$away=="TRUE" ,])
westindies.overall.away
westindies.overall.win.away<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE" ,])
westindies.overall.win.away
westindies.overall.lost.away<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="lost" &odidata$away=="TRUE",])
westindies.overall.lost.away
westindies.overall.tied.away<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="tied"&odidata$away=="TRUE" ,])
westindies.overall.tied.away
zimbabwe.overall.away<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$away=="TRUE" ,])
zimbabwe.overall.away
zimbabwe.overall.win.away<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="won"&odidata$away=="TRUE" ,])
zimbabwe.overall.win.away
zimbabwe.overall.lost.away<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="lost"&odidata$away=="TRUE" ,])
zimbabwe.overall.lost.away
zimbabwe.overall.tied.away<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="tied"&odidata$away=="TRUE" ,])
zimbabwe.overall.tied.away

team<-c("Australia","Australia","Australia","Australia","Bangladesh","Bangladesh","Bangladesh","Bangladesh","England","England","England","England","New Zealand","New Zealand","New Zealand","New Zealand","Pakistan","Pakistan","Pakistan","Pakistan","South Africa","South Africa","South Africa","South Africa","Sri Lanka","Sri Lanka","Sri Lanka","Sri Lanka","West Indies","West Indies","West Indies","West Indies","Zimbabwe","Zimbabwe","Zimbabwe","Zimbabwe")
team
match.stats<-c("played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied")
match.stats
value.match.stats.away<-c(australia.overall.away,australia.overall.win.away,australia.overall.lost.away,australia.overall.tied.away,bangladesh.overall.away,bangladesh.overall.win.away,bangladesh.overall.lost.away,bangladesh.overall.tied.away,england.overall.away,england.overall.win.away,england.overall.lost.away,england.overall.tied.away,newzealand.overall.away,newzealand.overall.win.away,newzealand.overall.lost.away,newzealand.overall.tied.away,pakistan.overall.away,pakistan.overall.win.away,pakistan.overall.lost.away,pakistan.overall.tied.away,southafrica.overall.away,southafrica.overall.win.away,southafrica.overall.lost.away,southafrica.overall.tied.away,srilanka.overall.away,srilanka.overall.win.away,srilanka.overall.lost.away,srilanka.overall.tied.away,westindies.overall.away,westindies.overall.win.away,westindies.overall.lost.away,westindies.overall.tied.away,zimbabwe.overall.away,zimbabwe.overall.win.away,zimbabwe.overall.lost.away,zimbabwe.overall.tied.away)
value.match.stats.away
india.vs.opp.away<-data.frame(team,match.stats,value.match.stats.away)
india.vs.opp.away

# creating the y-position for adding labels to the bars
india.vs.opp.away <- ddply(india.vs.opp.away, .(team),transform, pos =value.match.stats.away/2)
india.vs.opp.away
india.vs.opp.away$match.stats <- factor(india.vs.opp.away$match.stats, levels = c("played","won","lost","tied"))
#creating the graph
graph.india.vs.opp.away<-ggplot(india.vs.opp.away, aes(fill=match.stats, y=value.match.stats.away, x=team)) + geom_bar(position=position_dodge(), stat="identity")
graph.india.vs.opp.away#this is the graph
# adding the labels to the graph
fill <- c("gold","lawngreen","turquoise2","hotpink2")
graph.india.vs.opp.away+geom_text(aes(label = paste(value.match.stats.away),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD AGAINST TOP 10 TEST NATIONS AT AWAY ") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")

#at neutral

#1.	What is India's neutral record against test playing nations since 1 January, 2000 in ODI cricket?
australia.overall.neutral<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$neutral=="TRUE" ,])
australia.overall.neutral
australia.overall.win.neutral<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="won" &odidata$neutral=="TRUE",])
australia.overall.win.neutral
australia.overall.lost.neutral<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
australia.overall.lost.neutral
australia.overall.tied.neutral<-nrow(odidata[odidata$against.australia == "TRUE"&odidata$match.result.for.india=="tied"&odidata$neutral=="TRUE" ,])
australia.overall.tied.neutral
bangladesh.overall.neutral<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$neutral=="TRUE" ,])
bangladesh.overall.neutral
bangladesh.overall.win.neutral<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE"&odidata$neutral=="TRUE" ,])
bangladesh.overall.win.neutral
bangladesh.overall.lost.neutral<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
bangladesh.overall.lost.neutral
bangladesh.overall.tied.neutral<-nrow(odidata[odidata$against.bangladesh == "TRUE"&odidata$match.result.for.india=="tied" &odidata$neutral=="TRUE",])
bangladesh.overall.tied.neutral
england.overall.neutral<-nrow(odidata[odidata$against.england == "TRUE" &odidata$neutral=="TRUE",])
england.overall.neutral
england.overall.win.neutral<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE" ,])
england.overall.win.neutral
england.overall.lost.neutral<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
england.overall.lost.neutral
england.overall.tied.neutral<-nrow(odidata[odidata$against.england == "TRUE"&odidata$match.result.for.india=="tied"&odidata$neutral=="TRUE" ,])
england.overall.tied.neutral
newzealand.overall.neutral<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$neutral=="TRUE" ,])
newzealand.overall.neutral
newzealand.overall.win.neutral<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE" ,])
newzealand.overall.win.neutral
newzealand.overall.lost.neutral<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
newzealand.overall.lost.neutral
newzealand.overall.tied.neutral<-nrow(odidata[odidata$against.newzealand == "TRUE"&odidata$match.result.for.india=="tied" &odidata$neutral=="TRUE",])
newzealand.overall.tied.neutral
pakistan.overall.neutral<-nrow(odidata[odidata$against.pakistan == "TRUE" &odidata$neutral=="TRUE",])
pakistan.overall.neutral
pakistan.overall.win.neutral<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="won" &odidata$neutral=="TRUE",])
pakistan.overall.win.neutral
pakistan.overall.lost.neutral<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="lost" &odidata$neutral=="TRUE",])
pakistan.overall.lost.neutral
pakistan.overall.tied.neutral<-nrow(odidata[odidata$against.pakistan == "TRUE"&odidata$match.result.for.india=="tied"&odidata$neutral=="TRUE" ,])
pakistan.overall.tied.neutral
southafrica.overall.neutral<-nrow(odidata[odidata$against.southafrica == "TRUE" &odidata$neutral=="TRUE",])
southafrica.overall.neutral
southafrica.overall.win.neutral<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE" ,])
southafrica.overall.win.neutral
southafrica.overall.lost.neutral<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
southafrica.overall.lost.neutral
southafrica.overall.tied.neutral<-nrow(odidata[odidata$against.southafrica == "TRUE"&odidata$match.result.for.india=="tied" &odidata$neutral=="TRUE",])
southafrica.overall.tied.neutral
srilanka.overall.neutral<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$neutral=="TRUE" ,])
srilanka.overall.neutral
srilanka.overall.win.neutral<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE" ,])
srilanka.overall.win.neutral
srilanka.overall.lost.neutral<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
srilanka.overall.lost.neutral
srilanka.overall.tied.neutral<-nrow(odidata[odidata$against.srilanka == "TRUE"&odidata$match.result.for.india=="tied"&odidata$neutral=="TRUE" ,])
srilanka.overall.tied.neutral
westindies.overall.neutral<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$neutral=="TRUE" ,])
westindies.overall.neutral
westindies.overall.win.neutral<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE" ,])
westindies.overall.win.neutral
westindies.overall.lost.neutral<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="lost" &odidata$neutral=="TRUE",])
westindies.overall.lost.neutral
westindies.overall.tied.neutral<-nrow(odidata[odidata$against.westindies == "TRUE"&odidata$match.result.for.india=="tied"&odidata$neutral=="TRUE" ,])
westindies.overall.tied.neutral
zimbabwe.overall.neutral<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$neutral=="TRUE" ,])
zimbabwe.overall.neutral
zimbabwe.overall.win.neutral<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="won"&odidata$neutral=="TRUE" ,])
zimbabwe.overall.win.neutral
zimbabwe.overall.lost.neutral<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="lost"&odidata$neutral=="TRUE" ,])
zimbabwe.overall.lost.neutral
zimbabwe.overall.tied.neutral<-nrow(odidata[odidata$against.zimbabwe == "TRUE"&odidata$match.result.for.india=="tied"&odidata$neutral=="TRUE" ,])
zimbabwe.overall.tied.neutral

team<-c("Australia","Australia","Australia","Australia","Bangladesh","Bangladesh","Bangladesh","Bangladesh","England","England","England","England","New Zealand","New Zealand","New Zealand","New Zealand","Pakistan","Pakistan","Pakistan","Pakistan","South Africa","South Africa","South Africa","South Africa","Sri Lanka","Sri Lanka","Sri Lanka","Sri Lanka","West Indies","West Indies","West Indies","West Indies","Zimbabwe","Zimbabwe","Zimbabwe","Zimbabwe")
team
match.stats<-c("played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied","played","won","lost","tied")
match.stats
value.match.stats.neutral<-c(australia.overall.neutral,australia.overall.win.neutral,australia.overall.lost.neutral,australia.overall.tied.neutral,bangladesh.overall.neutral,bangladesh.overall.win.neutral,bangladesh.overall.lost.neutral,bangladesh.overall.tied.neutral,england.overall.neutral,england.overall.win.neutral,england.overall.lost.neutral,england.overall.tied.neutral,newzealand.overall.neutral,newzealand.overall.win.neutral,newzealand.overall.lost.neutral,newzealand.overall.tied.neutral,pakistan.overall.neutral,pakistan.overall.win.neutral,pakistan.overall.lost.neutral,pakistan.overall.tied.neutral,southafrica.overall.neutral,southafrica.overall.win.neutral,southafrica.overall.lost.neutral,southafrica.overall.tied.neutral,srilanka.overall.neutral,srilanka.overall.win.neutral,srilanka.overall.lost.neutral,srilanka.overall.tied.neutral,westindies.overall.neutral,westindies.overall.win.neutral,westindies.overall.lost.neutral,westindies.overall.tied.neutral,zimbabwe.overall.neutral,zimbabwe.overall.win.neutral,zimbabwe.overall.lost.neutral,zimbabwe.overall.tied.neutral)
value.match.stats.neutral
india.vs.opp.neutral<-data.frame(team,match.stats,value.match.stats.neutral)
india.vs.opp.neutral

# creating the y-position for adding labels to the bars
india.vs.opp.neutral <- ddply(india.vs.opp.neutral, .(team),transform, pos =value.match.stats.neutral/2)
india.vs.opp.neutral
india.vs.opp.neutral$match.stats <- factor(india.vs.opp.neutral$match.stats, levels = c("played","won","lost","tied"))
#creating the graph
graph.india.vs.opp.neutral<-ggplot(india.vs.opp.neutral, aes(fill=match.stats, y=value.match.stats.neutral, x=team)) + geom_bar(position=position_dodge(), stat="identity")
graph.india.vs.opp.neutral#this is the graph
# adding the labels to the graph
fill <- c("gold","lawngreen","turquoise2","hotpink2")
graph.india.vs.opp.neutral+geom_text(aes(label = paste(value.match.stats.neutral),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD AGAINST TOP 10 TEST NATIONS AT NEUTRAL ") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")

# effect of 100s by indian batsmen
win100<- nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$match.result.for.india=="won",])
win100
loss100<- nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$match.result.for.india=="lost",])
loss100
tie100<-nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$match.result.for.india=="tied",])
tie100
count100<-nrow(odidata[odidata$no.of.100.s..by.indian.batsmen,])
count100
per100win<-win100/count100*100
per100loss<-loss100/count100*100
per100tie<-tie100/count100*100
res.type.100<-c("won","lost","tied")
val.type.100<-c(per100win,per100loss,per100tie)
data.100.res<-data.frame(res.type.100,val.type.100)
graph.data.100.res<-ggplot(data.100.res, aes(fill=res.type.100, y=val.type.100, x=res.type.100)) + geom_bar(position=position_dodge(), stat="identity")+geom_text(aes(label = paste(round((val.type.100),2),"%"),y=pos,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD WHEN INDIAN BATSMEN SCORE 100(S)") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")
graph.data.100.res

# effect of 100s by indian batsmen
win100.chase<- nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$match.result.for.india=="won"&odidata$batting.turn=="2nd",])
win100.chase
loss100.chase<- nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$match.result.for.india=="lost"&odidata$batting.turn=="2nd",])
tie100.chase<-nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$match.result.for.india=="tied"&odidata$batting.turn=="2nd",])
loss100.chase
count100.chase<-nrow(odidata[odidata$no.of.100.s..by.indian.batsmen&odidata$batting.turn=="2nd",])
count100.chase
per100win.chase<-win100.chase/count100.chase*100
per100loss.chase<-loss100.chase/count100.chase*100
per100tie.chase<-tie100.chase/count100.chase*100
res.type.100<-c("won","lost","tied")
val.type.100.chase<-c(per100win.chase,per100loss.chase,per100tie.chase)
data.100.res.chase<-data.frame(res.type.100,val.type.100.chase)
graph.data.100.res.chase<-ggplot(data.100.res.chase, aes(fill=res.type.100, y=val.type.100.chase, x=res.type.100)) + geom_bar(position=position_dodge(), stat="identity")+geom_text(aes(label = paste(round((val.type.100.chase),2),"%"),y=val.type.100.chase/2,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD CHASING WHEN INDIAN BATSMEN SCORE 100(S)") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")
graph.data.100.res.chase

#------------------------------------------------------------------------------
wk5.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE",])
WK5.win.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$match.result.for.india=="won",])
wk5.loss.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$match.result.for.india=="lost",])
wk5.tie.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$match.result.for.india=="tied",])
wk5.no
WK5.win.no
wk5.loss.no
wk5.tie.no
per.WK5.win.no<-WK5.win.no/wk5.no*100
per.wk5.loss.no<-wk5.loss.no/wk5.no*100
per.wk5.tie.no<-wk5.tie.no/wk5.no*100

def.wk5.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$batting.turn=="2nd",])
def.WK5.win.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$match.result.for.india=="won"&odidata$batting.turn=="2nd",])
def.wk5.loss.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$match.result.for.india=="lost"&odidata$batting.turn=="2nd",])
def.wk5.tie.no<-nrow(odidata[odidata$indian.bowler.taking.5..wicket=="TRUE"&odidata$match.result.for.india=="tied"&odidata$batting.turn=="2nd",])
def.wk5.no
def.WK5.win.no
def.wk5.loss.no
def.wk5.tie.no

per.def.WK5.win.no<-def.WK5.win.no/def.wk5.no*100
per.def.wk5.loss.no<-def.wk5.loss.no/def.wk5.no*100
per.def.wk5.tie.no<-def.wk5.tie.no/def.wk5.no*100

# effect of 5+ wicktes by indian bowlers
res.type.5<-c("won","lost","tied")
val.type.5<-c(per.WK5.win.no,per.wk5.loss.no,per.wk5.tie.no)
data.5.res<-data.frame(res.type.5,val.type.5)
graph.data.5.res<-ggplot(data.5.res, aes(fill=res.type.5, y=val.type.5, x=res.type.5)) + geom_bar(position=position_dodge(), stat="identity")+geom_text(aes(label = paste(round((val.type.5),2),"%"),y=val.type.5/2,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD WHEN INDIAN BOWLER TAKES 5+ WICKETS") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")
graph.data.5.res

# effect of 5+ wicktes by indian bowlers while defending
res.type.5<-c("won","lost","tied")
val.type.5.def<-c(per.def.WK5.win.no,per.def.wk5.loss.no,per.def.wk5.tie.no)
data.5.res.def<-data.frame(res.type.5,val.type.5.def)
graph.data.5.res.def<-ggplot(data.5.res.def, aes(fill=res.type.5, y=val.type.5.def, x=res.type.5)) + geom_bar(position=position_dodge(), stat="identity")+geom_text(aes(label = paste(round((val.type.5.def),2),"%"),y=val.type.5.def/2,size = 4),colour="black",fontface="bold",position = position_dodge(width = 0.9))+theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) + ggtitle(" INDIA'S RECORD DEFENDING WHEN INDIAN BOWLER TAKES 5+ WICKETS") + scale_fill_manual(values=fill)+labs(x = "TEAMS ", y = "VALUES")
graph.data.5.res.def
