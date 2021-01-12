# model to calculate share of housing starts by type
# Peter Berrill 
# Last Update Nov 25 2020
setwd("C:/Users/pb637/Documents/Yale Courses/Research/RECS research/EST Data and Codes/github_upload")
rm(list=ls()) # clear workspace
cat("\014") # clear console
graphics.off() # remove graphics windows
library(sjPlot)
library(sandwich)
library(lmtest)
library(mgcv)
library(ggplot2)
library(margins)
rdd<-read.csv("HousingStartsData.csv")

rdd$Quarter<-rdd$Observation%%4
rdd$Quarter[rdd$Quarter==0]<-4
rdd$Quarter<-as.factor(rdd$Quarter)
rdd$sqrGDP<-sqrt(rdd$GDP)
rdd$lnGDP<-log(rdd$GDP)
rdd$Year<-rep(1959:2018, each=4)
# create modified policy data for counterfactual housing starts predictions
rddCF<-rdd
rddCF$TRA86<-0
rddCF$FIRREA<-0
# rddCF$TRA97<-0
rddCF$PH<-0
rddCFsm<-rddCF[50:240,]
rddsm<-rdd[50:240,]

# linear models of single family share of housing starts, with original data
s8<-lm(SFsharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + TRA97 + PH,data=rdd)
s12<-lm(SFsharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + PH,data=rdd)

s12m<-lm(SFsharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + PH + Mileage,data=rdd)
s12car<-lm(SFsharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + PH + CarPP,data=rdd)

s8c<-lm(SFcomSharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + TRA97 + PH,data=rdd)
s12c<-lm(SFcomSharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + PH,data=rdd)

s8cl<-lm(SFcomlagSharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + TRA97 + PH,data=rdd)
s12cl<-lm(SFcomlagSharepc ~ delPop + Quarter + RLGDP_NSA + TRA86 + FIRREA +MORTGAGE30US + PH,data=rdd)

# generate robust Newey West standard errors
s8Rob<-coeftest(s8,vcov = NeweyWest(s8,adjust=FALSE,prewhite = FALSE,lag = 2))
s12Rob<-coeftest(s12,vcov = NeweyWest(s12,adjust=FALSE,prewhite = FALSE,lag = 2))
s12clRob<-coeftest(s12cl,vcov = NeweyWest(s12cl,adjust=FALSE,prewhite = FALSE,lag = 2))

s12mRob<-coeftest(s12m,vcov = NeweyWest(s12m,adjust=FALSE,prewhite = FALSE,lag = 2))
s12carRob<-coeftest(s12car,vcov = NeweyWest(s12car,adjust=FALSE,prewhite = FALSE,lag = 2))

# regression coefficient summary table
tab_model(s8,s12, show.ci = 0)
tab_model(s12,s12c,s12cl,show.ci = 0)
tab_model(s12,s12m,s12car,show.ci = 0)
# generate predictions of single family share with favoured model, with policy counterfactual
sfsmod<-predict(s12,rddCFsm) # counterfactual starts
sfscmod<-predict(s12cl,rddCFsm) # from lagged model
sfsmodact<-s12[["fitted.values"]] # fitted starts from original model
sfscmodact<-s12cl[["fitted.values"]] # fitted starts from original model

sfcomp<-matrix(c(sfsmod,sfsmodact,rdd$SFsharepc[50:240]),191,3) # compare counterfactual starts, fitted starts, and actual starts
# do same as sfcomp for completed units
sfccomp<-matrix(c(sfscmod,sfscmodact,rdd$SFcomlagSharepc[50:240]),191,3) # compare counterfactual, fitted , and actual completions, based on estimated year of starting construction

rdd$sfsmodact<-rdd$SFshare
rdd$sfsmodact[50:240]<-0.01*sfsmodact

rddCF$sfsmod<-rddCF$SFshare
rddCF$sfsmod[50:240]<-0.01*sfsmod

rddCF$sfscmod<-rddCF$SFcomlagShare
rddCF$sfscmod[50:240]<-0.01*sfscmod


dt<-seq(1971.25,2018.75,0.25)
dt2<-seq(1959,2018.75,0.25)
# plot actual, modeled (fitted), and predicted counterfactual SF shares of housing starts
windows()
plot(dt,rdd$SFsharepc[50:240],type="l",ylim = c(20,90), main = "Actual and modeled SF share of quarterly housing starts", ylab = "Single-family share (%)",xlab ="Year")
lines(dt,sfsmod,col="blue")
lines(dt,sfsmodact,col="red")
legend(1998,33,legend = c("Historical","Modeled, policy CF","Modeled"),col = c("black","blue","red"),lty=1)

windows()
plot(dt,rdd$SFcomlagSharepc[50:240],type="l",ylim = c(20,90), main = "Actual and modeled SF share of quarterly housing completions", ylab = "Single-family share (%)",xlab ="Year")
lines(dt,sfscmod,col="blue")
lines(dt,sfscmodact,col="red")
legend(1998,33,legend = c("Historical","Modeled, policy CF","Modeled"),col = c("black","blue","red"),lty=1)

rddCF$startsSF<-rddCF$SF
rddCF$startsSF[50:240]<-0.01*sfsmod*rdd$All[50:240]
rddCF$diff<-rddCF$SF-rddCF$startsSF
delStock<-sum(rddCF$diff[61:224]) # calculate total change in starts/stock in the policy counterfactual

rddCF$compSF<-rddCF$SFComp
rddCF$compSF[53:240]<-rddCF$sfscmod[50:237]*rdd$AllCompLag[50:237]
rddCF$diffComp<-rddCF$SFComp-rddCF$compSF
delStockComp<-sum(rddCF$diffComp[61:224]) # calculate total change in completions/stock in the policy counterfactual


######## plot comparative charts ###############
SFact<-as.data.frame(1959:2018)
colnames(SFact)<-"Year"
SFact$share<-tapply(rdd$SFshare*rdd$All,rddCF$Year,sum)/tapply(rdd$All,rddCF$Year,sum)
SFact$Starts<-tapply(rdd$SF,rddCF$Year,sum)
SFact$Data<-"Historical"
SFact$Type<-"Single-family"

MFact<-SFact
MFact$Starts<-tapply(rdd$MF,rddCF$Year,sum)
MFact$Data<-"Historical"
MFact$Type<-"Multifamily"

SFmodact<-SFact
SFmodact$share<-tapply(rdd$sfsmodact*rdd$All,rddCF$Year,sum)/tapply(rdd$All,rdd$Year,sum)
SFmodact$Starts<-tapply(rdd$sfsmodact*rdd$All,rddCF$Year,sum)
SFmodact$Data<-"Modeled, no policy change"

SFmodCF<-SFact
SFmodCF$share<-tapply(rddCF$sfsmod*rddCF$All,rddCF$Year,sum)/tapply(rddCF$All,rddCF$Year,sum)
SFmodCF$Starts<-tapply(rddCF$sfsmod*rddCF$All,rddCF$Year,sum)
SFmodCF$Data<-"Policy counterfactual"

MFmodact<-SFmodact
MFmodact$share<-1-SFmodact$share
MFmodact$Starts<-tapply(rdd$All,rddCF$Year,sum)-tapply(rdd$sfsmodact*rdd$All,rddCF$Year,sum)
MFmodact$Data<-"Modeled, no policy change"
MFmodact$Type<-"Multifamily"

MFmodCF<-SFmodact
MFmodCF$share<-1-SFmodCF$share
MFmodCF$Starts<-tapply(rddCF$All,rddCF$Year,sum)-tapply(rddCF$sfsmod*rddCF$All,rddCF$Year,sum)
MFmodCF$Data<-"Policy counterfactual"
MFmodCF$Type<-"Multifamily"

B<-rbind(SFact,SFmodact,SFmodCF,MFact,MFmodact,MFmodCF)
A<-rbind(SFact,SFmodCF,MFact,MFmodCF)
# plof of total SF and MF starts, historical, and under policy counterfactual.
p <- ggplot(data=A, aes(x=Year, y=Starts, linetype=Data,color=Type)) + 
  labs(title = "Historical and counterfactual housing starts, 1959-2018",y="1,000 units") +
  geom_line(size=0.8) + theme_bw() + scale_color_manual(values=c("red","blue")) + geom_vline(xintercept=c(1973,1986,1989)) +  
  annotate("text",x=1967.5,y=1530, label="PHM") + annotate("segment",x=1969,xend = 1973,y=1510,yend = 1400) +
  annotate("text",x=1994,y=1530, label="FIRREA") + annotate("segment",x=1989,xend = 1993,y=1380,yend = 1500) +
  annotate("text",x=1978.5,y=1510, label="TRA86") + annotate("segment",x=1981,xend = 1986,y=1480,yend = 1300) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 15, face = "bold"),
        legend.text=element_text(size=13),legend.title=element_text(size=13))
windows()
p
########################## save annual starts ################
AnnSt<-as.data.frame(1959:2018)
colnames(AnnSt)<-"Year"
AnnSt$SF<-SFact$Starts
AnnSt$SF_CF<-SFmodCF$Starts
AnnSt$MF<-MFact$Starts
AnnSt$MF_CF<-MFmodCF$Starts
StartCF<-AnnSt[AnnSt$Year>1972,]
# calculate changes in actual starts, and modeled counterfactual starts, from 1986 onwards
StartCF$SingleFamilyDiff<-StartCF$SF-StartCF$SF_CF
StartCF$MultifamilyDiff<-StartCF$MF-StartCF$MF_CF
write.csv(StartCF,file="StartsCounterfactual_alt.csv",row.names=FALSE)