# script to estimate counterfactual US housing energy consumption based on RECS data and type/cohort distributions
# Peter Berrill 10/30/2019
# Updated Dec 11 2019 to focus on urban housing, and use new counterfactual stocks
# set up ################
rm(list=ls()) # clear workspace
cat("\014") # clear console
graphics.off() # remove graphics windows
setwd("C:/Users/pb637/Documents/Yale Courses/Research/RECS research/")
# packages
library(dplyr)
library(ggplot2)
library(abind)
library(reshape2)
library(magrittr)
library(readxl)

# load("RECSadj.Rdata")
load("RECSincOth.RData") # unadjusted (for matching AER totals)
# RECSadj$TOTBTUnew_wa<-RECSadj$BTUSPHwa+RECSadj$BTUCOLwa+RECSadj$BTUDHWwa+RECSadj$BTUOTH
# RECS$TOTBTUnew_wa<-RECS$BTUSPHwa+RECS$BTUCOLwa+RECS$BTUDHWwa+RECS$BTUOTH
RECS$TOTBTUnew<-RECS$BTUSPH+RECS$BTUCOL+RECS$BTUDHW+RECS$BTUOTH
# r<-RECSadj
r<-RECS
r$count<-1
r90<-filter(r,r$RECSYEAR==1990)
r15<-filter(r,r$RECSYEAR==2015)
r09<-filter(r,r$RECSYEAR==2009)

r90u<-filter(r,r$RECSYEAR==1990,r$URBAN==1)
r15u<-filter(r,r$RECSYEAR==2015,r$URBAN==1)
r09u<-filter(r,r$RECSYEAR==2009,r$URBAN==1)

r90r<-filter(r,r$RECSYEAR==1990,r$URBAN==0)
r15r<-filter(r,r$RECSYEAR==2015,r$URBAN==0)
r09r<-filter(r,r$RECSYEAR==2009,r$URBAN==0)

# housing distribution 1990
s90<-tapply(r90$NWEIGHT,list(r90$TYPEHUQ,r90$AgeCohort),sum)
# housing distribution 2015
s15<-tapply(r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
# housing distribution 2009
s09<-tapply(r09$NWEIGHT,list(r09$TYPEHUQ,r09$AgeCohort),sum)
# floor area disribution 1990

# urban housing distribution 1990u
s90u<-tapply(r90u$NWEIGHT,list(r90u$TYPEHUQ,r90u$AgeCohort),sum)
# urban housing distribution 2015u
s15u<-tapply(r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)
s15u_dist<-s15u/sum(s15u)

# urban housing distribution 2009u
s09u<-tapply(r09u$NWEIGHT,list(r09u$TYPEHUQ,r09u$AgeCohort),sum)

# rural housing distribution 1990r
s90r<-tapply(r90r$NWEIGHT,list(r90r$TYPEHUQ,r90r$AgeCohort),sum)
# rural housing distribution 2015r
s15r<-tapply(r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
s15r[is.na(s15r)]<-0
s15r_dist<-round(s15r/sum(s15r),3)
# rural housing distribution 2009r
s09r<-tapply(r09r$NWEIGHT,list(r09r$TYPEHUQ,r09r$AgeCohort),sum)

# energy 1990
e90<-tapply(r90$TOTBTUnew*r90$NWEIGHT,list(r90$TYPEHUQ,r90$AgeCohort),sum)
e90r<-tapply(r90r$TOTBTUnew*r90r$NWEIGHT,list(r90r$TYPEHUQ,r90r$AgeCohort),sum)
e90u<-tapply(r90u$TOTBTUnew*r90u$NWEIGHT,list(r90u$TYPEHUQ,r90u$AgeCohort),sum)
# energy 2015
e15<-tapply(r15$TOTBTUnew*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
e15r<-tapply(r15r$TOTBTUnew*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
e15r[is.na(e15r)]<-0
e15u<-tapply(r15u$TOTBTUnew*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)

sph15<-tapply(r15$BTUSPH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
sph15r<-tapply(r15r$BTUSPH*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
sph15r[is.na(sph15r)]<-0
sph15u<-tapply(r15u$BTUSPH*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)

spc15<-tapply(r15$BTUCOL*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
spc15r<-tapply(r15r$BTUCOL*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
spc15r[is.na(spc15r)]<-0
spc15u<-tapply(r15u$BTUCOL*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)

dhw15<-tapply(r15$BTUDHW*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
dhw15r<-tapply(r15r$BTUDHW*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
dhw15r[is.na(dhw15r)]<-0
dhw15u<-tapply(r15u$BTUDHW*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)

oth15<-tapply(r15$BTUOTH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
oth15r<-tapply(r15r$BTUOTH*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
oth15r[is.na(oth15r)]<-0
oth15u<-tapply(r15u$BTUOTH*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)

sphph15u<-sph15u/s15u
spcph15u<-spc15u/s15u
dhwph15u<-dhw15u/s15u
othph15u<-oth15u/s15u


# average energy per house, annual, in kBTU
eph90<-e90/s90
eph90r<-e90r/s90r
eph90u<-e90u/s90u

eph15<-e15/s15
eph15r<-e15r/s15r
eph15u<-e15u/s15u

cf1<-read.csv("Starts Counterfactual6.csv")
# diff80<-sum(cf1$Single.Family.Diff[1:5])
# diff90<-sum(cf1$Single.Family.Diff[6:15])
# diff00<-sum(cf1$Single.Family.Diff[16:25])
# diff10<-sum(cf1$Single.Family.Diff[26:30])

diffreg80<-sum(cf1$Single.Family.RegDiff[1:5])
diffreg90<-sum(cf1$Single.Family.RegDiff[6:15])
diffreg00<-sum(cf1$Single.Family.RegDiff[16:25])
diffreg10<-sum(cf1$Single.Family.RegDiff[26:30])
# define counterfactual urban housing stock in 2015, based on 65:35 split
# s15uhyp1<-s15u
s15uhyp2<-s15u
# 1980
# s15uhyp1[2:3,5]<-(sum(s15u[2:3,5])-(1000*diff80))*s15u[2:3,5]/sum(s15u[2:3,5])
# s15uhyp1[4:5,5]<-(sum(s15u[4:5,5])+(1000*diff80))*s15u[4:5,5]/sum(s15u[4:5,5])
# # 1990s
# s15uhyp1[2:3,6]<-(sum(s15u[2:3,6])-(1000*diff90))*s15u[2:3,6]/sum(s15u[2:3,6])
# s15uhyp1[4:5,6]<-(sum(s15u[4:5,6])+(1000*diff90))*s15u[4:5,6]/sum(s15u[4:5,6])
# # 2000s
# s15uhyp1[2:3,7]<-(sum(s15u[2:3,7])-(1000*diff00))*s15u[2:3,7]/sum(s15u[2:3,7])
# s15uhyp1[4:5,7]<-(sum(s15u[4:5,7])+(1000*diff00))*s15u[4:5,7]/sum(s15u[4:5,7])
# # 2010s
# s15uhyp1[2:3,8]<-(sum(s15u[2:3,8])-(1000*diff10))*s15u[2:3,8]/sum(s15u[2:3,8])
# s15uhyp1[4:5,8]<-(sum(s15u[4:5,8])+(1000*diff10))*s15u[4:5,8]/sum(s15u[4:5,8])

# regression based hypothetical stock
# 1980
s15uhyp2[2:3,5]<-(sum(s15u[2:3,5])-(1000*diffreg80))*s15u[2:3,5]/sum(s15u[2:3,5])
s15uhyp2[4:5,5]<-(sum(s15u[4:5,5])+(1000*diffreg80))*s15u[4:5,5]/sum(s15u[4:5,5])
# 1990s
s15uhyp2[2:3,6]<-(sum(s15u[2:3,6])-(1000*diffreg90))*s15u[2:3,6]/sum(s15u[2:3,6])
s15uhyp2[4:5,6]<-(sum(s15u[4:5,6])+(1000*diffreg90))*s15u[4:5,6]/sum(s15u[4:5,6])
# 2000s
s15uhyp2[2:3,7]<-(sum(s15u[2:3,7])-(1000*diffreg00))*s15u[2:3,7]/sum(s15u[2:3,7])
s15uhyp2[4:5,7]<-(sum(s15u[4:5,7])+(1000*diffreg00))*s15u[4:5,7]/sum(s15u[4:5,7])
# 2010s
s15uhyp2[2:3,8]<-(sum(s15u[2:3,8])-(1000*diffreg10))*s15u[2:3,8]/sum(s15u[2:3,8])
s15uhyp2[4:5,8]<-(sum(s15u[4:5,8])+(1000*diffreg10))*s15u[4:5,8]/sum(s15u[4:5,8])

# s15hyp1<-s15r+s15uhyp1
s15hyp2<-s15r+s15uhyp2

# s15u_diff<-s15uhyp1-s15u
s15u_diffreg<-s15uhyp2-s15u
# save(s15u,s15u_diff,s15u_diffreg,file="sdiff.RData")
save(s15u,s15u_diffreg,file="sdiff.RData")
# calculate counterfactual urban energy consumption
# e15uhyp1<-s15uhyp1*eph15u # based on 65:35 ratio
e15uhyp2<-s15uhyp2*eph15u # based on modeled housing starts
# counterfactual urban energy use by end-use
# sph15uhyp1<-s15uhyp1*sphph15u
# spc15uhyp1<-s15uhyp1*spcph15u
# dhw15uhyp1<-s15uhyp1*dhwph15u
# oth15uhyp1<-s15uhyp1*othph15u

sph15uhyp2<-s15uhyp2*sphph15u
spc15uhyp2<-s15uhyp2*spcph15u
dhw15uhyp2<-s15uhyp2*dhwph15u
oth15uhyp2<-s15uhyp2*othph15u

# save(sph15u,spc15u,dhw15u,oth15u,sph15uhyp1,spc15uhyp1,dhw15uhyp1,oth15uhyp1,sph15uhyp2,spc15uhyp2,dhw15uhyp2,oth15uhyp2,
#      sphph15u,spcph15u,dhwph15u,othph15u,eph15u,e15u,file="end_uses_CF.RData")
save(sph15u,spc15u,dhw15u,oth15u,sph15uhyp2,spc15uhyp2,dhw15uhyp2,oth15uhyp2,
     sphph15u,spcph15u,dhwph15u,othph15u,eph15u,e15u,file="end_uses_CF.RData")
# differece in urban energy use by end-use
# sphdiff1<-sph15u-sph15uhyp1
# spcdiff1<-spc15u-spc15uhyp1
# dhwdiff1<-dhw15u-dhw15uhyp1
# othdiff1<-oth15u-oth15uhyp1

sphdiff2<-sph15u-sph15uhyp2
spcdiff2<-spc15u-spc15uhyp2
dhwdiff2<-dhw15u-dhw15uhyp2
othdiff2<-oth15u-oth15uhyp2

# how much was the reduction in urban MF and SF energy since the 1990s?
# ratio_hyp1<-sum(e15uhyp1[2:5,6:8])/sum(e15u[2:5,6:8])
ratio_hyp2<-sum(e15uhyp2[2:5,6:8])/sum(e15u[2:5,6:8])
# what would total energy consumption have been?
# e15hyp1<-e15r+e15uhyp1
e15hyp2<-e15r+e15uhyp2
# energy consumption overall would just be 3% lower, but energy consumption in the affected housing types 
# (SF plus MF from 1986 onwards) would have been ~ 18% lower (20% 1990s onwards, 16% 1980s onwards).
# urban housing built in the 1990s and 2000s would have consumed 21% less energy
# tot_ratio_hyp1<-sum(e15hyp1)/sum(e15)
# urb_ratio_hyp1<-sum(e15uhyp1)/sum(e15u)
tot_ratio_hyp2<-sum(e15hyp2)/sum(e15)
urb_ratio_hyp2<-sum(e15uhyp2)/sum(e15u)

# ratio_cohort1<-colSums(e15uhyp1)/colSums(e15u)
ratio_cohort2<-colSums(e15uhyp2)/colSums(e15u)

# plot some figures of new stock and energy #########################3
# 2015 stock
s15u<-rbind(s15u,colSums(s15u))
ds<-as.data.frame(s15u[1:5,1:8]/sum(s15u[6,1:8]))
dis<-as.data.frame(100*as.numeric(unlist(ds)))
colnames(dis)<-"Percentage"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,20)+
  geom_col(aes(fill = Type), width = 0.7) +
  # theme_minimal() +
  labs(title = "a) Urban housing by Cohort and Type, 2015", y = "Portion (%)") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p

# 2015 stock, only from 1980s onwards, abs vales
s15u<-rbind(s15u,colSums(s15u))
ds<-as.data.frame(s15u[1:5,1:8])*1e-6
dis<-as.data.frame(as.numeric(unlist(ds)))
colnames(dis)<-"Units"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
dis$Period<-"<1980"
dis$Period[21:40]<-"Actual"

# hypothetical 2015 stock
s15uhyp2<-rbind(s15uhyp2,colSums(s15uhyp2))
ds2<-as.data.frame(s15uhyp2[1:5,1:8])*1e-6
dis2<-as.data.frame(as.numeric(unlist(ds2)))
colnames(dis2)<-"Percentage"
dis2$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis2$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

dis[41:60,]<-dis2[21:40,]
dis$Period[41:60]<-"No fed policy"
p <- ggplot(dis[21:60,], aes(x = Period, y = Units))+#ylim(0,20)+
  geom_col(aes(fill = Type), width = 0.5) +
  # theme_minimal() +
  labs(title = "a) Post-1980 Urban Type Mix, 2015", y = "Million Housing Units", x="Policy scenario") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p  +theme(legend.position = "none") 



# hypothetical 1 2015 stock
# s15uhyp1<-rbind(s15uhyp1,colSums(s15uhyp1))
# ds<-as.data.frame(s15uhyp1[1:5,1:8]/sum(s15uhyp1[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,20)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "65:35 growth counterfactual of urban housing, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p

# hypothetical 2 2015 stock
s15uhyp2<-rbind(s15uhyp2,colSums(s15uhyp2))
ds<-as.data.frame(s15uhyp2[1:5,1:8]/sum(s15uhyp2[6,1:8]))
dis<-as.data.frame(100*as.numeric(unlist(ds)))
colnames(dis)<-"Percentage"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,20)+
  geom_col(aes(fill = Type), width = 0.7) +
  # theme_minimal() +
  labs(title = "b) Policy counterfactual of urban housing, 2015", y = "Portion (%)") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p

# figure of  2015 urban energy
dse<-as.data.frame(e15u[1:5,1:8])*1.055e-9
dis<-as.data.frame(as.numeric(unlist(dse)))
colnames(dis)<-"Energy"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2000)+
  geom_col(aes(fill = Type), width = 0.7) +
  # theme_minimal() +
  labs(title = "Urban energy by Cohort and Type, 2015", y = "PJ") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p

# figure of hypothetical 1 2015 urban energy
# dse<-as.data.frame(e15uhyp1[1:5,1:8])*1.055e-9
# dis<-as.data.frame(as.numeric(unlist(dse)))
# colnames(dis)<-"Energy"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2000)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "65:35 growth counterfactual urban energy, 2015", y = "PJ") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p

# figure of hypothetical 2 2015 urban energy
dse<-as.data.frame(e15uhyp2[1:5,1:8])*1.055e-9
dis<-as.data.frame(as.numeric(unlist(dse)))
colnames(dis)<-"Energy"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2000)+
  geom_col(aes(fill = Type), width = 0.7) +
  # theme_minimal() +
  labs(title = "Policy counterfactual urban energy, 2015", y = "PJ") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p

# figure of hypothetical 1 2015 urban energy, 1980s onwards
# dse<-as.data.frame(e15uhyp1[1:5,5:8])*1.055e-9
# dis<-as.data.frame(as.numeric(unlist(dse)))
# colnames(dis)<-"Energy"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort))[5:8],each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),4)

# p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,1500)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "65:35 counterfactual of urban energy, 2015", y = "PJ") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p

# figure of hypothetical 2 2015 urban energy, 1980s onwards
dse<-as.data.frame(e15uhyp2[1:5,5:8])*1.055e-9
dis<-as.data.frame(as.numeric(unlist(dse)))
colnames(dis)<-"Energy"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort))[5:8],each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),4)

p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,1500)+
  geom_col(aes(fill = Type), width = 0.7) +
  # theme_minimal() +
  labs(title = "Policy counterfactual of urban energy, 2015", y = "PJ") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p

# figure of actual 2015 urban energy, 1980s onwards
dse<-as.data.frame(e15u[1:5,5:8])*1.055e-9
dis<-as.data.frame(as.numeric(unlist(dse)))
colnames(dis)<-"Energy"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort))[5:8],each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),4)

p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,1500)+
  geom_col(aes(fill = Type), width = 0.7) +
  # theme_minimal() +
  labs(title = "Urban energy by Cohort and Type, 2015", y = "PJ") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
scale_fill_hue(l=45)
windows()
p

# 
# # city type related counterfactuals ######################
# # load in city distributions
# # from ACS Table B25127 for 2015, 1yr estimate
# PX<-read_excel("City_distributions.xlsx",sheet = "Phoenix")
# LA<-read_excel("City_distributions.xlsx",sheet = "LA")
# NY<-read_excel("City_distributions.xlsx",sheet = "NY")
# Chicago<-read_excel("City_distributions.xlsx",sheet = "Chicago")
# Philadelphia<-read_excel("City_distributions.xlsx",sheet = "Philadelphia")
# Houston<-read_excel("City_distributions.xlsx",sheet = "Houston")
# 
# urbs<-sum(s15u[1:5,1:8])
# 
# s15PX<-urbs*PX
# e15PX<-s15PX*eph15u
# PXratio<-sum(e15PX)/sum(e15u)
# PXratioall<-sum(e15PX+e15r)/sum(e15)
# 
# s15LA<-urbs*LA
# e15LA<-s15LA*eph15u
# LAratio<-sum(e15LA)/sum(e15u)
# LAratioall<-sum(e15LA+e15r)/sum(e15)
# 
# s15NY<-urbs*NY
# e15NY<-s15NY*eph15u
# NYratio<-sum(e15NY)/sum(e15u)
# NYratioall<-sum(e15NY+e15r)/sum(e15)
# 
# s15Chicago<-urbs*Chicago
# e15Chicago<-s15Chicago*eph15u
# Chicagoratio<-sum(e15Chicago)/sum(e15u)
# Chicagoratioall<-sum(e15Chicago+e15r)/sum(e15)
# 
# s15Philadelphia<-urbs*Philadelphia
# e15Philadelphia<-s15Philadelphia*eph15u
# Philadelphiaratio<-sum(e15Philadelphia)/sum(e15u)
# Philadelphiaratioall<-sum(e15Philadelphia+e15r)/sum(e15)
# 
# s15Houston<-urbs*Houston
# e15Houston<-s15Houston*eph15u
# Houstonratio<-sum(e15Houston)/sum(e15u)
# Houstonratioall<-sum(e15Houston+e15r)/sum(e15)
# 
# HNY<-array(0,dim=c(5,8,61))
# # use a RAS technique to estimate a distribution with the MF portion of NY and the age of Houston
# HNY[,,1]<-as.numeric(unlist(Houston))
# NYtype<-rowSums(NY)
# HCo<-colSums(Houston)
# for (i in 0:29) {
# rs<-NYtype/rowSums(HNY[,,2*i+1])
# HNY[,,2*i+2]<-sweep(HNY[,,2*i+1],MARGIN = 1,FUN="*",STATS = rs)
# cs<-HCo/colSums(HNY[,,2*i+2])
# HNY[,,2*i+3]<-sweep(HNY[,,2*i+2],MARGIN = 2,FUN="*",STATS = cs)
# }
# s15HNY<-urbs*HNY[,,61]
# e15HNY<-s15HNY*eph15u
# HNYratio<-sum(e15HNY)/sum(e15u)
# HNYratioall<-sum(e15HNY+e15r)/sum(e15)
# 
# 
# ds<-as.data.frame(s15HNY[1:5,1:8]/sum(s15uhyp[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,25)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Houston-NY urban housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# ds<-as.data.frame(s15NY[1:5,1:8]/sum(s15uhyp[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,50)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "New York urban housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# ds<-as.data.frame(s15Houston[1:5,1:8]/sum(s15uhyp[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,25)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Houston urban housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 


# # floor area disribution 1990
# f90<-tapply(r90$NWEIGHT*r90$TOTSQFT,list(r90$TYPEHUQ,r90$AgeCohort),sum)
# # floor area distribution 2015
# f15<-tapply(r15$NWEIGHT*r15$TOTSQFT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
# # floor area distribution 2009
# f09<-tapply(r09$NWEIGHT*r09$TOTSQFT,list(r09$TYPEHUQ,r09$AgeCohort),sum)
# # energy 1990
# e90<-tapply(r90$TOTBTUnew*r90$NWEIGHT,list(r90$TYPEHUQ,r90$AgeCohort),sum)
# e90wa<-tapply(r90$TOTBTUnew_wa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$AgeCohort),sum)
# # energy 2015
# e15<-tapply(r15$TOTBTUnew*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
# e15wa<-tapply(r15$TOTBTUnew_wa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
# # average energy per house, annual, in kBTU
# eph90<-e90/s90
# eph15<-e15/s15
# eph90wa<-e90wa/s90
# eph15wa<-e15wa/s15
# write.csv(eph15,file="Scenarios/eph_2015.csv")
# write.csv(eph90,file="Scenarios/eph_1990.csv")
# # avg floor area per house, sqft
# fph90<-f90/s90
# fph15<-f15/s15
# write.csv(fph15,file="Scenarios/faph_2015.csv")
# write.csv(fph90,file="Scenarios/faph_1990.csv")
# # 09 confirms the trend of bigger houses in 1990-2000s from 2015 survey
# fph09<-f09/s09
# # household population 2015
# p15<-sum(r15$NHSLDMEM*r15$NWEIGHT)
# pop15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$AgeCohort),sum)
# hhs15<-pop15/s15[1:5,1:8]
# # make s90 and s15 compatible
# s90<-cbind(s90,matrix(0,5,2))
# colnames(s90)[7:8]<-c("2000s","2010s")
# s90[is.na(s90)]<-0
# s90<-rbind(s90,colSums(s90))
# s15<-rbind(s15,colSums(s15))
# # # Add cohort totals to stock distributions
# # s15<-cbind(s15,rowSums(s15))
# # colnames(s15)[9]<-"AllCohorts"
# # # housing stock characteristics and growth 1990-2015
# SFnew<-sum(s15[2:3,6:8])
# MFnew<-sum(s15[4:5,6:8])
# MHnew<-sum(s15[1,6:8])
# new<-sum(SFnew,MFnew,MHnew)
# avgFA<-(sum(f15)/sum(s15[6,]))/10.765
# avgFA_cap<-(sum(f15)/p15)/10.765
# # Scenario 1, new rates of cohort change ##############
# # cohort abs change, delstock
# ds<-s15[6,]-s90[6,]
# # cohort avg annual change, rate of del stock
# rds<-log(s15[6,]/s90[6,])/25
# rds[is.infinite(rds)]<-0
# # alter the rate of stock reduction for cohorts <1980
# decay_factor<-1.5
# # hypothetical decay/growth rates
# rds_hyp<-decay_factor*rds
# s15tot_hyp<-matrix(0,1,8)
# colnames(s15tot_hyp)<-colnames(s15)
# # pre 1980 houses decay faster
# s15tot_hyp[1:4]<-s90[6,1:4]*exp(25*rds_hyp[1:4])
# # 1980 houses stay the same
# s15tot_hyp[5]<-s15[6,5]
# # number of extra homes removed from the stock
# diff1<-sum(s15[6,1:4])-sum(s15tot_hyp[1:4])
# #extra houses built in 1990s
# h1990s<-sum(s90[6,1:4]*exp(10*rds_hyp[1:4]))-sum(s90[6,1:4]*exp(10*rds[1:4]))
# # extra houses builit in 2000s
# h2000s<-sum(s90[6,1:4]*exp(20*rds_hyp[1:4]))-sum(s90[6,1:4]*exp(20*rds[1:4]))-h1990s
# # extra houses built in 2010
# h2010s<-sum(s90[6,1:4]*exp(25*rds_hyp[1:4]))-sum(s90[6,1:4]*exp(25*rds[1:4])) - h1990s - h2000s
# 
# s15tot_hyp[6]<-s15[6,6]-h1990s
# s15tot_hyp[7]<-s15[6,7]-h2000s
# s15tot_hyp[8]<-s15[6,8]-h2010s
# # ratio of difference between actual 2015 stock and hyp 2015 stock
# r1<-s15tot_hyp/s15[6,]
# # hypothetical 2015 stock by type and age cohort
# s15_hyp<-sweep(s15,MARGIN = 2,FUN = '*',STATS = r1)
# s15_hyp1<-cbind(s15_hyp,rowSums(s15_hyp))
# # hypothetical energy consumption in 2015 with new stock distribution
# e15hyp<-s15_hyp[1:5,]*eph15
# pc_change_hyp1<-100*(sum(e15hyp)-sum(e15))/sum(e15)
# 
# # hyp 1 housing stock characteristics and growth 1990-2015
# SFnew1<-sum(s15_hyp[2:3,6:8])
# MFnew1<-sum(s15_hyp[4:5,6:8])
# MHnew1<-sum(s15_hyp[1,6:8])
# new1<-sum(SFnew1,MFnew1,MHnew1)
# f15_hyp1<-fph15*s15_hyp1[1:5,1:8]
# avgFA1<-(sum(f15_hyp1)/s15_hyp1[6,9])/10.765
# avgFA_cap1<-(sum(f15_hyp1)/p15)/10.765
# # hhs15_1<-pop15/s15_hyp[1:5,1:8] need to rethink this
# # Scenario 2, new rates of type change ############
# # 1990-2015 type split change from 3:1 to 59:41, in effect taking 5.6 million SF and making them MF
# # Add cohort totals to stock distributions
# s15<-cbind(s15,rowSums(s15))
# colnames(s15)[9]<-"AllCohorts"
# 
# s90<-cbind(s90,rowSums(s90))
# colnames(s90)[9]<-"AllCohorts"
# # type actual avg annual change, rate of del type
# rdt<-log(s15[,9]/s90[,9])/25
# # total growth of MF, SF housing of cohorts after 1980 between 1990 and 2015
# h_gr<-sum(s15[2:5,6:8])
# 
# 
# # define hypothetical growth of MF and SF so that change is equal to number of houses affected in hyp1
# # SF_gr<-MF_gr<-h_gr/2
# SF_gr_h2<-h_gr*0.589
# MF_gr_h2<-h_gr*0.411
# 
# # growth ratios by type wrt actual growth in stock of MF SF, 1990s-2010s houses b/w 1990-2015
# r_SF<-SF_gr_h2/sum(s15[2:3,6:8])
# r_MF<-MF_gr_h2/sum(s15[4:5,6:8])
# # define hypothetical stock for scenario 2
# s15_hyp2<-s15
# s15_hyp2[2:3,6:8]<-s15[2:3,6:8]*r_SF
# s15_hyp2[4:5,6:8]<-s15[4:5,6:8]*r_MF
# s15_hyp2[6,1:8]<-colSums(s15_hyp2[1:5,1:8])
# s15_hyp2[,9]<-rowSums(s15_hyp2[,1:8])
# # calculate hypothetical rates of change by type
# rdt_hyp<-rdt
# rdt_hyp[2]<-log(s15_hyp2[2,9]/s90[2,9])/25
# rdt_hyp[3]<-log(s15_hyp2[3,9]/s90[3,9])/25
# rdt_hyp[4]<-log(s15_hyp2[4,9]/s90[4,9])/25
# rdt_hyp[5]<-log(s15_hyp2[5,9]/s90[5,9])/25
# 
# # hyp 2 housing stock characteristics and growth 1990-2015
# SFnew2<-sum(s15_hyp2[2:3,6:8])
# MFnew2<-sum(s15_hyp2[4:5,6:8])
# MHnew2<-sum(s15_hyp2[1,6:8])
# new2<-sum(SFnew2,MFnew2,MHnew2)
# f15_hyp2<-fph15*s15_hyp2[1:5,1:8]
# avgFA2<-(sum(f15_hyp2)/s15_hyp2[6,9])/10.765
# avgFA_cap2<-(sum(f15_hyp2)/p15)/10.765
# 
# # hypothetical energy consumption in 2015 with new stock distribution
# e15hyp2<-s15_hyp2[1:5,1:8]*eph15
# pc_change_hyp2<-100*(sum(e15hyp2)-sum(e15))/sum(e15)
# diff2<-sum(s15_hyp2[2:3,6:8])-sum(s15[2:3,6:8])
# 
# # a third counterfactual would require more homes to be built after 1990, making up for 
# # higher demolition rates. In addition, the total new homes would have a higher proportion of
# # MF than the actual trend from 1990-2015. So, lets keep the ~5.67 million extra homes that are built post
# # 1990 to make up for additional demolitions, keeping the column sums constant by cohort, and in each column, 
# # increase the mix of MF, so that the total new homes built 1990-2015 are 41% MF homes.
# 
# 
# ## figure out CF 3
# # how many SF and MF houses in 2015 stock were built since 1990s? 34.4 mill
# h_gr
# # how many total houses in 2015 stock were built since 1990s? 37.6 mill
# sum(s15[1:5,6:8])
# # how many total house in 2015 stock built since 1990s in Hyp 1? 43.3 mill
# sum(s15_hyp[1:5,6:8])
# # of SF and MF houses built since 1990 in Hyp 1, how many are SF/MF? 39.6 mill total. 75% SF, 25% MF
# h_gr_h1<-sum(s15_hyp[2:5,6:8])
# SF_grr_h1<-sum(s15_hyp[2:3,6:8])/h_gr_h1
# MF_grr_h1<-sum(s15_hyp[4:5,6:8])/h_gr_h1
# # define the shares so that the total number of extra MF / less SF is equal to the the extra houses demolished with the higher decay rates
# SF_gr_h3<-h_gr_h1*0.61
# MF_gr_h3<-h_gr_h1*0.39
# diff3<-sum(s15_hyp[2:3,6:8])-SF_gr_h3
# r_SF3<-SF_gr_h3/sum(s15[2:3,6:8])
# r_MF3<-MF_gr_h3/sum(s15[4:5,6:8])
# # define hypothetical stock for scenario 2
# s15_hyp3<-cbind(s15_hyp,rep(0,6,6,1))
# s15_hyp3[2:3,6:8]<-s15[2:3,6:8]*r_SF3
# s15_hyp3[4:5,6:8]<-s15[4:5,6:8]*r_MF3
# s15_hyp3[6,1:8]<-colSums(s15_hyp3[1:5,1:8])
# s15_hyp3[,9]<-rowSums(s15_hyp3[,1:8])
# # calculate the difference in hyp 3
# e15hyp3<-s15_hyp3[1:5,1:8]*eph15
# pc_change_hyp3<-100*(sum(e15hyp3)-sum(e15))/sum(e15)
# # housing characteristics
# SFnew3<-sum(s15_hyp3[2:3,6:8])
# MFnew3<-sum(s15_hyp3[4:5,6:8])
# MHnew3<-sum(s15_hyp3[1,6:8])
# new3<-sum(SFnew3,MFnew3,MHnew3)
# f15_hyp3<-fph15*s15_hyp3[1:5,1:8]
# avgFA3<-(sum(f15_hyp3)/s15_hyp3[6,9])/10.765
# avgFA_cap3<-(sum(f15_hyp3)/p15)/10.765
# 
# # figures #########
# # figure of actual 2015 stock
# 
# # ds<-as.data.frame(tapply(r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)/sum(r15$NWEIGHT))
# ds<-as.data.frame(s15[1:5,1:8]/sum(s15[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,18)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Residential housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# # figure of hypothetical 2015 stock
# ds2<-as.data.frame(s15_hyp[1:5,1:8]/sum(s15_hyp[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds2)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,18)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Scenario 1 housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# 
# # figure of hypothetical2 2015 stock
# ds3<-as.data.frame(s15_hyp2[1:5,1:8]/sum(s15_hyp2[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds3)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,18)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Scenario 2 housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# # figure of hypothetical3 2015 stock
# ds4<-as.data.frame(s15_hyp3[1:5,1:8]/sum(s15_hyp3[6,1:8]))
# dis<-as.data.frame(100*as.numeric(unlist(ds4)))
# colnames(dis)<-"Percentage"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Percentage))+ylim(0,18)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Scenario 3 housing by Cohort and Type, 2015", y = "Portion (%)") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# 
# 
# 
# # figure of  2015 energy
# dse<-as.data.frame(e15[1:5,1:8])*1.055e-9
# dis<-as.data.frame(as.numeric(unlist(dse)))
# colnames(dis)<-"Energy"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2500)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Energy consumption by Cohort and Type, 2015", y = "PJ") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# # figure of hypo 2015 energy
# dse2<-as.data.frame(e15hyp[1:5,1:8])*1.055e-9
# dis<-as.data.frame(as.numeric(unlist(dse2)))
# colnames(dis)<-"Energy"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2500)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Scenario 1 Energy consumption by Cohort and Type, 2015", y = "PJ") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# # figure of hypo2 2015 energy
# dse3<-as.data.frame(e15hyp2[1:5,1:8])*1.055e-9
# dis<-as.data.frame(as.numeric(unlist(dse3)))
# colnames(dis)<-"Energy"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2500)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Scenario 2 Energy consumption by Cohort and Type, 2015", y = "PJ") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# 
# # figure of hypo3 2015 energy
# dse4<-as.data.frame(e15hyp3[1:5,1:8])*1.055e-9
# dis<-as.data.frame(as.numeric(unlist(dse4)))
# colnames(dis)<-"Energy"
# dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
# dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)
# 
# p <- ggplot(dis, aes(x = Cohort, y = Energy))+ylim(0,2500)+
#   geom_col(aes(fill = Type), width = 0.7) +
#   # theme_minimal() +
#   labs(title = "Scenario 3 Energy consumption by Cohort and Type, 2015", y = "PJ") +
#   theme(axis.text=element_text(size=10.5),
#         axis.title=element_text(size=12,face = "bold"),
#         plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
# scale_fill_hue(l=45)
# windows()
# p
# # summary results ###############
# tote15<-sum(e15)*1.055e-9
# tote15hpy1<-sum(e15hyp)*1.055e-9
# tote15hpy2<-sum(e15hyp2)*1.055e-9
# tote15hpy3<-sum(e15hyp3)*1.055e-9
# # calculations for ResStock ##########
# # try to find out how the cohort distributions for SFD change for CF2 wrt original
# sfd_ratio_cf2<-(s15_hyp2[2,1:8]/s15_hyp2[2,9])/(s15[2,1:8]/s15[2,9]) # sfd stock becomes older
