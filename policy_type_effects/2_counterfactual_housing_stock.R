# script to estimate counterfactual US housing energy consumption based on RECS data and counterfactual type distributions
# Peter Berrill 
# Last updated May 1 2020
# set up ################
rm(list=ls()) # clear workspace
cat("\014") # clear console
graphics.off() # remove graphics windows
# packages
library(dplyr)
library(ggplot2)
library(abind)
library(reshape2)
library(magrittr)
library(readxl)
# Read in RECS data for all RECS years since 1990, adjusted to include space heating from biomass and other non-counted fuels (coal, solar, district steam heat)
load("RECSincOth.RData") 
RECS$TOTBTU<-RECS$BTUSPH+RECS$BTUCOL+RECS$BTUDHW+RECS$BTUOTH
r<-RECS
r$count<-1
# RECS data for 1990, 2009, 2015
r90<-filter(r,r$RECSYEAR==1990) 
r15<-filter(r,r$RECSYEAR==2015)
r09<-filter(r,r$RECSYEAR==2009)
# RECS data for 1990, 2009, 2015, urban households only (includes suburban)
r90u<-filter(r,r$RECSYEAR==1990,r$URBAN==1)
r15u<-filter(r,r$RECSYEAR==2015,r$URBAN==1)
r09u<-filter(r,r$RECSYEAR==2009,r$URBAN==1)
# RECS data for 1990, 2009, 2015, non-urban households only
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
e90<-tapply(r90$TOTBTU*r90$NWEIGHT,list(r90$TYPEHUQ,r90$AgeCohort),sum)
e90r<-tapply(r90r$TOTBTU*r90r$NWEIGHT,list(r90r$TYPEHUQ,r90r$AgeCohort),sum)
e90u<-tapply(r90u$TOTBTU*r90u$NWEIGHT,list(r90u$TYPEHUQ,r90u$AgeCohort),sum)
# energy 2015
e15<-tapply(r15$TOTBTU*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
e15r<-tapply(r15r$TOTBTU*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
e15r[is.na(e15r)]<-0
e15u<-tapply(r15u$TOTBTU*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)
# space heat 2015
sph15<-tapply(r15$BTUSPH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
sph15r<-tapply(r15r$BTUSPH*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
sph15r[is.na(sph15r)]<-0
sph15u<-tapply(r15u$BTUSPH*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)
# space cooling 2015
spc15<-tapply(r15$BTUCOL*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
spc15r<-tapply(r15r$BTUCOL*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
spc15r[is.na(spc15r)]<-0
spc15u<-tapply(r15u$BTUCOL*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)
# hot water 2015
dhw15<-tapply(r15$BTUDHW*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
dhw15r<-tapply(r15r$BTUDHW*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
dhw15r[is.na(dhw15r)]<-0
dhw15u<-tapply(r15u$BTUDHW*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)
# other end-uses 2015
oth15<-tapply(r15$BTUOTH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$AgeCohort),sum)
oth15r<-tapply(r15r$BTUOTH*r15r$NWEIGHT,list(r15r$TYPEHUQ,r15r$AgeCohort),sum)
oth15r[is.na(oth15r)]<-0
oth15u<-tapply(r15u$BTUOTH*r15u$NWEIGHT,list(r15u$TYPEHUQ,r15u$AgeCohort),sum)
# end-use per house
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

cf1<-read.csv("StartsCounterfactual.csv")
diffreg80<-sum(cf1$SingleFamilyDiff[1:4])
diffreg90<-sum(cf1$SingleFamilyDiff[5:14])
diffreg00<-sum(cf1$SingleFamilyDiff[15:24])
diffreg10<-sum(cf1$SingleFamilyDiff[25:29])

# regression based hypothetical stock
s15uhyp2<-s15u
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

s15hyp2<-s15r+s15uhyp2
s15u_diffreg<-s15uhyp2-s15u
save(s15u,s15u_diffreg,file="sdiff.RData")

# calculate counterfactual urban energy consumption
e15uhyp2<-s15uhyp2*eph15u # based on modeled housing starts
# counterfactual urban energy use by end-use, assuming fixed energy intensity of houses by type and cohort
sph15uhyp2<-s15uhyp2*sphph15u
spc15uhyp2<-s15uhyp2*spcph15u
dhw15uhyp2<-s15uhyp2*dhwph15u
oth15uhyp2<-s15uhyp2*othph15u

save(sph15u,spc15u,dhw15u,oth15u,sph15uhyp2,spc15uhyp2,dhw15uhyp2,oth15uhyp2,
     sphph15u,spcph15u,dhwph15u,othph15u,eph15u,e15u,file="end_uses_CF.RData")

# plot some figures of new stock and energy #########################
# 2015 stock
s15u<-rbind(s15u,colSums(s15u))
ds<-as.data.frame(s15u[1:5,1:8]/sum(s15u[6,1:8]))
dis<-as.data.frame(100*as.numeric(unlist(ds)))
colnames(dis)<-"Percentage"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

# Supplementary Figure 4 a)
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

# Figure 1 a)
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

# hypothetical 2 2015 stock
s15uhyp2<-rbind(s15uhyp2,colSums(s15uhyp2))
ds<-as.data.frame(s15uhyp2[1:5,1:8]/sum(s15uhyp2[6,1:8]))
dis<-as.data.frame(100*as.numeric(unlist(ds)))
colnames(dis)<-"Percentage"
dis$Cohort<-rep(levels(as.factor(r$AgeCohort)),each=5)
dis$Type<-rep(c("Manuf. Hous","Sing-Fam Det","Sing-Fam Att","Mul-Fam Low","Mul-Fam High"),8)

# Supplementary Figure 4 b)
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
