# code to regress urban energy consumption by household and division
# Peter Berrill 12/5/2019
# get data #############
rm(list=ls()) # clear workspace
cat("\014") # clear console
graphics.off() # remove graphics windows
# packages
library(plyr)
library(dplyr)
library(nlme)
library(ggplot2)
library(sjPlot)
library(plm)
library(reshape2)
load("RECSurbanRegDataNew.Rdata")
load("sdiff.Rdata")
# make adaptations to sdiff files to fit with new typologies
s15u_diffreg<-s15u_diffreg*0.959 # reflect the difference between starts and completions
s15u_diffreg[,2]<-s15u_diffreg[,2]+s15u_diffreg[,3]
s15u_diffreg[,7]<-s15u_diffreg[,7]+s15u_diffreg[,8]
s15u_diffreg<-s15u_diffreg[,-c(3,8)]
colnames(s15u_diffreg)[2]<-"1950-60s"
colnames(s15u_diffreg)[6]<-"2000+"

s15u[,2]<-s15u[,2]+s15u[,3]
s15u[,7]<-s15u[,7]+s15u[,8]
s15u<-s15u[,-c(3,8)]
colnames(s15u)[2]<-"1950-60s"
colnames(s15u)[6]<-"2000+"

load("GHGI_eu_ty.RData") # GHG intensities in kBTU
ghgi_avg<-ghgi_avg/1.055 # convert ghg intensities for end-uses from kg/kBTU to kg/MJ, (and remove value for MH)
ghgi_sph<-ghgi_sph/1.055
ghgi_spc<-ghgi_spc/1.055
ghgi_dhw<-ghgi_dhw/1.055
ghgi_oth<-ghgi_oth/1.055
ghgi_eu<-bind_rows(ghgi_sph,ghgi_spc,ghgi_dhw,ghgi_oth)

ru15<-ru2[ru2$RECSYEAR=="2015",]
rh15<-rh[rh$RECSYEAR=="2015",]
rusmall<-ru2[,c("BTUSPH","NWEIGHT","TYPEHUQ","Cohort","AgeCohort","HDD65","NHSLDMEM","TOTHSQFT","TOTCSQFT","INC","FUELHEAT","RECSYEAR","HEATPRICE","CDD65",
                "BTUCOL","BTUDHW","BTUOTH","TOTSQFT","ACROOMS","TOTROOMS","NCOMBATH","WHEATSIZ","FUELH2O","POOL","RECBATH","WHEATAGE","EQUIPAGE","EQUIPM","KOWNRENT")]
# create variable TC which defines combinations of house types and cohort
rusmall$TC<-as.factor(paste(rusmall$TYPEHUQ,rusmall$Cohort))

rus15<-rusmall[rusmall$RECSYEAR=="2015",]
rus15<-rus15[!rus15$TYPEHUQ=="Man Housing",]
rus15$TYPEHUQ<-droplevels(rus15$TYPEHUQ)
rus15$TC<-droplevels(rus15$TC)

rus15$SPHFUEL<-as.character(rus15$FUELHEAT)
rus15[rus15$FUELHEAT=='LPG' | rus15$FUELHEAT=='NGAS',]$SPHFUEL<-"Gas"
rus15[rus15$FUELHEAT=='WOOD',]$SPHFUEL<-"Other/None"
rus15[rus15$FUELHEAT=='Other',]$SPHFUEL<-"Other/None"
rus15[rus15$FUELHEAT=='None',]$SPHFUEL<-"Other/None"
rus15[rus15$EQUIPM==4&rus15$FUELHEAT=='ELEC',]$SPHFUEL<-"Elec HP"
rus15[rus15$SPHFUEL=='ELEC',]$SPHFUEL<-"Elec Oth"
rus15[rus15$FUELHEAT=='OIL',]$SPHFUEL<-"Oil"

rus15$INC<-0.001*rus15$INC/0.5672 # reconvert income data from 1990 dollars to 2015 dollars
rus15$BTUSPH<-rus15$BTUSPH*1.055 # convert kBTU to MJ
rus15$BTUCOL<-rus15$BTUCOL*1.055 # convert kBTU to MJ
rus15$BTUDHW<-rus15$BTUDHW*1.055 # convert kBTU to MJ
rus15$BTUOTH<-rus15$BTUOTH*1.055 # convert kBTU to MJ
rus15$BTUTOT<-rus15$BTUSPH+rus15$BTUCOL+rus15$BTUDHW+rus15$BTUOTH
rh15$BTUTOT<-rh15$BTUSPH+rh15$BTUCOL+rh15$BTUDHW+rh15$BTUOTH

rus15$TYPE<-"MF"
rus15[rus15$TYPEHUQ=="SF Det" | rus15$TYPEHUQ=="SF Att",]$TYPE<-"SF"
rh15$TYPE<-"MF"
rh15[rh15$TYPEHUQ=="SF Det" | rh15$TYPEHUQ=="SF Att",]$TYPE<-"SF"
rh15[rh15$TYPEHUQ=="Man Housing",]$TYPE<-"MH"
rh15<-rh15[!rh15$TYPEHUQ=="Man Housing",]
rus15$count<-1
rus15$OldNew<-"Old"
rus15[rus15$AgeCohort=="1970s" | rus15$AgeCohort=="1980s" | rus15$AgeCohort=="1990s" | rus15$AgeCohort=="2000s" | rus15$AgeCohort=="2010s",]$OldNew<-"New"
rus15$OldNew<-as.factor(rus15$OldNew)
rus15<-within(rus15,TC<-relevel(TC, ref = "MF high 2000+")) # make high-density multifamily houses built in 2010s as the reference level for TC

eph15<-tapply(rus15$BTUTOT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT,rus15$TYPE,sum)
eph15[2]/eph15[1]

eph15<-tapply(rh15$BTUTOT*rh15$NWEIGHT,rh15$TYPE,sum)/tapply(rh15$NWEIGHT,rh15$TYPE,sum)
eph15[2]/eph15[1]

epp15<-tapply(rus15$BTUTOT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT*rus15$NHSLDMEM,rus15$TYPE,sum)
epp15[2]/epp15[1]

epp15<-tapply(rh15$BTUTOT*rh15$NWEIGHT,rh15$TYPE,sum)/tapply(rh15$NWEIGHT*rh15$NHSLDMEM,rh15$TYPE,sum)
epp15[2]/epp15[1]

faph15<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT,rus15$TYPE,sum)
faph15[2]/faph15[1]

faph15<-tapply(rh15$TOTSQFT*rh15$NWEIGHT,rh15$TYPE,sum)/tapply(rh15$NWEIGHT,rh15$TYPE,sum)
faph15[2]/faph15[1]

rus15$IncGroup="LowInc"
rus15[rus15$INC>30 &rus15$INC<100,]$IncGroup<-"MidInc"
rus15[rus15$INC>90,]$IncGroup<-"HiInc"

faphi15<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup),sum)
faphic15<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup,rus15$OldNew),sum)
hfaphic15<-tapply(rus15$TOTHSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup,rus15$OldNew),sum)
cfaphic15<-tapply(rus15$TOTCSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$IncGroup,rus15$OldNew),sum)
# heated, cooled, and total average floor area by type and old/new, with new referring to post-1969 houses
hfaphc15<-tapply(rus15$TOTHSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
cfaphc15<-tapply(rus15$TOTCSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
tfaphc15<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)

#MF/SF share rural
rr15<-rh15[rh15$URBAN==0,]
a<-tapply(rr15$NWEIGHT,list(rr15$TYPE,rr15$AgeCohort),sum)
# MF/SF share urban
b<-tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)
sfs<-as.data.frame(levels(rus15$AgeCohort))
sfs$SFshare_rural<-a[2,]/colSums(a)
sfs$SFshare_urban<-b[2,]/colSums(b)
windows()
plot(sfs$SFshare_rural*100,xaxt="n",ylim = c(60,100),pch=15,col="purple",main = "Rural and Urban single-family share of non-MH housing, 2015",ylab = "Single-family share (%)",xlab = "")
axis(1,at=1:8, labels = sfs$AgeCohort)
lines(sfs$SFshare_urban*100,type = "p",pch=17,col="darkgreen")
legend(6,95,legend = c("Rural","Urban"),pch=c(15,17),col=c("purple","darkgreen"))

rus15$HHSGroup<-"1-2"
rus15[rus15$NHSLDMEM==3,]$HHSGroup<-"3"
rus15[rus15$NHSLDMEM>3,]$HHSGroup<-"4+"

# create variable TCI which defines combinations of house types and cohort, and household income
rus15$TCI<-as.factor(paste(rus15$TYPEHUQ,rus15$Cohort,rus15$IncGroup))
rus15<-within(rus15,TCI<-relevel(TCI, ref = "MF high 2000+ HiInc")) # make high-density multifamily houses built in 2010s as the reference level for TC

# show breakdown of households by hhs and income group
THI<-tapply(rus15$NWEIGHT,list(rus15$IncGroup,rus15$HHSGroup,rus15$TYPE),sum)
thi<-melt(THI)
colnames(thi)<-c("Income","HHSize","Type","Households")
thi$IncHHS<-as.factor(paste(thi$Income,thi$HHSize,sep="_"))
thi$or<- c(7,1,4,8,2,5,9,3,6)

p <- ggplot(thi, aes(x = reorder(IncHHS,or), y = 1e-6*Households))+#ylim(0,20)+
  geom_col(aes(fill = Type), width = 0.75) +
  labs(title = "Urban Housing Type Mix by Income and Household Size, 2015",y="Households (mill)",x="Income and HH Size") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
windows()
p
p<-ggplot(thi,aes(fill=Type,x = reorder(IncHHS,or), y = Households)) + 
  geom_bar(position="fill", stat="identity",width = 0.75) +
  labs(title = "Urban Housing Type Mix by Income and Household Size, 2015",y="Percentage of househoulds",x="Income and HH Size") + scale_y_continuous(labels=scales::percent) + 
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
windows()
p 

# show breakdown of equipment age for space/water heating by house type
wht<-tapply(rus15$NWEIGHT,list(rus15$WHEATAGE,rus15$TYPE),sum)[1:5,]
wht<-rbind(wht,rep(0,2))

wht<-wht[1:5,] # remove NA/Don't KNow

w<-melt(wht)
colnames(w)<-c("Age","Type","Value")

p<-ggplot(w,aes(fill=Age,x =Type, y = Value)) + 
  geom_bar(position="fill", stat="identity",width = 0.75) +
  labs(title = "Age distribution of water heating equipment, 2015",y="Percentage of households",x="Type") + scale_y_continuous(labels=scales::percent) +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
windows()
p 

sht<-tapply(rus15$NWEIGHT,list(rus15$EQUIPAGE,rus15$TYPE),sum)
sht<-sht[2:6,] # remove NA/Don't KNow

s<-melt(sht)
colnames(s)<-c("Age","Type","Value")

p<-ggplot(s,aes(fill=Age,x =Type, y = Value)) + 
  geom_bar(position="fill", stat="identity",width = 0.75) +
  labs(title = "Age distribution of space heating equipment, 2015",y="Percentage of households",x="Type") + scale_y_continuous(labels=scales::percent) +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
windows()
p
# show breakdown of heating fuels by house type
HF<-tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$SPHFUEL),sum)
HF<-HF[,1:4] # remove Other/None

hf<-melt(HF)
colnames(hf)<-c("Type","Fuel","Households")

p<-ggplot(hf,aes(fill=Fuel,x =Type, y = Households)) + 
  geom_bar(position="fill", stat="identity",width = 0.75) +
  labs(title = "Fuel type distribution of space heating equipment, 2015",y="Percentage of households",x="Type") +scale_y_continuous(labels=scales::percent) +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2")
windows()
p

# show breakdown of water heating fuels by house type
rus15$DHWFUEL<-"Other/None"
rus15[rus15$FUELH2O=="NGAS" | rus15$FUELH2O=="LPG",]$DHWFUEL<-"Gas"
rus15[rus15$FUELH2O=="OIL",]$DHWFUEL<-"Oil"
rus15[rus15$FUELH2O=="ELEC",]$DHWFUEL<-"Elec"

WHF<-tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$DHWFUEL),sum)
WHF<-WHF[,1:3]

whf<-melt(WHF)
colnames(whf)<-c("Type","Fuel","Households")

pal2<-c("#D95F02", "#7570B3", "#E7298A") # 2nd to 4th colors of dark2 palette, https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
p<-ggplot(whf,aes(fill=Fuel,x =Type, y = Households)) + 
  geom_bar(position="fill", stat="identity",width = 0.75) +
  labs(title = "Fuel type distribution of water heating equipment, 2015",y="Percentage of households",x="Type") +scale_y_continuous(labels=scales::percent) +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_manual(values=pal2)
windows()
p

rus15$OWNED<-0
rus15[rus15$KOWNRENT==1,]$OWNED<-1

# check characteristics of heating system, age, and home age b/w owned and rented homes
a<-tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OWNED,rus15$SPHFUEL),sum)
#rental sf more likely to have elec/no heating, owned more likely to have gas, oil\
#a[2,2,]/sum(b[2,2,])
b<-tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OWNED,rus15$EQUIPAGE),sum)
# rental sf more likely to have younger heating equip
# b[2,2,]/sum(b[2,2,])
c<-tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OWNED,rus15$Cohort),sum)

# AC saturation b/w sf and mf
rh15$Cooled<-1
rh15[rh15$ACROOMS==0,]$Cooled<-0
mfAC<-tapply(rh15$NWEIGHT,list(rh15$TYPE,rh15$Cooled),sum)[1,]/tapply(rh15$NWEIGHT,list(rh15$TYPE),sum)[1]
sfAC<-tapply(rh15$NWEIGHT,list(rh15$TYPE,rh15$Cooled),sum)[2,]/tapply(rh15$NWEIGHT,list(rh15$TYPE),sum)[2]
# some graph of these?
mfACDiv<- tapply(rh15$NWEIGHT,list(rh15$TYPE,rh15$DIVISION,rh15$Cooled),sum)[1,,]/tapply(rh15$NWEIGHT,list(rh15$TYPE,rh15$DIVISION),sum)[1,]
sfACDiv<- tapply(rh15$NWEIGHT,list(rh15$TYPE,rh15$DIVISION,rh15$Cooled),sum)[2,,]/tapply(rh15$NWEIGHT,list(rh15$TYPE,rh15$DIVISION),sum)[2,]
CDD_Div<-tapply(rh15$NWEIGHT*rh15$CDD65,rh15$DIVISION,sum)/tapply(rh15$NWEIGHT,rh15$DIVISION,sum)
or<-as.numeric(names(sort(CDD_Div)))


ac<-data.frame(Division = c("New England","Mid Atlantic","E-N Central","W-N Central","South Atlantic","E-S Central","W-S Central","Mountain","Pacific"),
               MF=mfACDiv[,2],SF=sfACDiv[,2])
ac<-melt(ac)
names(ac)<-c("Division","Type","Adoption")
ac$order<-c(1,2,3,5,8,7,9,6,4)
ac$Adoption<-100*ac$Adoption
p<-ggplot(ac,aes(x=reorder(Division,order),y=Adoption,shape=Type,color=Type))+geom_point(size=4)+ ylim(41,99)+
  labs(title = "b) Access to space cooling, by Division and house type, 2015", y = "Percentage of homes owning AC equipment (%)",x="Census Division, in order of increasing CDD") + theme_bw() +
  theme(text = element_text(size=13),axis.text = element_text(size = 13),title=element_text(size=14),legend.position=c(.9,.5)) + scale_color_brewer(palette="Dark2")
windows() 
p

# graph of average total floor area in new homes by income groups and type
fa_it<-melt(tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$OldNew,rus15$TYPE,rus15$IncGroup),sum)[1,,]/tapply(rus15$NWEIGHT,list(rus15$OldNew,rus15$TYPE,rus15$IncGroup),sum)[1,,])
names(fa_it)<-c("Type","IncomeGroup","FloorArea")
levels(fa_it$IncomeGroup)<-c(levels(fa_it$IncomeGroup),"Avg")
fa_it[7,]<-c("MF","Avg",tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$OldNew,rus15$TYPE),sum)[1,1]/tapply(rus15$NWEIGHT,list(rus15$OldNew,rus15$TYPE),sum)[1,1])
fa_it[8,]<-c("SF","Avg",tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$OldNew,rus15$TYPE),sum)[1,2]/tapply(rus15$NWEIGHT,list(rus15$OldNew,rus15$TYPE),sum)[1,2])
levels(fa_it$Type)<-c(levels(fa_it$Type),"SF-30%","SF-50%")
fa_it[13:16,]<-fa_it[9:12,]<-fa_it[fa_it$Type=="SF",]
fa_it$FloorArea<-as.numeric(fa_it$FloorArea)
fa_it[9:12,]$FloorArea<-fa_it[9:12,]$FloorArea-0.3*fa_it[fa_it$Type=="SF" & fa_it$IncomeGroup=="Avg",]$FloorArea[1]
fa_it[13:16,]$FloorArea<-fa_it[13:16,]$FloorArea-0.5*fa_it[fa_it$Type=="SF" & fa_it$IncomeGroup=="Avg",]$FloorArea[1]
fa_it[9:12,]$Type<-"SF-30%"
fa_it[13:16,]$Type<-"SF-50%"
fa_it$FloorArea<-fa_it$FloorArea/10.765

fa_it$IncomeGroup<-factor(fa_it$IncomeGroup,levels = c("LowInc","MidInc","HiInc","Avg"))
p<-ggplot(fa_it,aes(x=IncomeGroup,y=FloorArea,shape=Type,color=Type))+geom_point(size=3)+ylim(0,350) + 
  labs(title = "Average Floor Area and CF Reductions by Income Group", y = "Average Floor Area (m2)",x="Income Group") + theme_bw() +
  theme(text = element_text(size=13),axis.text = element_text(size = 13)) + scale_color_brewer(palette="Dark2")
windows() 
p

# heating models ###############
hh_heat<-lme(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + RECSYEAR, data = rusmall, random = ~  1 | TC)
hh_heat15<-lme(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + TC, rus15)
hh_heat15_feOR<-lm(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + TC + OWNED, rus15)
hh_heat15_feint<-lm(BTUSPH~HDD65 + TOTHSQFT + INC * NHSLDMEM + TC, rus15)

hh_heat15_fe4<-lm(BTUSPH~HDD65 + TOTHSQFT + NHSLDMEM +  TCI, rus15)

tab_model(hh_heat15_fe4,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTHSQFT","INC","NHSLDMEM"))

yhat_fe<-hh_heat15_fe[["fitted.values"]]
tab_model(hh_heat15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTHSQFT","INC","NHSLDMEM"))
heat_TC<-hh_heat15_fe[["coefficients"]][6:28]

heat_TCI<-hh_heat15_fe4[["coefficients"]][5:75]
# hausman test ###############################
fe1<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM,data = rus15,model = "within",index = c("TC")) # need to include index, but doesn't come out in model results
re1<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM,data = rus15,model = "random",index = c("TC"))
phtest(fe1,re1)
# Hausman test rejects the hypothesis that the random effects model is consistent, and therefore we prefer the fixed effects model

# calculate counterfactual heating energy ###################################
sphFE15<-matrix(c(heat_TC[1:5],0,heat_TC[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
sphp<-summary(hh_heat15_fe)[["coefficients"]][6:28,4] # extract p values
sphpmat<-matrix(c(sphp[1:5],0,sphp[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

row.names(sphFE15)<-c("MF high","MF low","SF Att","SF Det")
row.names(sphpmat)<-c("MF high","MF low","SF Att","SF Det")
t2<-matrix(1:24,4,6)
row.names(t2)<-c("SF Det","SF Att","MF low","MF high")
# reorder based on t2 row order
sphFE15<-sphFE15[rownames(t2),,drop=FALSE]
sphpmat<-sphpmat[rownames(t2),,drop=FALSE]

colnames(sphFE15)<-colnames(s15u_diffreg)
colnames(sphpmat)<-colnames(s15u_diffreg)
hh_heat_FE<-melt(sphFE15)
hh_sph_p<-melt(sphpmat)
hh_heat_FE$p<-hh_sph_p$value
colnames(hh_heat_FE)<-c("TYPEHUQ","Cohort","Offset","p")
hh_heat_FE$TC<-as.factor(paste(hh_heat_FE$TYPEHUQ,hh_heat_FE$Cohort))
x <-hh_heat_FE[order(hh_heat_FE$TYPEHUQ),]
x$Offset<-x$Offset #-sphFEoff # remove the offset added earlier, for displaying the TC effects unaltered, not needed to adjust here anymore
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x1<-x
x1$Enduse<-"Space Heat"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, space heating",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, first Low Income ######################
sphFE15LI<-matrix( heat_TCI[ c(seq(2,14,3),seq(16,70,3))],4,6,byrow = TRUE)
sphpLI<-summary(hh_heat15_fe4)[["coefficients"]][c(seq(6,18,3),seq(20,74,3)),4] # extract p values
sphpLImat<-matrix(sphpLI,4,6,byrow = TRUE) 

row.names(sphFE15LI)<-c("MF high","MF low","SF Att","SF Det")
row.names(sphpLImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
sphFE15LI<-sphFE15LI[rownames(t2),,drop=FALSE]
sphpLImat<-sphpLImat[rownames(t2),,drop=FALSE]

colnames(sphFE15LI)<-colnames(s15u_diffreg)
colnames(sphpLImat)<-colnames(s15u_diffreg)

hh_heat_FELI<-melt(sphFE15LI)
hh_sph_pLI<-melt(sphpLImat)
hh_heat_FELI$p<-hh_sph_pLI$value
colnames(hh_heat_FELI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_heat_FELI$TC<-as.factor(paste(hh_heat_FELI$TYPEHUQ,hh_heat_FELI$Cohort))
x <-hh_heat_FELI[order(hh_heat_FELI$TYPEHUQ),]

x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x1LI<-x
x1LI$Enduse<-"Space Heat"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for LowInc households, space heating",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, second Mid Income ######################
sphFE15MI<-matrix( heat_TCI[ c(seq(3,15,3),seq(17,71,3))],4,6,byrow = TRUE)
sphpMI<-summary(hh_heat15_fe4)[["coefficients"]][c(seq(7,19,3),seq(21,75,3)),4] # extract p values
sphpMImat<-matrix(sphpMI,4,6,byrow = TRUE) 

row.names(sphFE15MI)<-c("MF high","MF low","SF Att","SF Det")
row.names(sphpMImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
sphFE15MI<-sphFE15MI[rownames(t2),,drop=FALSE]
sphpMImat<-sphpMImat[rownames(t2),,drop=FALSE]

colnames(sphFE15MI)<-colnames(s15u_diffreg)
colnames(sphpMImat)<-colnames(s15u_diffreg)

hh_heat_FEMI<-melt(sphFE15MI)
hh_sph_pMI<-melt(sphpMImat)
hh_heat_FEMI$p<-hh_sph_pMI$value
colnames(hh_heat_FEMI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_heat_FEMI$TC<-as.factor(paste(hh_heat_FEMI$TYPEHUQ,hh_heat_FEMI$Cohort))
x <-hh_heat_FEMI[order(hh_heat_FEMI$TYPEHUQ),]

x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x1MI<-x
x1MI$Enduse<-"Space Heat"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for MidInc households, space heating",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q

# make the same plot for the three income groups, third High Income ######################
sphFE15HI<-matrix( c(heat_TCI[ c(seq(1,13,3))],0,heat_TCI[ c(seq(18,69,3))]),4,6,byrow = TRUE)
sphpHI<-c(summary(hh_heat15_fe4)[["coefficients"]][c(seq(5,17,3)),4],0,summary(hh_heat15_fe4)[["coefficients"]][c(seq(22,73,3)),4]) # extract p values
sphpHImat<-matrix(sphpHI,4,6,byrow = TRUE) 

row.names(sphFE15HI)<-c("MF high","MF low","SF Att","SF Det")
row.names(sphpHImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
sphFE15HI<-sphFE15HI[rownames(t2),,drop=FALSE]
sphpHImat<-sphpHImat[rownames(t2),,drop=FALSE]

colnames(sphFE15HI)<-colnames(s15u_diffreg)
colnames(sphpHImat)<-colnames(s15u_diffreg)

hh_heat_FEHI<-melt(sphFE15HI)
hh_sph_pHI<-melt(sphpHImat)
hh_heat_FEHI$p<-hh_sph_pHI$value
colnames(hh_heat_FEHI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_heat_FEHI$TC<-as.factor(paste(hh_heat_FEHI$TYPEHUQ,hh_heat_FEHI$Cohort))
x <-hh_heat_FEHI[order(hh_heat_FEHI$TYPEHUQ),]

x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x1HI<-x
x1HI$Enduse<-"Space Heat"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for HighInc households, space heating",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# compare differences between SFD and MF high
typeComp<-matrix(c((sphFE15HI[1,]-sphFE15HI[4,]),(sphFE15MI[1,]-sphFE15MI[4,]),(sphFE15LI[1,]-sphFE15LI[4,]),(sphFE15[1,]-sphFE15[4,])),6,4)
colnames(typeComp)<-c("HiInc","MidInc","LowInc","Avg")
rownames(typeComp)<-colnames(sphFE15HI)

compTypes<-melt(typeComp)
colnames(compTypes)<-c("Cohort","Income","Difference")
x<-compTypes
windows()
q<-qplot(x=Cohort,y=Difference,data=x,geom = "point") + 
  geom_point(aes(colour = factor(Income),shape=factor(Income)),size=2.5)+
  labs(title = "Difference in space heating between SF Det and MF High", y = "Difference (MJ)")  +
  theme_bw()+scale_color_brewer(palette="Dark2")
q

# This now calculates energy consumption if all of the houses had the same socioeconomic and physical characteristics.
# Probably if households moved into MF houses, the houses would have to be smaller on average. 
# Here we calculate the effect of that, reduction of average house size in addition to changing type.

# average heated floor area by house type and new (built in or after 1970) / old (built before 1970)
HFA_TC<-tapply(rus15$TOTHSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
INC_TC<-tapply(rus15$INC*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)

# extract average heated floor area for 'new' SF and MF houses
HFA_SF<-HFA_TC[2,1]
# modified for new scenarios: extracted avg heated floor area for 'new' (post-1970) low-income sf home
# HFA_SF<-hfaphic15[2,2,1]
beta_hsf<-hh_heat15_fe[["coefficients"]][["TOTHSQFT"]] # use the beta coefficient for floor area based on the avg household model
beta_hinc<-hh_heat15_fe[["coefficients"]][["INC"]] # use the beta coefficient for floor area based on the avg household model
del_stock<-sum(s15u_diffreg[4:5,])
beta_hsf4<-hh_heat15_fe4[["coefficients"]][["TOTHSQFT"]] # use the beta coefficient for floor area based on the avg household model

avg_diff_hfa<-HFA_TC[2,1]-HFA_TC[1,1]
avg_diff_inc<-INC_TC[2,1]-INC_TC[1,1]

avg_diff_hfa_LI<-hfaphic15[2,2,1]-HFA_TC[1,1]
avg_diff_hfa_MI<-hfaphic15[2,3,1]-HFA_TC[1,1]

# changes in modern MF/SF if houses were 30%, 50% smaller than average SF, by type and cohort
sph_red_70_MF<-0.3*HFA_SF*beta_hsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
sph_red_70_SF<-0.3*HFA_SF*beta_hsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
sph_red_50_MF<-0.5*HFA_SF*beta_hsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number
sph_red_50_SF<-0.5*HFA_SF*beta_hsf*s15u_diffreg[2:3,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number

# cooling models #####################
hh_cool<-lme(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_cool15<-lme(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_cool15_fe<-lm(BTUCOL~CDD65 + TOTCSQFT + INC + NHSLDMEM + TC, rus15)
hh_cool15_fe4<-lm(BTUCOL~CDD65 + TOTCSQFT + NHSLDMEM +  TCI, rus15)

# summary regression tables of cooling models
tab_model(hh_cool15_fe,hh_cool15_fe4,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","CDD65","TOTCSQFT","INC","NHSLDMEM"))

# effects of TC on cooling
cool_TC<-hh_cool15_fe[["coefficients"]][6:28]
cool_TCI<-hh_cool15_fe4[["coefficients"]][5:75]
# calculate counterfactual cooling energy
spcFE15<-matrix(c(cool_TC[1:5],0,cool_TC[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
spcp<-summary(hh_cool15_fe)[["coefficients"]][6:28,4] # extract p values
spcpmat<-matrix(c(spcp[1:5],0,spcp[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

row.names(spcFE15)<-c("MF high","MF low","SF Att","SF Det")
row.names(spcpmat)<-c("MF high","MF low","SF Att","SF Det")
spcFE15<-spcFE15[rownames(t2),,drop=FALSE]
spcpmat<-spcpmat[rownames(t2),,drop=FALSE]
# plot dotchart
colnames(spcFE15)<-colnames(s15u_diffreg)
colnames(spcpmat)<-colnames(s15u_diffreg)
hh_cool_FE<-melt(spcFE15)
hh_spc_p<-melt(spcpmat)
hh_cool_FE$p<-hh_spc_p$value
colnames(hh_cool_FE)<-c("TYPEHUQ","Cohort","Offset","p")
hh_cool_FE$TC<-as.factor(paste(hh_cool_FE$TYPEHUQ,hh_cool_FE$Cohort))
x <-hh_cool_FE[order(hh_cool_FE$TYPEHUQ),]

x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x2<-x
x2$Enduse<-"Space Cool"

# ggplot
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, space cooling",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, first Low Income ######################
spcFE15LI<-matrix( cool_TCI[ c(seq(2,14,3),seq(16,70,3))],4,6,byrow = TRUE)
spcpLI<-summary(hh_cool15_fe4)[["coefficients"]][c(seq(6,18,3),seq(20,74,3)),4] # extract p values
spcpLImat<-matrix(spcpLI,4,6,byrow = TRUE) 

row.names(spcFE15LI)<-c("MF high","MF low","SF Att","SF Det")
row.names(spcpLImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
spcFE15LI<-spcFE15LI[rownames(t2),,drop=FALSE]
spcpLImat<-spcpLImat[rownames(t2),,drop=FALSE]

colnames(spcFE15LI)<-colnames(s15u_diffreg)
colnames(spcpLImat)<-colnames(s15u_diffreg)

hh_cool_FELI<-melt(spcFE15LI)
hh_spc_pLI<-melt(spcpLImat)
hh_cool_FELI$p<-hh_spc_pLI$value
colnames(hh_cool_FELI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_cool_FELI$TC<-as.factor(paste(hh_cool_FELI$TYPEHUQ,hh_cool_FELI$Cohort))
x <-hh_cool_FELI[order(hh_cool_FELI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x2LI<-x
x2LI$Enduse<-"Space Cool"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for LowInc households, space cooling",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, second Mid Income ######################
spcFE15MI<-matrix( cool_TCI[ c(seq(3,15,3),seq(17,71,3))],4,6,byrow = TRUE)
spcpMI<-summary(hh_cool15_fe4)[["coefficients"]][c(seq(7,19,3),seq(21,75,3)),4] # extract p values
spcpMImat<-matrix(spcpMI,4,6,byrow = TRUE) 

row.names(spcFE15MI)<-c("MF high","MF low","SF Att","SF Det")
row.names(spcpMImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
spcFE15MI<-spcFE15MI[rownames(t2),,drop=FALSE]
spcpMImat<-spcpMImat[rownames(t2),,drop=FALSE]

colnames(spcFE15MI)<-colnames(s15u_diffreg)
colnames(spcpMImat)<-colnames(s15u_diffreg)

hh_cool_FEMI<-melt(spcFE15MI)
hh_spc_pMI<-melt(spcpMImat)
hh_cool_FEMI$p<-hh_spc_pMI$value
colnames(hh_cool_FEMI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_cool_FEMI$TC<-as.factor(paste(hh_cool_FEMI$TYPEHUQ,hh_cool_FEMI$Cohort))
x <-hh_cool_FEMI[order(hh_cool_FEMI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x2MI<-x
x2MI$Enduse<-"Space Cool"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for MidInc households, space cooling",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q

# make the same plot for the three income groups, third High Income ######################
spcFE15HI<-matrix( c(cool_TCI[ c(seq(1,13,3))],0,cool_TCI[ c(seq(18,69,3))]),4,6,byrow = TRUE)
spcpHI<-c(summary(hh_cool15_fe4)[["coefficients"]][c(seq(5,17,3)),4],0,summary(hh_cool15_fe4)[["coefficients"]][c(seq(22,73,3)),4]) # extract p values
spcpHImat<-matrix(spcpHI,4,6,byrow = TRUE) 

row.names(spcFE15HI)<-c("MF high","MF low","SF Att","SF Det")
row.names(spcpHImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
spcFE15HI<-spcFE15HI[rownames(t2),,drop=FALSE]
spcpHImat<-spcpHImat[rownames(t2),,drop=FALSE]

colnames(spcFE15HI)<-colnames(s15u_diffreg)
colnames(spcpHImat)<-colnames(s15u_diffreg)

hh_cool_FEHI<-melt(spcFE15HI)
hh_spc_pHI<-melt(spcpHImat)
hh_cool_FEHI$p<-hh_spc_pHI$value
colnames(hh_cool_FEHI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_cool_FEHI$TC<-as.factor(paste(hh_cool_FEHI$TYPEHUQ,hh_cool_FEHI$Cohort))
x <-hh_cool_FEHI[order(hh_cool_FEHI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x2HI<-x
x2HI$Enduse<-"Space Cool"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for HighInc households, space cooling",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q

# compare differences between SFD and MF high
typeComp<-matrix(c((spcFE15HI[1,]-spcFE15HI[4,]),(spcFE15MI[1,]-spcFE15MI[4,]),(spcFE15LI[1,]-spcFE15LI[4,]),(spcFE15[1,]-spcFE15[4,])),6,4)
colnames(typeComp)<-c("HiInc","MidInc","LowInc","Avg")
rownames(typeComp)<-colnames(spcFE15HI)

compTypes<-melt(typeComp)
colnames(compTypes)<-c("Cohort","Income","Difference")
x<-compTypes
windows()
q<-qplot(x=Cohort,y=Difference,data=x,geom = "point") + 
  geom_point(aes(colour = factor(Income),shape=factor(Income)),size=2.5)+
  labs(title = "Difference in space cooling between SF Det and MF High", y = "Difference (MJ)")  +
  theme_bw()+scale_color_brewer(palette="Dark2")
q

# extract average cooled floor area for 'new' SF and MF houses
CFA_SF<-cfaphc15[2,1]

beta_csf<-hh_cool15_fe[["coefficients"]][["TOTCSQFT"]] # use the coefficient from the model with average households by type and cohort
beta_cinc<-hh_cool15_fe[["coefficients"]][["INC"]] # use the coefficient from the model with average households by type and cohort
avg_diff_cfa<-cfaphc15[2,1]-cfaphc15[1,1]

beta_csf4<-hh_cool15_fe4[["coefficients"]][["TOTCSQFT"]] # use the coefficient from the model with average households by type and cohort
avg_diff_cfa_LI<-cfaphic15[2,2,1]-cfaphc15[1,1]
avg_diff_cfa_MI<-cfaphic15[2,3,1]-cfaphc15[1,1]

# changes in modern MF/SF if houses were 30%, 50% smaller than average SF, by type and cohort
spc_red_70_MF<-0.3*CFA_SF*beta_csf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
spc_red_70_SF<-0.3*CFA_SF*beta_csf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
spc_red_50_MF<-0.5*CFA_SF*beta_csf*s15u_diffreg[4:5,]  # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number
spc_red_50_SF<-0.5*CFA_SF*beta_csf*s15u_diffreg[2:3,]  # how much SF would reduce energy if they were 50% smaller (compared to being full size of average SF) negative number

# hot water models ##################
hh_dhw<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_dhw15<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_dhw15_fe<-lm(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + TC, rus15)
hh_dhw15_fe4<-lm(BTUDHW~HDD65 + TOTSQFT + NHSLDMEM +  TCI, rus15)

# summary regression tables
tab_model(hh_dhw15_fe,hh_dhw15_fe4,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTSQFT","INC","NHSLDMEM"))

# effects of TC on hot water

dhw_TC<-hh_dhw15_fe[["coefficients"]][6:28]
dhw_TCI<-hh_dhw15_fe4[["coefficients"]][5:75]
# calculate counterfactual dhw energy
dhwFE15<-matrix(c(dhw_TC[1:5],0,dhw_TC[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
dhwp<-summary(hh_dhw15_fe)[["coefficients"]][6:28,4] # extract p values
dhwpmat<-matrix(c(dhwp[1:5],0,dhwp[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

row.names(dhwFE15)<-c("MF high","MF low","SF Att","SF Det")
row.names(dhwpmat)<-c("MF high","MF low","SF Att","SF Det")
dhwFE15<-dhwFE15[rownames(t2),,drop=FALSE]
dhwpmat<-dhwpmat[rownames(t2),,drop=FALSE]

# plot dotchart
colnames(dhwFE15)<-colnames(s15u_diffreg)
colnames(dhwpmat)<-colnames(s15u_diffreg)
hh_dhw_FE<-melt(dhwFE15)
hh_dhw_p<-melt(dhwpmat)
hh_dhw_FE$p<-hh_dhw_p$value
colnames(hh_dhw_FE)<-c("TYPEHUQ","Cohort","Offset","p")
hh_dhw_FE$TC<-as.factor(paste(hh_dhw_FE$TYPEHUQ,hh_dhw_FE$Cohort))
x<-hh_dhw_FE[order(hh_dhw_FE$TYPEHUQ),]
#x$Offset<-x$Offset-dhwFEoff
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x3<-x
x3$Enduse<-"Hot Water"

# ggplot
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, hot water",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q

# make the same plot for the three income groups, first Low Income ######################
dhwFE15LI<-matrix( dhw_TCI[ c(seq(2,14,3),seq(16,70,3))],4,6,byrow = TRUE)
dhwpLI<-summary(hh_dhw15_fe4)[["coefficients"]][c(seq(6,18,3),seq(20,74,3)),4] # extract p values
dhwpLImat<-matrix(dhwpLI,4,6,byrow = TRUE) 

row.names(dhwFE15LI)<-c("MF high","MF low","SF Att","SF Det")
row.names(dhwpLImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
dhwFE15LI<-dhwFE15LI[rownames(t2),,drop=FALSE]
dhwpLImat<-dhwpLImat[rownames(t2),,drop=FALSE]

colnames(dhwFE15LI)<-colnames(s15u_diffreg)
colnames(dhwpLImat)<-colnames(s15u_diffreg)

hh_dhw_FELI<-melt(dhwFE15LI)
hh_dhw_pLI<-melt(dhwpLImat)
hh_dhw_FELI$p<-hh_dhw_pLI$value
colnames(hh_dhw_FELI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_dhw_FELI$TC<-as.factor(paste(hh_dhw_FELI$TYPEHUQ,hh_dhw_FELI$Cohort))
x <-hh_dhw_FELI[order(hh_dhw_FELI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x3LI<-x
x3LI$Enduse<-"Hot Water"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for LowInc households, hot water",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, second Mid Income ######################
dhwFE15MI<-matrix( dhw_TCI[ c(seq(3,15,3),seq(17,71,3))],4,6,byrow = TRUE)
dhwpMI<-summary(hh_dhw15_fe4)[["coefficients"]][c(seq(7,19,3),seq(21,75,3)),4] # extract p values
dhwpMImat<-matrix(dhwpMI,4,6,byrow = TRUE) 

row.names(dhwFE15MI)<-c("MF high","MF low","SF Att","SF Det")
row.names(dhwpMImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
dhwFE15MI<-dhwFE15MI[rownames(t2),,drop=FALSE]
dhwpMImat<-dhwpMImat[rownames(t2),,drop=FALSE]

colnames(dhwFE15MI)<-colnames(s15u_diffreg)
colnames(dhwpMImat)<-colnames(s15u_diffreg)

hh_dhw_FEMI<-melt(dhwFE15MI)
hh_dhw_pMI<-melt(dhwpMImat)
hh_dhw_FEMI$p<-hh_dhw_pMI$value
colnames(hh_dhw_FEMI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_dhw_FEMI$TC<-as.factor(paste(hh_dhw_FEMI$TYPEHUQ,hh_dhw_FEMI$Cohort))
x <-hh_dhw_FEMI[order(hh_dhw_FEMI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x3MI<-x
x3MI$Enduse<-"Hot Water"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for MidInc households, hot water",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q

# make the same plot for the three income groups, third High Income ######################
dhwFE15HI<-matrix( c(dhw_TCI[ c(seq(1,13,3))],0,dhw_TCI[ c(seq(18,69,3))]),4,6,byrow = TRUE)
dhwpHI<-c(summary(hh_dhw15_fe4)[["coefficients"]][c(seq(5,17,3)),4],0,summary(hh_dhw15_fe4)[["coefficients"]][c(seq(22,73,3)),4]) # extract p values
dhwpHImat<-matrix(dhwpHI,4,6,byrow = TRUE) 

row.names(dhwFE15HI)<-c("MF high","MF low","SF Att","SF Det")
row.names(dhwpHImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
dhwFE15HI<-dhwFE15HI[rownames(t2),,drop=FALSE]
dhwpHImat<-dhwpHImat[rownames(t2),,drop=FALSE]

colnames(dhwFE15HI)<-colnames(s15u_diffreg)
colnames(dhwpHImat)<-colnames(s15u_diffreg)

hh_dhw_FEHI<-melt(dhwFE15HI)
hh_dhw_pHI<-melt(dhwpHImat)
hh_dhw_FEHI$p<-hh_dhw_pHI$value
colnames(hh_dhw_FEHI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_dhw_FEHI$TC<-as.factor(paste(hh_dhw_FEHI$TYPEHUQ,hh_dhw_FEHI$Cohort))
x <-hh_dhw_FEHI[order(hh_dhw_FEHI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x3HI<-x
x3HI$Enduse<-"Hot Water"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for HighInc households, hot water",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q

# compare differences between SFD and MF high
typeComp<-matrix(c((dhwFE15HI[1,]-dhwFE15HI[4,]),(dhwFE15MI[1,]-dhwFE15MI[4,]),(dhwFE15LI[1,]-dhwFE15LI[4,]),(dhwFE15[1,]-dhwFE15[4,])),6,4)
colnames(typeComp)<-c("HiInc","MidInc","LowInc","Avg")
rownames(typeComp)<-colnames(dhwFE15HI)

compTypes<-melt(typeComp)
colnames(compTypes)<-c("Cohort","Income","Difference")
x<-compTypes
windows()
q<-qplot(x=Cohort,y=Difference,data=x,geom = "point") + 
  geom_point(aes(colour = factor(Income),shape=factor(Income)),size=2.5)+
  labs(title = "Difference in hot water between SF Det and MF High", y = "Difference (MJ)")  +
  theme_bw()+scale_color_brewer(palette="Dark2")
q

# 
SQFT_all<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)

# extract average total floor area for 'new' SF and MF houses
TFA_SF<-tfaphc15[2,1]

# floor area coefficient for dhw
beta_hwtsf<- hh_dhw15_fe[["coefficients"]][["TOTSQFT"]]
beta_hwinc<- hh_dhw15_fe[["coefficients"]][["INC"]]
avg_diff_tfa<-tfaphc15[2,1]-tfaphc15[1,1]

beta_hwtsf4<-hh_dhw15_fe4[["coefficients"]][["TOTSQFT"]] # use the coefficient from the model with average households by type and cohort
avg_diff_tfa_LI<-faphic15[2,2,1]-tfaphc15[1,1]
avg_diff_tfa_MI<-faphic15[2,3,1]-tfaphc15[1,1]

dhw_red_70_MF<-0.3*TFA_SF*beta_hwtsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
dhw_red_70_SF<-0.3*TFA_SF*beta_hwtsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
dhw_red_50_MF<-0.5*TFA_SF*beta_hwtsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number
dhw_red_50_SF<-0.5*TFA_SF*beta_hwtsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 50% smaller (compared to being full size of average SF) negative number

# new other energy models ##################
hh_oth<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_oth15<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_oth15_fe<-lm(BTUOTH~TOTSQFT + INC + NHSLDMEM + TC, rus15)
hh_oth15_fe4<-lm(BTUOTH~TOTSQFT +  NHSLDMEM + TCI, rus15)

tab_model(hh_oth15_fe,hh_oth15_fe4,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","TOTSQFT","INC","NHSLDMEM"))

# summary regression tables
tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,
          terms = c("(Intercept)","HDD65","CDD65","TOTHSQFT","TOTSQFT","INC","NHSLDMEM","TOTCSQFT"))

tab_model(hh_heat15_fe4,hh_cool15_fe4,hh_dhw15_fe4,hh_oth15_fe4,show.ci = 0,show.se = TRUE,
          terms = c("(Intercept)","HDD65","CDD65","TOTHSQFT","TOTSQFT","NHSLDMEM","TOTCSQFT"))
tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,digits = 0,show.p = FALSE)
tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,digits = 0)

oth_TC<-hh_oth15_fe[["coefficients"]][5:27]
oth_TCI<-hh_oth15_fe4[["coefficients"]][4:74]
# calculate counterfactual oth energy
othFE15<-matrix(c(oth_TC[1:5],0,oth_TC[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
othp<-summary(hh_oth15_fe)[["coefficients"]][5:27,4] # extract p values
othpmat<-matrix(c(othp[1:5],0,othp[6:23]),4,6,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

row.names(othFE15)<-c("MF high","MF low","SF Att","SF Det")
row.names(othpmat)<-c("MF high","MF low","SF Att","SF Det")
othFE15<-othFE15[rownames(t2),,drop=FALSE]
othpmat<-othpmat[rownames(t2),,drop=FALSE]

# plot dotchart
colnames(othFE15)<-colnames(s15u_diffreg)
colnames(othpmat)<-colnames(s15u_diffreg)
hh_oth_FE<-melt(othFE15)
hh_oth_p<-melt(othpmat)
hh_oth_FE$p<-hh_oth_p$value
colnames(hh_oth_FE)<-c("TYPEHUQ","Cohort","Offset","p")
hh_oth_FE$TC<-as.factor(paste(hh_oth_FE$TYPEHUQ,hh_oth_FE$Cohort))
x <-hh_oth_FE[order(hh_oth_FE$TYPEHUQ),]

#x$Offset<-x$Offset-othFEoff
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x4<-x
x4$Enduse<-"Other"

# ggplot, average effects of TC on other energy use
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, other",x="Coefficient (MJ)")+ 
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q

# make the same plot for the three income groups, first Low Income ######################
othFE15LI<-matrix( oth_TCI[ c(seq(2,14,3),seq(16,70,3))],4,6,byrow = TRUE)
othpLI<-summary(hh_oth15_fe4)[["coefficients"]][c(seq(5,17,3),seq(19,73,3)),4] # extract p values
othpLImat<-matrix(othpLI,4,6,byrow = TRUE) 

row.names(othFE15LI)<-c("MF high","MF low","SF Att","SF Det")
row.names(othpLImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
othFE15LI<-othFE15LI[rownames(t2),,drop=FALSE]
othpLImat<-othpLImat[rownames(t2),,drop=FALSE]

colnames(othFE15LI)<-colnames(s15u_diffreg)
colnames(othpLImat)<-colnames(s15u_diffreg)

hh_oth_FELI<-melt(othFE15LI)
hh_oth_pLI<-melt(othpLImat)
hh_oth_FELI$p<-hh_oth_pLI$value
colnames(hh_oth_FELI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_oth_FELI$TC<-as.factor(paste(hh_oth_FELI$TYPEHUQ,hh_oth_FELI$Cohort))
x <-hh_oth_FELI[order(hh_oth_FELI$TYPEHUQ),]

x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x4LI<-x
x4LI$Enduse<-"Other"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for LowInc households, Other",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, second Mid Income ######################
othFE15MI<-matrix( oth_TCI[ c(seq(3,15,3),seq(17,71,3))],4,6,byrow = TRUE)
othpMI<-summary(hh_oth15_fe4)[["coefficients"]][c(seq(6,18,3),seq(20,74,3)),4] # extract p values
othpMImat<-matrix(othpMI,4,6,byrow = TRUE) 

row.names(othFE15MI)<-c("MF high","MF low","SF Att","SF Det")
row.names(othpMImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
othFE15MI<-othFE15MI[rownames(t2),,drop=FALSE]
othpMImat<-othpMImat[rownames(t2),,drop=FALSE]

colnames(othFE15MI)<-colnames(s15u_diffreg)
colnames(othpMImat)<-colnames(s15u_diffreg)

hh_oth_FEMI<-melt(othFE15MI)
hh_oth_pMI<-melt(othpMImat)
hh_oth_FEMI$p<-hh_oth_pMI$value
colnames(hh_oth_FEMI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_oth_FEMI$TC<-as.factor(paste(hh_oth_FEMI$TYPEHUQ,hh_oth_FEMI$Cohort))
x <-hh_oth_FEMI[order(hh_oth_FEMI$TYPEHUQ),]
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x4MI<-x
x4MI$Enduse<-"Other"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for MidInc households, Other",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q
# make the same plot for the three income groups, third High Income ######################
othFE15HI<-matrix( c(oth_TCI[ c(seq(1,13,3))],0,oth_TCI[ c(seq(18,69,3))]),4,6,byrow = TRUE)
othpHI<-c(summary(hh_oth15_fe4)[["coefficients"]][c(seq(4,16,3)),4],0,summary(hh_oth15_fe4)[["coefficients"]][c(seq(21,72,3)),4]) # extract p values
othpHImat<-matrix(othpHI,4,6,byrow = TRUE) 

row.names(othFE15HI)<-c("MF high","MF low","SF Att","SF Det")
row.names(othpHImat)<-c("MF high","MF low","SF Att","SF Det")
# reorder based on t2 row order
othFE15HI<-othFE15HI[rownames(t2),,drop=FALSE]
othpHImat<-othpHImat[rownames(t2),,drop=FALSE]

colnames(othFE15HI)<-colnames(s15u_diffreg)
colnames(othpHImat)<-colnames(s15u_diffreg)

hh_oth_FEHI<-melt(othFE15HI)
hh_oth_pHI<-melt(othpHImat)
hh_oth_FEHI$p<-hh_oth_pHI$value
colnames(hh_oth_FEHI)<-c("TYPEHUQ","Cohort","Offset","p")
hh_oth_FEHI$TC<-as.factor(paste(hh_oth_FEHI$TYPEHUQ,hh_oth_FEHI$Cohort))
x <-hh_oth_FEHI[order(hh_oth_FEHI$TYPEHUQ),]
#x$Offset<-x$Offset-othFEoff # remove the offset added earlier, for displaying the TC effects unaltered
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x4HI<-x
x4HI$Enduse<-"Other"

q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects for HighInc households, Other",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") +scale_color_brewer(palette="Dark2")
windows()
q

# compare differences between SFD and MF high
typeComp<-matrix(c((othFE15HI[1,]-othFE15HI[4,]),(othFE15MI[1,]-othFE15MI[4,]),(othFE15LI[1,]-othFE15LI[4,]),(othFE15[1,]-othFE15[4,])),6,4)
colnames(typeComp)<-c("HiInc","MidInc","LowInc","Avg")
rownames(typeComp)<-colnames(othFE15HI)

compTypes<-melt(typeComp)
colnames(compTypes)<-c("Cohort","Income","Difference")
x<-compTypes
windows()
q<-qplot(x=Cohort,y=Difference,data=x,geom = "point") + 
  geom_point(aes(colour = factor(Income),shape=factor(Income)),size=2.5)+
  labs(title = "Difference in Other between SF Det and MF High", y = "Difference (MJ)")  +
  theme_bw()+scale_color_brewer(palette="Dark2")
q

# coefficient of total floor area for other end uses
beta_othtsf<- hh_oth15_fe[["coefficients"]][["TOTSQFT"]] # use coefficient from model with average household
beta_othinc<- hh_oth15_fe[["coefficients"]][["INC"]]

beta_othtsf4<-hh_oth15_fe4[["coefficients"]][["TOTSQFT"]] # use the coefficient from the model with LMI households by type and cohort

oth_red_70_MF<-0.3*TFA_SF*beta_othtsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
oth_red_70_SF<-0.3*TFA_SF*beta_othtsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
oth_red_50_MF<-0.5*TFA_SF*beta_othtsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number
oth_red_50_SF<-0.5*TFA_SF*beta_othtsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 50% smaller (compared to being full size of average SF) negative number

## fig 3 plot ##########
X<-rbind(x1,x2,x3,x4)
X$TYPEHUQ<-revalue(X$TYPEHUQ, c("SF Det"="Single-family detached","SF Att"="Single-family attached","MF low"="Multifamily low","MF high"="Multifamily high"))
X$Signif<-"p>0.05"
X[X$p<0.05,]$Signif<-"p<0.05"
X$TYPE<-"Single-family"
X[X$TYPEHUQ=="Multifamily high" | X$TYPEHUQ=="Multifamily low",]$TYPE<-"Multifamily"
XX<-X
XXX<-X[X$TYPEHUQ=="Multifamily high-density" | X$TYPEHUQ=="Single-family Detached" ,]

q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + xlim(-9000,52000)+ 
  geom_point(aes(colour = factor(Enduse),size=Signif),size=XX$Sig) + 
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) +
  labs(title = "House type and cohort effects",x="Effect on energy end-use consumption (MJ)",colour = 'End-use') +  
  theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
                                                           legend.background = element_rect(color="gray45"),legend.title.align=0.5,plot.title = element_text(face = "bold"))
windows()
q 
q + theme(legend.position="none")
# for low income
XLI<-rbind(x1LI,x2LI,x3LI,x4LI)
# levels(x)[levels(X)==]
XLI$TYPEHUQ<-revalue(XLI$TYPEHUQ, c("SF Det"="Single-family detached","SF Att"="Single-family attached","MF low"="Multifamily low","MF high"="Multifamily high"))
XLI$Signif<-"p>0.05"
XLI[XLI$p<0.05,]$Signif<-"p<0.05"
XLI$TYPE<-"Single-family"
XLI[XLI$TYPEHUQ=="Multifamily high" | XLI$TYPEHUQ=="Multifamily low",]$TYPE<-"Multifamily"
XX<-XLI
XXX<-XLI[XLI$TYPEHUQ=="Multifamily high-density" | XLI$TYPEHUQ=="Single-family Detached" ,]

q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + xlim(-9000,52000)+ 
  geom_point(aes(colour = factor(Enduse),size=Signif),size=XX$Sig) + 
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) +
  labs(title = "House type and cohort effects, Low-income households",x="Effect on energy end-use consumption (MJ)",colour = 'End-use') +  
  theme_bw() + scale_color_brewer(palette="Dark2") + 
  theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
        legend.background = element_rect(color="gray45"),legend.title.align=0.5,plot.title = element_text(face = "bold"))
windows()
q + theme(legend.position="none")

# for mid income
XMI<-rbind(x1MI,x2MI,x3MI,x4MI)

XMI$TYPEHUQ<-revalue(XMI$TYPEHUQ, c("SF Det"="Single-family detached","SF Att"="Single-family attached","MF low"="Multifamily low","MF high"="Multifamily high"))
XMI$Signif<-"p>0.05"
XMI[XMI$p<0.05,]$Signif<-"p<0.05"
XMI$TYPE<-"Single-family"
XMI[XMI$TYPEHUQ=="Multifamily high" | XMI$TYPEHUQ=="Multifamily low",]$TYPE<-"Multifamily"
XX<-XMI
XXX<-XMI[XMI$TYPEHUQ=="Multifamily high-density" | XMI$TYPEHUQ=="Single-family Detached" ,]

q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + xlim(-9000,52000)+ 
  geom_point(aes(colour = factor(Enduse),size=Signif),size=XX$Sig) + 
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) +
  labs(title = "House type and cohort effects, Mid-income households",x="Effect on energy end-use consumption (MJ)",colour = 'End-use') +  
  theme_bw() + scale_color_brewer(palette="Dark2") + 
  theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
        legend.background = element_rect(color="gray45"),legend.title.align=0.5,plot.title = element_text(face = "bold"))
windows()
q + theme(legend.position="none")

# for high income
XHI<-rbind(x1HI,x2HI,x3HI,x4HI)
XHI$TYPEHUQ<-revalue(XHI$TYPEHUQ, c("SF Det"="Single-family detached","SF Att"="Single-family attached","MF low"="Multifamily low","MF high"="Multifamily high"))
XHI$Signif<-"p>0.05"
XHI[XHI$p<0.05,]$Signif<-"p<0.05"
XHI$TYPE<-"Single-family"
XHI[XHI$TYPEHUQ=="Multifamily high" | XHI$TYPEHUQ=="Multifamily low",]$TYPE<-"Multifamily"
XX<-XHI
XXX<-XHI[XHI$TYPEHUQ=="Multifamily high-density" | XHI$TYPEHUQ=="Single-family Detached" ,]

q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + xlim(-9000,52000)+ 
  geom_point(aes(colour = factor(Enduse),size=Signif),size=XX$Sig) + 
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) +
  labs(title = "House type and cohort effects, High-income households",x="Effect on energy end-use consumption (MJ)",colour = 'End-use') +  
  theme_bw() + scale_color_brewer(palette="Dark2") + 
  theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
        legend.background = element_rect(color="gray45"),legend.title.align=0.5,plot.title = element_text(face = "bold"))
windows()
q + theme(legend.position="none")
# load and convert original and stock counterfactual energy consumption data ##############
load("end_uses_CF.RData")

# define function to combine the 50s and 60s cohorts, and 2000s and 2010s cohorts, and remove the extra columns, and remove MF, and convert to MJ
com_mat<-function(s) {
                      s[,2]<-s[,2]+s[,3];
                      s[,7]<-s[,7]+s[,8];
                      s<-s[,-c(3,8)];
                      colnames(s)[2]<-"1950-60s";
                      colnames(s)[6]<-"2000+";
                      s<-1.055*s; # convert from kBTU to MJ
                      s
}

ll<-list(sph15u,spc15u,dhw15u,oth15u,sph15uhyp2,spc15uhyp2,dhw15uhyp2,oth15uhyp2,e15u)
lln<-c('sph15u','spc15u','dhw15u','oth15u','sph15uhyp2','spc15uhyp2','dhw15uhyp2','oth15uhyp2','e15u')
ll2<-lapply(ll, com_mat)
for (i in 1:length(lln)) {
  assign(lln[i],ll2[[i]])
}
# recalculate energy per house
eph15u<-e15u/s15u
# end-use per house
sphph15u<-sph15u/s15u
spcph15u<-spc15u/s15u
dhwph15u<-dhw15u/s15u
othph15u<-oth15u/s15u

# calculate ghg associated with end each end use (and all end uses) for each house type and cohort
sphghgph15u<-diag(ghgi_sph)%*%sphph15u # kg co2e
spcghgph15u<-diag(ghgi_spc)%*%spcph15u # kg co2e
dhwghgph15u<-diag(ghgi_dhw)%*%dhwph15u # kg co2e
othghgph15u<-diag(ghgi_oth)%*%othph15u # kg co2e
ghgph15u<-sphghgph15u+spcghgph15u+dhwghgph15u+othghgph15u

# calculate CF effects ########
row.names(sph15u)<-c("Manufactured Home","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(oth15u)<-row.names(dhw15u)<-row.names(spc15u)<-row.names(sph15u)

row.names(sph15uhyp2)<-c("Manufactured Home","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(oth15uhyp2)<-row.names(dhw15uhyp2)<-row.names(spc15uhyp2)<-row.names(sph15uhyp2)

# base case
sph0<-melt(sph15u)
sph0$Stock<-as.factor("Base: No type change,")
sph0$Energy<-as.factor("Same floor area")
sph0$enduse<-as.factor("Space Heat")
sph0$Scenario<-as.factor("Base") 
sph0$ghg<-sph0$value*ghgi_avg[1] # ghg in kg co2e. now using average for all house types, can change that if desired
sph0$ghg2<-sph0$value*rep(ghgi_sph,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# new counterfactual 1, with type change, and assuming 65% LI and 35% MI households move
sphFE15LMI<-(0.65*sphFE15LI+0.35*sphFE15MI)-(0.65*sphFE15LI+0.35*sphFE15MI)[4,6] # assume difference per household is 65% the difference per LI and 35% the difference per MI. Weighted avg
sphFEavgrefLMI<-sphph15u[5,6]+0.65*beta_hsf4*avg_diff_hfa_LI + 0.35*beta_hsf4*avg_diff_hfa_MI #
sphFE15offLMI<-sphFE15LMI+sphFEavgrefLMI
sphCF1diffSF<-s15u_diffreg[2:5,]*sphFE15offLMI ## 
sphCF1<-sph15u
sphCF1[2:5,]<-sph15u[2:5,]+sphCF1diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
sphCF1<-melt(sphCF1)
sphCF1$Stock<-as.factor("CF1: LMI SinFam -> MulFam,")
sphCF1$Energy<-as.factor("Same floor area")
sphCF1$enduse<-as.factor("Space Heat")
sphCF1$Scenario<-as.factor("CF1")
sphCF1$ghg<-sphCF1$value*ghgi_avg[1] # ghg emissions in kg co2e
sphCF1$ghg2<-sphCF1$value*rep(ghgi_sph,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 2, assuming type change from SF to MF, but no change in floor area
sphFEoff<-sphph15u[5,6]+beta_hsf*avg_diff_hfa+beta_hinc*avg_diff_inc # estimate of average consumption in the reference house assuming it has same income and floor area as SF avg
sphFE15off<-sphFE15+sphFEoff
sphCF2diffSF<-s15u_diffreg[2:5,]*sphFE15off
sphCF2<-sph15u
sphCF2[2:5,]<-sph15u[2:5,]+sphCF2diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
sphCF2<-melt(sphCF2)
sphCF2$Stock<-as.factor("CF2: Avg SinFam -> MulFam,")
sphCF2$Energy<-as.factor("Same floor area")
sphCF2$enduse<-as.factor("Space Heat")
sphCF2$Scenario<-as.factor("CF2")
sphCF2$ghg<-sphCF2$value*ghgi_avg[1] # ghg emissions in kg co2e
sphCF2$ghg2<-sphCF2$value*rep(ghgi_sph,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 3, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
sphCF3<-sph15u
sphCF3[2:5,]<-sph15u[2:5,]+sphCF2diffSF # type distribution change
sphCF3[4:5,]<-sphCF3[4:5,]-sph_red_70_MF # add floor space adjustment change
sphCF3<-melt(sphCF3)
sphCF3$Stock<-as.factor("CF3: LMI SinFam -> MulFam,")
sphCF3$Energy<-as.factor("Multifam 70% size of single-fam")
sphCF3$enduse<-as.factor("Space Heat")
sphCF3$Scenario<-as.factor("CF3")
sphCF3$ghg<-sphCF3$value*ghgi_avg[1] # ghg emissions in kg co2e
sphCF3$ghg2<-sphCF3$value*rep(ghgi_sph,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# new counterfactual 4, assuming type change SF to MF among LMI households, plus a reduction in size of the new MF to 50% of the size of the average SF
sphCF4<-sph15u
sphCF4[2:5,]<-sph15u[2:5,]+sphCF2diffSF # type distribution change, based on the new LMI TC coefficients
sphCF4[4:5,]<-sphCF4[4:5,]-sph_red_50_MF # add floor space adjustment change
sphCF4<-melt(sphCF4)
sphCF4$Stock<-as.factor("CF4: LMI SinFam -> MulFam,")
sphCF4$Energy<-as.factor("Multifam 50% size of single-fam")
sphCF4$enduse<-as.factor("Space Heat")
sphCF4$Scenario<-as.factor("CF4")
sphCF4$ghg<-sphCF4$value*ghgi_avg[1] # ghg emissions in kg co2e
sphCF4$ghg2<-sphCF4$value*rep(ghgi_sph,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# space cooling scenarios
# base case
spc0<-melt(spc15u)
spc0$Stock<-as.factor("Base: No type change,")
spc0$Energy<-as.factor("Same floor area")
spc0$enduse<-as.factor("Space Cool")
spc0$Scenario<-as.factor("Base")
spc0$ghg<-spc0$value*ghgi_avg[2] # ghg emissions in kg co2e
spc0$ghg2<-spc0$value*rep(ghgi_spc,6) # ghg in kg co2e. using type specific averages, and local elec mix

# new counterfactual 1, with type change, and assuming 65% LI and 35% MI households move
spcFE15LMI<-(0.65*spcFE15LI+0.35*spcFE15MI)-(0.65*spcFE15LI+0.35*spcFE15MI)[4,6]  #spcFEoff # assume difference per household is 65% the difference per LI and 35% the difference per MI. Weighted avg
spcFEavgrefLMI<-spcph15u[5,6]+0.65*beta_csf4*avg_diff_cfa_LI + 0.35*beta_csf4*avg_diff_cfa_MI #
spcFE15offLMI<-spcFE15LMI+spcFEavgrefLMI
spcCF1diffSF<-s15u_diffreg[2:5,]*spcFE15offLMI
spcCF1<-spc15u
spcCF1[2:5,]<-spc15u[2:5,]+spcCF1diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
spcCF1<-melt(spcCF1)
spcCF1$Stock<-as.factor("CF1: LMI SinFam -> MulFam,")
spcCF1$Energy<-as.factor("Same floor area")
spcCF1$enduse<-as.factor("Space Cool")
spcCF1$Scenario<-as.factor("CF1")
spcCF1$ghg<-spcCF1$value*ghgi_avg[1] # ghg emissions in kg co2e
spcCF1$ghg2<-spcCF1$value*rep(ghgi_spc,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix


# counterfactual 2, assuming type change from SF to MF, but no change in floor area
spcFEoff<-spcph15u[5,6]+beta_csf*avg_diff_cfa+beta_cinc*avg_diff_inc # estimate of average consumption in the reference house assuming it has same income and floor area as SF avg
spcFE15off<-spcFE15+spcFEoff# alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged
spcCF2diffSF<-s15u_diffreg[2:5,]*spcFE15off
spcCF2<-spc15u
spcCF2[2:5,]<-spc15u[2:5,]+spcCF2diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
spcCF2<-melt(spcCF2)
spcCF2$Stock<-as.factor("CF2: Avg SinFam -> MulFam,")
spcCF2$Energy<-as.factor("Same floor area")
spcCF2$enduse<-as.factor("Space Cool")
spcCF2$Scenario<-as.factor("CF2")
spcCF2$ghg<-spcCF2$value*ghgi_avg[1] # ghg emissions in kg co2e
spcCF2$ghg2<-spcCF2$value*rep(ghgi_spc,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 3, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
spcCF3<-spc15u
spcCF3[2:5,]<-spc15u[2:5,]+spcCF2diffSF # type distribution change from CF2, based on type change on average household
spcCF3[4:5,]<-spcCF3[4:5,]-spc_red_70_MF # add floor space adjustment change
spcCF3<-melt(spcCF3)
spcCF3$Stock<-as.factor("CF3: LMI SinFam -> MulFam,")
spcCF3$Energy<-as.factor("Multifam 70% size of single-fam")
spcCF3$enduse<-as.factor("Space Cool")
spcCF3$Scenario<-as.factor("CF3")
spcCF3$ghg<-spcCF3$value*ghgi_avg[1] # ghg emissions in kg co2e
spcCF3$ghg2<-spcCF3$value*rep(ghgi_spc,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# new counterfactual 4, assuming type change SF to MF among LMI households, plus a reduction in size of the new MF to 50% of the size of the average SF
spcCF4<-spc15u
spcCF4[2:5,]<-spc15u[2:5,]+spcCF2diffSF # type distribution change, based on type change on average household
spcCF4[4:5,]<-spcCF4[4:5,]-spc_red_50_MF # add floor space adjustment change
spcCF4<-melt(spcCF4)
spcCF4$Stock<-as.factor("CF4: LMI SinFam -> MulFam,")
spcCF4$Energy<-as.factor("Multifam 50% size of single-fam")
spcCF4$enduse<-as.factor("Space Cool")
spcCF4$Scenario<-as.factor("CF4")
spcCF4$ghg<-spcCF4$value*ghgi_avg[1] # ghg emissions in kg co2e
spcCF4$ghg2<-spcCF4$value*rep(ghgi_spc,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix
# 
# 
# hot water scenarios new
# base case
dhw0<-melt(dhw15u)
dhw0$Stock<-as.factor("Base: No type change,")
dhw0$Energy<-as.factor("Same floor area")
dhw0$enduse<-as.factor("Hot Water")
dhw0$Scenario<-as.factor("Base")
dhw0$ghg<-dhw0$value*ghgi_avg[2] # ghg emissions in kg co2e
dhw0$ghg2<-dhw0$value*rep(ghgi_dhw,6) # ghg in kg co2e. using type specific averages, and local elec mix

# new counterfactual 1, with type change, and assuming 65% LI and 35% MI households move
dhwFE15LMI<-(0.65*dhwFE15LI+0.35*dhwFE15MI)-(0.65*dhwFE15LI+0.35*dhwFE15MI)[4,6]# assume difference per household is 65% the difference per LI and 35% the difference per MI. Weighted avg
dhwFEavgrefLMI<-dhwph15u[5,6]+0.65*beta_hwtsf4*avg_diff_tfa_LI + 0.35*beta_hwtsf4*avg_diff_tfa_MI 
dhwFE15offLMI<-dhwFE15LMI+dhwFEavgrefLMI
dhwCF1diffSF<-s15u_diffreg[2:5,]*dhwFE15offLMI
dhwCF1<-dhw15u
dhwCF1[2:5,]<-dhw15u[2:5,]+dhwCF1diffSF
dhwCF1<-melt(dhwCF1)
dhwCF1$Stock<-as.factor("CF1: LMI SinFam -> MulFam,")
dhwCF1$Energy<-as.factor("Same floor area")
dhwCF1$enduse<-as.factor("Hot Water")
dhwCF1$Scenario<-as.factor("CF1")
dhwCF1$ghg<-dhwCF1$value*ghgi_avg[1] # ghg emissions in kg co2e
dhwCF1$ghg2<-dhwCF1$value*rep(ghgi_dhw,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 2, assuming type change from SF to MF, but no change in floor area, avg household
dhwFEoff<-dhwph15u[5,6]+beta_hwtsf*avg_diff_tfa+beta_hwinc*avg_diff_inc
dhwFE15off<-dhwFE15+dhwFEoff # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged
dhwCF2diffSF<-s15u_diffreg[2:5,]*dhwFE15off
dhwCF2<-dhw15u
dhwCF2[2:5,]<-dhw15u[2:5,]+dhwCF2diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
dhwCF2<-melt(dhwCF2)
dhwCF2$Stock<-as.factor("CF2: Avg SinFam -> MulFam,")
dhwCF2$Energy<-as.factor("Same floor area")
dhwCF2$enduse<-as.factor("Hot Water")
dhwCF2$Scenario<-as.factor("CF2")
dhwCF2$ghg<-dhwCF2$value*ghgi_avg[1] # ghg emissions in kg co2e
dhwCF2$ghg2<-dhwCF2$value*rep(ghgi_dhw,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 3, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
dhwCF3<-dhw15u
dhwCF3[2:5,]<-dhw15u[2:5,]+dhwCF2diffSF # type distribution change
dhwCF3[4:5,]<-dhwCF3[4:5,]-dhw_red_70_MF # add floor space adjustment change
dhwCF3<-melt(dhwCF3)
dhwCF3$Stock<-as.factor("CF3: LMI SinFam -> MulFam,")
dhwCF3$Energy<-as.factor("Multifam 70% size of single-fam")
dhwCF3$enduse<-as.factor("Hot Water")
dhwCF3$Scenario<-as.factor("CF3")
dhwCF3$ghg<-dhwCF3$value*ghgi_avg[1] # ghg emissions in kg co2e
dhwCF3$ghg2<-dhwCF3$value*rep(ghgi_dhw,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# new counterfactual 4, assuming type change SF to MF among LMI households, plus a reduction in size of the new MF to 50% of the size of the average SF
dhwCF4<-dhw15u
dhwCF4[2:5,]<-dhw15u[2:5,]+dhwCF2diffSF # type distribution change, based on the new LMI TC coefficients
dhwCF4[4:5,]<-dhwCF4[4:5,]-dhw_red_50_MF # add floor space adjustment change
dhwCF4<-melt(dhwCF4)
dhwCF4$Stock<-as.factor("CF4: LMI SinFam -> MulFam,")
dhwCF4$Energy<-as.factor("Multifam 50% size of single-fam")
dhwCF4$enduse<-as.factor("Hot Water")
dhwCF4$Scenario<-as.factor("CF4")
dhwCF4$ghg<-dhwCF4$value*ghgi_avg[1] # ghg emissions in kg co2e
dhwCF4$ghg2<-dhwCF4$value*rep(ghgi_dhw,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# other end use scenarios new
# base case
oth0<-melt(oth15u)
oth0$Stock<-as.factor("Base: No type change,")
oth0$Energy<-as.factor("Same floor area")
oth0$enduse<-as.factor("Other")
oth0$Scenario<-as.factor("Base")
oth0$ghg<-oth0$value*ghgi_avg[2] # ghg emissions in kg co2e
oth0$ghg2<-oth0$value*rep(ghgi_oth,6) # ghg in kg co2e. using type specific averages, and local elec mix

# new counterfactual 1, with type change, and assuming 65% LI and 35% MI households move
othFE15LMI<-(0.65*othFE15LI+0.35*othFE15MI)-(0.65*othFE15LI+0.35*othFE15MI)[4,6] # assume difference per household is 65% the difference per LI and 35% the difference per MI. Weighted avg
othFEavgrefLMI<-othph15u[5,6]+0.65*beta_othtsf4*avg_diff_tfa_LI + 0.35*beta_othtsf4*avg_diff_tfa_MI 
othFE15offLMI<-othFE15LMI+othFEavgrefLMI
othCF1diffSF<-s15u_diffreg[2:5,]*othFE15offLMI
othCF1<-oth15u
othCF1[2:5,]<-oth15u[2:5,]+othCF1diffSF
othCF1diffSF<-s15u_diffreg[2:5,]*othFE15LMI ## NB, interesting to compare what happens when using LI, MI, LMI. LMI is not the average of the other 2!
othCF1<-oth15u
othCF1[2:5,]<-oth15u[2:5,]+othCF1diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
othCF1<-melt(othCF1)
othCF1$Stock<-as.factor("CF1: LMI SinFam -> MulFam,")
othCF1$Energy<-as.factor("Same floor area")
othCF1$enduse<-as.factor("Other")
othCF1$Scenario<-as.factor("CF1")
othCF1$ghg<-othCF1$value*ghgi_avg[1] # ghg emissions in kg co2e
othCF1$ghg2<-othCF1$value*rep(ghgi_oth,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 2, assuming type change from SF to MF, but no change in floor area, avg household
othFEoff<-othph15u[5,6]+beta_othtsf*avg_diff_tfa+beta_othinc*avg_diff_inc
othFE15off<-othFE15+othFEoff
othCF2diffSF<-s15u_diffreg[2:5,]*othFE15off
othCF2<-oth15u
othCF2[2:5,]<-oth15u[2:5,]+othCF2diffSF # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
othCF2<-melt(othCF2)
othCF2$Stock<-as.factor("CF2: Avg SinFam -> MulFam,")
othCF2$Energy<-as.factor("Same floor area")
othCF2$enduse<-as.factor("Other")
othCF2$Scenario<-as.factor("CF2")
othCF2$ghg<-othCF2$value*ghgi_avg[1] # ghg emissions in kg co2e
othCF2$ghg2<-othCF2$value*rep(ghgi_oth,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# counterfactual 3, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
othCF3<-oth15u
othCF3[2:5,]<-oth15u[2:5,]+othCF2diffSF # type distribution change
othCF3[4:5,]<-othCF3[4:5,]-oth_red_70_MF # add floor space adjustment change
othCF3<-melt(othCF3)
othCF3$Stock<-as.factor("CF3: LMI SinFam -> MulFam,")
othCF3$Energy<-as.factor("Multifam 70% size of single-fam")
othCF3$enduse<-as.factor("Other")
othCF3$Scenario<-as.factor("CF3")
othCF3$ghg<-othCF3$value*ghgi_avg[1] # ghg emissions in kg co2e
othCF3$ghg2<-othCF3$value*rep(ghgi_oth,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix

# new counterfactual 4, assuming type change SF to MF among LMI households, plus a reduction in size of the new MF to 50% of the size of the average SF
othCF4<-oth15u
othCF4[2:5,]<-oth15u[2:5,]+othCF2diffSF # type distribution change, based on the new LMI TC coefficients
othCF4[4:5,]<-othCF4[4:5,]-oth_red_50_MF # add floor space adjustment change
othCF4<-melt(othCF4)
othCF4$Stock<-as.factor("CF4: LMI SinFam -> MulFam,")
othCF4$Energy<-as.factor("Multifam 50% size of single-fam")
othCF4$enduse<-as.factor("Other")
othCF4$Scenario<-as.factor("CF4")
othCF4$ghg<-othCF4$value*ghgi_avg[1] # ghg emissions in kg co2e
othCF4$ghg2<-othCF4$value*rep(ghgi_oth,6) # ghg in kg co2e. using type specific averages, differences by type due to electricity share, and local elec mix


## plot energy effects for selected scenarios ##################
CF<-rbind(sph0,spc0,dhw0,oth0,sphCF1,spcCF1,dhwCF1,othCF1,sphCF2,spcCF2,dhwCF2,othCF2,sphCF3,spcCF3,dhwCF3,othCF3,sphCF4,spcCF4,dhwCF4,othCF4)

colnames(CF)[1]<-"Type"
colnames(CF)[2]<-"Cohort"
CF$value<-1e-9*CF$value # MJ to PJ
CF$ghg<-1e-9*CF$ghg # kg to mega tons
CF$ghg2<-1e-9*CF$ghg2 # kg to mega tons
CF$houses[1:120]<-matrix(s15u)
CF$houses[121:600]<-matrix(s15u+s15u_diffreg)
CF$GJ_house<-1e6*CF$value/CF$houses

a<-subset(CF,Cohort==c("1980s"))
b<-subset(CF,Cohort==c("1990s"))
c<-subset(CF,Cohort==c("2000+"))
e<-subset(CF,Cohort==c("1970s"))
cf<-rbind(e,a,b,c)
cf2<-cf
levels(cf2$Cohort)<-list("<1950"="<1950","'50-60s"="1950-60s","'70s"="1970s","'80s"="1980s","'90s"="1990s","2000+"="2000+")

cf2<-transform(cf2,Stock=factor(Stock,levels=c("Base: No type change,","CF1: Avg SinFam -> MulFam,","CF2: LMI SinFam -> MulFam,","CF3: LMI SinFam -> MulFam,","CF4: LMI SinFam -> MulFam,")),
               Type=factor(Type,levels=c("Manufactured Home","Multifamily-high","Multifamily-low","Single-family Attached","Single-family Detached" )),
               Scenario=factor(Scenario,levels = c("Base","CF1","CF2","CF3","CF4")),
               enduse=factor(enduse,levels = c("Other","Hot Water","Space Cool","Space Heat")))

cf3<-cf2
pos_region<-levels(cf3$Cohort)[3:6]
r<-ggplot(data=cf3, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") +
  facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r

# Original figure 4 b)
cf4<-cf3
levels(cf4$Stock)[levels(cf4$Stock)=="CF1: Avg SinFam -> MulFam,"] <- "Type mix: No Fed Policy"
levels(cf4$Stock)[levels(cf4$Stock)=="CF2: LMI SinFam -> MulFam," | levels(cf4$Stock)=="CF3: LMI SinFam -> MulFam," | levels(cf4$Stock)=="CF4: LMI SinFam -> MulFam,"] <- "Type mix: No Policy, only LMI hh change"
levels(cf4$Stock)[levels(cf4$Stock)=="Base: No type change,"] <- "Type mix: Actual"

cf4$Labels<-cf4$Stock
levels(cf4$Labels)[levels(cf4$Scenario)=="CF1"] <- "Type switch, LMI hh \n Same size"
levels(cf4$Labels)[levels(cf4$Scenario)=="CF2"] <- "Type switch, avg hh \n Same size"
levels(cf4$Labels)[levels(cf4$Scenario)=="CF3"] <- "Type switch, avg hh \n 30% smaller"
levels(cf4$Labels)[levels(cf4$Scenario)=="CF4"] <- "Type switch, avg hh \n 50% smaller"
levels(cf4$Labels)[levels(cf4$Scenario)=="Base"] <- "2015 type mix"

cf4$Labels<-"2015 type mix"
cf4[cf4$Scenario=="CF1",]$Labels<-"Type switch, LMI hh \n Same size"
cf4[cf4$Scenario=="CF2",]$Labels<-"Type switch, avg hh \n Same size"
cf4[cf4$Scenario=="CF3",]$Labels<- "Type switch, avg hh \n 30% smaller"
cf4[cf4$Scenario=="CF4",]$Labels<-"Type switch, avg hh \n 50% smaller"

pos_region<-levels(cf4$Cohort)[3:6]
r<-ggplot(data=cf4, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + 
  geom_bar(stat="identity") +
  facet_wrap(~Scenario + Stock + Energy, nrow = 1) +
  labs(title="b) Scenarios of urban energy consumption, 2015", x="Cohort",  y="PJ", fill="Type") + #ylim(0,1100) +
  theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
  scale_fill_brewer(palette="Dark2")
windows()
r + theme(legend.position="bottom")
# including detail on end-uses, sum by cohort
cf5<-melt(tapply(cf4$value,list(cf4$Type,cf4$enduse,cf4$Scenario),sum))
colnames(cf5)<-c("Type","enduse","Scenario","value")
r<-ggplot(data=cf5, aes(x=enduse, y=value, fill=Type)) + #scale_x_discrete(limits=levels(cf5$enduse)) + 
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Scenario, nrow = 1) +
  labs(title="b) Scenarios of urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type") + #ylim(0,1100) +
  theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
  scale_fill_brewer(palette="Dark2")
windows()
r + theme(legend.position="bottom")

cf4[cf4$Type=="Manufactured Home",]$value<-0
cf4$lab<-1
cf4[cf4$Scenario=="CF1",]$lab<-2
cf4[cf4$Scenario=="CF2",]$lab<-3
cf4[cf4$Scenario=="CF3",]$lab<-4
cf4[cf4$Scenario=="CF4",]$lab<-5
# New figure 4 b) simple summary version reorder(IncHHS,or)
r<-ggplot(data=cf4, aes(x=reorder(Labels,lab), y=value, fill=Type))  + geom_bar(stat="identity",width = 0.7) + ylim(0,4200) +
  labs(title="b) Scenarios of urban energy consumption, post-1970 urban housing, 2015", x="Energy Scenario",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=18,face = "bold"),axis.text = element_text(size = 14),axis.title=element_text(size=14,face = "bold")) +
  scale_fill_brewer(palette="Dark2") + geom_text(x=cf4$lab,y=rep(4200,400),label=cf4$Scenario,size=5)
windows()
r + theme(legend.position="none")  

# GHG version of figure 1 b), for appendix
r<-ggplot(data=cf4, aes(x=Cohort, y=ghg, fill=Type)) + scale_x_discrete(limits=pos_region) + 
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Scenario + Stock + Energy, nrow = 1) +
  labs(title="b) Scenarios of urban GHG emissions by Type-Cohort, 2015", x="Cohort",  y="Mega Ton CO2-eq", fill="Type") +
  theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
  scale_fill_brewer(palette="Dark2")
windows()
r + theme(legend.position="bottom") 

# GHG version of figure 1 b), for appendix, this time with type-specific ghg emissions factors by end-use
r<-ggplot(data=cf4, aes(x=Cohort, y=ghg2, fill=Type)) + scale_x_discrete(limits=pos_region) + 
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Scenario + Stock + Energy, nrow = 1) +
  labs(title="b) Scenarios of urban GHG emissions by Type-Cohort, 2015", x="Cohort",  y="Mega Ton CO2-eq", fill="Type") +
  theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
  scale_fill_brewer(palette="Dark2")
windows()
r + theme(legend.position="bottom") 

## comparisons by CF ######################
stock_change<-sum(s15u_diffreg[2:3,])
stock_change_pc<-stock_change/sum(s15u)

base_PJ<-sum(CF[CF$Scenario=="Base",]$value)
# base_PJ<- sum(CF[CF$Scenario=="Base" & !CF$Type=="Manufactured Home",]$value)# optional definition excluding MH
sum(CF[CF$Scenario=="Base" & !CF$Type=="Manufactured Home",]$value)
CF1_PJ<-sum(CF[CF$Scenario=="CF1",]$value)
CF1_red<-base_PJ-CF1_PJ
CF1_red_pc<-CF1_red/base_PJ # total % reduction in 2015 urban energy in CF1

base_GHG<-sum(CF[CF$Scenario=="Base",]$ghg2) # the house-type specific cfs are likely more accurate
CF1_GHG<-sum(CF[CF$Scenario=="CF1",]$ghg2)
CF1_red_GHG<-base_GHG-CF1_GHG
CF1_red_GHG_pc<-CF1_red_GHG/base_GHG # total % reduction in 2015 urban energy in CF1

## changes in/per affected households 
E_avg_aff<--1*sum((s15u_diffreg*eph15u)[2:3,])/del_stock # energy consumption MJ in average affected house
G_avg_aff<--1*sum((s15u_diffreg*ghgph15u)[2:3,])/del_stock # ghg emissions in average affected house, in kg
CF1_ph<-CF1_red*1e9/del_stock # energy reduction from CF1 per affected house, MJ
CF1_red_ph_pc<-CF1_ph/E_avg_aff # percentage energy reduction in CF1 per affected house, 27%

CF1_g_ph<-CF1_red_GHG*1e9/del_stock # ghg reduction in CF 1 per affected house
CF1_g_red_ph_pc<-CF1_g_ph/G_avg_aff # percentage GHG reduction in CF1 per affected house, 26%

CF2_PJ<-sum(CF[CF$Scenario=="CF2",]$value)
CF2_red<-base_PJ-CF2_PJ
CF2_red_pc<-CF2_red/base_PJ # total % reduction in 2015 urban energy in CF2
CF2_ph<-CF2_red*1e9/del_stock # reduction from CF2 per affected house
CF2_red_ph_pc<-CF2_ph/E_avg_aff # percentage energy reduction in CF1 per affected house, 39%
CF2_GHG<-sum(CF[CF$Scenario=="CF2",]$ghg2)
CF2_red_GHG<-base_GHG-CF2_GHG
CF2_red_GHG_pc<-CF2_red_GHG/base_GHG # total % reduction in 2015 urban ghg in CF2
CF2_g_ph<-CF2_red_GHG*1e9/del_stock # ghg reduction in CF 2 per affected house
CF2_g_red_ph_pc<-CF2_g_ph/G_avg_aff # percentage GHG reduction in CF2 per affected house, 38%


CF3_PJ<-sum(CF[CF$Scenario=="CF3",]$value)
CF3_red<-base_PJ-CF3_PJ
CF3_red_pc<-CF3_red/base_PJ # total % reduction in 2015 urban energy in CF3
CF3_ph<-CF3_red*1e9/del_stock # reduction from CF3 per affected house
CF3_red_ph_pc<-CF3_ph/E_avg_aff

CF4_PJ<-sum(CF[CF$Scenario=="CF4",]$value)
CF4_red<-base_PJ-CF4_PJ
CF4_red_pc<-CF4_red/base_PJ # total % reduction in 2015 urban energy in CF4
CF4_ph<-CF4_red*1e9/del_stock # reduction from CF4 per affected house
CF4_red_ph_pc<-CF4_ph/E_avg_aff

# changes in end-use per scenario, in PJ
redSPHcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$value)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Space Heat",]$value)
redSPHcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$value)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Heat",]$value)
redSPHcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$value)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Space Heat",]$value)
redSPHcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$value)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Heat",]$value)

redSPCcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$value)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Space Cool",]$value)
redSPCcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$value)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Cool",]$value)
redSPCcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$value)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Space Cool",]$value)
redSPCcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$value)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Cool",]$value)

redDHWcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$value)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Hot Water",]$value)
redDHWcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$value)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Hot Water",]$value)
redDHWcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$value)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Hot Water",]$value)
redDHWcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$value)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Hot Water",]$value)

redOTHcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$value)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Other",]$value)
redOTHcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$value)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Other",]$value)
redOTHcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$value)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Other",]$value)
redOTHcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$value)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Other",]$value)

# changes in end-use GHG per scenario, in Mega t CO2
redSPH_GHGcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$ghg2)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Space Heat",]$ghg2)
redSPH_GHGcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$ghg2)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Heat",]$ghg2)
redSPH_GHGcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$ghg2)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Space Heat",]$ghg2)
redSPH_GHGcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Heat",]$ghg2)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Heat",]$ghg2)

redSPC_GHGcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$ghg2)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Space Cool",]$ghg2)
redSPC_GHGcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$ghg2)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Cool",]$ghg2)
redSPC_GHGcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$ghg2)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Space Cool",]$ghg2)
redSPC_GHGcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Space Cool",]$ghg2)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Cool",]$ghg2)

redDHW_GHGcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$ghg2)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Hot Water",]$ghg2)
redDHW_GHGcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$ghg2)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Hot Water",]$ghg2)
redDHW_GHGcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$ghg2)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Hot Water",]$ghg2)
redDHW_GHGcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Hot Water",]$ghg2)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Hot Water",]$ghg2)

redOTH_GHGcf1<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$ghg2)-sum(CF[CF$Scenario=="CF1" & CF$enduse=="Other",]$ghg2)
redOTH_GHGcf2<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$ghg2)-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Other",]$ghg2)
redOTH_GHGcf3<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$ghg2)-sum(CF[CF$Scenario=="CF3" & CF$enduse=="Other",]$ghg2)
redOTH_GHGcf4<-sum(CF[CF$Scenario=="Base" & CF$enduse=="Other",]$ghg2)-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Other",]$ghg2)

sphcf1sf<-sum(rowSums(sph15uhyp2-sph15u)[2:3])*1e-9
sphcf1mf<-sum(rowSums(sph15uhyp2-sph15u)[4:5])*1e-9
sphcf1sfpc<-sphcf1sf/(sum(sph15u)*1e-9)
sphcf1mfpc<-sphcf1mf/(sum(sph15u)*1e-9)

spccf1sf<-sum(rowSums(spc15uhyp2-spc15u)[2:3])*1e-9
spccf1mf<-sum(rowSums(spc15uhyp2-spc15u)[4:5])*1e-9
spccf1sfpc<-spccf1sf/(sum(spc15u)*1e-9)
spccf1mfpc<-spccf1mf/(sum(spc15u)*1e-9)

dhwcf1sf<-sum(rowSums(dhw15uhyp2-dhw15u)[2:3])*1e-9
dhwcf1mf<-sum(rowSums(dhw15uhyp2-dhw15u)[4:5])*1e-9
dhwcf1sfpc<-dhwcf1sf/(sum(dhw15u)*1e-9)
dhwcf1mfpc<-dhwcf1mf/(sum(dhw15u)*1e-9)

othcf1sf<-sum(rowSums(oth15uhyp2-oth15u)[2:3])*1e-9
othcf1mf<-sum(rowSums(oth15uhyp2-oth15u)[4:5])*1e-9
othcf1sfpc<-othcf1sf/(sum(oth15u)*1e-9)
othcf1mfpc<-othcf1mf/(sum(oth15u)*1e-9)

cf1sf<-sum(sphcf1sf,spccf1sf,dhwcf1sf,othcf1sf)
cf1mf<-sum(sphcf1mf,spccf1mf,dhwcf1mf,othcf1mf)
cf1sf_pc<-cf1sf/sum(1e-9*e15u)
cf1mf_pc<-cf1mf/sum(1e-9*e15u)

# how do the average energy intensities of houses added and removed compare with the original average intensity?
ei_sfrem<-1e6*cf1sf/stock_change
ei_mfadd<-1e6*cf1mf/stock_change
ei_avg<-sum(1e-3*e15u)/sum(s15u)
avg_red_cf_house<-1+ei_mfadd/ei_sfrem
# how do the reductions vary by end-use and scenario?
red_use_scen<-tapply(cf2$value,list(cf2$enduse,cf2$Scenario),sum)-tapply(cf2$value,list(cf2$enduse,cf2$Scenario),sum)[,1]

# check feasibility of scenarios regarding offsets
# to do this I need to compare e.g. space heat in SFD in CF4 vs CF2, can look as specific cohorts too
# if the offsets are not large enough, the sum will give a negative number, if too large, will be positive
sfdsphcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Heat"& CF$Type=="Single-family Detached",]$value)
sfdsphcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Heat"& CF$Type=="Single-family Detached",]$value)
sfdsphcf4-sfdsphcf2

sfdspccf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Cool"& CF$Type=="Single-family Detached",]$value)
sfdspccf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Cool"& CF$Type=="Single-family Detached",]$value)
sfdspccf4-sfdspccf2

sfddhwcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Hot Water"& CF$Type=="Single-family Detached",]$value)
sfddhwcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Hot Water"& CF$Type=="Single-family Detached",]$value)
sfddhwcf4-sfddhwcf2

sfdothcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Other"& CF$Type=="Single-family Detached",]$value)
sfdothcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Other"& CF$Type=="Single-family Detached",]$value)
sfdothcf4-sfdothcf2
#
sfasphcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Heat"& CF$Type=="Single-family Attached",]$value)
sfasphcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Heat"& CF$Type=="Single-family Attached",]$value)
sfasphcf4-sfasphcf2

sfaspccf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Cool"& CF$Type=="Single-family Attached",]$value)
sfaspccf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Cool"& CF$Type=="Single-family Attached",]$value)
sfaspccf4-sfaspccf2

sfadhwcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Hot Water"& CF$Type=="Single-family Attached",]$value)
sfadhwcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Hot Water"& CF$Type=="Single-family Attached",]$value)
sfadhwcf4-sfadhwcf2

sfaothcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Other"& CF$Type=="Single-family Attached",]$value)
sfaothcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Other"& CF$Type=="Single-family Attached",]$value)
sfaothcf4-sfaothcf2
#### graph of average affected house ####### 
# graph showing energy consumption in average SF since 1986 and energy if those houses
# were a) converted to MF and b) reduced in size by 30%. in GJ
rCF<-matrix(rep(0,54),6,9) # matrix showing reductions from four scenarios and four resstock measures
rCF[1]<-E_avg_aff*1e-3 # convert to GJ
rCF[2:5,2]<-1e6*c(redSPHcf1,redSPCcf1,redDHWcf1,redOTHcf1)/del_stock # convert from PJ to GJ
rCF[1,2]<-rCF[1]-sum(rCF[2:5,2])
rCF[2:5,3]<-1e6*c(redSPHcf2,redSPCcf2,redDHWcf2,redOTHcf2)/del_stock
rCF[1,3]<-rCF[1]-sum(rCF[2:5,3])
# rCF[2:5,4]<-1e-3*c(sum(sph_red_70_MF),sum(spc_red_70_MF),sum(dhw_red_70_MF),sum(oth_red_70_MF))/del_stock
rCF[2:5,4]<-1e6*c(redSPHcf3,redSPCcf3,redDHWcf3,redOTHcf3)/del_stock
rCF[1,4]<-rCF[1]-sum(rCF[2:5,4])
# rCF[2:5,5]<-1e-3*c(sum(sph_red_50_MF),sum(spc_red_50_MF),sum(dhw_red_50_MF),sum(oth_red_50_MF))/del_stock
rCF[2:5,5]<-1e6*c(redSPHcf4,redSPCcf4,redDHWcf4,redOTHcf4)/del_stock
rCF[1,5]<-rCF[1]-sum(rCF[2:5,5])

# reductions in SF housing from ResStock model, adapted to GJ final energy
rCF[6,6]<-1.755 # avg hh reduction (GJ) from switch to LED lighting
rCF[6,7]<-3.73 # avg hh reduction (GJ) from switching electric furnace to VSHP at wear out
rCF[6,8]<-9.092  # avg hh reduction (GJ) from drill/fill cavity wall insulation
rCF[6,9]<-11.025  # avg hh reduction (GJ) from all economic electricity efficiency measures

rCFpc<-rCF*100/rCF[1]
colnames(rCFpc)<-c("Base","Type switch LMI hh \n same size","Type switch avg hh \n same size", 
                   "Type switch avg hh \n 30% smaller","Type switch avg hh\n 50% smaller","LED lighting","Elec. furnace \n to VSHP","Cavity-wall \n insulation","All economic \n elec. savings")
rownames(rCFpc)<-c("Total consumption","Space heating","Space cooling","Domestic hot water","Other end-use","ResStock efficiency measures")
comp<-melt(rCFpc[2:6,2:9])
colnames(comp)<-c("Source","Scenario","Reduction")
comp$Scen<-""
comp[comp$Scenario=="Type switch LMI hh \n same size",]$Scen<-"CF1"
comp[comp$Scenario=="Type switch avg hh \n same size",]$Scen<-"CF2"
comp[comp$Scenario=="Type switch avg hh \n 30% smaller",]$Scen<-"CF3"
comp[comp$Scenario=="Type switch avg hh\n 50% smaller",]$Scen<-"CF4"
comp$lab<-rep(c(1:8),each=5)

# Figure 5
p <- ggplot(data=comp, aes(x = Scenario, y = Reduction))+ ylim(0,49)+
  geom_col(aes(fill = Source), width = 0.75) +
  labs(title = "Comparison of energy reduction strategies for single-family households", y = "Energy Reduction (%)", x="Reduction strategy") + theme_bw() +
  theme(axis.text=element_text(size=11.5),
        axis.title=element_text(size=13,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Set1") + geom_text(x=comp$lab,y=rep(49,40),label=comp$Scen)
windows()
p + theme(legend.position="bottom",legend.text=element_text(size=11))  

rCF_GHG<-matrix(rep(0,25),5,5)
rCF_GHG[1]<-G_avg_aff # in kg
rCF_GHG[2:5,2]<-1e9*c(redSPH_GHGcf1,redSPC_GHGcf1,redDHW_GHGcf1,redOTH_GHGcf1)/del_stock
rCF_GHG[1,2]<-rCF_GHG[1]-sum(rCF_GHG[2:5,2])
rCF_GHG[2:5,3]<-1e9*c(redSPH_GHGcf2,redSPC_GHGcf2,redDHW_GHGcf2,redOTH_GHGcf2)/del_stock
rCF_GHG[1,3]<-rCF_GHG[1]-sum(rCF_GHG[2:5,3])
rCF_GHG[2:5,4]<-1e9*c(redSPH_GHGcf3,redSPC_GHGcf3,redDHW_GHGcf3,redOTH_GHGcf3)/del_stock
rCF_GHG[1,4]<-rCF_GHG[1]-sum(rCF_GHG[2:5,4])
rCF_GHG[2:5,5]<-1e9*c(redSPH_GHGcf4,redSPC_GHGcf4,redDHW_GHGcf4,redOTH_GHGcf4)/del_stock
rCF_GHG[1,5]<-rCF_GHG[1]-sum(rCF_GHG[2:5,5])

colnames(rCF_GHG)<-c("Base","CF1: LMI hh type switch \n same size","CF2: Avg hh type switch \n same size", 
                       "CF3: Avg hh type switch \n 30% smaller","CF4: Avg hh type switch \n 50% smaller")
rownames(rCF_GHG)<-c("Total consumption","Space heating","Space cooling","Domestic hot water","Other end-use")
# appendix figure S4
compGHGabs<-melt(rCF_GHG[2:5,2:5])
colnames(compGHGabs)<-c("Source","Scenario","Reduction")
compGHGabs$Scen<-substr(compGHGabs$Scenario,1,3)
p <- ggplot(data=compGHGabs, aes(x = Scen, y = Reduction))+ #ylim(0,42)+
  geom_col(aes(fill = Source), width = 0.75) +
  labs(title = "a) Absolute GHG reductions in single-family homes, by scenario", y = "GHG Reduction (kg CO2-eq)", x="Reduction strategy") + theme_bw() +
  theme(axis.text=element_text(size=11.5),
        axis.title=element_text(size=13,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Set1")
windows()
p + theme(legend.position="bottom",legend.text=element_text(size=11))  

rCF_GHGpc<-rCF_GHG*100/rCF_GHG[1]
compGHG<-melt(rCF_GHGpc[2:5,2:5])
colnames(compGHG)<-c("Source","Scenario","Reduction")
compGHG$Scen<-substr(compGHG$Scenario,1,3)

p <- ggplot(data=compGHG, aes(x = Scen, y = Reduction))+ #ylim(0,42)+
  geom_col(aes(fill = Source), width = 0.75) +
  labs(title = "b) Percentage GHG reductions in single-family homes, by scenario", y = "GHG Reduction (%)", x="Reduction strategy") + theme_bw() +
  theme(axis.text=element_text(size=11.5),
        axis.title=element_text(size=13,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Set1")
windows()
p + theme(legend.position="bottom",legend.text=element_text(size=11))  