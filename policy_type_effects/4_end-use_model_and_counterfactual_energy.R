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
load("RECSurbanRegData.Rdata")
load("sdiff.Rdata")
ru15<-ru2[ru2$RECSYEAR=="2015",]
ru15$INC<-ru15$INC/0.5672
rusf15<-ru15[ru15$TYPEHUQ=="SF Det",]
rusmall<-ru2[,c("BTUSPH","NWEIGHT","TYPEHUQ","Cohort","AgeCohort","HDD65","NHSLDMEM","TOTHSQFT","TOTCSQFT","INC","FUELHEAT","RECSYEAR","HEATPRICE","CDD65","BTUCOL","BTUDHW","BTUOTH","TOTSQFT","ACROOMS","TOTROOMS","NCOMBATH","WHEATSIZ","FUELH2O","POOL","RECBATH")]
# create variable TC which defines combinations of house types and cohort
rusmall$TC<-as.factor(paste(rusmall$TYPEHUQ,rusmall$AgeCohort))

rus15<-rusmall[rusmall$RECSYEAR=="2015",]
rus15$INC<-0.001*rus15$INC/0.5672 # reconvert income data from 1990 dollars to 2015 dollars
rus15$BTUSPH<-rus15$BTUSPH*1.055 # convert kBTU to MJ
rus15$BTUCOL<-rus15$BTUCOL*1.055 # convert kBTU to MJ
rus15$BTUDHW<-rus15$BTUDHW*1.055 # convert kBTU to MJ
rus15$BTUOTH<-rus15$BTUOTH*1.055 # convert kBTU to MJ
rus15$BTUTOT<-rus15$BTUSPH+rus15$BTUCOL+rus15$BTUDHW+rus15$BTUOTH
rus15$TYPE<-"MF"
rus15[rus15$TYPEHUQ=="SF Det" | rus15$TYPEHUQ=="SF Att",]$TYPE<-"SF"
rus15[rus15$TYPEHUQ=="Man Housing",]$TYPE<-"MH"
rus15$TYPE<-as.factor(rus15$TYPE)
rus15$count<-1
rus15$OldNew<-"Old"
rus15[rus15$AgeCohort=="1990s" | rus15$AgeCohort=="2000s" | rus15$AgeCohort=="2010s",]$OldNew<-"New"
rus15$OldNew<-as.factor(rus15$OldNew)
rus15<-within(rus15,TC<-relevel(TC, ref = "MF high 2010s")) # make high-density multifamily houses built in 2010s as the reference level for TC

eph15<-tapply(rus15$BTUTOT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT,rus15$TYPE,sum)
eph15[3]/eph15[1]

faph15<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT,rus15$TYPE,sum)
faph15[3]/faph15[1]

# heating models ###############
hh_heat<-lme(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + RECSYEAR, data = rusmall, random = ~  1 | TC)
hh_heat15<-lme(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + TC, rus15)

yhat_fe<-hh_heat15_fe[["fitted.values"]]
tab_model(hh_heat,hh_heat15,hh_heat15_fe,show.ci = 0)
tab_model(hh_heat15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTHSQFT","INC","NHSLDMEM"))
heat_TC<-hh_heat15_fe[["coefficients"]][6:44]

# hausman test ###############################
fe1<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM,data = rus15,model = "within",index = c("TC")) # need to include index, but doesn't come out in model results
re1<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM,data = rus15,model = "random",index = c("TC"))
phtest(fe1,re1)
# Hausman test rejects the hypothesis that the random effects model is consistent, and therefore we prefer the fixed effects model

# calculate counterfactual heating energy ###################################
sphFE15<-matrix(c(heat_TC[1:15],0,heat_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
sphp<-summary(hh_heat15_fe)[["coefficients"]][6:44,4] # extract p values
sphpmat<-matrix(c(sphp[1:15],0,sphp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

sphFEoff<-1.03*median(rus15$BTUSPH)
sphFE15<-sphFE15+sphFEoff # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

row.names(sphFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(sphpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
t2<-matrix(1:40,5,8)
row.names(t2)<-c("Man Housing","SF Det","SF Att","MF low","MF high")
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
x$Offset<-x$Offset-sphFEoff
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
# 
red_sphFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 

# This now calculates energy consumption if all of the houses had the same socioeconomic and physical characteristics.
# Probably if households moved into MF houses, the houses would have to be smaller on average. 
# Here we calculate the effect of that, reduction of average house size in addition to changing type.

# average heated floor area by house type and new (built in or after 1990) / old (built before 1990)
HFA_TC<-tapply(rus15$TOTHSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)

# extract average heated floor area for 'new' SF and MF houses
HFA_SF<-HFA_TC[3,1]
HFA_MF<-HFA_TC[2,1]
beta_hsf<-hh_heat15_fe[["coefficients"]][["TOTHSQFT"]]
del_stock<-sum(s15u_diffreg[4:5,])
# total space heating reduction if new MF houses were 80%, 70%, 60% size of SF
sph_red_80<-0.2*HFA_SF*beta_hsf*del_stock*1e-9 # MJ to PJ
sph_red_70<-0.3*HFA_SF*beta_hsf*del_stock*1e-9
sph_red_60<-0.4*HFA_SF*beta_hsf*del_stock*1e-9
# sph_type_red70<-red_sphFE-sph_red_70
# changes in modern MF/SF if houses were 30%, 50% smaller than average SF, by type and cohort
sph_red_70_MF<-0.3*HFA_SF*beta_hsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
sph_red_70_SF<-0.3*HFA_SF*beta_hsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
sph_red_50_MF<-0.5*HFA_SF*beta_hsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number

# average consumption of space heating and housing characteristics by type and cohort
hh_heatRE <- ranef(hh_heat, aug = TRUE)

# cooling models #####################
hh_cool<-lme(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_cool15<-lme(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_cool15_fe<-lm(BTUCOL~CDD65 + TOTCSQFT + INC + NHSLDMEM + TC, rus15)


# summary regression tables of cooling models
tab_model(hh_cool,hh_cool15,hh_cool15_fe,show.ci = FALSE)
tab_model(hh_cool15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","CDD65","TOTCSQFT","INC","NHSLDMEM"))
# effects of TC on cooling
cool_TC<-hh_cool15_fe[["coefficients"]][6:44]

spcFE15<-matrix(c(cool_TC[1:15],0,cool_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
spcp<-summary(hh_cool15_fe)[["coefficients"]][6:44,4] # extract p values
spcpmat<-matrix(c(spcp[1:15],0,spcp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

spcFEoff<-2*median(spcFE15)
spcFE15<-spcFE15+spcFEoff# alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

row.names(spcFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(spcpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
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
x$Offset<-x$Offset-spcFEoff # remove alteration to fixed effects
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
# reduction in space cooling with counterfactual type-cohort distribution
red_spcFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 

# average cooled floor area by house type and new (built in or after 1990) / old (built before 1990)
CFA_TC<-tapply(rus15$TOTCSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
# extract average cooled floor area for 'new' SF and MF houses
CFA_SF<-CFA_TC[3,1]
CFA_MF<-CFA_TC[2,1]
beta_csf<-hh_cool15_fe[["coefficients"]][["TOTCSQFT"]]

# total space cooling reduction if new MF houses were 80%, 70%, 60% size of SF
spc_red_80<-0.2*CFA_SF*beta_csf*del_stock*1e-9 # MJ to PJ
spc_red_70<-0.3*CFA_SF*beta_csf*del_stock*1e-9
spc_red_60<-0.4*CFA_SF*beta_csf*del_stock*1e-9
# spc_type_red70<-red_spcFE-spc_red_70
# changes in modern MF/SF if houses were 30%, 50% smaller than average SF, by type and cohort
spc_red_70_MF<-0.3*CFA_SF*beta_csf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
spc_red_70_SF<-0.3*CFA_SF*beta_csf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
spc_red_50_MF<-0.5*CFA_SF*beta_csf*s15u_diffreg[4:5,]  # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number

# hot water models ##################
hh_dhw<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_dhw15<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_dhw15_fe<-lm(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + TC, rus15)
# summary regression tables
tab_model(hh_dhw,hh_dhw15,hh_dhw15_fe,show.ci = FALSE)
tab_model(hh_dhw15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTSQFT","INC","NHSLDMEM"))
# effects of TC on hot water
dhw_TC<-hh_dhw15_fe[["coefficients"]][6:44]

dhwFE15<-matrix(c(dhw_TC[1:15],0,dhw_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
dhwp<-summary(hh_dhw15_fe)[["coefficients"]][6:44,4] # extract p values
dhwpmat<-matrix(c(dhwp[1:15],0,dhwp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

dhwFEoff<-1.1*median(rus15$BTUDHW)
dhwFE15<-dhwFE15+dhwFEoff # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

row.names(dhwFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(dhwpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
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
x$Offset<-x$Offset-dhwFEoff
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

# reduction in space dhwing with alternative cohort 
red_dhwFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 
# 
SQFT_all<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)
# average total floor area by house type and new (built in or after 1990) / old (built before 1990)
TFA_TC<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
# extract average total floor area for 'new' SF and MF houses
TFA_SF<-TFA_TC[3,1]
TFA_MF<-TFA_TC[2,1]
# compare size and energy consumption of SF and MF
TFA_T<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE),sum)
TFA_T[3]/TFA_T[2] # size ratio for SF to MF
E_T<-tapply(rus15$BTUTOT*rus15$NWEIGHT,list(rus15$TYPE),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE),sum)
E_T[3]/E_T[2] # energy  ratio for SF to MF
# floor area coefficient for dhw
beta_sqft<- hh_dhw15_fe[["coefficients"]][["TOTSQFT"]]

# total hot water reduction if new MF houses were 80%, 70%, 60% size of SF
dhw_red_80<-0.2*TFA_SF*beta_sqft*del_stock*1e-9
dhw_red_70<-0.3*TFA_SF*beta_sqft*del_stock*1e-9
dhw_red_60<-0.4*TFA_SF*beta_sqft*del_stock*1e-9
dhw_type_red70<-red_dhwFE-dhw_red_70
dhw_red_70_MF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
dhw_red_70_SF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
dhw_red_50_MF<-0.5*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number

# new other energy models ##################
hh_oth<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_oth15<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
hh_oth15_fe<-lm(BTUOTH~TOTSQFT + INC + NHSLDMEM + TC, rus15)
tab_model(hh_oth,hh_oth15,hh_oth15_fe,show.ci = FALSE)
tab_model(hh_oth15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","TOTSQFT","INC","NHSLDMEM"))
# summary regression tables
tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,
          terms = c("(Intercept)","HDD65","CDD65","TOTHSQFT","TOTSQFT","INC","NHSLDMEM","TOTCSQFT"))
tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,digits = 0,show.p = FALSE)

oth_TC<-hh_oth15_fe[["coefficients"]][5:43]

othFE15<-matrix(c(oth_TC[1:15],0,oth_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
othp<-summary(hh_oth15_fe)[["coefficients"]][5:43,4] # extract p values
othpmat<-matrix(c(othp[1:15],0,othp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
othFEoff<-1.05*mean(rus15$BTUOTH) # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged
othFE15<-othFE15+othFEoff

row.names(othFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(othpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
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

x$Offset<-x$Offset-othFEoff
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x4<-x
x4$Enduse<-"Other"

# ggplot
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, other",x="Coefficient (MJ)") +
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q

red_othFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 
# coefficient of total floor area for other end uses
beta_sqft<- hh_oth15_fe[["coefficients"]][["TOTSQFT"]]

# total other end-use reduction if new MF houses were 80%, 70%, 60% size of SF
oth_red_80<-0.2*TFA_SF*beta_sqft*del_stock*1e-9
oth_red_70<-0.3*TFA_SF*beta_sqft*del_stock*1e-9
oth_red_60<-0.4*TFA_SF*beta_sqft*del_stock*1e-9
# oth_type_red70<-red_othFE-oth_red_70
oth_red_70_MF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
oth_red_70_SF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
oth_red_50_MF<-0.5*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number

## fig 3 plot ##########
X<-rbind(x1,x2,x3,x4)
# levels(x)[levels(X)==]
X$TYPEHUQ<-revalue(X$TYPEHUQ, c("SF Det"="Single-family Detached","SF Att"="Single-family Attached","MF low"="Multifamily low-density","MF high"="Multifamily high-density"))
X$Signif<-"p>0.05"
X[X$p<0.05,]$Signif<-"p<0.05"
X$TYPE<-"Single-family"
X[X$TYPEHUQ=="Multifamily high-density" | X$TYPEHUQ=="Multifamily low-density",]$TYPE<-"Multifamily"
X[X$TYPEHUQ=="Man Housing",]$TYPE<-"Man Housing"
XX<-X[!X$TYPEHUQ=="Man Housing",]
XXX<-X[X$TYPEHUQ=="Multifamily high-density" | X$TYPEHUQ=="Single-family Detached" ,]

q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + 
  geom_point(aes(colour = factor(Enduse),size=Signif),size=XX$Sig) + 
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) +
  labs(title = "House type and cohort effects",x="Effect on energy end-use consumption (MJ)",colour = 'End-use') +  
  theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
                                                           legend.background = element_rect(color="gray45"),legend.title.align=0.5,plot.title = element_text(face = "bold"))
windows()
q 

# load and convert original and stock counterfactual energy consumption data ##############
load("end_uses_CF.RData")
# convert from kBTU to MJ
sph15u<-1.055*sph15u
spc15u<-1.055*spc15u
dhw15u<-1.055*dhw15u
oth15u<-1.055*oth15u

sph15uhyp2<-1.055*sph15uhyp2
spc15uhyp2<-1.055*spc15uhyp2
dhw15uhyp2<-1.055*dhw15uhyp2
oth15uhyp2<-1.055*oth15uhyp2

e15u<-1.055*e15u
eph15u<-1.055*eph15u
# plot CF effects ########
row.names(sph15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(spc15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(dhw15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(oth15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")

row.names(sph15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(spc15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(dhw15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(oth15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")

# base case
sph0<-melt(sph15u)
sph0$Stock<-as.factor("Base: No type change,")
sph0$Energy<-as.factor("Same floor area")
sph0$enduse<-as.factor("Space Heat")
sph0$Scenario<-as.factor("Base") 

# counterfactual 1, assuming type change from SF to MF, but no change in floor area
sphCF2<-sph15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15) # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
sphCF2<-melt(sphCF2)
sphCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
sphCF2$Energy<-as.factor("Same floor area")
sphCF2$enduse<-as.factor("Space Heat")
sphCF2$Scenario<-as.factor("CF1")
sphCF2diffSF<-sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15)

# counterfactual 2, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
sphCF3<-sph15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15) # type distribution change
sphCF3[4:5,]<-sphCF3[4:5,]-sph_red_70_MF # add floor space adjustment change
sphCF3<-melt(sphCF3)
sphCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
sphCF3$Energy<-as.factor("Multifam 70% size of single-fam")
sphCF3$enduse<-as.factor("Space Heat")
sphCF3$Scenario<-as.factor("CF2")

# counterfactual 4, assuming type change SFto MF, with a fixed energy intensity, i.e. the average MF intensity does not change when N million extra households live in MF instead of SF
sphCF1<-melt(sph15uhyp2)
sphCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
sphCF1$Energy<-as.factor("Fixed energy intensity")
sphCF1$enduse<-as.factor("Space Heat")
sphCF1$Scenario<-as.factor("CF4")
sphCF1diffSF<-(sum(sph15u[2:3,])-sum(sph15uhyp2[2:3,]))*1e-9

# counterfactual 3, no type shift, but a reduction in size of the same SF that are affected by the type counterfactual by 30% of the size of the average SF
sphCF4<-sph15u
sphCF4[2:3,]<-sphCF4[2:3,]+sph_red_70_SF  # add floor space adjustment change
sphCF4<-melt(sphCF4)
sphCF4$Stock<-as.factor("CF3: No type change,")
sphCF4$Energy<-as.factor("Smaller SF (70%)")
sphCF4$enduse<-as.factor("Space Heat")
sphCF4$Scenario<-as.factor("CF3")

# counterfactual 5, assuming type change SF to MF, plus a reduction in size of the new MF to 50% of the size of the average SF
sphCF5<-sph15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15) # type distribution change
sphCF5[4:5,]<-sphCF5[4:5,]-sph_red_50_MF # add floor space adjustment change
sphCF5<-melt(sphCF5)
sphCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
sphCF5$Energy<-as.factor("MulFam 50% size of SinFam")
sphCF5$enduse<-as.factor("Space Heat")
sphCF5$Scenario<-as.factor("CF5")

# space cooling scenarios
# base case
spc0<-melt(spc15u)
spc0$Stock<-as.factor("Base: No type change,")
spc0$Energy<-as.factor("Same floor area")
spc0$enduse<-as.factor("Space Cool")
spc0$Scenario<-as.factor("Base")

# counterfactual 1, assuming type change from SF to MF, but no change in floor area
spcCF2<-spc15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15) # calculate urban energy consumption by type/cohort, by adding to the actual data the product of the stock change (# houses by type-cohort) and the adjusted (as described above) offsets by type-cohort
spcCF2<-melt(spcCF2)
spcCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
spcCF2$Energy<-as.factor("Same floor area")
spcCF2$enduse<-as.factor("Space Cool")
spcCF2$Scenario<-as.factor("CF1")

# counterfactual 2, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
spcCF3<-spc15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15)
spcCF3[4:5,]<-spcCF3[4:5,]-spc_red_70_MF
spcCF3<-melt(spcCF3)
spcCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
spcCF3$Energy<-as.factor("Multifam 70% size of single-fam")
spcCF3$enduse<-as.factor("Space Cool")
spcCF3$Scenario<-as.factor("CF2")

# counterfactual 3, no type shift, but a reduction in size of the same SF that are affected by the type counterfactual by 30% of the size of the average SF
spcCF4<-spc15u
spcCF4[2:3,]<-spcCF4[2:3,]+spc_red_70_SF
spcCF4<-melt(spcCF4)
spcCF4$Stock<-as.factor("CF3: No type change,")
spcCF4$Energy<-as.factor("Smaller SF (70%)")
spcCF4$enduse<-as.factor("Space Cool")
spcCF4$Scenario<-as.factor("CF3")

# counterfactual 4, assuming type change SFto MF, with a fixed energy intensity, i.e. the average MF intensity does not change when N million extra households live in MF instead of SF
spcCF1<-melt(spc15uhyp2)
spcCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
spcCF1$Energy<-as.factor("Fixed energy intensity")
spcCF1$enduse<-as.factor("Space Cool")
spcCF1$Scenario<-as.factor("CF4")

# counterfactual 5, assuming type change SF to MF, plus a reduction in size of the new MF to 50% of the size of the average SF
spcCF5<-spc15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15)
spcCF5[4:5,]<-spcCF5[4:5,]-spc_red_50_MF
spcCF5<-melt(spcCF5)
spcCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
spcCF5$Energy<-as.factor("MulFam 50% size of SinFam")
spcCF5$enduse<-as.factor("Space Cool")
spcCF5$Scenario<-as.factor("CF5")

# hot water scenarios
# base case
dhw0<-melt(dhw15u)
dhw0$Stock<-as.factor("Base: No type change,")
dhw0$Energy<-as.factor("Same floor area")
dhw0$enduse<-as.factor("Hot Water")
dhw0$Scenario<-as.factor("Base")

# counterfactual 1, assuming type change from SF to MF, but no change in floor area
dhwCF2<-dhw15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15)
dhwCF2<-melt(dhwCF2)
dhwCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
dhwCF2$Energy<-as.factor("Same floor area")
dhwCF2$enduse<-as.factor("Hot Water")
dhwCF2$Scenario<-as.factor("CF1")

# counterfactual 2, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
dhwCF3<-dhw15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15)
dhwCF3[4:5,]<-dhwCF3[4:5,]-dhw_red_70_MF
dhwCF3<-melt(dhwCF3)
dhwCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
dhwCF3$Energy<-as.factor("Multifam 70% size of single-fam")
dhwCF3$enduse<-as.factor("Hot Water")
dhwCF3$Scenario<-as.factor("CF2")

# counterfactual 3, no type shift, but a reduction in size of the same SF that are affected by the type counterfactual by 30% of the size of the average SF
dhwCF4<-dhw15u
dhwCF4[2:3,]<-dhwCF4[2:3,]+dhw_red_70_SF
dhwCF4<-melt(dhwCF4)
dhwCF4$Stock<-as.factor("CF3: No type change,")
dhwCF4$Energy<-as.factor("Smaller SF (70%)")
dhwCF4$enduse<-as.factor("Hot Water")
dhwCF4$Scenario<-as.factor("CF3")

# counterfactual 4, assuming type change SFto MF, with a fixed energy intensity, i.e. the average MF intensity does not change when N million extra households live in MF instead of SF
dhwCF1<-melt(dhw15uhyp2)
dhwCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
dhwCF1$Energy<-as.factor("Fixed energy intensity")
dhwCF1$enduse<-as.factor("Hot Water")
dhwCF1$Scenario<-as.factor("CF4")

# counterfactual 5, assuming type change SF to MF, plus a reduction in size of the new MF to 50% of the size of the average SF
dhwCF5<-dhw15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15)
dhwCF5[4:5,]<-dhwCF5[4:5,]-dhw_red_50_MF
dhwCF5<-melt(dhwCF5)
dhwCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
dhwCF5$Energy<-as.factor("MulFam 50% size of SinFam")
dhwCF5$enduse<-as.factor("Hot Water")
dhwCF5$Scenario<-as.factor("CF5")

# other end-use scenarios
# base case
oth0<-melt(oth15u)
oth0$Stock<-as.factor("Base: No type change,")
oth0$Energy<-as.factor("Same floor area")
oth0$enduse<-as.factor("Other")

# counterfactual 1, assuming type change from SF to MF, but no change in floor area
othCF2<-oth15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15)
othCF2<-melt(othCF2)
othCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
othCF2$Energy<-as.factor("Same floor area")
othCF2$enduse<-as.factor("Other")
oth0$Scenario<-as.factor("Base")
othCF2$Scenario<-as.factor("CF1")

# counterfactual 2, assuming type change SF to MF, plus a reduction in size of the new MF to 70% of the size of the average SF
othCF3<-oth15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15)
othCF3[4:5,]<-othCF3[4:5,]-oth_red_70_MF
othCF3<-melt(othCF3)
othCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
othCF3$Energy<-as.factor("Multifam 70% size of single-fam")
othCF3$enduse<-as.factor("Other")
othCF3$Scenario<-as.factor("CF2")

# counterfactual 3, no type shift, but a reduction in size of the same SF that are affected by the type counterfactual by 30% of the size of the average SF
othCF4<-oth15u
othCF4[2:3,]<-othCF4[2:3,]+oth_red_70_SF
othCF4<-melt(othCF4)
othCF4$Stock<-as.factor("CF3: No type change,")
othCF4$Energy<-as.factor("Smaller SF (70%)")
othCF4$enduse<-as.factor("Other")
othCF4$Scenario<-as.factor("CF3")

# counterfactual 4, assuming type change SFto MF, with a fixed energy intensity, i.e. the average MF intensity does not change when N million extra households live in MF instead of SF
othCF1<-melt(oth15uhyp2)
othCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
othCF1$Energy<-as.factor("Fixed energy intensity")
othCF1$enduse<-as.factor("Other")
othCF1$Scenario<-as.factor("CF4")

# counterfactual 5, assuming type change SF to MF, plus a reduction in size of the new MF to 50% of the size of the average SF
othCF5<-oth15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15)
othCF5[4:5,]<-othCF5[4:5,]-oth_red_50_MF
othCF5<-melt(othCF5)
othCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
othCF5$Energy<-as.factor("MulFam 50% size of SinFam")
othCF5$enduse<-as.factor("Other")
othCF5$Scenario<-as.factor("CF5")
## plot energy effects for selected scenarios ##################
CF<-rbind(sph0,spc0,dhw0,oth0,sphCF1,spcCF1,dhwCF1,othCF1,sphCF2,spcCF2,dhwCF2,othCF2,sphCF3,spcCF3,dhwCF3,othCF3,sphCF4,spcCF4,dhwCF4,othCF4,sphCF5,spcCF5,dhwCF5,othCF5)

colnames(CF)[1]<-"Type"
colnames(CF)[2]<-"Cohort"
CF$value<-1e-9*CF$value # MJ to PJ
cf<-CF[CF$Cohort==c("1980s","1990s","2000s","2010s"),]
cf <- CF[ which(CF$Cohort==c("1980s","1990s","2000s","2010s")),]
a<-subset(CF,Cohort==c("1980s"))
b<-subset(CF,Cohort==c("1990s"))
c<-subset(CF,Cohort==c("2000s"))
d<-subset(CF,Cohort==c("2010s"))
cf<-rbind(a,b,c,d)
cf2<-cf
levels(cf2$Cohort)<-list("<1950"="<1950","'50s"="1950s","'60s"="1960s","'70s"="1970s","'80s"="1980s","'90s"="1990s","'00s"="2000s","'10s"="2010s")
cf2<-transform(cf2,Stock=factor(Stock,levels=c("Base: No type change,","CF1: SinFam -> MulFam,","CF2: SinFam -> MulFam,","CF3: No type change,","CF4: SinFam -> MulFam,","CF5: SinFam -> MulFam," )),
               Type=factor(Type,levels=c("Manufactured Housing","Multifamily-high","Multifamily-low","Single-family Attached","Single-family Detached" )),
               Scenario=factor(Scenario,levels = c("Base","CF1","CF2","CF3","CF4","CF5")),
               enduse=factor(enduse,levels = c("Other","Hot Water","Space Cool","Space Heat")))
pos_region<-levels(cf2$Cohort)[5:8]
r<-ggplot(data=cf2, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r

# Alternative visualization, grouping by cohort rather than scenario
pos_region<-levels(cf2$Scenario)
r<-ggplot(data=cf2, aes(x=Scenario, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  facet_grid(~Cohort) +
  # facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Scenario",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r
# Another alternative visualization, filling this time by enduses
pos_region<-levels(cf2$Cohort)[5:8]
r<-ggplot(data=cf2, aes(x=Cohort, y=value, fill=enduse)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="End-use") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r
## summarys by scenario and end-use
pos_region<-levels(CF$Scenario)
r<-ggplot(data=CF, aes(x=Scenario, y=value, fill=enduse)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Cohort) +
  # facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Scenario",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r

r<-ggplot(data=cf2, aes(x=Scenario, y=value, fill=enduse)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  # facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Scenario",  y="PJ", fill="End-use") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r

## 
e<-subset(cf2,Scenario==c("Base"))
f<-subset(cf2,Scenario==c("CF1"))
g<-subset(cf2,Scenario==c("CF2"))
cf3<-rbind(e,f,g)
pos_region<-levels(cf3$Cohort)[5:8]
r<-ggplot(data=cf3, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Stock + Energy, nrow = 1) +
  labs(title="Actual/counterfactual urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=14),strip.text.x = element_text(size = 11),axis.text = element_text(size = 11)) +
  scale_fill_brewer(palette="Dark2")
windows()
r

# Final figure 1 b)
cf4<-cf3
levels(cf4$Stock)[levels(cf4$Stock)=="CF1: SinFam -> MulFam," | levels(cf4$Stock)=="CF2: SinFam -> MulFam,"] <- "Type mix: No Fed Policy"
levels(cf4$Stock)[levels(cf4$Stock)=="Base: No type change,"] <- "Type mix: Actual"
# cf4[cf4$Stock=="CF1: SF -> MF,",]$Stock<-as.factor("No Fed Policy")
pos_region<-levels(cf4$Cohort)[5:8]
r<-ggplot(data=cf4, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + 
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Scenario + Stock + Energy, nrow = 1) +
  labs(title="b) Scenarios of urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type") +
  theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
  scale_fill_brewer(palette="Dark2")
windows()
r + theme(legend.position="bottom") 

## comparisons by CF ######################
stock_change<-sum(s15u_diffreg[2:3,])
stock_change_pc<-stock_change/sum(s15u)

base_PJ<-sum(CF[CF$Scenario=="Base",]$value)
CF1_PJ<-sum(CF[CF$Scenario=="CF1",]$value)
CF1_red<-base_PJ-CF1_PJ
CF1_red_pc<-CF1_red/base_PJ # total % reduction in 2015 urban energy in CF1
## changes in/per affected households 
E_avg_aff<--1*sum((s15u_diffreg*eph15u)[2:3,])/del_stock # energy consumption MJ in average affected house
CF1_ph<-CF1_red*1e9/del_stock # reduction from CF1 per affected house
CF1_red_ph_pc<-CF1_ph/E_avg_aff

CF2_PJ<-sum(CF[CF$Scenario=="CF2",]$value)
CF2_red<-base_PJ-CF2_PJ
CF2_red_pc<-CF2_red/base_PJ # total % reduction in 2015 urban energy in CF2
CF2_ph<-CF2_red*1e9/del_stock # reduction from CF2 per affected house
CF2_red_ph_pc<-CF2_ph/E_avg_aff

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

CF5_PJ<-sum(CF[CF$Scenario=="CF5",]$value)
CF5_red<-base_PJ-CF5_PJ
CF5_red_pc<-CF5_red/base_PJ# total % reduction in 2015 urban energy in CF5
CF5_ph<-CF5_red*1e9/del_stock # reduction from CF5 per affected house
CF5_red_ph_pc<-CF5_ph/E_avg_aff


# changes in end-use per scenario
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
rCF<-matrix(rep(0,54),6,9)
rCF[1]<-E_avg_aff*1e-3 # convert to GJ
rCF[2:5,2]<-1e6*c(redSPHcf1,redSPCcf1,redDHWcf1,redOTHcf1)/del_stock
rCF[1,2]<-rCF[1]-sum(rCF[2:5,2])
rCF[2:5,3]<-1e6*c(redSPHcf2,redSPCcf2,redDHWcf2,redOTHcf2)/del_stock
rCF[1,3]<-rCF[1]-sum(rCF[2:5,3])
rCF[2:5,4]<-1e-3*c(sum(sph_red_70_MF),sum(spc_red_70_MF),sum(dhw_red_70_MF),sum(oth_red_70_MF))/del_stock
rCF[1,4]<-rCF[1]-sum(rCF[2:5,4])
rCF[2:5,5]<-1e-3*c(sum(sph_red_50_MF),sum(spc_red_50_MF),sum(dhw_red_50_MF),sum(oth_red_50_MF))/del_stock
rCF[1,5]<-rCF[1]-sum(rCF[2:5,5])

# reductions in SF housing from ResStock model, adapted to GJ final energy
rCF[6,6]<-1.755 # avg hh reduction (GJ) from switch to LED lighting
rCF[6,7]<-3.73 # avg hh reduction (GJ) from switching electric furnace to VSHP at wear out
rCF[6,8]<-9.092  # avg hh reduction (GJ) from drill/fill cavity wall insulation
rCF[6,9]<-11.025  # avg hh reduction (GJ) from all economic electricity efficiency measures

rCFpc<-rCF*100/rCF[1]
colnames(rCFpc)<-c("Base","CF1: type switch \n same size","CF2: type switch \n & 30% smaller", 
                   "30% smaller","50% smaller","LED lighting","Elec. furnace \n to VSHP","Cavity-wall \n insulation","All economic \n elec. savings")
rownames(rCFpc)<-c("Total consumption","Space heating","Space cooling","Domestic hot water","Other end-use","ResStock efficiency measures")
comp<-melt(rCFpc[2:6,2:9])
colnames(comp)<-c("Source","Scenario","Reduction")

# Figure 2
p <- ggplot(data=comp, aes(x = Scenario, y = Reduction))+ ylim(0,42)+
  geom_col(aes(fill = Source), width = 0.75) +
  labs(title = "Comparison of energy reduction strategies in single-family homes", y = "Energy Reduction (%)", x="Reduction strategy") + theme_bw() +
  theme(axis.text=element_text(size=11.5),
        axis.title=element_text(size=13,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Set1")
windows()
p + theme(legend.position="bottom",legend.text=element_text(size=11))  

# Figure 1 b)
# r<-ggplot(data=cf4, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + 
#   geom_bar(stat="identity") + # coord_flip() +
#   facet_wrap(~Scenario + Stock + Energy, nrow = 1) +
#   labs(title="b) Scenarios of urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type")  + #theme_bw() +
#   theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
#   scale_fill_brewer(palette="Dark2")
# windows()
# r + theme(legend.position="bottom") 
