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
load("RECSurbanhreg.Rdata")
load("sdiff.Rdata")
ru15<-ru[ru$RECSYEAR=="2015",]
ru15$INC<-ru15$INC/0.5672
rusf15<-ru15[ru15$TYPEHUQ=="SF Det",]
rusmall<-ru[,c("BTUSPH","NWEIGHT","TYPEHUQ","Cohort","AgeCohort","HDD65","NHSLDMEM","TOTHSQFT","TOTCSQFT","INC","FUELHEAT","RECSYEAR","HEATPRICE","CDD65","BTUCOL","BTUDHW","BTUOTH","TOTSQFT","ACROOMS","TOTROOMS","NCOMBATH","WHEATSIZ","FUELH2O","POOL","RECBATH")]
# rusmall$TC<-paste(rusmall$TYPEHUQ,rusmall$Cohort)
rusmall$TC<-as.factor(paste(rusmall$TYPEHUQ,rusmall$AgeCohort))
rusmall$INC<-0.001*rusmall$INC/0.5672
rus15<-rusmall[rusmall$RECSYEAR=="2015",]
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


eph15<-tapply(rus15$BTUTOT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT,rus15$TYPE,sum)
eph15[3]/eph15[1]

faph15<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,rus15$TYPE,sum)/tapply(rus15$NWEIGHT,rus15$TYPE,sum)
faph15[3]/faph15[1]

# basic model will include income, climate, houshold size, house size, recsyear
# variations will include heatfuel, and omit house size, to show their effect
# random (type-cohort) effects will be defined by random intercepts
## household heating models ###############
# fm2 <- lme(BTUSPH~HDD65, rusmall, random = ~ HDD65 | TC)
# plot(ranef(fm2))
# fm2RE <- ranef(fm2, aug = TRUE)
# plot(fm2RE, form = ~ TYPEHUQ)
# plot(fm2RE, form = HDD65 ~ TYPEHUQ)
# 
# fm3 <- lme(BTUSPH~HDD65, rusmall, random = ~ -1 + HDD65 | TC)
# plot(ranef(fm3))
# fm3RE <- ranef(fm3, aug = TRUE)
# plot(fm3RE, form = ~ TYPEHUQ)
# plot(fm3RE, form = HDD65 ~ TYPEHUQ)
# 
# fm4 <- lme(BTUSPH~HDD65+NHSLDMEM, rusmall, random = ~ -1 + HDD65 | TC)
# plot(ranef(fm4))
# fm4RE <- ranef(fm4, aug = TRUE)
# plot(fm4RE, form = ~ TYPEHUQ)
# plot(fm4RE, form = HDD65 ~ TYPEHUQ)
# # hh size pretty inconsequential
# tab_model(fm2,fm3,fm4)
# 
# fm5 <- lme(BTUSPH~HDD65+INC, rusmall, random = ~ -1 + HDD65 | TC)
# plot(ranef(fm5))
# fm5RE <- ranef(fm5, aug = TRUE)
# plot(fm5RE, form = ~ TYPEHUQ)
# plot(fm5RE, form = HDD65 ~ TYPEHUQ)
# # income makes a difference
# tab_model(fm2,fm3,fm5)
# 
# fm6 <- lme(BTUSPH~HDD65+TOTHSQFT, rusmall, random = ~ -1 + HDD65 | TC)
# plot(ranef(fm6))
# fm6RE <- ranef(fm6, aug = TRUE)
# plot(fm6RE, form = ~ TYPEHUQ)
# plot(fm6RE, form = HDD65 ~ TYPEHUQ)
# # sqft makes a difference
# tab_model(fm2,fm3,fm6)
# 
# fm7 <- lme(BTUSPH~HDD65+TOTHSQFT+INC, rusmall, random = ~  HDD65 | TC)
# plot(ranef(fm7))
# fm7RE <- ranef(fm7, aug = TRUE)
# plot(fm7RE, form = ~ TYPEHUQ)
# plot(fm7RE, form = HDD65 ~ TYPEHUQ)
# # income and dqft are together still significant
# tab_model(fm2,fm3,fm6,fm7)
# 
# fm8 <- lme(BTUSPH~HDD65+TOTHSQFT+INC + FUELHEAT, rusmall, random = ~  HDD65 | TC)
# plot(ranef(fm8))
# fm8RE <- ranef(fm8, aug = TRUE)
# plot(fm8RE, form = ~ TYPEHUQ)
# plot(fm8RE, form = HDD65 ~ TYPEHUQ)
# plot(fm8RE, form = (Intercept) ~ TYPEHUQ)
# # income and dqft are together still significant
# tab_model(fm2,fm3,fm6,fm8)
# # add in survey year to account for differences in sampling and methodologies
# fm9 <- lme(BTUSPH~HDD65+TOTHSQFT+INC + FUELHEAT + RECSYEAR, rusmall, random = ~ -1 + HDD65 | TC)
# plot(ranef(fm9))
# fm9RE <- ranef(fm9, aug = TRUE)
# plot(fm9RE, form = ~ TYPEHUQ)
# plot(fm9RE, form = ~ Cohort)
# plot(fm9RE, form = HDD65 ~ TYPEHUQ)
# # plot(fm9RE, form = (Intercept) ~ TYPEHUQ)
# # income and dqft are together still significant
# tab_model(fm2,fm3,fm6,fm9)
# 
# fm10<-lme(BTUSPH~HDD65+TOTHSQFT+INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
# plot(ranef(fm10))
# fm10RE <- ranef(fm10, aug = TRUE)
# plot(fm10RE, form = ~ TYPEHUQ)
# plot(fm10RE, form = (Intercept) ~ TYPEHUQ)
# plot(fm10RE, form = (Intercept) ~ HDD65)
# means<-tapply(fm10RE$`(Intercept)`,fm10RE$TYPEHUQ,mean)
# windows()
# boxplot(fm10RE$`(Intercept)`~fm10RE$TYPEHUQ,main = "Differences in space heating by house type",ylab="kBTU",yaxt="none",ylim=c(-20000,30000))
# axis(2, seq(-30000,30000,10000),las=2,labels = formatC(seq(-30000,30000,10000),big.mark = ",",format = "d")) 
# points(1:5, as.numeric(means), pch = 19, cex = 0.75,col= "red")
# abline(h = 0, col = "darkgreen",lwd=2)
# rusmall$HEATPRICE[is.na(rusmall$HEATPRICE)]<-0
# fm11<-lme(BTUSPH~HDD65+TOTHSQFT+INC + NHSLDMEM + RECSYEAR + HEATPRICE, rusmall, random = ~  1 | TC)
# plot(ranef(fm11))
# fm11RE <- ranef(fm11, aug = TRUE)
# plot(fm11RE, form = ~ TYPEHUQ)
# plot(fm11RE, form = (Intercept) ~ TYPEHUQ)

# actual heating models ###############
# rus15<-within(rus15,TC<-relevel(TC, ref = "SF Det <1950"))
rus15<-within(rus15,TC<-relevel(TC, ref = "MF high 2010s"))
hh_heat<-lme(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + RECSYEAR, data = rusmall, random = ~  1 | TC)
hh_heat15<-lme(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
# hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + TYPEHUQ + Cohort, rus15)
hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + TC, rus15)
# hh_heat15_fe2<-lm(BTUSPH~HDD65 + TOTHSQFT + INCreal + NHSLDMEM + TC, rus15)

# hh_heat15_fe1<-lm(BTUSPH~HDD65 + TOTHSQFT + INC + NHSLDMEM + TC -1, rus15)
yhat_fe<-hh_heat15_fe[["fitted.values"]]
# yhat_fe1<-hh_heat15_fe1[["fitted.values"]] # its actually the same model, don't trust the increase in R^2
tab_model(hh_heat,hh_heat15,hh_heat15_fe,show.ci = 0)
tab_model(hh_heat15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTHSQFT","INC","NHSLDMEM"))
# heat_type<-hh_heat15_fe[["coefficients"]][12:15]
# heat_cohort<-hh_heat15_fe[["coefficients"]][4:9]
heat_TC<-hh_heat15_fe[["coefficients"]][6:44]
# heat_TC<-heat_TC-min(heat_TC[25:39]) 

# fe_heat_TC<-matrix(rep(0,40),5,8)
# fe_heat_TC[2:5,]<-matrix(rep(heat_type,7),4,7)
# fe_heat_TC[,2:7]<-fe_heat_TC[,2:7]+matrix(rep(heat_cohort,5),5,6,byrow = TRUE)

# hausman test ###############################
# fe1<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM,data = rus15,model = "within",index = c("TC")) # need to include index, but doesn't come out in model results
# # fe2<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM + TC,data = rus15,model = "within") # doesn't work
# re1<-plm(BTUSPH~HDD65+TOTHSQFT+INC+NHSLDMEM,data = rus15,model = "random",index = c("TC"))
# phtest(fe1,re1)
# Hausman test rejects the hypothesis that the random effects model is consistent, and therefore we prefer the fixed effects model
# calculate CF heating energy ###################################
# sphRE<-matrix(data=hh_heat[["coefficients"]][["random"]][["TC"]],5,8,byrow = TRUE)
# sphRE15<-matrix(data=c(hh_heat15[["coefficients"]][["random"]][["TC"]][2:33],hh_heat15[["coefficients"]][["random"]][["TC"]][1],hh_heat15[["coefficients"]][["random"]][["TC"]][34:40]),5,8,byrow = TRUE)
# sphFE15<-matrix(c(heat_TC[1:32],0,heat_TC[33:39]),5,8,byrow = TRUE)
sphFE15<-matrix(c(heat_TC[1:15],0,heat_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
sphp<-summary(hh_heat15_fe)[["coefficients"]][6:44,4] # extract p values
sphpmat<-matrix(c(sphp[1:15],0,sphp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
# sphFE15<-sphFE15-min(sphFE15)-(min(sphFE15,2)-sort(sphFE15)[2])/2 # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged
# sphFE15<-sphFE15+tapply(rus15$BTUSPH,rus15$TYPEHUQ,mean)[5]
sphFEoff<-1.03*median(rus15$BTUSPH)
# sphFEoff<-mean(rus15$BTUSPH)

sphFE15<-sphFE15+sphFEoff # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

# sphFE15<-sphFE15-min(sphFE15)
# row.names(sphRE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(sphFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(sphpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
t2<-matrix(1:40,5,8)
row.names(t2)<-c("Man Housing","SF Det","SF Att","MF low","MF high")
# sphRE15<-sphRE15[rownames(t2),,drop=FALSE]
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
# x$color[x$TYPEHUQ=="MF high"] <- "red"
# x$color[x$TYPEHUQ=="MF low"] <- "blue"
# x$color[x$TYPEHUQ=="SF Det"] <- "black"
# x$color[x$TYPEHUQ=="SF Att"] <- "brown"
# x$color[x$TYPEHUQ=="Man Housing"] <- "darkgreen"
# windows()
# dotchart(x$Offset,labels = x$TC,main="Coefficients for house type and cohor",
#          xlab="Space Heating (MJ)",color=x$color,cex = 0.8,pch=19)
#  ggplot
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

# size of average sf house from 90s and 2000s (which make up 90% of affected houses)
# newSF<-rus15[rus15$AgeCohort== c("1990s","2000s") & rus15$TYPEHUQ==c("SF Det","SF Att"),] # this did not work
# newMF<-rus15[rus15$AgeCohort== c("1990s","2000s") & rus15$TYPEHUQ==c("MF low","MF high"),]
HFA_TC<-tapply(rus15$TOTHSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)


HFA_SF<-HFA_TC[3,1]
HFA_MF<-HFA_TC[2,1]
beta_hsf<-hh_heat15_fe[["coefficients"]][["TOTHSQFT"]]
del_stock<-sum(s15u_diffreg[4:5,])
# space heating reduction if new MF houses were 80%, 70%, 60% size of SF
sph_red_80<-0.2*HFA_SF*beta_hsf*del_stock*1e-9 # MJ to PJ
sph_red_70<-0.3*HFA_SF*beta_hsf*del_stock*1e-9
sph_red_60<-0.4*HFA_SF*beta_hsf*del_stock*1e-9
sph_type_red70<-red_sphFE-sph_red_70
sph_red_70_MF<-0.3*HFA_SF*beta_hsf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
sph_red_70_SF<-0.3*HFA_SF*beta_hsf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
sph_red_50_MF<-0.5*HFA_SF*beta_hsf*s15u_diffreg[4:5,]
# boxplot(hh_heat_FE$Offset~hh_heat_FE$TYPEHUQ,main = "Differences in space heating by type and cohort range")

# would like to be able to replicate some of these graphs using FE data
# windows()
# plot(ranef(hh_heat),ylab = "Type and Cohort",xlab = "Effects on space heating (kBTU)")
hh_heatRE <- ranef(hh_heat, aug = TRUE)
# plot(hh_heatRE, form = ~ TYPEHUQ)
# plot(hh_heatRE, form = (Intercept) ~ TYPEHUQ)
# plot(hh_heatRE, form = (Intercept) ~ HDD65)
# could do something like this, would need to fix the order first.
# hh_heatRE$`(Intercept)`<-hh_heat_FE$Offset

# old, plot boxplot of effects by type and cohort ##################
# hh_heat_type<-lme(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + RECSYEAR, data = rusmall, random = ~  1 | TYPEHUQ)
# hh_heat_type15<-lme(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM, data = rus15, random = ~  1 | TYPEHUQ)
# 
# plot(ranef(hh_heat_type))
# tab_model(hh_heat,hh_heat_type)
# plots
# means<-tapply(hh_heatRE$`(Intercept)`,hh_heatRE$TYPEHUQ,mean)
# type_off<-hh_heat_type[["coefficients"]][["random"]][["TYPEHUQ"]]
# windows()
# par(mar=c(5.1,6.1,4.1,2.1))
# boxplot(hh_heatRE$`(Intercept)`~hh_heatRE$TYPEHUQ,main = "Differences in space heating by type and cohort range",ylab="",yaxt="none",ylim=c(-20000,30000))
# title(ylab="Random effects by type, cohort (kBTU)", line=4)
# axis(2, seq(-30000,30000,10000),las=2,labels = formatC(seq(-30000,30000,10000),big.mark = ",",format = "d")) 
# points(1:5, as.numeric(means), pch = 19, cex = 0.75,col= "red")
# points(1:5, as.numeric(type_off), pch = 19, cex = 0.75,col= "blue")
# abline(h = 0, col = "darkgreen",lwd=2)
# legend(4.5,20000,legend= paste("RanEff by","\n", "type only"),col="blue",pch = 16,cex=0.85)

# hh_heat_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + RECSYEAR + TYPEHUQ + Cohort, rusmall)
# 
# hh_heat_int<-lm(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + RECSYEAR + TYPEHUQ*Cohort, rusmall)
# tab_model(hh_heat,hh_heat_fe,hh_heat_int,show.ci = 0,dv.labels = c('SPH Random effects','SPH Fixed effects','SPH FE w/ interaction'))

# try with ggplot
# p<- ggplot(hh_heatRE, aes(x = TYPEHUQ, y = `(Intercept)`)) + stat_boxplot(geom ='errorbar') + 
#   geom_boxplot() + geom_point(data=t,aes(x=x,y=`(Intercept)`)) + 
#   labs(title = "Differences in space heating by type and cohort range") + ylab(label="Intercept Offset (kBTU)") + xlab(label="House Type") +
#   theme_bw() + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + coord_cartesian(ylim = c(-20000, 30000)) + 
#   geom_hline(yintercept=0, color = "darkgreen",lwd=1) #+ theme(text = element_text(size=13))
# d1 <- data.frame(x = 1, y = c(1:1000,1502))
# d2 <- data.frame(
#   y = c(boxplot.stats(d1$y)$stats, 1440), 
#   x = 1, 
#   label = c('cohort min', '1st quartile', 'median', '3rd quartile', 'cohort max', paste('offset by','\n', 'type only'))
# )
# leg <- ggplot(d1, aes(x, y))   + stat_boxplot(geom ='errorbar', width = 0.2) +  geom_boxplot(width = 0.2) +  
#   geom_text(aes(x = 1.15, label = label), d2, hjust = 0) +  
#   xlim(0.8, 1.5) +
#   theme_void() + theme(panel.background = element_rect(fill = 'white', color = 1),text = element_text(size=7))
# windows()
# p + annotation_custom(ggplotGrob(leg),xmin = 4.5, xmax = 5.5, ymin = 10000, ymax = 28000)

# cooling models #####################
hh_cool<-lme(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_cool15<-lme(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
# hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + TYPEHUQ + Cohort, rus15)
# hh_cool15_fe<-lm(BTUCOL~CDD65 + ACROOMS + INC + NHSLDMEM + TC, rus15)
hh_cool15_fe<-lm(BTUCOL~CDD65 + TOTCSQFT + INC + NHSLDMEM + TC, rus15)
tab_model(hh_cool,hh_cool15,hh_cool15_fe,show.ci = FALSE)
# tab_model(hh_cool15_fe,hh_cool15_fe2,show.ci = FALSE)
tab_model(hh_cool15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","CDD65","TOTCSQFT","INC","NHSLDMEM"))

cool_TC<-hh_cool15_fe[["coefficients"]][6:44]

# spcRE15<-matrix(data=c(hh_cool15[["coefficients"]][["random"]][["TC"]][2:33],
                       # hh_cool15[["coefficients"]][["random"]][["TC"]][1],
                       # hh_cool15[["coefficients"]][["random"]][["TC"]][34:40]),5,8,byrow = TRUE)
spcFE15<-matrix(c(cool_TC[1:15],0,cool_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
spcp<-summary(hh_cool15_fe)[["coefficients"]][6:44,4] # extract p values
spcpmat<-matrix(c(spcp[1:15],0,spcp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
# spcFE•15<-spcFE15+median(rus15$BTUCOL)
# spcFE15<-matrix(c(cool_TC[1:32],0,cool_TC[33:39]),5,8,byrow = TRUE)
# spcFEoff<--min(spcFE15)+sort(spcFE15)[3]
spcFEoff<-2*median(spcFE15)

spcFE15<-spcFE15+spcFEoff# alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged
# using min is not a perfect offset, it still generates a mildly negative value in SF Att while not doing enough to decrease SF Det or increase MF high, but it is okay.
# row.names(spcRE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(spcFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(spcpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
# spcRE15<-spcRE15[rownames(t2),,drop=FALSE]
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
x$Offset<-x$Offset-spcFEoff
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x2<-x
x2$Enduse<-"Space Cool"
# x$color[x$TYPEHUQ=="MF high"] <- "red"
# x$color[x$TYPEHUQ=="MF low"] <- "blue"
# x$color[x$TYPEHUQ=="SF Det"] <- "black"
# x$color[x$TYPEHUQ=="SF Att"] <- "brown"
# x$color[x$TYPEHUQ=="Man Housing"] <- "darkgreen"
# windows()
# dotchart(x$Offset,labels = x$TC,main="Offsets by Type and Cohort",
#          xlab="Space Cooling (kBTU)",color=x$color,cex = 0.8,pch=19)
# ggplot
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, space cooling",x="Coefficient (MJ)") +
  # theme(panel.background = element_rect(fill = NA),
  # panel.grid.major = element_line(colour = "grey70"),
  # panel.ontop = TRUE,legend.position="none")
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q
# reduction in space cooling with alternative cohort 
# red_spcRE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcRE15))*1.055e-9 # reduction in space heating in PJ, 2015, mixed effect model
red_spcFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 


# spcRE<-matrix(data=hh_cool[["coefficients"]][["random"]][["TC"]],5,7,byrow = TRUE)
# row.names(spcRE)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
# spcRE<-spcRE[rownames(t2),,drop=FALSE]
# red_spc<-sum(sweep(s15u_diffreg3,MARGIN = 1,FUN = "*",STATS = spcRE))*1.055e-9 # change in space cooling in PJ

# AC_SF<-sum(newSF$ACROOMS*newSF$NWEIGHT)/sum(newSF$NWEIGHT)
# AC_MF<-sum(newMF$ACROOMS*newMF$NWEIGHT)/sum(newMF$NWEIGHT)
CFA_TC<-tapply(rus15$TOTCSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
CFA_SF<-CFA_TC[3,1]
CFA_MF<-CFA_TC[2,1]
beta_csf<-hh_cool15_fe[["coefficients"]][["TOTCSQFT"]]
  
spc_red_80<-0.2*CFA_SF*beta_csf*del_stock*1e-9 # MJ to PJ
spc_red_70<-0.3*CFA_SF*beta_csf*del_stock*1e-9
spc_red_60<-0.4*CFA_SF*beta_csf*del_stock*1e-9
spc_type_red70<-red_spcFE-spc_red_70
spc_red_70_MF<-0.3*CFA_SF*beta_csf*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
spc_red_70_SF<-0.3*CFA_SF*beta_csf*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
spc_red_50_MF<-0.5*CFA_SF*beta_csf*s15u_diffreg[4:5,] 
# windows() ##############
# plot(ranef(hh_cool),ylab = "Type and Cohort",xlab = "Effects on space cooling (kBTU)")
# hh_coolRE <- ranef(hh_cool, aug = TRUE)
# plot(hh_coolRE, form = ~ TYPEHUQ)
# plot(hh_coolRE, form = (Intercept) ~ TYPEHUQ)
# plot(hh_coolRE, form = (Intercept) ~ CDD65)

# hh_cool_type<-lme(BTUCOL~CDD65 + ACROOMS + Cohort + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TYPEHUQ)
# plot(ranef(hh_cool_type))
# tab_model(hh_cool,hh_cool_type)

# #plots 
# type_off<-hh_cool_type[["coefficients"]][["random"]][["TYPEHUQ"]]
# windows()
# par(mar=c(5.1,6.1,4.1,2.1))
# boxplot(hh_coolRE$`(Intercept)`~hh_coolRE$TYPEHUQ,main = "Differences in space cooling by type and cohort range",ylab="",yaxt="none")#,ylim=c(-20000,30000))
# title(ylab="Random effects by type, cohort (kBTU)", line=4)
# axis(2, seq(-2000,2000,1000),las=2,labels = formatC(seq(-2000,2000,1000),big.mark = ",",format = "d")) 
# # points(1:5, as.numeric(means), pch = 19, cex = 0.75,col= "red")
# points(1:5, as.numeric(type_off), pch = 19, cex = 0.75,col= "blue")
# abline(h = 0, col = "darkgreen",lwd=2)
# legend(4.5,1200,legend= paste("RanEff by","\n", "type only"),col="blue",pch = 16,cex=0.85,y.intersp = 1.5)

# new hot water models ##################
hh_dhw<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_dhw15<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
# hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + TYPEHUQ + Cohort, rus15)
hh_dhw15_fe<-lm(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + TC, rus15)
tab_model(hh_dhw,hh_dhw15,hh_dhw15_fe,show.ci = FALSE)
tab_model(hh_dhw15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","HDD65","TOTSQFT","INC","NHSLDMEM"))

dhw_TC<-hh_dhw15_fe[["coefficients"]][6:44]
# dhwRE15<-matrix(data=c(hh_dhw15[["coefficients"]][["random"]][["TC"]][2:33],
                       # hh_dhw15[["coefficients"]][["random"]][["TC"]][1],
                       # hh_dhw15[["coefficients"]][["random"]][["TC"]][34:40]),5,8,byrow = TRUE)
dhwFE15<-matrix(c(dhw_TC[1:15],0,dhw_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
dhwp<-summary(hh_dhw15_fe)[["coefficients"]][6:44,4] # extract p values
dhwpmat<-matrix(c(dhwp[1:15],0,dhwp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place

dhwFEoff<-1.1*median(rus15$BTUDHW)
dhwFE15<-dhwFE15+dhwFEoff # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchange

# row.names(dhwRE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(dhwFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(dhwpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
# dhwRE15<-dhwRE15[rownames(t2),,drop=FALSE]
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
# x$color[x$TYPEHUQ=="MF high"] <- "red"
# x$color[x$TYPEHUQ=="MF low"] <- "blue"
# x$color[x$TYPEHUQ=="SF Det"] <- "black"
# x$color[x$TYPEHUQ=="SF Att"] <- "brown"
# x$color[x$TYPEHUQ=="Man Housing"] <- "darkgreen"
# windows()
# dotchart(x$Offset,labels = x$TC,main="Offsets by Type and Cohort",
#          xlab="Hot Water (kBTU)",color=x$color,cex = 0.8,pch=19)
# ggplot
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, hot water",x="Coefficient (MJ)") +
  # theme(panel.background = element_rect(fill = NA),
  # panel.grid.major = element_line(colour = "grey70"),
  # panel.ontop = TRUE,legend.position="none")
  # scale_colour_manual(name = NULL,values = c("black","black"),
  #                     limits = c("Size:   p>0.05","p<0.05"),
  #                     guide = guide_legend(label.position="left",override.aes = list(shape=c(19,19),size = c(1,2)))) +theme_bw() +
  # theme(legend.direction = "horizontal",
  #       legend.position = "top",
  #       legend.justification = "right",
  #       legend.key = element_rect(fill = "white"),
  #       legend.key.size = unit(.5, "mm"),
  #       legend.margin = margin(b = 0, 0, 0, 0) )
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q
# reduction in space dhwing with alternative cohort 
# red_dhwRE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwRE15))*1.055e-9 # reduction in space heating in PJ, 2015, mixed effect model
red_dhwFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 
# 
SQFT_all<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$AgeCohort),sum)
# SQFT_SF<-sum(newSF$TOTSQFT*newSF$NWEIGHT)/sum(newSF$NWEIGHT)
# SQFT_MF<-sum(newMF$TOTSQFT*newMF$NWEIGHT)/sum(newMF$NWEIGHT)
TFA_TC<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE,rus15$OldNew),sum)
TFA_SF<-TFA_TC[3,1]
TFA_MF<-TFA_TC[2,1]
TFA_T<-tapply(rus15$TOTSQFT*rus15$NWEIGHT,list(rus15$TYPE),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE),sum)
TFA_T[3]/TFA_T[2] # size ratio for SF to MF
E_T<-tapply(rus15$BTUTOT*rus15$NWEIGHT,list(rus15$TYPE),sum)/tapply(rus15$NWEIGHT,list(rus15$TYPE),sum)
E_T[3]/E_T[2] # size ratio for SF to MF
beta_sqft<- hh_dhw15_fe[["coefficients"]][["TOTSQFT"]]

dhw_red_80<-0.2*TFA_SF*beta_sqft*del_stock*1e-9
dhw_red_70<-0.3*TFA_SF*beta_sqft*del_stock*1e-9
dhw_red_60<-0.4*TFA_SF*beta_sqft*del_stock*1e-9
dhw_type_red70<-red_dhwFE-dhw_red_70
dhw_red_70_MF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
dhw_red_70_SF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
dhw_red_50_MF<-0.5*TFA_SF*beta_sqft*s15u_diffreg[4:5,]

# new other energy models ##################
hh_oth<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
hh_oth15<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM, data = rus15, random = ~  1 | TC)
# hh_heat15_fe<-lm(BTUSPH~HDD65 + TOTHSQFT + Cohort + INC + NHSLDMEM + TYPEHUQ + Cohort, rus15)
hh_oth15_fe<-lm(BTUOTH~TOTSQFT + INC + NHSLDMEM + TC, rus15)
tab_model(hh_oth,hh_oth15,hh_oth15_fe,show.ci = FALSE)
tab_model(hh_oth15_fe,show.ci = 0,show.se = TRUE,terms = c("(Intercept)","TOTSQFT","INC","NHSLDMEM"))
# all end uses
# tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE)
# # tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,
#           terms = c("(Intercept)","HDD65","CDD65","TOTHSQFT","TOTSQFT","INC","NHSLDMEM","ACROOMS"))
tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,
          terms = c("(Intercept)","HDD65","CDD65","TOTHSQFT","TOTSQFT","INC","NHSLDMEM","TOTCSQFT"))

tab_model(hh_heat15_fe,hh_cool15_fe,hh_dhw15_fe,hh_oth15_fe,show.ci = 0,show.se = TRUE,digits = 0,show.p = FALSE)

oth_TC<-hh_oth15_fe[["coefficients"]][5:43]
# oth_TC<-oth_TC-min(oth_TC[25:39]) # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

# othRE15<-matrix(data=c(hh_oth15[["coefficients"]][["random"]][["TC"]][2:33],
#                        hh_oth15[["coefficients"]][["random"]][["TC"]][1],
#                        hh_oth15[["coefficients"]][["random"]][["TC"]][34:40]),5,8,byrow = TRUE)
othFE15<-matrix(c(oth_TC[1:15],0,oth_TC[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
othp<-summary(hh_oth15_fe)[["coefficients"]][5:43,4] # extract p values
othpmat<-matrix(c(othp[1:15],0,othp[16:39]),5,8,byrow = TRUE) # arrange so that MF high 2010s (the reference level) is in the right place
othFEoff<-mean(c(mean(rus15$BTUOTH),median(rus15$BTUOTH))) # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

othFEoff<-1.05*mean(rus15$BTUOTH) # alteration so that in counterfactual energy calculation, the difference is a reduction in SF and an increase in MF, overall result on reduction is unchanged

othFE15<-othFE15+othFEoff

# row.names(othRE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(othFE15)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
row.names(othpmat)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
# othRE15<-othRE15[rownames(t2),,drop=FALSE]
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

# x$color[x$TYPEHUQ=="MF high"] <- "red"
# x$color[x$TYPEHUQ=="MF low"] <- "blue"
# x$color[x$TYPEHUQ=="SF Det"] <- "black"
# x$color[x$TYPEHUQ=="SF Att"] <- "brown"
# x$color[x$TYPEHUQ=="Man Housing"] <- "darkgreen"
x$Offset<-x$Offset-othFEoff
x$Sig<-1.5
x[x$p<0.05,]$Sig<-3
x4<-x
x4$Enduse<-"Other"
# windows()
# dotchart(x$Offset,labels = x$TC,main="Offsets by Type and Cohort",
#          xlab="Other end-uses (kBTU)",color=x$color,cex = 0.8,pch=19)
# ggplot
q<-qplot(x=Offset,y=Cohort,data=x,geom="point") + 
  geom_point(aes(colour = factor(TYPEHUQ)),size=x$Sig) +
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
  labs(title = "House type and cohort effects, other",x="Coefficient (MJ)") +
  # theme(panel.background = element_rect(fill = NA),
  # panel.grid.major = element_line(colour = "grey70"),
  # panel.ontop = TRUE,legend.position="none")
  theme_bw()+theme(legend.position="none") + scale_color_brewer(palette="Dark2")
windows()
q

# reduction in space othing with alternative cohort 
# red_othRE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othRE15))*1.055e-9 # reduction in space heating in PJ, 2015, mixed effect model
red_othFE<-sum(sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15))*1e-9 # reduction in space heating in PJ, 2015, fixed effect model 
# 

# SQFT_SF<-sum(newSF$TOTSQFT*newSF$NWEIGHT)/sum(newSF$NWEIGHT)
# SQFT_MF<-sum(newMF$TOTSQFT*newMF$NWEIGHT)/sum(newMF$NWEIGHT)
beta_sqft<- hh_oth15_fe[["coefficients"]][["TOTSQFT"]]

oth_red_80<-0.2*TFA_SF*beta_sqft*del_stock*1e-9
oth_red_70<-0.3*TFA_SF*beta_sqft*del_stock*1e-9
oth_red_60<-0.4*TFA_SF*beta_sqft*del_stock*1e-9
oth_type_red70<-red_othFE-oth_red_70
oth_red_70_MF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 30% smaller (compared to being full size of average SF) positive number
oth_red_70_SF<-0.3*TFA_SF*beta_sqft*s15u_diffreg[2:3,] # how much SF would reduce energy if they were 30% smaller (compared to being full size of average SF) negative number
oth_red_50_MF<-0.5*TFA_SF*beta_sqft*s15u_diffreg[4:5,] # how much MF would reduce energy if they were 50% smaller (compared to being full size of average SF) positive number


## alternative fig 3 
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

# XXX <- X[ which(X$TYPEHUQ==c("SF Det","MF high")),]
q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + 
  geom_point(aes(colour = factor(Enduse),size=Signif),size=XX$Sig) + 
  facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) +
  labs(title = "House type and cohort effects",x="Effect on energy end-use consumption (MJ)",colour = 'End-use') +  
  theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
                                                           legend.background = element_rect(color="gray45"),legend.title.align=0.5,plot.title = element_text(face = "bold"))
windows()
q 
## TMI #################
# q<-qplot(x=Offset,y=Cohort,data=XX,geom="point") + 
#   geom_point(aes(colour = factor(Enduse),shape=factor(TYPEHUQ)),size=XX$Sig) + 
#   facet_wrap(~ TYPE, scales = "free_y", ncol = 1) +
#   labs(title = "House type and cohort effects",x="Coefficient (MJ)",colour = 'End-use') +  
#   theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
#                                                            legend.background = element_rect(color="gray45"),legend.title.align=0.5)
# windows()
# q


# q<-ggplot(XX, aes(x=Offset,y=Cohort)) + geom_point(aes(colour = factor(Enduse),size=Sig)) + 
#   facet_wrap(~TYPEHUQ,ncol = 1) +
#   labs(title = "House type and cohort effects",x="Coefficient (MJ)",colour = 'End-use') +  
#   theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
#                                                            legend.background = element_rect(fill="gray90"))
# windows()
# q
# 
# bp<-ggplot(XX, aes(x=Offset,y=Cohort,colour=EU,size=Sig)) + geom_point() + 
#   facet_wrap(~TYPEHUQ,ncol = 1)
# ##
# q<-ggplot(XX, aes(x=Offset,y=Cohort)) + geom_point(aes(colour = factor(Enduse),size=Signif)) #+ scale_size_discrete(max=4)
#   facet_wrap(~TYPEHUQ,ncol = 1) +
#   labs(title = "House type and cohort effects",x="Coefficient (MJ)",colour = 'End-use') +  
#   theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
#                                                            legend.background = element_rect(fill="gray90"))
# windows()
# q
#######
X1<-x1
X1$Offset<-X1$Offset/median(rus15$BTUSPH)
X2<-x2
X2$Offset<-X2$Offset/median(rus15$BTUCOL)
X3<-x3
X3$Offset<-X3$Offset/median(rus15$BTUDHW)
X4<-x4
X4$Offset<-X4$Offset/median(rus15$BTUOTH)

xy<-rbind(X1,X2,X3,X4)
xxy<-xy[!xy$TYPEHUQ=="Man Housing",]
XY<-xy[xy$TYPEHUQ=="MF high" | xy$TYPEHUQ=="SF Det" ,]

# q<-qplot(x=Offset,y=Cohort,data=XY,geom="point") + 
#   geom_point(aes(colour = factor(Enduse)),size=XY$Sig) +
#   facet_wrap(~ TYPEHUQ, scales = "free_y", ncol = 1) + 
#   labs(title = "House type and cohort effects",x=" Median-normalized Coefficient",colour = 'End-use') +
#   theme_bw() + scale_color_brewer(palette="Dark2") + theme(strip.text = element_text(size=11),legend.text = element_text(size=11),
#                                                            legend.background = element_rect(color="gray45"),legend.title.align=0.5)
# windows()
# q

# load and convert
load("end_uses_CF.RData")
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
# row.names(sph15u)<-c("Man Housing","SinFam Det","SinFam Att","MulFam low","MulFam high")
row.names(sph15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(spc15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(dhw15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(oth15u)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")

row.names(sph15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(spc15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(dhw15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")
row.names(oth15uhyp2)<-c("Manufactured Housing","Single-family Detached","Single-family Attached","Multifamily-low","Multifamily-high")

sph0<-melt(sph15u)
sph0$Stock<-as.factor("Base: No type change,")
sph0$Energy<-as.factor("Same floor area")
sph0$enduse<-as.factor("Space Heat")
sph0$Scenario<-as.factor("Base") 
# why does sum(sph0$value)*1e-9 not equal sum(sweep(s15u,MARGIN = 1,FUN = "*",STATS = sphFE15))*1e-9?
# the second one is not meaningful; it is the whole stock multiplied by the coefficients, i.e. the change in sph expected if all houses changed
# the first one is the total space heat in the unchanged scenario
sphCF2<-sph15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15)
sphCF2<-melt(sphCF2)
sphCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
sphCF2$Energy<-as.factor("Same floor area")
sphCF2$enduse<-as.factor("Space Heat")
sphCF2$Scenario<-as.factor("CF1")
sphCF2diffSF<-sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15)


sphCF3<-sph15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15)
sphCF3[4:5,]<-sphCF3[4:5,]-sph_red_70_MF
sphCF3<-melt(sphCF3)
sphCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
sphCF3$Energy<-as.factor("Multifam 70% size of single-fam")
sphCF3$enduse<-as.factor("Space Heat")
sphCF3$Scenario<-as.factor("CF2")

sphCF1<-melt(sph15uhyp2)
sphCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
sphCF1$Energy<-as.factor("Fixed energy intensity")
sphCF1$enduse<-as.factor("Space Heat")
sphCF1$Scenario<-as.factor("CF4")
sphCF1diffSF<-(sum(sph15u[2:3,])-sum(sph15uhyp2[2:3,]))*1e-9

sphCF4<-sph15u
sphCF4[2:3,]<-sphCF4[2:3,]+sph_red_70_SF
sphCF4<-melt(sphCF4)
sphCF4$Stock<-as.factor("CF3: No type change,")
sphCF4$Energy<-as.factor("Smaller SF (70%)")
sphCF4$enduse<-as.factor("Space Heat")
sphCF4$Scenario<-as.factor("CF3")

sphCF5<-sph15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = sphFE15)
sphCF5[4:5,]<-sphCF5[4:5,]-sph_red_50_MF
sphCF5<-melt(sphCF5)
sphCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
sphCF5$Energy<-as.factor("MulFam 50% size of SinFam")
sphCF5$enduse<-as.factor("Space Heat")
sphCF5$Scenario<-as.factor("CF5")

spc0<-melt(spc15u)
spc0$Stock<-as.factor("Base: No type change,")
spc0$Energy<-as.factor("Same floor area")
spc0$enduse<-as.factor("Space Cool")
spc0$Scenario<-as.factor("Base")

spcCF2<-spc15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15)
spcCF2<-melt(spcCF2)
spcCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
spcCF2$Energy<-as.factor("Same floor area")
spcCF2$enduse<-as.factor("Space Cool")
spcCF2$Scenario<-as.factor("CF1")

spcCF3<-spc15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15)
spcCF3[4:5,]<-spcCF3[4:5,]-spc_red_70_MF
spcCF3<-melt(spcCF3)
spcCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
spcCF3$Energy<-as.factor("Multifam 70% size of single-fam")
spcCF3$enduse<-as.factor("Space Cool")
spcCF3$Scenario<-as.factor("CF2")

spcCF4<-spc15u
spcCF4[2:3,]<-spcCF4[2:3,]+spc_red_70_SF
spcCF4<-melt(spcCF4)
spcCF4$Stock<-as.factor("CF3: No type change,")
spcCF4$Energy<-as.factor("Smaller SF (70%)")
spcCF4$enduse<-as.factor("Space Cool")
spcCF4$Scenario<-as.factor("CF3")

spcCF1<-melt(spc15uhyp2)
spcCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
spcCF1$Energy<-as.factor("Fixed energy intensity")
spcCF1$enduse<-as.factor("Space Cool")
spcCF1$Scenario<-as.factor("CF4")

spcCF5<-spc15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = spcFE15)
spcCF5[4:5,]<-spcCF5[4:5,]-spc_red_50_MF
spcCF5<-melt(spcCF5)
spcCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
spcCF5$Energy<-as.factor("MulFam 50% size of SinFam")
spcCF5$enduse<-as.factor("Space Cool")
spcCF5$Scenario<-as.factor("CF5")

dhw0<-melt(dhw15u)
dhw0$Stock<-as.factor("Base: No type change,")
dhw0$Energy<-as.factor("Same floor area")
dhw0$enduse<-as.factor("Hot Water")
dhw0$Scenario<-as.factor("Base")

dhwCF2<-dhw15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15)
dhwCF2<-melt(dhwCF2)
dhwCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
dhwCF2$Energy<-as.factor("Same floor area")
dhwCF2$enduse<-as.factor("Hot Water")
dhwCF2$Scenario<-as.factor("CF1")

dhwCF3<-dhw15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15)
dhwCF3[4:5,]<-dhwCF3[4:5,]-dhw_red_70_MF
dhwCF3<-melt(dhwCF3)
dhwCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
dhwCF3$Energy<-as.factor("Multifam 70% size of single-fam")
dhwCF3$enduse<-as.factor("Hot Water")
dhwCF3$Scenario<-as.factor("CF2")

dhwCF4<-dhw15u
dhwCF4[2:3,]<-dhwCF4[2:3,]+dhw_red_70_SF
dhwCF4<-melt(dhwCF4)
dhwCF4$Stock<-as.factor("CF3: No type change,")
dhwCF4$Energy<-as.factor("Smaller SF (70%)")
dhwCF4$enduse<-as.factor("Hot Water")
dhwCF4$Scenario<-as.factor("CF3")

dhwCF1<-melt(dhw15uhyp2)
dhwCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
dhwCF1$Energy<-as.factor("Fixed energy intensity")
dhwCF1$enduse<-as.factor("Hot Water")
dhwCF1$Scenario<-as.factor("CF4")

dhwCF5<-dhw15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = dhwFE15)
dhwCF5[4:5,]<-dhwCF5[4:5,]-dhw_red_50_MF
dhwCF5<-melt(dhwCF5)
dhwCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
dhwCF5$Energy<-as.factor("MulFam 50% size of SinFam")
dhwCF5$enduse<-as.factor("Hot Water")
dhwCF5$Scenario<-as.factor("CF5")

oth0<-melt(oth15u)
oth0$Stock<-as.factor("Base: No type change,")
oth0$Energy<-as.factor("Same floor area")
oth0$enduse<-as.factor("Other")

othCF2<-oth15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15)
othCF2<-melt(othCF2)
othCF2$Stock<-as.factor("CF1: SinFam -> MulFam,")
othCF2$Energy<-as.factor("Same floor area")
othCF2$enduse<-as.factor("Other")
oth0$Scenario<-as.factor("Base")
othCF2$Scenario<-as.factor("CF1")

othCF3<-oth15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15)
othCF3[4:5,]<-othCF3[4:5,]-oth_red_70_MF
othCF3<-melt(othCF3)
othCF3$Stock<-as.factor("CF2: SinFam -> MulFam,")
othCF3$Energy<-as.factor("Multifam 70% size of single-fam")
othCF3$enduse<-as.factor("Other")
othCF3$Scenario<-as.factor("CF2")

othCF4<-oth15u
othCF4[2:3,]<-othCF4[2:3,]+oth_red_70_SF
othCF4<-melt(othCF4)
othCF4$Stock<-as.factor("CF3: No type change,")
othCF4$Energy<-as.factor("Smaller SF (70%)")
othCF4$enduse<-as.factor("Other")
othCF4$Scenario<-as.factor("CF3")

othCF1<-melt(oth15uhyp2)
othCF1$Stock<-as.factor("CF4: SinFam -> MulFam,")
othCF1$Energy<-as.factor("Fixed energy intensity")
othCF1$enduse<-as.factor("Other")
othCF1$Scenario<-as.factor("CF4")

othCF5<-oth15u+sweep(s15u_diffreg,MARGIN = 1,FUN = "*",STATS = othFE15)
othCF5[4:5,]<-othCF5[4:5,]-oth_red_50_MF
othCF5<-melt(othCF5)
othCF5$Stock<-as.factor("CF5: SinFam -> MulFam,")
othCF5$Energy<-as.factor("MulFam 50% size of SinFam")
othCF5$enduse<-as.factor("Other")
othCF5$Scenario<-as.factor("CF5")
## plot ##################
CF<-rbind(sph0,spc0,dhw0,oth0,sphCF1,spcCF1,dhwCF1,othCF1,sphCF2,spcCF2,dhwCF2,othCF2,sphCF3,spcCF3,dhwCF3,othCF3,sphCF4,spcCF4,dhwCF4,othCF4,sphCF5,spcCF5,dhwCF5,othCF5)
# sph<-rbind(sph0,sphCF1,sphCF2,sphCF3,sphCF4)
# spc<-rbind(spc0,spcCF1,spcCF2,spcCF3,spcCF4)
# dhw<-rbind(dhw0,dhwCF1,dhwCF2,dhwCF3,dhwCF4)
# oth<-rbind(oth0,othCF1,othCF2,othCF3,othCF4)
colnames(CF)[1]<-"Type"
colnames(CF)[2]<-"Cohort"
CF$value<-1e-9*CF$value # MJ to PJ
# colnames(oth)[1]<-"Type"
# colnames(oth)[2]<-"Cohort"
cf<-CF[CF$Cohort==c("1980s","1990s","2000s","2010s"),]
cf <- CF[ which(CF$Cohort==c("1980s","1990s","2000s","2010s")),]
a<-subset(CF,Cohort==c("1980s"))
b<-subset(CF,Cohort==c("1990s"))
c<-subset(CF,Cohort==c("2000s"))
d<-subset(CF,Cohort==c("2010s"))
cf<-rbind(a,b,c,d)
cf2<-cf
levels(cf2$Cohort)<-list("<1950"="<1950","'50s"="1950s","'60s"="1960s","'70s"="1970s","'80s"="1980s","'90s"="1990s","'00s"="2000s","'10s"="2010s")
# cf2<-transform(cf2,Energy=factor(Energy,levels=c("No size change","MF 70% size of SF","Smaller SF (70%)","Fixed energy intensity" )))
cf2<-transform(cf2,Stock=factor(Stock,levels=c("Base: No type change,","CF1: SinFam -> MulFam,","CF2: SinFam -> MulFam,","CF3: No type change,","CF4: SinFam -> MulFam,","CF5: SinFam -> MulFam," )),
               Type=factor(Type,levels=c("Manufactured Housing","Multifamily-high","Multifamily-low","Single-family Attached","Single-family Detached" )),
               Scenario=factor(Scenario,levels = c("Base","CF1","CF2","CF3","CF4","CF5")),
               enduse=factor(enduse,levels = c("Other","Hot Water","Space Cool","Space Heat")))
# -c("Manuf. Housing","Single-fam Det","Single-fam Att","Multifam-low","Multifam-high")
pos_region<-levels(cf2$Cohort)[5:8]
r<-ggplot(data=cf2, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + #scale_fill_discrete(limits=pos_type)
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
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

## actual plot
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
# Alternative titles
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
CF1_red_pc<-CF1_red/base_PJ
## changes in/per affected households 
E_avg_aff<--1*sum((s15u_diffreg*eph15u)[2:3,])/del_stock # energy consumption MJ in average affected house
CF1_ph<-CF1_red*1e9/del_stock # reduction from CF1 per affected house
CF1_red_ph_pc<-CF1_ph/E_avg_aff

CF2_PJ<-sum(CF[CF$Scenario=="CF2",]$value)
CF2_red<-base_PJ-CF2_PJ
CF2_red_pc<-CF2_red/base_PJ
CF2_ph<-CF2_red*1e9/del_stock # reduction from CF1 per affected house
CF2_red_ph_pc<-CF2_ph/E_avg_aff

CF3_PJ<-sum(CF[CF$Scenario=="CF3",]$value)
CF3_red<-base_PJ-CF3_PJ
CF3_red_pc<-CF3_red/base_PJ
CF3_ph<-CF3_red*1e9/del_stock # reduction from CF1 per affected house
CF3_red_ph_pc<-CF3_ph/E_avg_aff

CF4_PJ<-sum(CF[CF$Scenario=="CF4",]$value)
CF4_red<-base_PJ-CF4_PJ
CF4_red_pc<-CF4_red/base_PJ
CF4_ph<-CF4_red*1e9/del_stock # reduction from CF1 per affected house
CF4_red_ph_pc<-CF4_ph/E_avg_aff

CF5_PJ<-sum(CF[CF$Scenario=="CF5",]$value)
CF5_red<-base_PJ-CF5_PJ
CF5_red_pc<-CF5_red/base_PJ
CF5_ph<-CF5_red*1e9/del_stock # reduction from CF5 per affected house
CF5_red_ph_pc<-CF5_ph/E_avg_aff
sphcf1sf<-sum(rowSums(sph15uhyp2-sph15u)[2:3])*1e-9
sphcf1mf<-sum(rowSums(sph15uhyp2-sph15u)[4:5])*1e-9
sphcf1sfpc<-sphcf1sf/(sum(sph15u)*1e-9)
sphcf1mfpc<-sphcf1mf/(sum(sph15u)*1e-9)
# above calcs don't match with this measure of change in SPH in CF1, due to differnt labeling?
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
# to do this i need to compare e.g. space heat in SFD in CF4 vs CF2, can look as specific cohorts too
# if the offsets are not large enough, the sum will give a negative number, if too large, will be positive
sfdsphcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Heat"& CF$Type=="SF Det",]$value)
sfdsphcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Heat"& CF$Type=="SF Det",]$value)
sfdsphcf4-sfdsphcf2

sfdspccf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Cool"& CF$Type=="SF Det",]$value)
sfdspccf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Cool"& CF$Type=="SF Det",]$value)
sfdspccf4-sfdspccf2

sfddhwcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Hot Water"& CF$Type=="SF Det",]$value)
sfddhwcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Hot Water"& CF$Type=="SF Det",]$value)
sfddhwcf4-sfddhwcf2

sfdothcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Other"& CF$Type=="SF Det",]$value)
sfdothcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Other"& CF$Type=="SF Det",]$value)
sfdothcf4-sfdothcf2
#
sfasphcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Heat"& CF$Type=="SF Att",]$value)
sfasphcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Heat"& CF$Type=="SF Att",]$value)
sfasphcf4-sfasphcf2

sfaspccf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Space Cool"& CF$Type=="SF Att",]$value)
sfaspccf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Space Cool"& CF$Type=="SF Att",]$value)
sfaspccf4-sfaspccf2

sfadhwcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Hot Water"& CF$Type=="SF Att",]$value)
sfadhwcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Hot Water"& CF$Type=="SF Att",]$value)
sfadhwcf4-sfadhwcf2

sfaothcf4<-sum(CF[CF$Scenario=="CF4" & CF$enduse=="Other"& CF$Type=="SF Att",]$value)
sfaothcf2<-sum(CF[CF$Scenario=="CF2" & CF$enduse=="Other"& CF$Type=="SF Att",]$value)
sfaothcf4-sfaothcf2
#### graph of average affected house #######
# want to make graph showing energy consumption in average SF since 1986 and energy if those houses
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
# start here 
p <- ggplot(data=comp, aes(x = Scenario, y = Reduction))+ ylim(0,42)+
  geom_col(aes(fill = Source), width = 0.75) +
  # theme_minimal() +
  labs(title = "Comparison of energy reduction strategies in single-family homes", y = "Energy Reduction (%)", x="Reduction strategy") + theme_bw() +
  theme(axis.text=element_text(size=11.5),
        axis.title=element_text(size=13,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Set1")
# scale_fill_hue(l=45) axis.text=element_text(size=10.5)
windows()
p + theme(legend.position="bottom",legend.text=element_text(size=11))  

r<-ggplot(data=cf4, aes(x=Cohort, y=value, fill=Type)) + scale_x_discrete(limits=pos_region) + 
  geom_bar(stat="identity") + # coord_flip() +
  # facet_grid(~Scenario) +
  facet_wrap(~Scenario + Stock + Energy, nrow = 1) +
  labs(title="b) Scenarios of urban energy consumption by Type-Cohort, 2015", x="Cohort",  y="PJ", fill="Type")  + #theme_bw() +
  theme(plot.title = element_text(size=16,face = "bold"),strip.text.x = element_text(size = 12),axis.text = element_text(size = 11),axis.title=element_text(size=12,face = "bold")) +
  scale_fill_brewer(palette="Dark2")
windows()
r + theme(legend.position="bottom") 


# hot water models ###############
# # if we include fuelh20, makes a huge improvement in model fit. Dont want to do that yet
# hh_dhw<-lme(BTUDHW~HDD65 + TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
# dhwRE<-matrix(data=hh_dhw[["coefficients"]][["random"]][["TC"]],5,7,byrow = TRUE)
# row.names(dhwRE)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
# dhwRE<-dhwRE[rownames(t2),,drop=FALSE]
# red_dhw<-sum(sweep(s15u_diff3,MARGIN = 1,FUN = "*",STATS = dhwRE))*1.055e-9 # change in space cooling in PJ
# 
# 
# windows()
# plot(ranef(hh_dhw),ylab = "Type and Cohort",xlab = "Effects on water heating (kBTU)")
# plot(ranef(hh_dhw))
# hh_dhwRE <- ranef(hh_dhw, aug = TRUE)
# plot(hh_dhwRE, form = ~ TYPEHUQ)
# plot(hh_dhwRE, form = (Intercept) ~ TYPEHUQ)
# plot(hh_dhwRE, form = (Intercept) ~ HDD65)
# 
# hh_dhw_type<-lme(BTUDHW~HDD65 + TOTSQFT + Cohort + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TYPEHUQ)
# plot(ranef(hh_dhw_type))
# tab_model(hh_dhw,hh_dhw_type)
# #plots 
# type_off<-hh_dhw_type[["coefficients"]][["random"]][["TYPEHUQ"]]
# windows()
# par(mar=c(5.1,6.1,4.1,2.1))
# boxplot(hh_dhwRE$`(Intercept)`~hh_dhwRE$TYPEHUQ,main = "Differences in water heating by type and cohort range",ylab="",yaxt="none")#,ylim=c(-20000,30000))
# title(ylab="Random effects by type, cohort (kBTU)", line=4)
# axis(2, seq(-2000,2000,1000),las=2,labels = formatC(seq(-2000,2000,1000),big.mark = ",",format = "d")) 
# # points(1:5, as.numeric(means), pch = 19, cex = 0.75,col= "red")
# points(1:5, as.numeric(type_off), pch = 19, cex = 0.75,col= "blue")
# abline(h = 0, col = "darkgreen",lwd=2)
# legend(4.5,2000,legend= paste("RanEff by","\n", "type only"),col="blue",pch = 16,cex=0.85,y.intersp = 1.5)
# 
# # other end use models #############
# hh_oth<-lme(BTUOTH~TOTSQFT + INC + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TC)
# othRE<-matrix(data=hh_oth[["coefficients"]][["random"]][["TC"]],5,7,byrow = TRUE)
# row.names(othRE)<-c("Man Housing","MF high","MF low","SF Att","SF Det")
# othRE<-othRE[rownames(t2),,drop=FALSE]
# red_oth<-sum(sweep(s15u_diff3,MARGIN = 1,FUN = "*",STATS = othRE))*1.055e-9 # change in space cooling in PJ
# 
# 
# windows()
# plot(ranef(hh_oth),ylab = "Type and Cohort",xlab = "Effects on other energy (kBTU)")
# hh_othRE <- ranef(hh_oth, aug = TRUE)
# plot(hh_othRE, form = ~ TYPEHUQ)
# plot(hh_othRE, form = (Intercept) ~ TYPEHUQ)
# 
# hh_oth_type<-lme(BTUOTH~TOTSQFT + INC + Cohort + NHSLDMEM + RECSYEAR, rusmall, random = ~  1 | TYPEHUQ)
# plot(ranef(hh_oth_type))
# tab_model(hh_oth,hh_oth_type)
# #plots 
# type_off<-hh_oth_type[["coefficients"]][["random"]][["TYPEHUQ"]]
# windows()
# par(mar=c(5.1,6.1,4.1,2.1))
# boxplot(hh_othRE$`(Intercept)`~hh_othRE$TYPEHUQ,main = "Differences in other energy use by type and cohort range",ylab="",yaxt="none")#,ylim=c(-20000,30000))
# title(ylab="Random effects by type, cohort (kBTU)", line=4)
# axis(2, seq(-6000,6000,2000),las=2,labels = formatC(seq(-6000,6000,2000),big.mark = ",",format = "d")) 
# # points(1:5, as.numeric(means), pch = 19, cex = 0.75,col= "red")
# points(1:5, as.numeric(type_off), pch = 19, cex = 0.75,col= "blue")
# abline(h = 0, col = "darkgreen",lwd=2)
# legend(4.5,2000,legend= paste("RanEff by","\n", "type only"),col="blue",pch = 16,cex=0.85,y.intersp = 1.5)

# prepare division level regressions #########
# rd<-ru
# rd$Cohort<-as.factor(rd$Cohort)
# d<-rd[rd$TYPEHUQ=="SF Det" | rd$TYPEHUQ =="SF Att",]
# c<-rd[rd$TYPEHUQ=="SF Det",]
# e<-rd[rd$AgeCohort=="<1950" | rd$TYPEHUQ =="1950s" | rd$TYPEHUQ == "1960s",]
# f<-rd[rd$FUELHEAT=="ELEC",]
# w<-rd[rd$FUELH2O=="ELEC",]
# pop<-1e-6*as.numeric(unlist(tapply(rd$NHSLDMEM*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# sph<-1.055e-9*as.numeric(unlist(tapply(rd$BTUSPH*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# col<-1.055e-9*as.numeric(unlist(tapply(rd$BTUCOL*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# dhw<-1.055e-9*as.numeric(unlist(tapply(rd$BTUDHW*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# oth<-1.055e-9*as.numeric(unlist(tapply(rd$BTUOTH*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# pcSFD<-100*as.numeric(unlist(tapply(c$NWEIGHT,list(c$RECSYEAR,c$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# pcOLD<-100*as.numeric(unlist(tapply(e$NWEIGHT,list(e$RECSYEAR,e$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# pcAC<-100*as.numeric(unlist(tapply(rd$NWEIGHT*rd$AIRCOND,list(rd$RECSYEAR,rd$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# pcElHt<-100*as.numeric(unlist(tapply(f$NWEIGHT,list(f$RECSYEAR,f$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# pcElHW<-100*as.numeric(unlist(tapply(w$NWEIGHT,list(w$RECSYEAR,w$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# sqft<-as.numeric(unlist(tapply(rd$TOTHSQFT*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# tsqft<-as.numeric(unlist(tapply(rd$TOTSQFT*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# hdd<-as.numeric(unlist(tapply(rd$HDD65*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# cdd<-as.numeric(unlist(tapply(rd$CDD65*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# hh<-as.numeric(unlist(tapply(rd$NHSLDMEM*rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)/tapply(rd$NWEIGHT,list(rd$RECSYEAR,rd$DIVISION),sum)))
# year<-as.numeric(unlist(tapply(as.numeric(as.character(rd$RECSYEAR)),list(rd$RECSYEAR,rd$DIVISION),mean)))
# div<-as.numeric(unlist(tapply(as.numeric(rd$DIVISION),list(rd$RECSYEAR,rd$DIVISION),mean)))
# reg<-data.frame(year,div,pop,pcSFD,pcOLD,pcAC,pcElHt,pcElHW,sqft,tsqft,hh,sph,col,dhw,oth,cdd, hdd)
# cr<-round(cor(reg),2)
# # linear models at division level, all end-uses
# divh<-lm(sph ~ pop + hdd + pcSFD + pcOLD + hh,data = reg)
# divc<-lm(col ~ pop + cdd + pcSFD + pcOLD + hh,data = reg)
# divhw<-lm(dhw ~ pop + hdd + pcSFD + pcOLD + hh,data = reg)
# divoth<-lm(oth ~ pop + pcSFD + pcOLD + hh,data = reg)
# tab_model(divh,divc,divhw,divoth,show.ci = 0)
# 
# divh2<-lm(sph ~ pop + hdd + pcSFD + pcOLD + hh + pcElHt,data = reg)
# divhw2<-lm(dhw ~ pop + hdd + pcSFD + pcOLD + hh + pcElHW,data = reg)
# tab_model(divh2,divc,divhw2,divoth,show.ci = 0)
