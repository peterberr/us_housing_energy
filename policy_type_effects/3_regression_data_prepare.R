# create dataset for RECS regressions
# Peter Berrill
# last updated Jan 11 2021

rm(list=ls()) # clear workspace
cat("\014") # clear console
library(dplyr)
# Read in RECS data for all RECS years since 1990, adjusted to include space heating from biomass and other non-counted fuels (coal, solar, district steam heat)
load("RECSincOth.Rdata")

# make some data cleaning adjustments ########### 
# remove year 1997 because of poor data quality, especially surrounding floor area estimates
rh<-RECS[!(RECS$RECSYEAR==1997),] # rh is the dataset I will use for the household regression

rh$AgeCohort<-as.factor(rh$AgeCohort)

levels(rh$EQUIPAGE)<-levels(recode(rh$EQUIPAGE,"0"="NA/DontKnow","1"="<2YR","2"="2-4YR","3"="5-9YR","4"="10-19YR","5"="20+YR"))
rh<-within(rh,EQUIPAGE<-relevel(EQUIPAGE, ref = "NA/DontKnow"))

levels(rh$TYPEHUQ)<-levels(recode(rh$TYPEHUQ,"2"="SF Det","1"="Manuf Housing","3"="SF Att","4"="MF low","5"="MF high"))
levels(rh$WHEATAGE)<-levels(recode(rh$WHEATAGE,"99"="NA/DontKnow","1"="<2YR","2"="2-4YR","3"="5-9YR","4"="10-19YR","5"="20+YR"))
levels(rh$WHEATSIZ)<-levels(recode(rh$WHEATSIZ,"9"="NA/DontKnow","1"="Small","2"="Medium","3"="Large"))
rh<-within(rh,WHEATSIZ<-relevel(WHEATSIZ, ref = "NA/DontKnow"))

rh$HEATGAS <- rh$HEATEL <-rh$HEATFO <- rh$HEATLP <- rh$HEATKR <-rh$HEATPRICE <- rh$HEATWD<-rep(NA,dim(rh)[1])
for (j in 1:dim(rh)[1]) {
  if (rh$FUELHEAT[j]==1) {rh$HEATGAS[j]<-1; rh$HEATPRICE[j]<-rh$HEATGAS[j]*rh$BTURATENG[j]}
  if (rh$FUELHEAT[j]==2) {rh$HEATLP[j]<-1; rh$HEATPRICE[j]<-rh$HEATLP[j]*rh$BTURATELP[j]}
  if (rh$FUELHEAT[j]==3) {rh$HEATFO[j]<-1; rh$HEATPRICE[j]<-rh$HEATFO[j]*rh$BTURATEFO[j]}
  if (rh$FUELHEAT[j]==4) {rh$HEATKR[j]<-1; rh$HEATPRICE[j]<-rh$HEATKR[j]*rh$BTURATEKR[j]}
  if (rh$FUELHEAT[j]==5) {rh$HEATEL[j]<-1; rh$HEATPRICE[j]<-rh$HEATEL[j]*rh$BTURATEEL[j]}
  if (rh$FUELHEAT[j]==7) {rh$HEATWD[j]<-1; rh$HEATPRICE[j]<-rh$HEATWD[j]*rh$BTURATEWD[j]}
  
}
rh[rh$FUELHEAT=="4",]$FUELHEAT<-"3" # treat kerosene as fuel oil
rh$FUELHEAT[rh$FUELHEAT == "6" | rh$FUELHEAT == "8" | rh$FUELHEAT =="9"]<-"21" # treat coal, solar, and district steam as 'other'
levels(rh$FUELHEAT)<-levels(recode(rh$FUELHEAT,"3"="OIL","0" = "None","1"="NGAS","2"="LPG","5"="ELEC","7"="WOOD","21"="Other"))
rh$FUELHEAT<-droplevels(rh$FUELHEAT)
rh<-within(rh, FUELHEAT<-relevel(FUELHEAT, ref = "None"))
# 
# # edit some factor variables to change the reference #######
rh<-within(rh, TYPEHUQ<-relevel(TYPEHUQ, ref = "Manuf Housing")) # make level 2 ("Single-family detached") as the reference for TYPEHUQ
rh$FUELH2O[rh$FUELH2O=="6" | rh$FUELH2O =="8" | rh$FUELH2O == "9"]<-"21"
rh$FUELH2O<-droplevels(rh$FUELH2O)
levels(rh$FUELH2O)<-levels(recode(rh$FUELH2O,"3"="OIL","0" = "None","1"="NGAS","2"="LPG","5"="ELEC","7"="WOOD","21"="Other"))
rh<-within(rh, FUELH2O<-relevel(FUELH2O, ref = "None"))
# 
# filter/clean data , removing observations likely to skew results
rh$RECSYEAR=as.factor(rh$RECSYEAR)
rh$BUSINESS[is.na(rh$BUSINESS)]<-0 # households with home business
rh$HBUSNESS[is.na(rh$HBUSNESS)]<-0 # households with home business
rh$FARM[is.na(rh$FARM)]<-0 # household running a farm
rh$OTHERUSE[is.na(rh$OTHERUSE)]<-0 # households with other non-standard energy use
rh$TENANT[is.na(rh$TENANT)]<-0 # tenant is a flag to indicate whether energy bills are used to cover fuel use by another household, such as a tenant
rh<-rh[!(rh$BUSINESS==1| rh$HBUSNESS==1 |rh$FARM==1 |rh$OTHERUSE==1 |rh$TENANT==1),]

rh$NumStories<-rh$STORIES
rh$NumStories[rh$NumStories == 10]<-1
rh$NumStories[rh$NumStories == 20]<-2
rh$NumStories[rh$NumStories == 31]<-3
rh$NumStories[rh$NumStories == 4 |rh$NumStories == 5 |rh$NumStories == 8 | rh$NumStories == 9 | rh$NumStories == 40 | rh$NumStories == 50| rh$NumStories == 99| rh$NumStories == -2]<-0
rh$NumStories[rh$NumStories == 32]<-4
rh$SF<-0
rh$SF[rh$TYPEHUQ=="SF Det" |rh$TYPEHUQ=="SF Att" ]<-1

# make some extra changes and create data set for urban households only 
rh<-within(rh,EQUIPAGE<-relevel(EQUIPAGE, ref = "NA/DontKnow"))
rh<-within(rh,WHEATSIZ<-relevel(WHEATSIZ, ref = "NA/DontKnow"))
rh<-within(rh, FUELHEAT<-relevel(FUELHEAT, ref = "None"))
levels(rh$TYPEHUQ)[levels(rh$TYPEHUQ)=="Manuf Housing"] <- "Man Housing"
rh<-within(rh, TYPEHUQ<-relevel(TYPEHUQ, ref = "Man Housing")) # make level 2  "Man Housing" as the reference for TYPEHUQ
rh<-within(rh, FUELH2O<-relevel(FUELH2O, ref = "None"))
rh$Cohort<-rh$AgeCohort
rh$Cohort[rh$Cohort== "2010s"]<-"2000s"
rh$Cohort[rh$Cohort== "1960s"]<-"1950s"
rh$Cohort<-droplevels(rh$Cohort)
levels(rh$Cohort)[levels(rh$Cohort)=="2000s"] <- "2000+"
levels(rh$Cohort)[levels(rh$Cohort)=="1950s"] <- "1950-60s"
# make dataframe with urban observations only
rh2<-rh[rh$URBAN==1,]
ru2<-rh[rh$URBAN==1,]
save(rh,ru2,file="RECSUrbanRegDataNew.Rdata")

# calcualte GHGI of end-uses, check the units #########
load("RECSGHG.RData")
ru15<-RECSGHG[RECSGHG$RECSYEAR=="2015",]
ru15<-ru15[ru15$URBAN==1,]
ghgi_sph<-tapply(ru15$NWEIGHT*ru15$GHGSPH,ru15$TYPEHUQ,sum)/tapply(ru15$NWEIGHT*ru15$BTUSPH,ru15$TYPEHUQ,sum)
ghgi_spc<-tapply(ru15$NWEIGHT*ru15$GHGCOL,ru15$TYPEHUQ,sum)/tapply(ru15$NWEIGHT*ru15$BTUCOL,ru15$TYPEHUQ,sum)
ghgi_dhw<-tapply(ru15$NWEIGHT*ru15$GHGDHW,ru15$TYPEHUQ,sum)/tapply(ru15$NWEIGHT*ru15$BTUDHW,ru15$TYPEHUQ,sum)
ghgi_oth<-tapply(ru15$NWEIGHT*ru15$GHGOTH,ru15$TYPEHUQ,sum)/tapply(ru15$NWEIGHT*ru15$BTUOTH,ru15$TYPEHUQ,sum)

ghgi_avg<-sum(ru15$NWEIGHT*ru15$GHGSPH)/sum(ru15$NWEIGHT*ru15$BTUSPH)
ghgi_avg[2]<-sum(ru15$NWEIGHT*ru15$GHGCOL)/sum(ru15$NWEIGHT*ru15$BTUCOL)
ghgi_avg[3]<-sum(ru15$NWEIGHT*ru15$GHGDHW)/sum(ru15$NWEIGHT*ru15$BTUDHW)
ghgi_avg[4]<-sum(ru15$NWEIGHT*ru15$GHGOTH)/sum(ru15$NWEIGHT*ru15$BTUOTH)
save(ghgi_sph,ghgi_spc,ghgi_dhw,ghgi_oth,ghgi_avg,file='GHGI_eu_ty.RData')
