rm(list=ls())

## data collation
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "geofacet", "ggpubr", "ggthemes", 'readxl', 'lubridate','dplyr','readr')
lapply(packages, library, character.only = TRUE)

map <- read_sf("/Volumes/ssd/02 extreme weather&policy/05 行政区域/py/市.shp")
sf::sf_use_s2(FALSE) # to fix 4 features with invalid spherical geometry
map$area <- st_area(map)/1000000 #km2

map$prov_code <- as.integer(substr(map$shi_dm,1,2))
prov_code <- unique(map$prov_code)
dat <- cbind(city_code=map$shi_dm, city=map$shi_mc, prov_code=map$prov_code, city_area=map$area)
dat[duplicated(dat[,c('city_code')])==T,]
dat <- as.data.frame(dat)
## climate
windmean21<-read_excel("/Volumes/ssd/02 extreme weather&policy/02 windspeed/shi/2021meanmax.xlsx")
windmean21 <- reshape2::melt(windmean21, id.vars = c('shi_mc'), value.name = 'windmean')
colnames(windmean21) <- c('citycode','time','windmean')
windmean22<-read_excel("/Volumes/ssd/02 extreme weather&policy/02 windspeed/shi/2022meanmax.xlsx")
windmean22 <- reshape2::melt(windmean22, id.vars = c('shi_mc'), value.name = 'windmean')
colnames(windmean22) <- c('citycode','time','windmean')
windmean23<-read_excel("/Volumes/ssd/02 extreme weather&policy/02 windspeed/shi/2023meanmax.xlsx")
windmean23 <- reshape2::melt(windmean23, id.vars = c('shi_mc'), value.name = 'windmean')
colnames(windmean23) <- c('citycode','time','windmean')

wind<-rbind(windmean21,windmean22)
wind<-rbind(wind,windmean23)
wind$time <- as.character(wind$time)
wind$time <- as.integer(wind$time)

prec21<-read_excel("/Volumes/ssd/02 extreme weather&policy/01 prec/shi/2021.xlsx")
prec21 <- reshape2::melt(prec21, id.vars = c('shi_mc'), value.name = 'prec')
colnames(prec21) <- c('citycode','time','prec')
prec21$time <- as.character(prec21$time)
prec21$time <- as.integer(prec21$time)
prec21$time<-20210000+prec21$time

prec22<-read_excel("/Volumes/ssd/02 extreme weather&policy/01 prec/shi/2022.xlsx")
prec22 <- reshape2::melt(prec22, id.vars = c('shi_mc'), value.name = 'prec')
colnames(prec22) <- c('citycode','time','prec')
prec22$time <- as.character(prec22$time)
prec22$time <- as.integer(prec22$time)
prec22$time<-20220000+prec22$time

prec23<-read_excel("/Volumes/ssd/02 extreme weather&policy/01 prec/shi/2023.xlsx")
prec23 <- reshape2::melt(prec23, id.vars = c('shi_mc'), value.name = 'prec')
colnames(prec23) <- c('citycode','time','prec')
#prec23<-merge(precaverage,prec23,by=c('citycode','time'))
prec23$time <- as.character(prec23$time)
prec23$time <- as.integer(prec23$time)
prec23$time<-20230000+prec23$time

prec<-rbind(prec21,prec22)
prec<-rbind(prec,prec23)
colnames(prec)<-c('city','time','prec')
prec <- prec %>%
  left_join(dat, by = "city") 

climate <- merge(prec, wind, by.x = c('city', 'time'), by.y = c('citycode', 'time'))
str(climate)
climate[duplicated(climate[,c('citycode', 'time')])==T,]
climate <- unique(climate)

climate <- climate[order(climate$time, decreasing = F),]
str(climate)

# outflow
outflow <- read_excel('/Volumes/ssd/02 extreme weather&policy/04 mobility/mobility scale.xlsx',  sheet = 'move out',)
outflow$city <- NULL
outflow[duplicated(outflow$`city_code`)==T,]

outflow <- reshape2::melt(outflow, id = c('city_code'), value.name = 'outflow')
colnames(outflow) <- c('citycode', 'time', 'outflow')
outflow$citycode <- as.integer(outflow$citycode)
outflow$time <- as.character(outflow$time)
outflow$time <- as.integer(outflow$time)
str(outflow)
outflow[duplicated(outflow[,c('citycode', 'time')])==T,]

# inflow
inflow <- read_excel('/Volumes/ssd/02 extreme weather&policy/04 mobility/mobility scale.xlsx',  sheet = 'move in',)
# correct wrong city code
# inflow$`city code`[inflow$city == '温州市'] <- '330300'
inflow$city <- NULL
inflow[duplicated(inflow$`city_code`)==T,]

inflow <- reshape2::melt(inflow, id = c('city_code'), value.name = 'inflow')
colnames(inflow) <- c('citycode', 'time', 'inflow')
inflow$citycode <- as.integer(inflow$citycode)
inflow$time <- as.character(inflow$time)
inflow$time <- as.integer(inflow$time)
str(inflow)
# intra-city mobility
intra <- read_excel('/Volumes/ssd/02 extreme weather&policy/04 mobility/mobility scale.xlsx',  sheet = 'intra-city mobility',)

intra$city <- NULL
intra[duplicated(intra$city_code)==T,]

intra <- reshape2::melt(intra, id = c('city_code'), value.name = 'intraflow')
colnames(intra) <- c('citycode', 'time', 'intraflow')
intra$citycode <- as.integer(intra$citycode)
intra$time <- as.character(intra$time)
intra$time <- as.integer(intra$time)
str(intra)

mobility <- merge(outflow, inflow, by=c('citycode', 'time'))
mobility <- merge(mobility, intra, by=c('citycode', 'time'))
mobility$netflow <- mobility$inflow - mobility$outflow
## covid interventions
# Stringency index
stringency <- read_excel('/Volumes/ssd/02 extreme weather&policy/08 NPI/Policy index_province.xlsx',  sheet = 'StringencyIndex',)
stringency$province <- NULL
stringency <- reshape2::melt(stringency, id = c('prov_code'), value.name = 'Stringency')
colnames(stringency) <- c('prov_code', 'time', 'Stringency')
stringency$prov_code <- as.integer(stringency$prov_code)
stringency$time <- as.character(stringency$time)
stringency$time <- as.integer(stringency$time)
str(stringency)
stringency0 <- read_excel('/Volumes/ssd/02 extreme weather&policy/08 NPI/Policy index_province.xlsx',  sheet = 'StringencyIndex',)
stringency<-merge(stringency,stringency0[,c('province','prov_code')],by=c('prov_code'),all.x=T)

stringency2 <- read_csv("/Volumes/ssd/02 extreme weather&policy/08 NPI/ox/OxCGRT_CHN_differentiated_withnotes_2022.csv")
stringency2 <- stringency2[,c("RegionName","Date","StringencyIndex_Vaccinated")]
colnames(stringency2) <- c('province', 'time', 'Stringency')
stringency2$time <- as.character(stringency2$time)
stringency2$time <- as.integer(stringency2$time)
stringency2<-stringency2[stringency2$time>20220308,]
stringency2<-merge(stringency2,stringency0[,c('province','prov_code')],by=c('province'),all.x=T)

stringency<-rbind(stringency,stringency2)

## socioeconomic factors
pop <- read_csv("/Volumes/ssd/02 extreme weather&policy/07 socioeco/7_census_shi.csv")
pop<-pop[,c(3,6,13,14,15)]
colnames(pop)<-c('city','pop','urbanization','oldrate','youngrate')

gdp <- read_excel("/Volumes/ssd/02 extreme weather&policy/07 socioeco/gdp.xlsx")
gdp<-gdp[,c(1,5)]
colnames(gdp)<-c('city','gdppercapita')

socieco<-merge(pop,gdp,by='city',all.x=T)
socieco<-merge(dat[,c('city_code', 'prov_code', 'city')],socieco,by='city',all.x=T)

## merge
data <- merge(mobility, climate, by=c('citycode', 'time'),all.x=T)
data <- merge(data, socieco, by.x=c('citycode','prov_code'),by.y=c('city_code','prov_code'),all.x=T)
data <- merge(data, stringency, by=c('prov_code', 'time'),all.x=T)
data$Stringency[is.na(data$Stringency)==T] <- 0
data$time0<-ymd(data$time)
data$year<-year(data$time0)

data0<-data[data$year>2020,]

#####ts exposure
##data from the first approach(calculated from arcgis pro)
pop34 <- read_csv("/Volumes/ssd/02 extreme weather&policy/02 windspeed/sata/pop34.csv")
pop34 <- reshape2::melt(pop34, id = c('shi_dm'), value.name = 'pop34')
colnames(pop34) <- c('citycode', 'time', 'pop34')
pop34$citycode <- as.integer(pop34$citycode)
pop34$time <- as.character(pop34$time)
pop34$time <- as.integer(pop34$time)
pop34<-pop34%>%drop_na(pop34)

pop50 <- read_csv("/Volumes/ssd/02 extreme weather&policy/02 windspeed/sata/pop50.csv")
pop50 <- reshape2::melt(pop50, id = c('shi_dm'), value.name = 'pop50')
colnames(pop50) <- c('citycode', 'time', 'pop50')
pop50$citycode <- as.integer(pop50$citycode)
pop50$time <- as.character(pop50$time)
pop50$time <- as.integer(pop50$time)
pop50<-pop50%>%drop_na(pop50)

pop64 <- read_csv("/Volumes/ssd/02 extreme weather&policy/02 windspeed/sata/pop64.csv")
pop64 <- reshape2::melt(pop64, id = c('shi_dm'), value.name = 'pop64')
colnames(pop64) <- c('citycode', 'time', 'pop64')
pop64$citycode <- as.integer(pop64$citycode)
pop64$time <- as.character(pop64$time)
pop64$time <- as.integer(pop64$time)
pop64<-pop64%>%drop_na(pop64)

pop<-full_join(pop34,pop50,by=c('citycode','time'))
pop<-full_join(pop,pop64,by=c('citycode','time'))

#####landfall
land34 <- read_csv("/Volumes/ssd/02 extreme weather&policy/02 windspeed/sata/day/34.csv")
land34 <- reshape2::melt(land34, id = c('shi_dm'), value.name = 'land34')
colnames(land34) <- c('citycode', 'time', 'land34')
land34$citycode <- as.integer(land34$citycode)
land34$time <- as.character(land34$time)
land34$time <- as.integer(land34$time)
land34<-land34%>%drop_na(land34)

land50 <- read_csv("/Volumes/ssd/02 extreme weather&policy/02 windspeed/sata/day/50.csv")
land50 <- reshape2::melt(land50, id = c('shi_dm'), value.name = 'land50')
colnames(land50) <- c('citycode', 'time', 'land50')
land50$citycode <- as.integer(land50$citycode)
land50$time <- as.character(land50$time)
land50$time <- as.integer(land50$time)
land50<-land50%>%drop_na(land50)

land64 <- read_csv("/Volumes/ssd/02 extreme weather&policy/02 windspeed/sata/day/64.csv")
land64 <- reshape2::melt(land64, id = c('shi_dm'), value.name = 'land64')
colnames(land64) <- c('citycode', 'time', 'land64')
land64$citycode <- as.integer(land64$citycode)
land64$time <- as.character(land64$time)
land64$time <- as.integer(land64$time)
land64<-land64%>%drop_na(land64)

landfall<-full_join(land34, land50, by = c("citycode", "time"))
landfall<-full_join(landfall, land64, by = c("citycode", "time"))

expose<-full_join(pop,landfall, by = c("citycode", "time"))

expose <- expose %>% mutate(across(everything(), ~ replace(., is.na(.), 0)))

expose$pop34_2<-expose$pop34+expose$land34
expose$pop50_2<-expose$pop50+expose$land50
expose$pop64_2<-expose$pop64+expose$land64

expose <- expose %>%
  mutate(across(-c(citycode, time), ~ ifelse(. > 1, 1, .)))

####ts exposure
##data from the second approach
ts <-read_csv("/Volumes/ssd/02 extreme weather&policy/14 ts/affected_population_2023only.csv")
ts$ratio<-ifelse(ts$ratio<0.003,0,ts$ratio)
colnames(ts)<-c('tfbh','date','radius','citycode','hratio')
ts_wide <- ts %>%
  filter(year(date)>2020)%>%
  pivot_wider(names_from = 'radius', values_from = 'hratio')

data0<-merge(data0,expose, by=c('citycode','time'),all.x=T)
data0<-merge(data0,ts_wide, by=c('citycode','date'),all.x=T)
# warning

city<-read_csv("/Volumes/ssd/02 extreme weather&policy/03 policy/city/city tswarning2.csv")
colnames(city)<-c('city','time0','level')
city<-merge(city,dat,by='city',all.x=T)
city<-city[,c(1,2,3,4)]
city$Cpolicy<-NA
city$Cpolicy[city$level==c('蓝色')]<-1
city$Cpolicy[city$level==c('黄色')]<-2
city$Cpolicy[city$level==c('橙色')]<-3
city$Cpolicy[city$level==c('红色')]<-4
city<-city[,c(2,4,5)]
colnames(city)<-c('time0','citycode','Cpolicy')
city$time0 <- ymd(city$time0)
data0<-merge(data0,city,by=c('citycode','time0'),all.x=T)

pro<-read_csv("/Volumes/ssd/02 extreme weather&policy/03 policy/city/province tswarning2.csv")
pro$Ppolicy<-NA
pro$Ppolicy[pro$level==c('蓝色')]<-1
pro$Ppolicy[pro$level==c('黄色')]<-2
pro$Ppolicy[pro$level==c('橙色')]<-3
pro$Ppolicy[pro$level==c('红色')]<-4
pro<-pro[,c(2,4,5)]
colnames(pro)<-c('time0','prov_code','Ppolicy')
data0<-merge(data0,pro,by=c('prov_code','time0'),all.x=T)

nation<-read_csv("/Volumes/ssd/02 extreme weather&policy/03 policy/city/nation tswarning2.csv")
nation$Npolicy<-NA
nation$Npolicy[nation$level2==c('蓝色')]<-1
nation$Npolicy[nation$level2==c('黄色')]<-2
nation$Npolicy[nation$level2==c('橙色')]<-3
nation$Npolicy[nation$level2==c('红色')]<-4
nation<-nation[,c(2,3,5)]
colnames(nation)<-c('time0','typhoon','Npolicy')
nation$time0 <- ymd(nation$time0)
data0<-merge(data0,nation,by=c('time0'),all.x=T)

data0$weekday<-wday(data0$time0)

###rainstorm policy
rain<-read_csv("/Volumes/ssd/02 extreme weather&policy/03 policy/city/rainstorm2.csv")
rain<-merge(rain,dat,by='city',all.x=T)
rain$Rpolicy<-NA
rain$Rpolicy[rain$level==c('蓝色')]<-1
rain$Rpolicy[rain$level==c('黄色')]<-2
rain$Rpolicy[rain$level==c('橙色')]<-3
rain$Rpolicy[rain$level==c('红色')]<-4

rain<-rain[,c(5,6,9)]
colnames(rain)<-c('time0','citycode','Rpolicy')
rain$time0 <- ymd(rain$time0)
data0<-merge(data0,rain,by=c('citycode','time0'),all.x=T)

###select affected province

expose$prov_code <- as.integer(substr(expose$citycode,1,2))
unique(expose$prov_code)

city$prov_code <- as.integer(substr(city$citycode,1,2))
unique(city$prov_code)

datat<-data0%>%
  filter(prov_code%in%expose$prov_code|prov_code%in%city$prov_code|prov_code%in%c('11','13','41'))
unique(datat$province)

datat<-datat[!is.na(datat$prec),]


datat$gdp<-as.numeric(datat$gdp)
datat$gdp[is.na(datat$gdp)==F]<-log(datat$gdp)
datat$gdppercapita<-as.numeric(datat$gdppercapita)
datat$gdppercapita[is.na(datat$gdppercapita)==F]<-log(datat$gdppercapita)
datat$pop<-log(datat$pop)

datat$Npolicy[is.na(datat$Npolicy)]<-0
datat$Cpolicy[is.na(datat$Cpolicy)]<-0
datat$Ppolicy[is.na(datat$Ppolicy)]<-0
datat$Rpolicy[is.na(datat$Rpolicy)]<-0

datat$`34kt`[is.na(datat$`34kt`)]<-0
datat$`50kt`[is.na(datat$`50kt`)]<-0
datat$`64kt`[is.na(datat$`64kt`)]<-0
datat$`80kt`[is.na(datat$`80kt`)]<-0
datat$`100kt`[is.na(datat$`100kt`)]<-0

datat$area34[is.na(datat$area34)==T] <- 0
datat$area50[is.na(datat$area50)==T] <- 0
datat$area64[is.na(datat$area64)==T] <- 0
datat$pop34[is.na(datat$pop34)==T] <- 0
datat$pop50[is.na(datat$pop50)==T] <- 0
datat$pop64[is.na(datat$pop64)==T] <- 0
datat$pop34_2[is.na(datat$pop34_2)==T] <- 0
datat$pop50_2[is.na(datat$pop50_2)==T] <- 0
datat$pop64_2[is.na(datat$pop64_2)==T] <- 0

data<- datat %>%
  distinct(citycode, time, .keep_all = TRUE)

#######calculate baseline
base202101<-data0[((data0$time>='20210604')&(data0$time<='20210610')),]
base202101<-base202101%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base202101$year<-2021
base202101$period<-1

base20212<-data0[((data0$time>='20210711')&(data0$time<='20210717')),]
base20212<-base20212%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20212$year<-2021
base20212$period<-2

base20213<-data0[((data0$time>='20210904')&(data0$time<='20210910')),]
base20213<-base20213%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20213$year<-2021
base20213$period<-3

base20221<-data0[((data0$time>='20220623')&(data0$time<='20220629')),]
base20221<-base20221%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20221$year<-2022
base20221$period<-1

base20222<-data0[((data0$time>='20220723')&(data0$time<='20220729')),]
base20222<-base20222%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20222$year<-2022
base20222$period<-2

base20223<-data0[((data0$time>='20220816')&(data0$time<='20220822')),]
base20223<-base20223%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20223$year<-2022
base20223$period<-3

base202204<-data0[((data0$time>='20221008')&(data0$time<='20221014')),]
base202204<-base202204%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base202204$year<-2022
base202204$period<-4


base20231<-data0[((data0$time>='20230708')&(data0$time<='20230714')),]
base20231<-base20231%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20231$year<-2023
base20231$period<-1

base20232<-data0[((data0$time>='20230822')&(data0$time<='20230828')),]
base20232<-base20232%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20232$year<-2023
base20232$period<-3

base20233<-data0[((data0$time>='20230921')&(data0$time<='20230927')),]
base20233<-base20233%>%
  group_by(citycode)%>%
  summarise(intrabase=mean(intraflow),inbase=mean(inflow),outbase=mean(outflow),stringencybase=mean(Stringency),tempbase=mean(max_temp),windbase=mean(windmean),precbase=mean(prec))
base20233$year<-2023
base20233$period<-2

base<-rbind(base202101,base20212,base20213,base20221,base20222,base20223,base202204,base20231,base20232,base20233)
###
day<- read_excel("/Volumes/ssd/02 extreme weather&policy/03 policy/taifeng_china.xls", 
                 sheet = "all")
data<-merge(data,day,by='time',all.y=T)


data1 <- merge(data,base, by=c('citycode','year','period'),all.x=T)

data1$intrac<-data1$intraflow/data1$intrabase
data1$inflowc<-data1$inflow/data1$inbase
data1$outflowc<-data1$outflow/data1$outbase

data1$Stringencyc<-data1$Stringency-data1$stringencybase
data1$tempc<-data1$max_temp-data1$tempbase
data1$windc<-data1$windmean-data1$windbase
data1$precc<-data1$prec-data1$precbase

holiday<-read_excel("/Volumes/ssd/02 extreme weather&policy/07 socioeco/festival and holiday.xlsx")
colnames(holiday)<-c('time','holiday')

holiday$holiday<-1
data1<-merge(data1,holiday,by='time',all.x=T)
data1$holiday[is.na(data1$holiday)==T] <- 0

data1<-data1[data1$citycode!='810000'&data1$citycode!='820000'&data1$citycode!='460300',]

data1 <- data1[order( data1$citycode,data1$time)]

fwrite(datat, '/Volumes/ssd/02 extreme weather&policy/10 data/data 0925.csv', row.names = F)

###test baseline, table s9
model<-data[data$year==2023,]

model<-lm(intrac~windc+precc+pop34+pop50+pop64+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data23)
summary(model)
model<-lm(inflowc~windc+precc+pop34+pop50+pop64+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data23)
summary(model)
model<-lm(outflowc~windc+precc+pop34+pop50+pop64+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data23)
summary(model)

data21<-data[data$year==2021,]
model<-lm(intrac~windc+precc+pop34+pop50+pop64+Stringencyc+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data21)
summary(model)
model<-lm(inflowc~windc+precc+pop34+pop50+pop64+Stringencyc+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data21)
summary(model)
model<-lm(outflowc~windc+precc+pop34+pop50+pop64+Stringencyc+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data21)
summary(model)

data22<-data[data$year==2022,]
model<-lm(intrac~windc+precc+pop34+pop50+pop64+Stringencyc+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data22)
summary(model)
model<-lm(inflowc~windc+precc+pop34+pop50+pop64+Stringencyc+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data22)
summary(model)
model<-lm(outflowc~windc+precc+pop34+pop50+pop64+Stringencyc+Npolicy+Cpolicy+Ppolicy+Rpolicy+holiday,data=data22)
summary(model)



