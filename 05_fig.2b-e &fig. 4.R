
rm(list=ls())
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "geofacet", "ggpubr", "ggthemes", 'readxl', 'lubridate','ggplot2',"ggside","readxl",'gghalves')
lapply(packages, library, character.only = TRUE)
# Load data and filter for the year 2023
data <- read.csv("~/Desktop/02 extreme weather&policy/10 data/data0617 all.csv", header = T)
data<-data[data$year==2023,]

# Round selected columns to 2 decimal places
data$pop34 <- round(data$pop34, 2)
data$pop50 <- round(data$pop50, 2)
data$pop64 <- round(data$pop64, 2)
data$precc2 <- round(data$precc / 5) * 5

data$precc2<-ifelse(data$precc2>155,155,data$precc2)
data$precc2<-ifelse(data$precc2< -15,-15,data$precc2)

data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    prec_lag1 = lag(precc2, 1),
    prec_lag2 = lag(precc2, 2),
    prec_lag3 = lag(precc2, 3)
  ) %>%
  ungroup()
# Add lag variables for precipitation
data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    kt34_lag1 = lag(pop34_2, 1),
    kt34_lag2 = lag(pop34_2, 2),
    kt34_lag3 = lag(pop34_2, 3)
  ) %>%
  ungroup()

data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    kt50_lag1 = lag(pop50_2, 1),
    kt50_lag2 = lag(pop50_2, 2),
    kt50_lag3 = lag(pop50_2, 3)
  ) %>%
  ungroup()

data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    kt64_lag1 = lag(pop64_2, 1),
    kt64_lag2 = lag(pop64_2, 2),
    kt64_lag3 = lag(pop64_2, 3)
  ) %>%
  ungroup()

data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    Cpolicy_lag1 = lag(Cpolicy, 1),
    Cpolicy_lag2 = lag(Cpolicy, 2),
  ) %>%
  ungroup()

data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    Ppolicy_lag1 = lag(Ppolicy, 1),
    Ppolicy_lag2 = lag(Ppolicy, 2),
    Ppolicy_lag3 = lag(Ppolicy, 3)
  ) %>%
  ungroup()

data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    Npolicy_lag1 = lag(Npolicy, 1),
    Npolicy_lag2 = lag(Npolicy, 2),
    Npolicy_lag3 = lag(Npolicy, 3)
  ) %>%
  ungroup()


data <- data %>%
  group_by(citycode) %>%
  arrange(time) %>%
  mutate(
    Rpolicy_lag1 = lag(Rpolicy, 1),
    Rpolicy_lag2 = lag(Rpolicy, 2),
  ) %>%
  ungroup()


####import mb of ts and rainfall
X34 <- read_csv("23/3/intra/result/er 34kt.csv")
X34<-X34[,c(1,16)]
colnames(X34)<-c('er34kt','pop34_2')

X341 <- read_csv("23/3/intra/result/er 34kt.csv")
X341<-X341[,c(2,16)]
colnames(X341)<-c('er34kt1','kt34_lag1')

X342 <- read_csv("23/3/intra/result/er 34kt.csv")
X342<-X342[,c(3,16)]
colnames(X342)<-c('er34kt2','kt34_lag2')

X343 <- read_csv("23/3/intra/result/er 34kt.csv")
X343<-X343[,c(4,16)]
colnames(X343)<-c('er34kt3','kt34_lag3')


X50 <- read_csv("23/3/intra/result/er 50kt.csv")
X50<-X50[,c(1,23)]
colnames(X50)<-c('er50kt','pop50_2')

X501 <- read_csv("23/3/intra/result/er 50kt.csv")
X501<-X501[,c(2,23)]
colnames(X501)<-c('er50kt1','kt50_lag1')

X502 <- read_csv("23/3/intra/result/er 50kt.csv")
X502<-X502[,c(3,23)]
colnames(X502)<-c('er50kt2','kt50_lag2')

X503 <- read_csv("23/3/intra/result/er 50kt.csv")
X503<-X503[,c(4,23)]
colnames(X503)<-c('er50kt3','kt50_lag3')

X64 <- read_csv("23/3/intra/result/er 64kt.csv")
X64<-X64[,c(1,23)]
colnames(X64)<-c('er64kt','pop64_2')

X641 <- read_csv("23/3/intra/result/er 64kt.csv")
X641<-X641[,c(2,23)]
colnames(X641)<-c('er64kt1','kt64_lag1')
X642 <- read_csv("23/3/intra/result/er 64kt.csv")
X642<-X642[,c(3,23)]
colnames(X642)<-c('er64kt2','kt64_lag2')
X643 <- read_csv("23/3/intra/result/er 64kt.csv")
X643<-X643[,c(4,23)]
colnames(X643)<-c('er64kt3','kt64_lag3')

all<-merge(data,X34,by='pop34_2',all.x=T)
all<-merge(all,X341,by='kt34_lag1',all.x=T)
all<-merge(all,X342,by='kt34_lag2',all.x=T)
all<-merge(all,X343,by='kt34_lag3',all.x=T)
all<-merge(all,X50,by='pop50_2',all.x=T)
all<-merge(all,X501,by='kt50_lag1',all.x=T)
all<-merge(all,X502,by='kt50_lag2',all.x=T)
all<-merge(all,X503,by='kt50_lag3',all.x=T)
all<-merge(all,X64,by='pop64_2',all.x=T)
all<-merge(all,X641,by='kt64_lag1',all.x=T)
all<-merge(all,X642,by='kt64_lag2',all.x=T)
all<-merge(all,X643,by='kt64_lag3',all.x=T)

prec <- read_csv("23/3/intra/result/er prec.csv")
prec<-prec[,c(1,16)]
colnames(prec)<-c('erprec','precc2')

prec1 <- read_csv("23/3/intra/result/er prec.csv")
prec1<-prec1[,c(2,16)]
colnames(prec1)<-c('erprec1','prec_lag1')

prec2 <- read_csv("23/3/intra/result/er prec.csv")
prec2<-prec2[,c(3,16)]
colnames(prec2)<-c('erprec2','prec_lag2')

prec3 <- read_csv("23/3/intra/result/er prec.csv")
prec3<-prec3[,c(4,16)]
colnames(prec3)<-c('erprec3','prec_lag3')

#summary(data$precc2)
#summary(prec$precc2)

all<-merge(all,prec,by='precc2',all.x=T)
all<-merge(all,prec1,by='prec_lag1',all.x=T)
all<-merge(all,prec2,by='prec_lag2',all.x=T)
all<-merge(all,prec3,by='prec_lag3',all.x=T)


####import mb of early warning
cpolicy <- read_csv("23/3/intra/result/er cpolicy.csv")
cpolicy<-cpolicy[,c(1,9)]
colnames(cpolicy)<-c('ercpolicy','Cpolicy')

cpolicy1 <- read_csv("23/3/intra/result/er cpolicy.csv")
cpolicy1<-cpolicy1[,c(2,9)]
colnames(cpolicy1)<-c('ercpolicy1','Cpolicy_lag1')

ppolicy <- read_csv("23/3/intra/result/er ppolicy.csv")
ppolicy<-ppolicy[,c(1,9)]
colnames(ppolicy)<-c('erppolicy','Ppolicy')

ppolicy1 <- read_csv("23/3/intra/result/er ppolicy.csv")
ppolicy1<-ppolicy1[,c(2,9)]
colnames(ppolicy1)<-c('erppolicy1','Ppolicy_lag1')

ppolicy2 <- read_csv("23/3/intra/result/er ppolicy.csv")
ppolicy2<-ppolicy2[,c(3,9)]
colnames(ppolicy2)<-c('erppolicy2','Ppolicy_lag2')

ppolicy3 <- read_csv("23/3/intra/result/er ppolicy.csv")
ppolicy3<-ppolicy3[,c(4,9)]
colnames(ppolicy3)<-c('erppolicy3','Ppolicy_lag3')

npolicy <- read_csv("23/3/intra/result/er npolicy.csv")
npolicy<-npolicy[,c(1,9)]
colnames(npolicy)<-c('ernpolicy','Npolicy')

npolicy1 <- read_csv("23/3/intra/result/er npolicy.csv")
npolicy1<-npolicy1[,c(2,9)]
colnames(npolicy1)<-c('ernpolicy1','Npolicy_lag1')

npolicy2 <- read_csv("23/3/intra/result/er npolicy.csv")
npolicy2<-npolicy2[,c(3,9)]
colnames(npolicy2)<-c('ernpolicy2','Npolicy_lag2')

npolicy3 <- read_csv("23/3/intra/result/er npolicy.csv")
npolicy3<-npolicy3[,c(4,9)]
colnames(npolicy3)<-c('ernpolicy3','Npolicy_lag3')

rpolicy <- read_csv("23/3/intra/result/er Rpolicy.csv")
rpolicy<-rpolicy[,c(1,9)]
colnames(rpolicy)<-c('errpolicy','Rpolicy')

rpolicy1 <- read_csv("23/3/intra/result/er Rpolicy.csv")
rpolicy1<-rpolicy1[,c(2,9)]
colnames(rpolicy1)<-c('errpolicy1','Rpolicy_lag1')

all<-merge(all,cpolicy,by='Cpolicy',all.x=T)
all<-merge(all,cpolicy1,by='Cpolicy_lag1',all.x=T)
all<-merge(all,ppolicy,by='Ppolicy',all.x=T)
all<-merge(all,ppolicy1,by='Ppolicy_lag1',all.x=T)
all<-merge(all,ppolicy2,by='Ppolicy_lag2',all.x=T)
all<-merge(all,ppolicy3,by='Ppolicy_lag3',all.x=T)
all<-merge(all,npolicy,by='Npolicy',all.x=T)
all<-merge(all,npolicy1,by='Npolicy_lag1',all.x=T)
all<-merge(all,npolicy2,by='Npolicy_lag2',all.x=T)
all<-merge(all,npolicy3,by='Npolicy_lag3',all.x=T)
all<-merge(all,rpolicy,by='Rpolicy',all.x=T)
all<-merge(all,rpolicy1,by='Rpolicy_lag1',all.x=T)
####calculate er contribution of the exposure day
all$disaster<- 1-(1-all$er34kt)*(1-all$er50kt)*(1-all$er64kt)*(1-all$erprec)

all$warning<-1-(1-all$ercpolicy)*(1-all$erppolicy)*(1-all$ernpolicy)*(1-all$errpolicy)

#contribution of city-level tc warning
all$warningcp<-all$ercpolicy/(1-(1-all$warning)*(1-all$disaster))
#contribution of provincial-level tc warning
all$warningpp<-all$erppolicy/(1-(1-all$warning)*(1-all$disaster))

#contribution of national-level tc warning
all$warningnp<-all$ernpolicy/(1-(1-all$warning)*(1-all$disaster))

#contribution of national-level tc warning
all$warningrp<-all$errpolicy/(1-(1-all$warning)*(1-all$disaster))

#contribution of all warning
all$warningall<-all$warning/(1-(1-all$warning)*(1-all$disaster))


####calculate er contribution of the exposure -1 day
all[is.na(all)] <- 0
all$warning1<-1-(1-all$ercpolicy1)*(1-all$erppolicy1)*(1-all$ernpolicy1)*(1-all$errpolicy1)*(1-all$warning)

all$disaster1<- 1-(1-all$er34kt1)*(1-all$er50kt1)*(1-all$er64kt1)*(1-all$erprec1)*(1-all$disaster)


#contribution of city-level tc warning
all$warningcp1<-(1-(1-all$ercpolicy)*(1-all$ercpolicy1))/(1-(1-all$warning1)*(1-all$disaster1))

#contribution of provincial-level tc warning
all$warningpp1<-(1-(1-all$erppolicy)*(1-all$erppolicy1))/(1-(1-all$warning1)*(1-all$disaster1))

#contribution of national-level tc warning
all$warningnp1<-(1-(1-all$ernpolicy)*(1-all$ernpolicy1))/(1-(1-all$warning1)*(1-all$disaster1))

#contribution of national-level tc warning
all$warningrp1<-(1-(1-all$errpolicy)*(1-all$errpolicy1))/(1-(1-all$warning1)*(1-all$disaster1))

#contribution of all warning
all$warningall1<-all$warning1/(1-(1-all$warning1)*(1-all$disaster1))



#contribution of all warning
all$warningall1increase<-1-(1-all$ercpolicy1)*(1-all$erppolicy1)*(1-all$ernpolicy1)*(1-all$errpolicy1)/(1-(1-all$warning1)*(1-all$disaster1))

####calculate er contribution of the exposure -2 day

all$warning2<-1-(1-all$erppolicy2)*(1-all$ernpolicy2)*(1-all$warning1)

all$disaster2<- 1-(1-all$erprec2)*(1-all$er34kt2)*(1-all$er50kt2)*(1-all$er64kt2)*(1-all$disaster1)

#contribution of city-level tc warning
all$warningcp2<-(1-(1-all$ercpolicy)*(1-all$ercpolicy1))/(1-(1-all$warning2)*(1-all$disaster2))

#contribution of provincial-level tc warning
all$warningpp2<-(1-(1-all$erppolicy)*(1-all$erppolicy1)*(1-all$erppolicy2))/(1-(1-all$warning2)*(1-all$disaster2))

#contribution of national-level tc warning
all$warningnp2<-(1-(1-all$ernpolicy)*(1-all$ernpolicy1)*(1-all$ernpolicy2))/(1-(1-all$warning2)*(1-all$disaster2))

#contribution of national-level tc warning
all$warningrp2<-(1-(1-all$errpolicy)*(1-all$errpolicy1))/(1-(1-all$warning2)*(1-all$disaster2))

#contribution of all warning
all$warningall2<-all$warning2/(1-(1-all$warning2)*(1-all$disaster2))

#contribution of all warning
all$warningall2increase<-1-(1-all$erppolicy2)*(1-all$ernpolicy2)/(1-(1-all$warning2)*(1-all$disaster2))

####calculate er contribution of the exposure -3 day

all$warning3<-1-(1-all$erppolicy3)*(1-all$ernpolicy3)*(1-all$warning2)

all$disaster3<- 1-(1-all$erprec3)*(1-all$er34kt3)*(1-all$er50kt3)*(1-all$er64kt3)*(1-all$disaster2)

#contribution of city-level tc warning
all$warningcp3<-(1-(1-all$ercpolicy)*(1-all$ercpolicy1))/(1-(1-all$warning3)*(1-all$disaster3))

#contribution of provincial-level tc warning
all$warningpp3<-(1-(1-all$erppolicy)*(1-all$erppolicy1)*(1-all$erppolicy2)*(1-all$erppolicy3))/(1-(1-all$warning3)*(1-all$disaster3))

#contribution of national-level tc warning
all$warningnp3<-(1-(1-all$ernpolicy)*(1-all$ernpolicy1)*(1-all$ernpolicy2)*(1-all$ernpolicy3))/(1-(1-all$warning3)*(1-all$disaster3))

#contribution of national-level tc warning
all$warningrp3<-(1-(1-all$errpolicy)*(1-all$errpolicy1))/(1-(1-all$warning3)*(1-all$disaster3))

#contribution of all warning
all$warningall3<-all$warning3/(1-(1-all$warning3)*(1-all$disaster3))
#contribution of all warning
all$warningall3increase<-1-(1-all$erppolicy3)*(1-all$ernpolicy3)/(1-(1-all$warning3)*(1-all$disaster3))

all$warningcity3<-1-(1-all$ercpolicy)*(1-all$ercpolicy1)
all$warningprov3<-1-(1-all$erppolicy)*(1-all$erppolicy1)*(1-all$erppolicy2)*(1-all$erppolicy3)
all$warningna3<-1-(1-all$ernpolicy)*(1-all$ernpolicy1)*(1-all$ernpolicy2)*(1-all$ernpolicy3)
all$warningrain3<-1-(1-all$errpolicy)*(1-all$errpolicy1)
all$ts3<-1-(1-all$er34kt)*(1-all$er50kt)*(1-all$er64kt)*(1-all$er34kt1)*(1-all$er50kt1)*(1-all$er64kt1)*(1-all$er34kt2)*(1-all$er50kt2)*(1-all$er64kt2)*(1-all$er34kt3)*(1-all$er50kt3)*(1-all$er64kt3)
all$prec3<-1-(1-all$erprec)*(1-all$erprec1)*(1-all$erprec2)*(1-all$erprec3)

all$ts<-0
all$ts<-ifelse(all$pop34_2>0,1,all$ts)
all$ts<-ifelse(all$pop64_2>0,3,all$ts)
all$ts<-ifelse(all$pop50_2>0&all$pop64_2==0,2,all$ts)

all2<-all[all$pop34_2>0|all$pop50_2>0|all$pop64_2>0,]
summary(all2$warningall)

data_long <- all2 %>%
  pivot_longer(cols = c(warningcp, warningpp, warningnp, warningrp, warningall),
               names_to = "variable", values_to = "value")

#data_long$value <- data_long$value * 100  

ggplot(data_long, aes(x = factor(variable, levels = c("warningcp", "warningpp", "warningnp", "warningrp", "warningall")), y = value)) +
  geom_boxplot(width=0.5) +
  theme_classic () +
  geom_signif(comparisons = list(
    c("warningcp", "warningall"),
    c("warningpp", "warningall"),
    c("warningnp", "warningall"),
    c("warningrp", "warningall")),
    map_signif_level = TRUE, test = "wilcox.test", step_increase = 0.08)+
  labs( x = "Warning",y = "Protection rate (%)")+
  scale_y_continuous(labels = scales::percent)

data_long2 <- all2 %>%
  pivot_longer(cols = c(warningall, warningall1, warningall2, warningall3),
               names_to = "variable", values_to = "value")

ggplot(data_long2, aes(x = factor(variable, levels = c("warningall", "warningall1", "warningall2", "warningall3")), y = value)) +
  geom_boxplot(width=0.5) +
  theme_classic () +
  geom_signif(comparisons = list(
    c("warningall1", "warningall"),
    c("warningall2", "warningall"),
    c("warningall3", "warningall")),
    map_signif_level = TRUE, test = "wilcox.test", step_increase = 0.1)+
    labs( x = "Warning",y = "Protection rate (*100%)")  +
  scale_y_continuous(limits = c(0, 1.3),labels = scales::percent)


data_long3 <- all2 %>%
  pivot_longer(cols = c(warningcp3, warningpp3, warningnp3, warningrp3, warningall3),
               names_to = "variable", values_to = "value")
data_long3$value <- data_long3$value * 100  
ggplot(data_long3, aes(x = factor(variable, levels = c("warningcp3", "warningpp3", "warningnp3", "warningrp3", "warningall3")), y = value)) +
  geom_boxplot(width=0.5) +
  theme_classic2() +
  geom_signif(comparisons = list(
    c("warningcp3", "warningall3"),
    c("warningpp3", "warningall3"),
    c("warningnp3", "warningall3"),
    c("warningrp3", "warningall3")
   ),
    map_signif_level = TRUE, test = "wilcox.test", step_increase = 0.08)+
  labs( x = "Warning",y = "Protection rate (%)")   



data_long7 <- all2 %>%  
  filter(ts %in% c(1, 2, 3)) %>%  
  pivot_longer(cols = starts_with("warning"),  
               names_to = "variable",   
               values_to = "value") %>%  
  mutate(value = value * 100) 

data_long7 <- all2 %>%  
  filter(ts %in% c(1, 2, 3)) %>%  
  pivot_longer(cols = starts_with("warning"),  
               names_to = "variable",   
               values_to = "value") %>%  
  mutate(value = value * 100, 
         ts = factor(ts)) 

ggplot(data_long7, aes(x = factor(variable, levels = c("warningcp3", "warningpp3", "warningnp3", "warningrp3", "warningall3")),   
                      y = value, fill = ts)) +  
  geom_boxplot(width = 0.5, outlier.shape = NA) +  
  geom_signif(comparisons = list(  
    c("warningcp3", "warningall3"),  
    c("warningpp3", "warningall3"),  
    c("warningnp3", "warningall3"),  
    c("warningrp3", "warningall3")),  
    map_signif_level = TRUE, test = "wilcox.test", test.args = list(exact = FALSE),   
    step_increase = 0.08) +  
  facet_wrap(~ ts) +  
  ylim(0,100) +  
  theme_classic() +  
  labs(x = "Warning", y = "Protection rate (%)", fill = "Time Series (ts)")

#####resilience index

event_data <- read_csv("event_data_all.csv")
event_data<-event_data[event_data$year==2023,]
event_data<-event_data[event_data$days>-4,]

cpolicysum <- read_csv("23/3/intra/result/er cpolicy.csv")
cpolicysum<-cpolicysum[,c(9,10)]
colnames(cpolicysum)<-c('Cpolicy','ercpolicysum')
ppolicysum <- read_csv("23/3/intra/result/er ppolicy.csv")
ppolicysum<-ppolicysum[,c(9,10)]
colnames(ppolicysum)<-c('Ppolicy','erppolicysum')
npolicysum <- read_csv("23/3/intra/result/er npolicy.csv")
npolicysum<-npolicysum[,c(9,10)]
colnames(npolicysum)<-c('Npolicy','ernpolicysum')

rpolicysum <- read_csv("23/3/intra/result/er Rpolicy.csv")
rpolicysum<-rpolicysum[,c(9,10)]
colnames(rpolicysum)<-c('Rpolicy','errpolicysum')

event_data<-merge(event_data,cpolicysum,by='Cpolicy',all.x=T)
event_data<-merge(event_data,ppolicysum,by='Ppolicy',all.x=T)
event_data<-merge(event_data,npolicysum,by='Npolicy',all.x=T)
event_data<-merge(event_data,rpolicysum,by='Rpolicy',all.x=T)

event_data$warningsum <- 0
event_data$warningsum <- 1-(1-event_data$ercpolicysum)*(1-event_data$erppolicysum)*(1-event_data$ernpolicysum)*(1-event_data$errpolicysum)
event_data$warningtssum <- 1-(1-event_data$ercpolicysum)*(1-event_data$erppolicysum)*(1-event_data$ernpolicysum)
event_data$warningcitysum <- 1-(1-event_data$ercpolicysum)*(1-event_data$errpolicysum)


resl <- event_data %>%
  group_by(citycode, typhoon_name) %>%
  summarise(
         resl0 = sum(ifelse(intrac < 0, intrac, 0), na.rm = TRUE),
         warningcts = sum(ercpolicysum, na.rm = TRUE),
         warningpts = sum(erppolicysum, na.rm = TRUE),
         warningnts = sum(ernpolicysum, na.rm = TRUE),
         warningrts = sum(errpolicysum, na.rm = TRUE),
         warningsum = sum(warningsum, na.rm = TRUE),
         warningtssum = sum(warningtssum, na.rm = TRUE),
         warningcitysum = sum(warningcitysum, na.rm = TRUE),
         tm = n_distinct(ifelse(intrac < 0, days, 0)),
         exposure_type = first(exposure_type),  # 假设每个组的exposure_type值相同
         warning_tpye = max(Cpolicy, na.rm = TRUE),
         warning_time = min(ifelse(Cpolicy> 0, days, 1)),
         .groups = "drop")

resl<-resl[resl$resl0!=0,]

resl$resl1<-1+resl$resl0/resl$tm
resl$resl2<-1+(resl$resl0+resl$warningsum)/resl$tm

resl$reslincrease<-resl$resl2-resl$resl1

summary(resl$reslincrease1/resl$reslincrease)

resl <- resl %>%
  mutate(
    contribution_warningcts = ifelse(warningcts > 0, (1 - warningpts) * (1 - warningnts) * (1 - warningrts), 0),
    contribution_warningpts = ifelse(warningpts > 0, (1 - warningcts) * (1 - warningnts) * (1 - warningrts), 0),
    contribution_warningnts = ifelse(warningnts > 0, (1 - warningcts) * (1 - warningpts) * (1 - warningrts), 0),
    contribution_warningrts = ifelse(warningrts > 0, (1 - warningcts) * (1 - warningpts) * (1 - warningnts), 0)
  )

# calculation contributions
resl <- resl %>%
  mutate(
    total_contribution = contribution_warningcts + contribution_warningpts + contribution_warningnts + contribution_warningrts
  )

resl <- resl %>%
  mutate(
    proportion_warningcts = ifelse(total_contribution > 0, contribution_warningcts / total_contribution, 0),
    proportion_warningpts = ifelse(total_contribution > 0, contribution_warningpts / total_contribution, 0),
    proportion_warningnts = ifelse(total_contribution > 0, contribution_warningnts / total_contribution, 0),
    proportion_warningrts = ifelse(total_contribution > 0, contribution_warningrts / total_contribution, 0)
  )


library(ggpubr)
resl<-resl[resl$typhoon_name!='haikui'&resl$typhoon_name!="saola",]
exposure_order <- c( "34","50","64")  
resl$exposure_type <- factor(resl$exposure_type, levels = exposure_order) 

ggplot(resl, aes(x = exposure_type, y = resl1, group = as.factor(exposure_type))) +
  geom_half_violin(trim = FALSE,
                   side="r",  
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.15,fill = "white") +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Comprehensive Urban Resilience ")+
  scale_y_continuous(limits = c(0.7, 1.1), expand = c(0, 0))+
  coord_flip() +  
  theme_bw() 

ggplot(resl, aes(x = exposure_type, y = resl2, group = as.factor(exposure_type))) +
  geom_half_violin(trim = FALSE, 
                   side="r", 
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.15,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Urban Resilience to TC")+
  scale_y_continuous(limits = c(0.7, 1.1), expand = c(0, 0))+
  coord_flip() + 
  theme_bw() 

ggplot(resl, aes(x = exposure_type, y = reslincrease, group = as.factor(exposure_type))) +
  geom_half_violin(trim = FALSE, 
                   side="r", 
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Urban Resilience Improvement")+
  scale_y_continuous(limits = c(-0.1, 0.25), expand = c(0, 0))+
  coord_flip() +  # 横向绘制
  theme_bw() 

df_resl <- resl %>%
  group_by(exposure_type) %>%
  summarise(
    reslincrease_mean = mean(reslincrease, na.rm = TRUE),
    proportion_warningcts = mean(proportion_warningcts, na.rm = TRUE),
    proportion_warningpts = mean(proportion_warningpts, na.rm = TRUE),
    proportion_warningnts = mean(proportion_warningnts, na.rm = TRUE),
    proportion_warningrts = mean(proportion_warningrts, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("proportion_"),
    names_to = "proportion_type",
    values_to = "proportion_value"
  )

df_resl <- df_resl %>%
  mutate(
    proportion_reslincrease = reslincrease_mean * proportion_value
  )

proportion_order <- c( "proportion_warningrts","proportion_warningnts","proportion_warningpts","proportion_warningcts")  
df_resl$proportion_type <- factor(df_resl$proportion_type , levels = proportion_order) 


ggplot(df_resl, aes(x = as.factor(exposure_type), y = proportion_reslincrease, fill = proportion_type)) +
  geom_bar(stat = "identity", position = "stack",width=0.5) +
  scale_fill_manual(values = c("proportion_warningcts" = "#1F618D", 
                               "proportion_warningpts" = "#5499C7",
                               "proportion_warningnts" = "#A9CCE3", 
                               "proportion_warningrts" = "#D4E6F1")) +
  labs(x = "Exposure Type", y = "Mean Reslincrease") +
  theme_bw() +
  coord_flip() 


ggplot(resl, aes(x = warning_tpye, y = resl1, group = as.factor(warning_tpye))) +
  geom_half_violin(trim = FALSE, 
                   side="r",  
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Comprehensive Urban Resilience ")+
  scale_y_continuous(limits = c(0.63, 1.18), expand = c(0, 0))+
  coord_flip() + 
  theme_bw() 


ggplot(resl, aes(x = warning_tpye, y = resl2, group = as.factor(warning_tpye))) +
  geom_half_violin(trim = FALSE, 
                   side="r",  
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Urban Resilience to TC ")+
  scale_y_continuous(limits = c(0.63, 1.18), expand = c(0, 0))+
  coord_flip() + 
  theme_bw() 


ggplot(resl, aes(x = warning_tpye, y = reslincrease, group = as.factor(warning_tpye))) +
  geom_half_violin(trim = FALSE, 
                   side="r", 
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Urban Resilience Improvement")+
  scale_y_continuous(limits = c(-0.1, 0.25), expand = c(0, 0))+
  coord_flip() +  
  theme_bw() 

df_resl <- resl %>%
  group_by(warning_tpye) %>%
  summarise(
    reslincrease_mean = mean(reslincrease, na.rm = TRUE),
    proportion_warningcts = mean(proportion_warningcts, na.rm = TRUE),
    proportion_warningpts = mean(proportion_warningpts, na.rm = TRUE),
    proportion_warningnts = mean(proportion_warningnts, na.rm = TRUE),
    proportion_warningrts = mean(proportion_warningrts, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("proportion_"),
    names_to = "proportion_type",
    values_to = "proportion_value"
  )

df_resl <- df_resl %>%
  mutate(
    proportion_reslincrease = reslincrease_mean * proportion_value
  )

proportion_order <- c( "proportion_warningrts","proportion_warningnts","proportion_warningpts","proportion_warningcts")  
df_resl$proportion_type <- factor(df_resl$proportion_type , levels = proportion_order) 

ggplot(df_resl, aes(x = as.factor(warning_tpye), y = proportion_reslincrease, fill = proportion_type)) +
  geom_bar(stat = "identity", position = "stack",width=0.5) +
  scale_fill_manual(values = c("proportion_warningcts" = "#1F618D", 
                               "proportion_warningpts" = "#5499C7",
                               "proportion_warningnts" = "#A9CCE3", 
                               "proportion_warningrts" = "#D4E6F1")) +
  labs(x = "Exposure Type", y = "Mean Reslincrease") +
  theme_bw() +
  coord_flip() 

###different lead time
time_order <- c( "1","0","-1","-2","-3")  
resl$warning_time <- factor(resl$warning_time , levels = time_order) 

ggplot(resl, aes(x = warning_time, y = resl1, group = as.factor(warning_time))) +
  geom_half_violin(trim = FALSE, 
                   side="r",  
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Comprehensive Urban Resilience")+
  scale_y_continuous(limits = c(0.7, 1.13), expand = c(0, 0))+
  coord_flip() +  
  theme_bw() 

ggplot(resl, aes(x = warning_time, y = resl2, group = as.factor(warning_time))) +
  geom_half_violin(trim = FALSE, 
                   side="r",  
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Urban Resilience to TC")+
  scale_y_continuous(limits = c(0.7, 1.13), expand = c(0, 0))+
  coord_flip() + 
  theme_bw() 

ggplot(resl, aes(x = warning_time, y = reslincrease, group = as.factor(warning_time))) +
  geom_half_violin(trim = FALSE, 
                   side="r",  
                   scale='width',width=1) + 
  stat_boxplot(geom="errorbar",width=0.1,size=0.7,color="black")+
  geom_boxplot(size=0.4,
               width=0.2,fill = "white",) +
  scale_fill_manual(values = c("#D2B48C", "#A0522D", "#8B4513",'blue','grey')) +
  stat_summary(fun = "mean", geom = "point", shape = 21, size = 2, fill = "red", color = "black") +
  labs(title = "Urban Resilience Improvement")+
  scale_y_continuous(limits = c(-0.1, 0.25), expand = c(0, 0))+
  coord_flip() + 
  theme_bw() 


df_resl <- resl %>%
  group_by(warning_time) %>%
  summarise(
    reslincrease_mean = mean(reslincrease, na.rm = TRUE),
    proportion_warningcts = mean(proportion_warningcts, na.rm = TRUE),
    proportion_warningpts = mean(proportion_warningpts, na.rm = TRUE),
    proportion_warningnts = mean(proportion_warningnts, na.rm = TRUE),
    proportion_warningrts = mean(proportion_warningrts, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("proportion_"),
    names_to = "proportion_type",
    values_to = "proportion_value"
  )

df_resl <- df_resl %>%
  mutate(
    proportion_reslincrease = reslincrease_mean * proportion_value
  )

proportion_order <- c( "proportion_warningrts","proportion_warningnts","proportion_warningpts","proportion_warningcts")  
df_resl$proportion_type <- factor(df_resl$proportion_type , levels = proportion_order) 


ggplot(df_resl, aes(x = as.factor(warning_time), y = proportion_reslincrease, fill = proportion_type)) +
  geom_bar(stat = "identity", position = "stack",width=0.5) +
  scale_fill_manual(values = c("proportion_warningcts" = "#1F618D", 
                               "proportion_warningpts" = "#5499C7",
                               "proportion_warningnts" = "#A9CCE3", 
                               "proportion_warningrts" = "#D4E6F1")) +
  labs(x = "Exposure Type", y = "Mean Reslincrease") +
  theme_bw() +
  coord_flip() 
