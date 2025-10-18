## R script to prepare data and lagged variables for INLA-DLNM modelling

# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

# load INLA
library(INLA)
library(MASS)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep","readxl",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

## load data
# Climate and case/intervention/holiday data in the first week for lagged values 
data_all <- read.csv("/Volumes/ssd/02 extreme weather&policy/10 data/data0617 all.csv", header = T)
data_all<-data_all[data_all$year==2023,]

# Test correlation for Fig S8 & Fig S9
# filtered_data <- data_all[,c("windc","precc","tempc","pop34_2","pop50_2","pop64_2","X34kt", "X50kt", "X64kt", "X80kt","X100kt","Cpolicy",'Ppolicy','Npolicy','Rpolicy',"gdppercapita","pop")]
# filtered_data2 <- data_all[data_all$pop34_2>0,c("pop34_2","pop50_2","pop64_2","Cpolicy",'Ppolicy','Npolicy','Rpolicy')]
# filtered_data3 <- data_all[data_all$X34kt>0,c("X34kt", "X50kt", "X64kt", "X80kt","X100kt","Cpolicy",'Ppolicy','Npolicy','Rpolicy')]
# Calculate the correlation matrix for filtered data
# cor_matrix <- cor(filtered_data, use = "complete.obs")
# # Set up the PDF file and specify its dimensions
# pdf("correlation_matrix.pdf", width = 12, height = 12)
# 
# # Generate the correlation plot
# ggcorrplot(cor_matrix,
#            method = "square",               # Use squares to represent correlation
#            lab = TRUE,                      # Display correlation coefficients
#            title = "Correlation Matrix of Filtered Data",
#            colors = c("blue", "white", "red"), # Specify the gradient colors
#            hc.order = FALSE,                # Keep the original order without reordering
#            type = "lower",                  # Display only the lower triangle of the matrix
#            outline.color = "white"          # Set the outline color
# ) +
#   scale_fill_gradientn(colours = c("blue", "white", "red"),
#                        limits = c(0, 1),      # Set limits from 0 to 1
#                        breaks = seq(0, 1, 0.5), # Scale ticks from 0 to 1 at intervals of 0.5
#                        labels = seq(0, 1, 0.5)) # Display scale labels
# 
# # Close the PDF device to save the file
# dev.off()

####cities in different geographic locations
# coastal<-read_excel("/Volumes/ssd/02 extreme weather&policy/07 socioeco/gdp0.xlsx")
# coastal<-coastal[,c(1,9)]
# colnames(coastal)<-c('city','coastalcities')
# 
# data_all<-merge(data_all,coastal,by.x='city.x',by.y='city',all.x=T)
# data_all$coastalcities[is.na(data_all$coastalcities)]<-0
# 
# data_all<-data_all[data_all$coastalcities==0,]
# unique(data_all$citycode)

####cities with different tc frequency
# frequent1 <- data_all %>%
#   filter(X34kt > 0) %>%
#   group_by(citycode) %>%
#   summarise(exposeall= n_distinct(time0))
# summary(frequent1$exposeall)
# 
# data_all<-data_all[data_all$year==2023,]
# frequent2 <- data_all %>%
#   filter(X34kt > 0) %>%
#   group_by(citycode) %>%
#   summarise(expose= n_distinct(time0))
# summary(frequent2$expose)
# 
# data_all<-merge(data_all,frequent1,by='citycode',all.x=T)
# data_all<-merge(data_all,frequent2,by='citycode',all.x=T)
# 
# data_all$frequ<-0
# data_all$frequ<-ifelse(data_all$exposeall>6&data_all$expose>4,2,data_all$frequ) ##high 
# unique(data_all$citycode[data_all$frequ==2])
# data_all$frequ<-ifelse((!data_all$exposeall<6)&data_all$expose<=4,1,data_all$frequ) ##low
# unique(data_all$citycode[data_all$frequ==1])

# data_all <- data_all[!is.na(data_all$citycode), ]
# data_all<-data_all[data_all$frequ==1,]


####cities with different socio-economic status
# gdp <- read_excel("/Volumes/ssd/02 extreme weather&policy/07 socioeco/gdp.xlsx")
# gdp<-gdp[,c(1,5)]
# colnames(gdp)<-c('city','gdppercapita')
# gdp$gdppercapita<-as.numeric(gdp$gdppercapita)
# gdp$gdppercapita<-log(gdp$gdppercapita)
# 
# data_all<-merge(data_all,gdp,by.x='city.x',by.y='city',all.x=T)
# quantile(data_all$gdppercapita.y,0.7,na.rm=TRUE)
# 
# data_all$rich<-ifelse(data_all$gdppercapita.y>11.23963,1,0)
# data_all$poor<-ifelse(data_all$gdppercapita.y<10.67602,1,0)
# 
# data_all<-data_all[data_all$poor==1,]
# unique(data_all$citycode)

# load shape file 
map_all <- read_sf("/Volumes/ssd/02 extreme weather&policy/05 行政区域/city_cn/city_cn.shp")
map <- map_all[map_all$city_code %in% unique(data_all$citycode),]
# dim(map)

# Create adjacency matrix
sf::sf_use_s2(FALSE) # to fix 4 features with invalid spherical geometry
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "/Volumes/ssd/02 extreme weather&policy/10 data/map.graph"
# if (!file.exists(g.file)) nb2INLA(g.file, nb.map)
nb2INLA(g.file, nb.map)

## integrate data
# Create lagged variables
# set max lag - by day
nlag<-14
nlag2<-7
nlag3<-21

lag_wind<-tsModel::Lag(data_all$windc, group = data_all$citycode, k = 0:nlag)
lag_prec <- tsModel::Lag(data_all$precc, group = data_all$citycode, k = 0:nlag)
lag_Stringencyc <- tsModel::Lag(data_all$Stringencyc, group = data_all$citycode, k = 0:nlag)

lag_34<-tsModel::Lag(data_all$pop34_2, group = data_all$citycode, k = 0:nlag)
lag_50<-tsModel::Lag(data_all$pop50_2, group = data_all$citycode, k = 0:nlag3)
lag_64<-tsModel::Lag(data_all$pop64_2, group = data_all$citycode, k = 0:nlag3)

lag_npolicy <- tsModel::Lag(data_all$Npolicy, group = data_all$citycode, k = 0:nlag2)
lag_cpolicy <- tsModel::Lag(data_all$Cpolicy, group = data_all$citycode, k = 0:nlag2)
lag_ppolicy <- tsModel::Lag(data_all$Ppolicy, group = data_all$citycode, k = 0:nlag2)
lag_rpolicy <- tsModel::Lag(data_all$Rpolicy, group = data_all$citycode, k = 0:nlag2)
###hollland wind model
# lag_h34<-tsModel::Lag(data_all$X34kt, group = data_all$citycode, k = 0:nlag)
# lag_h50<-tsModel::Lag(data_all$X50kt, group = data_all$citycode, k = 0:nlag3)
# lag_h64<-tsModel::Lag(data_all$X64kt, group = data_all$citycode, k = 0:nlag3)
# lag_h80<-tsModel::Lag(data_all$X80kt, group = data_all$citycode, k = 0:nlag3)
# lag_h100<-tsModel::Lag(data_all$X100kt, group = data_all$citycode, k = 0:nlag3)

# Remove weeks from lagged variables
lag_wind <- lag_wind[is.na(data_all$week)==F,]
lag_prec <- lag_prec[is.na(data_all$week)==F,]
lag_Stringencyc<-lag_Stringencyc[is.na(data_all$week)==F,]

lag_34<-lag_34[is.na(data_all$week)==F,]
lag_50<-lag_50[is.na(data_all$week)==F,]
lag_64<-lag_64[is.na(data_all$week)==F,]
lag_npolicy <- lag_npolicy[is.na(data_all$week)==F,]
lag_cpolicy <- lag_cpolicy[is.na(data_all$week)==F,]
lag_ppolicy <- lag_ppolicy[is.na(data_all$week)==F,]
lag_rpolicy <- lag_rpolicy[is.na(data_all$week)==F,]

# lag_h34<-lag_h34[is.na(data_all$week)==F,]
# lag_h50<-lag_h50[is.na(data_all$week)==F,]
# lag_h64<-lag_h64[is.na(data_all$week)==F,]
# lag_h80<-lag_h80[is.na(data_all$week)==F,]
# lag_h100<-lag_h100[is.na(data_all$week)==F,]

data <- data_all[is.na(data_all$week)==F,]
head(data)

## define dimensions
# re-define time indicator 
unique(data$time)
# data$time <- data$time - 7

# total number of days
nday <- length(unique(data$time))
# total number of weeks
nweek <- length(unique(data$week))

# total number of cities
ncity <- length(unique(data$citycode))
# total number of provinces
nprov <- length(unique(data$prov_code))

# define cross-basis matrix (combining nonlinear exposure and lag functions)
# set lag knots
lagknot1 = equalknots(0:nlag, 2)
lagknot2 = equalknots(0:nlag2, 2)
lagknot3 = equalknots(0:nlag3, 2)
# fun 'ns' - Generate a Basis Matrix for Natural Cubic Splines
var <- lag_wind
basis_wind <- crossbasis(var,
                    argvar = list(fun = "ns", knots = equalknots(data$windc, 2)),
                    arglag = list(fun = "ns", knots = nlag/2))
head(basis_wind)


var <- lag_prec
basis_prec <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$precc, 2)),
                         arglag = list(fun = "ns", knots = nlag/2))
head(basis_prec)

var <- lag_34
basis_34 <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$pop34_2, 2)),
                         arglag = list(fun = "ns", knots = nlag/2))
head(basis_34)

var <- lag_50
basis_50 <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$pop50_2, 2)),
                       arglag = list(fun = "ns", knots = nlag3/2))
head(basis_50)

var <- lag_64
basis_64 <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$pop64_2, 2)),
                       arglag = list(fun = "ns", knots = nlag3/2))
head(basis_64)


# var <- lag_h34
# basis_h34 <- crossbasis(var,
#                        argvar = list(fun = "ns", knots = equalknots(data$X34kt, 2)),
#                        arglag = list(fun = "ns", knots = nlag/2))
# head(basis_h34)
# 
# var <- lag_h50
# basis_h50 <- crossbasis(var,
#                        argvar = list(fun = "ns", knots = equalknots(data$X50kt, 2)),
#                        arglag = list(fun = "ns", knots = nlag3/2))
# head(basis_h50)
# 
# var <- lag_h64
# basis_h64 <- crossbasis(var,
#                        argvar = list(fun = "ns", knots = equalknots(data$X64kt, 2)),
#                        arglag = list(fun = "ns", knots = nlag3/2))
# head(basis_h64)
# 
# var <- lag_h80
# basis_h80 <- crossbasis(var,
#                         argvar = list(fun = "ns", knots = equalknots(data$X80kt, 2)),
#                         arglag = list(fun = "ns", knots = nlag3/2))
# head(basis_h80)
# 
# var <- lag_h100
# basis_h100 <- crossbasis(var,
#                         argvar = list(fun = "ns", knots = equalknots(data$X100kt, 2)),
#                         arglag = list(fun = "ns", knots = nlag3/2))
# head(basis_h100)

# covid intervention stringency
# var <- lag_Stringencyc
# basis_Stringencyc <- crossbasis(var,
#                           argvar = list(fun="ns", knots = equalknots(data$Stringencyc, 2)),
#                           arglag = list(fun="ns", knots = lagknot3))
# head(basis_Stringencyc)

var <- lag_npolicy
basis_npolicy <- crossbasis(var,
          argvar = list(fun="ns", knots = equalknots(data$Npolicy, 2)),
                            arglag = list(fun="ns", knots = lagknot2))
head(basis_npolicy)

var <- lag_cpolicy
basis_cpolicy <- crossbasis(var,
                            argvar = list(fun="ns", knots = equalknots(data$Cpolicy, 2)),
                            arglag = list(fun="ns", knots = lagknot2))
head(basis_cpolicy)
summary(data$Ppolicy)
var <- lag_ppolicy
basis_ppolicy <- crossbasis(var,
                            argvar = list(fun="ns", knots = equalknots(data$Ppolicy, 2)),
                            arglag = list(fun="ns", knots = lagknot2))
head(basis_ppolicy)

var <- lag_rpolicy
basis_rpolicy <- crossbasis(var,
                            argvar = list(fun="ns", knots = equalknots(data$Rpolicy, 2)),
                            arglag = list(fun="ns", knots = lagknot2))
head(basis_rpolicy)


# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_wind) = paste0("basis_wind.", colnames(basis_wind))
colnames(basis_prec) = paste0("basis_prec.", colnames(basis_prec))

colnames(basis_34) = paste0("basis_34.", colnames(basis_34))
colnames(basis_50) = paste0("basis_50.", colnames(basis_50))
colnames(basis_64) = paste0("basis_64.", colnames(basis_64))

# colnames(basis_h34) = paste0("basis_h34.", colnames(basis_h34))
# colnames(basis_h50) = paste0("basis_h50.", colnames(basis_h50))
# colnames(basis_h64) = paste0("basis_h64.", colnames(basis_h64))
# colnames(basis_h80) = paste0("basis_h80.", colnames(basis_h80))
# colnames(basis_h100) = paste0("basis_h100.", colnames(basis_h100))

# colnames(basis_Stringencyc) = paste0("basis_Stringencyc.", colnames(basis_Stringencyc))
colnames(basis_npolicy) = paste0("basis_npolicy.", colnames(basis_npolicy))
colnames(basis_cpolicy) = paste0("basis_cpolicy.", colnames(basis_cpolicy))
colnames(basis_ppolicy) = paste0("basis_ppolicy.", colnames(basis_ppolicy))
colnames(basis_rpolicy) = paste0("basis_rpolicy.", colnames(basis_rpolicy))

# create city index 

data$city_index <- rep(1:ncity, nday)


# state length
k <- unique(data$prov_code)

for (j in 1:nprov){
  data$prov_index[data$prov_code == k[j]] <- j 
}
data$pop
# create week index

data$week_index <- data$week

#### set up data and priors for INLA model
## set data for models - try intraflow first, then inflow and outflow
Y  <- data$intrac # response variable
N  <- length(Y) # total number of data points
# random variable
T1 <- data$weekday # for random effect to account for day-of-week effect
T2 <- data$week_index # for random effect to account for inter-week variability
S1 <- data$city_index # for city-level spatial random effect
S2 <- data$prov_index # for provincial interaction with daily random effect
# Other variables
Vp <- data$gdppercapita
Vh <- data$holiday
Vpop <- data$pop


# create dataframe for model testing
df <- data.frame(Y, T1, T2, S1, S2, Vh)
# define priors

precision.prior2 <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
# inla model function

# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
mymodel <- function(formula, data = df, family = "Gaussian", config = FALSE)

  {
  model <- inla(formula = formula, data = data, family = family, 
       control.inla = list(strategy = 'adaptive',int.strategy='eb'), 
       control.compute = list(dic = TRUE, config = config, 
                              cpo = TRUE, return.marginals = FALSE),
       control.fixed = list(correlation.matrix = TRUE, 
                            prec.intercept = 1, prec =1),
       control.predictor = list(link = 1, compute = TRUE), 
       verbose = FALSE)
  model <- inla.rerun(model)
  return(model)
}
