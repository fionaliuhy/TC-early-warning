## R script to prepare data and lagged variables for INLA-DLNM modelling

# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
rm(list=ls())
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
data_all <- read.csv("/Volumes/LaCie/02 extreme weather&policy/10 data/data0617 all.csv", header = T)
data_all<-data_all[data_all$year==2023,]
data_all$intrac<-1-data_all$intrac


# load shape file 
map_all <- read_sf("/Volumes/LaCie/02 extreme weather&policy/05 行政区域/city_cn/city_cn.shp")
map <- map_all[map_all$city_code %in% unique(data_all$citycode),]
# dim(map)

# Create adjacency matrix
sf::sf_use_s2(FALSE) # to fix 4 features with invalid spherical geometry
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "/Volumes/LaCie/02 extreme weather&policy/10 data/map.graph"
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

lag_34<-tsModel::Lag(data_all$pop34_2, group = data_all$citycode, k = 0:nlag)
lag_50<-tsModel::Lag(data_all$pop50_2, group = data_all$citycode, k = 0:nlag3)
lag_64<-tsModel::Lag(data_all$pop64_2, group = data_all$citycode, k = 0:nlag3)

lag_npolicy <- tsModel::Lag(data_all$Npolicy, group = data_all$citycode, k = 0:nlag2)
lag_cpolicy <- tsModel::Lag(data_all$Cpolicy, group = data_all$citycode, k = 0:nlag2)
lag_ppolicy <- tsModel::Lag(data_all$Ppolicy, group = data_all$citycode, k = 0:nlag2)
lag_rpolicy <- tsModel::Lag(data_all$Rpolicy, group = data_all$citycode, k = 0:nlag2)

# Remove weeks from lagged variables
lag_wind <- lag_wind[is.na(data_all$week)==F,]
lag_prec <- lag_prec[is.na(data_all$week)==F,]

lag_34<-lag_34[is.na(data_all$week)==F,]
lag_50<-lag_50[is.na(data_all$week)==F,]
lag_64<-lag_64[is.na(data_all$week)==F,]
lag_npolicy <- lag_npolicy[is.na(data_all$week)==F,]
lag_cpolicy <- lag_cpolicy[is.na(data_all$week)==F,]
lag_ppolicy <- lag_ppolicy[is.na(data_all$week)==F,]
lag_rpolicy <- lag_rpolicy[is.na(data_all$week)==F,]

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
summary(data$pop50_2)

var <- lag_64
basis_64 <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(data$pop64_2, 2)),
                       arglag = list(fun = "ns", knots = nlag3/2))
head(basis_64)


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

##################interaction
########city_level TC* with/without national/provincial TC

data$nationaltc<-ifelse(data$Npolicy>0,1,0)
data$provincialtc<-ifelse(data$Ppolicy>0,1,0)

basis_cpolicy_withn<-basis_cpolicy*(data$nationaltc)    ###focus on with national tc
basis_cpolicy_withp<-basis_cpolicy*(data$provincialtc)    

########rainstorm* with/without city-level tc 

data$citytc<-ifelse(data$Cpolicy>0,1,0)

data$citytc2<-ifelse(data$Cpolicy>2,1,0)

basis_rpolicy_withc<-basis_rpolicy*(data$citytc)    
basis_rpolicy_withoutc<-basis_rpolicy*(1-data$citytc)    
basis_rpolicy_withhighc<-basis_rpolicy*(data$citytc2) 

######city-level tc warning in different tc intensity
data$only34<-ifelse(data$pop34_2>0&data$pop50_2==0&data$pop64_2==0,1,0)
data$above34<-ifelse(data$pop34_2>0&data$pop50_2>0,1,0)

basis_cpolicy_only34<-basis_cpolicy*(data$only34)    
basis_cpolicy_above34<-basis_cpolicy*(data$above34)


# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_wind) = paste0("basis_wind.", colnames(basis_wind))
colnames(basis_prec) = paste0("basis_prec.", colnames(basis_prec))

colnames(basis_34) = paste0("basis_34.", colnames(basis_34))
colnames(basis_50) = paste0("basis_50.", colnames(basis_50))
colnames(basis_64) = paste0("basis_64.", colnames(basis_64))

colnames(basis_npolicy) = paste0("basis_npolicy.", colnames(basis_npolicy))
colnames(basis_cpolicy) = paste0("basis_cpolicy.", colnames(basis_cpolicy))
colnames(basis_ppolicy) = paste0("basis_ppolicy.", colnames(basis_ppolicy))
colnames(basis_rpolicy) = paste0("basis_rpolicy.", colnames(basis_rpolicy))

colnames(basis_cpolicy_withn) = paste0("basis_cpolicy_withn.", colnames(basis_cpolicy_withn))

colnames(basis_cpolicy_withp) = paste0("basis_cpolicy_withp.", colnames(basis_cpolicy_withp))

colnames(basis_rpolicy_withc) = paste0("basis_rpolicy_withc.", colnames(basis_rpolicy_withc))
colnames(basis_rpolicy_withoutc) = paste0("basis_rpolicy_withoutc.", colnames(basis_rpolicy_withoutc))
colnames(basis_rpolicy_withhighc) = paste0("basis_rpolicy_withhighc.", colnames(basis_rpolicy_withhighc))

colnames(basis_cpolicy_only34) = paste0("basis_cpolicy_only34.", colnames(basis_cpolicy_only34))
colnames(basis_cpolicy_above34) = paste0("basis_cpolicy_above34.", colnames(basis_cpolicy_above34))


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

summary(data$prov_code)
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


#######run model

baseformula <- Y ~ 1 + f(T1, replicate = S1, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "/Volumes/LaCie/02 extreme weather&policy/10 data/map.graph",
    scale.model = TRUE, hyper = precision.prior2) +Vh

fr1.1 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy+basis_ppolicy+basis_npolicy+basis_rpolicy)

fr1.2 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy_withn+basis_ppolicy+basis_npolicy+basis_rpolicy)
fr1.3 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy_withp+basis_ppolicy+basis_npolicy+basis_rpolicy)

fr2.2 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy_withn+basis_ppolicy+basis_npolicy+basis_rpolicy_withc)
fr2.3 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy_withp+basis_ppolicy+basis_npolicy+basis_rpolicy_withoutc)
fr2.4 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy_withp+basis_ppolicy+basis_npolicy+basis_rpolicy_withhighc)

fr3.2 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy_only34+basis_ppolicy+basis_npolicy+basis_rpolicy)

formulas <- list(fr2.4)
formulas <- list(fr1.2,fr1.3,fr2.2,fr2.3,fr3.2,fr3.3)
# create model label string
lab <- c("model_r2.4")
lab <- c("model_r1.2","model_r1.3","model_r2.2","model_r2.3","model_r3.2","model_r3.3")

models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("23/3/intrarevision/", lab[i],".RData"))})

##############model interpretation
load("23/3/intrarevision/model_r3.1.RData")
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_cpolicy", model$names.fixed)

# extract predictions from the cpolicy DLNM centred on no city-level ts warning
predt <- crosspred(basis_cpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
## lag response for different warning scenarios
pdf("23/3/intrarevision/cpolicy_only34.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow

mn <- which(round(vars, 2) == 1)
mx <- which(round(vars, 2) == 2)
mx2 <- which(round(vars, 2) == 3)
mx3 <- which(round(vars, 2) == 4)

# define colours
col1 <- "#0000FF"
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- "#FFD700"
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- "#FFA500"
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

col4 <- "#FF0000"
tcol4 <- do.call(rgb, c(as.list(col2rgb(col4)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag2, 0.25)

# blue
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1,
     xlab = "Lag (day)", ylab = "Mobility Reduction under Exposure (*100%)", main = "",
     ylim = range(0.0, 0.20), frame.plot = T, axes = F)
axis(1, at = 0:nlag2, labels = 0:nlag2)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)
# yellow
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)
# orange
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)
abline(h = 1, lty = 3)
# red
lines(lagbylag, rr[mx3,], col = col4, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx3,],rev(rr.uci[mx3,]))
polygon(xx, yy, col = tcol4, border = tcol4)
abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("TC warning = ",vars[mn]),
                  paste0("TC warning = ", vars[mx]),
                  paste0("TC warning = ", vars[mx2]),
                  paste0("TC warning = ", vars[mx3])),
       col = c(col1, col2, col3, col4),
       lwd = 2, lty = 1, bty = "n",
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = 1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

###
load("23/3/intrarevision/model_r2.4.RData")
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with basis_TEMP crossbasis
indt <- grep("basis_rpolicy", model$names.fixed)

# extract predictions from the TEMP DLNM centred on overall mean Temperature 3
predt <- crosspred(basis_rpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
## lag response for different Temperature scenarios
pdf("23/3/intrarevision/rpolicy_withhighc.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow

mn <- which(round(vars, 2) == 1)
mx <- which(round(vars, 2) == 2)
mx2 <- which(round(vars, 2) == 3)
mx3 <- which(round(vars, 2) == 4)

# define colours
col1 <- "#0000FF"
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- "#FFD700"
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- "#FFA500"
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

col4 <- "#FF0000"
tcol4 <- do.call(rgb, c(as.list(col2rgb(col4)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag2, 0.25)

# blue
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1,
     xlab = "Lag (day)", ylab = "Mobility Reduction under Exposure (*100%)", main = "",
     ylim = range(0.0, 0.08), frame.plot = T, axes = F)
axis(1, at = 0:nlag2, labels = 0:nlag2)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)
# yellow
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)
# orange
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)
abline(h = 1, lty = 3)
# red
lines(lagbylag, rr[mx3,], col = col4, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx3,],rev(rr.uci[mx3,]))
polygon(xx, yy, col = tcol4, border = tcol4)
abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Rainstorm warning = ",vars[mn]),
                  paste0("Rainstorm warning = ", vars[mx]),
                  paste0("Rainstorm warning = ", vars[mx2]),
                  paste0("Rainstorm warning = ", vars[mx3])),
       col = c(col1, col2, col3, col4),
       lwd = 2, lty = 1, bty = "n",
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = 1.25, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

