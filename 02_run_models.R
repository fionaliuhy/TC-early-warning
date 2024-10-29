# R script to run INLA models of increasing complexity
# WARNING: the script may take over a day to run

source("00_load_packages_data.R")

###first: choose the best base model
baseformula <- Y ~ 1 + f(T1, replicate = S1, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "/Volumes/ssd/02 extreme weather&policy/10 data/map.graph",
    scale.model = TRUE, hyper = precision.prior2) +Vh

baseformula <- Y ~ 1 + f(T1, replicate = S1, model = "rw1", cyclic = TRUE, constr = TRUE,
                         scale.model = TRUE,  hyper = precision.prior2) +
  f(S1, model = "bym2", replicate = T2, graph = "/Volumes/ssd/02 extreme weather&policy/10 data/map.graph",
    scale.model = TRUE, hyper = precision.prior2) +Vh+Vpop


formulas <- list(baseformula)

lab <- c("base","base1")

models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                   save(model, file = paste0("intra/", lab[i],".RData"))})

# create table to store DIC and select best model
table0 <- data.table(Model  =  c( "base","base1"),
                     DIC = NA,
                     logscore = NA) 

for(i in 1:length(formulas))
{
  load(paste0("intra/",lab[i],".RData"))
  table0$DIC[i] <- round(model$dic$dic, 2)
  table0$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table0

# define position of best fitting model
best.fit <- which.min(table0$DIC)

# Write results of model selection
fwrite(table0, file = "intra/best_model_selection0.csv", quote = FALSE,
       row.names = FALSE)

###second: define formulas by updating the baseline formula with different warnings and hazardous weather

f1.1 <- update.formula(baseformula, ~. + basis_wind)
f1.2 <- update.formula(baseformula, ~. + basis_prec)
f1.3 <- update.formula(baseformula, ~. + basis_34)
f1.4 <- update.formula(baseformula, ~. + basis_50)
f1.5 <- update.formula(baseformula, ~. + basis_64)
##test whether dlnms improve model fit
f1.6 <- update.formula(baseformula, ~. + basis_npolicy)
f1.60 <- update.formula(baseformula, ~. + np)
f1.7 <- update.formula(baseformula, ~. + basis_cpolicy)
f1.70 <- update.formula(baseformula, ~. + cp)
f1.8 <- update.formula(baseformula, ~. + basis_ppolicy)
f1.80 <- update.formula(baseformula, ~. + pp)
f1.9 <- update.formula(baseformula, ~. + basis_rpolicy)
f1.90 <- update.formula(baseformula, ~. + rp)
f1.1x <- update.formula(baseformula, ~. + basis_Stringencyc)

f1.11 <- update.formula(baseformula, ~. + basis_h34)
f1.12 <- update.formula(baseformula, ~. + basis_h50)
f1.13 <- update.formula(baseformula, ~. + basis_h64)
f1.14 <- update.formula(baseformula, ~. + basis_h80)
f1.15 <- update.formula(baseformula, ~. + basis_h100)


# create a list of formulas
formulas <- list(f1.1,f1.2,f1.3,f1.4,f1.5,f1.6,f1.7,f1.8,f1.9)

# create model label string

lab <- c("model_1.1","model_1.2","model_1.3","model_1.4","model_1.5","model_1.6","model_1.7","model_1.8","model_1.9")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
              function(i) {
                model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                save(model, file = paste0("intra/", lab[i],".RData"))})

# create table to store DIC and select best model
table1 <- data.table(Model  =  c( "wind","prec","34","50","64","npolicy_dlnm","npolicy","cpolicy_dlnm","cpolicy","ppolicy_dlnm","ppolicy","rpolicy_dlnm","rpolicy"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
  {
  load(paste0("intra/",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 2)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table1

# define position of best fitting model
best.fit <- which.min(table1$DIC)

# Write results of model selection
fwrite(table1, file = "intra/best_model_selection1.csv", quote = FALSE,
       row.names = FALSE)



###third: define formulas by updating the baseline formula with different combinations of hazardous weather combinations

f2.1 <- update.formula(baseformula, ~. + basis_wind + basis_prec )
f2.2 <- update.formula(baseformula, ~. + basis_wind + basis_prec +basis_34)
f2.3 <- update.formula(baseformula, ~. + basis_wind + basis_prec +basis_34+basis_50)
f2.4 <- update.formula(baseformula, ~. + basis_wind + basis_prec +basis_34+basis_50+basis_64)

###fourth: define formulas by updating the Hazardous weather model with different combinations of warnings
#most fit is the comprehensive model

f2.5<- update.formula(baseformula, ~. + basis_wind + basis_prec+basis_34+basis_50+basis_64+basis_cpolicy)
f2.6<- update.formula(baseformula, ~. + basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy+basis_ppolicy)
f2.7<- update.formula(baseformula, ~. + basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy+basis_ppolicy+basis_npolicy)
f2.8 <- update.formula(baseformula, ~. +basis_wind + basis_prec +basis_34+basis_50+basis_64+basis_cpolicy+basis_ppolicy+basis_npolicy+basis_rpolicy)

# create a list of formulas

formulas <- list(f2.1,f2.2,f2.3,f2.4,f2.5,f2.6,f2.7,f2.8)

# create model label string

lab <- c("model_2.1","model_2.2","model_2.3","model_2.4","model_2.5","model_2.6","model_2.7","model_2.8")

# # create a function to run a model for each formula in the list and save the model output to file
# # WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
              function(i) {
                model <- mymodel(formula = formulas[[i]], data = df, family = "Gaussian", config = FALSE)
                save(model, file = paste0("intra/", lab[i],".RData"))})

# create table to store DIC and select best model
table2 <- data.table(Model  =  c("wind+prec","wind+prec+34","wind+prec+34+50","wind+prec+34+50+64",
                                 "cpolicy", "cpolicy+ppolicy","cpolicy+ppolicy+npolicy",
                                 "cpolicy+ppolicy+npolicy+rpolicy"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
  {
  load(paste0("intra/",lab[i],".RData"))
  table2$DIC[i] <- round(model$dic$dic, 2)
  table2$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table2

# define position of best fitting model
best.fit <- which.min(table2$DIC)

# Write results of model selection
fwrite(table2, file = "intra/best_model_selection2.0.csv", quote = FALSE,
       row.names = FALSE)