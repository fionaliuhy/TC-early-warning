### R script to explore exposure-lag-response relationships

# load packages and pre-processed data
source("00_load_packages_data.R")

load("intra/model_2.8.RData")
##### city tc warning ----
# Coefficient and covariance
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_cpolicy", model$names.fixed)

# extract predictions from the cpolicy DLNM centred on no city-level ts warning
predt <- crosspred(basis_cpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
## lag response for different warning scenarios--fig. s3a
pdf("intra/result/cpolicy .pdf", width = 6, height = 6)

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
     ylim = range(0.0, 0.16), frame.plot = T, axes = F)
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

###get mobility reduction--fig.2
predt <- crosspred(basis_cpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 
rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$cpolicy<-rownames(x)

write.csv(x, file = "intra/result/er cpolicy.csv", quote = FALSE, row.names = FALSE)


#####provincial warning
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_ppolicy", model$names.fixed)

predt <- crosspred(basis_ppolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
## lag response for different warning scenarios--fig. s3b
pdf("intra/result/ppolicy .pdf", width = 6, height = 6)

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
     ylim = range(0.0, 0.16), frame.plot = T, axes = F)
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

# get exposures values--fig. 2
predt <- crosspred(basis_ppolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$ppolicy<-rownames(x)
write.csv(x, file = "intra/result/er ppolicy.csv", quote = FALSE, row.names = FALSE)

#####national warning
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with crossbasis
indt <- grep("basis_npolicy", model$names.fixed)

predt <- crosspred(basis_rpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
## lag response for different warning scenarios--fig. s3c
pdf("intra/result/npolicy .pdf", width = 6, height = 6)

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
     ylim = range(0.0, 0.16), frame.plot = T, axes = F)
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

# get exposures values  fig. 2
predt <- crosspred(basis_rpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$npolicy<-rownames(x)
write.csv(x, file = "intra/result/er npolicy.csv", quote = FALSE, row.names = FALSE)

#####red rainstorm warning
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with crossbasis
indt <- grep("basis_rpolicy", model$names.fixed)

predt <- crosspred(basis_rpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
## lag response for different warning scenarios--fig. s3d
pdf("intra/result/rpolicy .pdf", width = 6, height = 6)

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
     ylim = range(0.0, 0.16), frame.plot = T, axes = F)
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


# get exposures values--fig. 2
predt <- crosspred(basis_rpolicy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$rpolicy<-rownames(x)
write.csv(x, file = "intra/result/er rpolicy.csv", quote = FALSE, row.names = FALSE)


#####34 kt ts
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_34", model$names.fixed)

predt <- crosspred(basis_34, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 
# get exposures values
vars <- predt$predvar

##fig. s4a
pdf(paste0("intra/result/34 countour.pdf"), width = 6.5, height = 6)

y <- predt$predvar
x <- seq(0, nlag, 0.25)
z <- t(1-predt$matRRfit)

pal <-  rev(brewer.pal(11, "BrBG")) 
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 0)), col2(sum(levels > 0)))

filled.contour(x, y, z,
               xlab = "Lag", ylab = "Intervention", main = "",
               ylim = c(0, 1),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, seq(0,nlag,1)) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.2, text = "gdpl", las = 2, cex = 1.2, line = 2)
dev.off()
# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables--fig. 2
predt <- crosspred(basis_34, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 
rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$pop34<-rownames(x)
write.csv(x, file = "intra/result/er 34kt.csv", quote = FALSE, row.names = FALSE)

#####50 kt ts
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_50", model$names.fixed)

predt <- crosspred(basis_50, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 

# get exposures values
vars <- predt$predvar

##fig. s4b
pdf(paste0("intra/result/50 countour.pdf"), width = 6.5, height = 6)

y <- predt$predvar
x <- seq(0, nlag3, 0.25)
z <- t(1-predt$matRRfit)

pal <-  rev(brewer.pal(11, "BrBG")) 
levels <- pretty(z, 30)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 0)), col2(sum(levels > 0)))

filled.contour(x, y, z,
               xlab = "Lag", ylab = "Intervention", main = "",
               ylim = c(0, 1),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag3, seq(0,nlag3,1)) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.2, text = "gdpl", las = 2, cex = 1.2, line = 2)

dev.off()
#fig. 2
predt <- crosspred(basis_50, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag =1, cen = 0) 
rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$pop50<-rownames(x)
write.csv(x, file = "intra/result/er 50kt.csv", quote = FALSE, row.names = FALSE)


#####64 kt ts
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_64", model$names.fixed)

predt <- crosspred(basis_64, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 

# get exposures values
vars <- predt$predvar
##fig. s4c
pdf(paste0("intra/result/64 countour.pdf"), width = 6.5, height = 6)

y <- predt$predvar
x <- seq(0, nlag3, 0.25)
z <- t(1-predt$matRRfit)

pal <-  rev(brewer.pal(11, "BrBG")) 
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 0)), col2(sum(levels > 0)))

filled.contour(x, y, z,
               xlab = "Lag", ylab = "Intervention", main = "",
               ylim = c(0, 1),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag3, seq(0,nlag3,1)) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.2, text = lab[j], las = 2, cex = 1.2, line = 2)

dev.off()

# fig. 2
predt <- crosspred(basis_64, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$pop64<-rownames(x)
write.csv(x, file = "intra/result/er 64kt.csv", quote = FALSE, row.names = FALSE)

#####rainstorm
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

indt <- grep("basis_prec", model$names.fixed)

predt <- crosspred(basis_prec, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = 0) 

# get exposures values
vars <- predt$predvar

##fig. s4d
pdf(paste0("intra/result/prec countour.pdf"), width = 6.5, height = 6)

y <- predt$predvar
x <- seq(0, nlag3, 0.25)
z <- t(1-predt$matRRfit)

pal <-  rev(brewer.pal(11, "BrBG")) 
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 0)), col2(sum(levels > 0)))

filled.contour(x, y, z,
               xlab = "Lag", ylab = "Intervention", main = "",
               ylim = c(0, 1),
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag3, seq(0,nlag3,1)) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.2, text = lab[j], las = 2, cex = 1.2, line = 2)

dev.off()

# fig. 2
predt <- crosspred(basis_prec, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 1, cen = 0) 

rr <- 1-predt$matRRfit
rr.lci <- 1-predt$matRRhigh
rr.uci <- 1-predt$matRRlow
rr[rr <= 0] <- 0
rr.lci[rr.lci <= 0] <- 0
rr.uci[rr.uci <= 0] <- 0

x<-as.data.frame(rr)
x$prec<-rownames(x)
write.csv(x, file = "intra/result/er prec.csv", quote = FALSE, row.names = FALSE)





