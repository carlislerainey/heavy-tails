## clear workspace
rm(list = ls())

## set seed 
set.seed(4897350)

## set working directory
setwd("~/Dropbox/projects/heavy-tails")

## load packages
library(arm)  # a variety of useful functions\
library(compactr)
library(sandwich)
library(car)
library(quantreg)
library(texreg)
#library(multiwayvcov)

## load and listwise delete data
cg <- read.csv("data/cg.tab", sep = "\t")

## delete cases following Clark and Golder's .do file
### no recognizable parties
cg <- cg[cg$countrynumber != 163, ]
cg <- cg[cg$countrynumber != 165, ]
cg <- cg[cg$countrynumber != 197, ]
cg <- cg[cg$countrynumber != 189, ]
cg <- cg[cg$countrynumber != 146, ]
cg <- cg[cg$countrynumber != 198, ]
cg <- cg[cg$countrynumber != 167, ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1958), ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1960), ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1962), ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1964), ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1966), ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1968), ]
cg <- cg[!(cg$countrynumber == 70 & cg$year == 1970), ]
cg <- cg[!(cg$countrynumber == 12 & cg$year == 1963), ]
### drop fused votes
cg <- cg[cg$countrynumber != 67, ]
cg <- cg[cg$countrynumber != 76, ]
cg <- cg[!(cg$countrynumber == 59 & cg$year == 1957), ]
cg <- cg[!(cg$countrynumber == 59 & cg$year == 1971), ]
cg <- cg[!(cg$countrynumber == 59 & cg$year == 1985), ]
cg <- cg[!(cg$countrynumber == 59 & cg$year == 1989), ]
cg <- cg[!(cg$countrynumber == 59 & cg$year == 1993), ]
cg <- cg[!(cg$countrynumber == 57 & cg$year == 1990), ]
cg <- cg[!(cg$countrynumber == 54 & cg$year == 1966), ]
cg <- cg[!(cg$countrynumber == 54 & cg$year == 1970), ]
cg <- cg[!(cg$countrynumber == 54 & cg$year == 1974), ]
cg <- cg[!(cg$countrynumber == 54 & cg$year == 1986), ]
### drop countries with large "others"
cg <- cg[!(cg$enep_others > 15 & cg$enep_others < 100), ]
### drop countries with majoritarian uppers
cg <- cg[cg$countrynumber != 132, ]
cg <- cg[cg$countrynumber != 29, ]
cg <- cg[!(cg$countrynumber == 87 & cg$year == 1988), ]
cg <- cg[!(cg$countrynumber == 87 & cg$year == 1992), ]
cg <- cg[!(cg$countrynumber == 87 & cg$year == 1996), ]
cg <- cg[!(cg$countrynumber == 116 & cg$year == 1987), ]
cg <- cg[!(cg$countrynumber == 116 & cg$year == 1996), ]
### create standardized versions of the variables
cg$st.eneg <- rescale(cg$eneg)
cg$st.logavemag <- rescale(log(cg$avemag))
cg$st.uppertier <- rescale(cg$uppertier)
cg$st.enpres <- rescale(cg$enpres)
cg$st.proximity1 <- rescale(cg$proximity1)
### create a country-year variable
cg$country.year <- paste(cg$country, " (", cg$year, ")", sep = "")

vars <- c("enep1", "eneg", "avemag", "uppertier", 
          "enpres", "proximity1", "st.eneg", "st.logavemag", 
          "st.uppertier", "st.enpres", "st.proximity1", "country", 
          "year", "country.year")

## subsets of data
cg.old <- na.omit(cg[cg$old == 1, vars])



## replicate Clark and Golder's main model
### col 4, table 2
f.cg <- enep1 ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1
m1 <- lm(f.cg, data = cg.old)

## plot residuals
pdf("doc/figs/cg-residuals-hist.pdf", height = 4, width = 6)
layout(c(1, 2, 3))
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
h1 <- hist(residuals(m1)/sd(residuals(m1)), breaks = 20, plot = FALSE)
eplot(xlim = mm(h1$breaks), ylim = mm(h1$counts),
      xlab = "Standardized Residuals",
      ylab = "Counts",
      ylabpos = 2.2)
plot(h1, add = TRUE,
     border = "black",
     col = "grey70")
text(4, 100, expression(paste("Shapiro-Wilk p-value: ", 2.8%*%10^-18)))
dev.off()

## qq-plots
### a function to plot the points 
plot.qq <- function(e) {
  qn <- qqnorm(e, plot.it = FALSE)
  points(qn)
  #abline(a = 0, b = 1)
  qqline(e)
}
### a function to label the residuals
label.it <- function(p = 1:3, e, pos = 2) {
  order <- rev(order(e))
  qn.x <- qqnorm(e, plot.it = FALSE)$x
  qn.y <- qqnorm(e, plot.it = FALSE)$y
  order <- rev(order(qn.y))
  labels <- cg.old[names(e), "country.year"]
  labels.o <- labels[order]
  qn.x.o <- qn.x[order]
  qn.y.o <- qn.y[order]
  text(qn.x.o[p], qn.y.o[p], labels.o[p], pos = pos, cex = .7)
}
### do the plotting
pdf("doc/figs/cg-qq-plot.pdf", height = 4, width = 6)
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
eplot(xlim = c(-4, 4), ylim = c(-4, 10),
      xlab = "Normal (Theoretical) Quantiles",
      ylab = "Data Quantiles")
plot.qq(residuals(m1))
#text(1.5, 7.8, "Brazil (1962)", cex = 0.8)
label.it(p = 1:3, residuals(m1), pos = c(2, 4, 2))
dev.off()

# ## residuals versus fitted
# x <- predict(m1)
# y <- residuals(m1)
# eplot(xlim = mm(x), ylim = mm(y),
#       xlab = "Fitted Values",
#       ylab = "Residuals")
# points(x, y)
# fit <- loess(y ~ x)
# new.x <- seq(0, 10, by = 0.1)
# new.data <- data.frame(x = new.x)
# plx <- predict(fit, newdata = nd, se = TRUE)
# lines(new.x, plx$fit)
# lines(new.x, plx$fit - qt(0.95,plx$df)*plx$se, lty=2)
# lines(new.x, plx$fit + qt(0.95,plx$df)*plx$se, lty=2)
# #lines(lowess(x, y), lwd = 3)
# abline(h = 0)

## a function to perform the Box-Cox transformation
bc <- function(y, lambda) {
  if (lambda != 0) {
    y.trans <- (y^lambda - 1)/lambda
  }
  if (lambda == 0) {
    y.trans <- log(y)
  }
  return(y.trans)
}

bc.inv <- function(y, lambda) {
  if (lambda != 0) {
    y.trans <- (lambda*y + 1)^(1/lambda)
  }
  if (lambda == 0) {
    y.trans <- exp(y)
  }
  return(y.trans)
}

boxcox(m1)
## models with log and Box-Cox transformation
##
f.log <- log(enep1) ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1
f.bc <- bc(enep1, -1/3) ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1
m2 <- lm(f.log, data = cg.old)
m3 <- lm(f.bc, data = cg.old)

## histogram new residuals from log and bc models
pdf("doc/figs/cg-trans-residuals-hist.pdf", height = 4, width = 11)
par(mfrow = c(1, 2), mar = c(1/2, 1/2, 1/2, 1/2), oma = c(3, 3, 1, 1))
h2 <- hist(residuals(m2)/sd(residuals(m2)), breaks = 20, plot = FALSE)
h3 <- hist(residuals(m3)/sd(residuals(m3)), breaks = 20, plot = FALSE)
eplot(xlim = mm(h2$breaks), ylim = mm(h2$counts),
      xlab = "Standardized Residuals",
      ylab = "Counts",
      ylabpos = 2.2, 
      main = "Log Transformation")
plot(h2, add = TRUE,
     border = "black",
     col = "grey70")
text(2.5, 120, expression(paste("Shapiro-Wilk p-value: ", 1.6%*%10^-6)), cex = 0.8)
aplot(expression(paste("Box-Cox Tranformation with ", lambda == -1/3, sep = "")))
plot(h3, add = TRUE,
     border = "black",
     col = "grey70")
text(2.5, 120, expression(paste("Shapiro-Wilk p-value: ", 0.002)), cex = 0.8)
dev.off()

### qqplots for the new residuals
pdf("doc/figs/cg-trans-qq-plot.pdf", height = 4, width = 11)
par(mfrow = c(1, 2), mar = c(1/2, 1/2, 1/2, 1/2), oma = c(3, 3, 1, 1))
eplot(xlim = c(-4, 4), ylim = c(-1, 1.2),
      xlab = "Normal (Theoretical) Quantiles",
      ylab = "Data Quantiles",
      main = "Log Transformation",
      ylabpos = 2.3)
plot.qq(residuals(m2))
#label.it(p = 1:3, residuals(m1), pos = c(2, 4, 2))
aplot(expression(paste("Box-Cox Tranformation with ", lambda == -1/3, sep = "")))
plot.qq(residuals(m3))
#aplot(expression(paste("Simulation from a ", t[7], sep = "")))
#plot.qq(rt(length(residuals(m3)), df = 7)/5)
dev.off()

fitdistr(residuals(m3), "t", 
         start = list(s = 0.1, df = 10),
         m = 0, lower = 0)


m4 <- rlm(f.bc, data = cg.old, method = "M", psi = psi.bisquare)
screenreg(list(m3, m4), digits = 3)

fitdistr(residuals(m4), "t", 
         start = list(s = 0.1, df = 10),
         m = 0, lower = 0)


plot(residuals(m3), residuals(m4))

## cluster bootstrap simulations
boot <- function(method = "ls", lambda = 1, n.bs = 1000) {
  f <- bc(enep1, lambda) ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1
  if (method == "ls") {
    m <- lm(f, d = cg.old)    
  }
  if (method == "bw") {
    m <- rlm(f, d = cg.old, method = "M", psi = psi.bisquare)    
  }
  clusters <- cg.old[, "country"]
  cluster.names <- unique(clusters)
  bs.coef <- NULL
  pb <- txtProgressBar(min = 0, max = n.bs, style = 3)
  for (i in 1:n.bs) {
    d.bs <- NULL
    for (j in 1:length(cluster.names)) {
      bs.cl <- sample(cluster.names, 1)
      d.bs <-  rbind(d.bs, cg.old[clusters == bs.cl, ])
    }
    #start <- lqs(f, data = d.bs)
    if (method == "ls") {
      m.bs <- lm(f, data = d.bs)
    }
    if (method == "bw") {
      m.bs <- rlm(f, data = d.bs, method = "M", psi = psi.bisquare, init = m, maxit = 1000)
    }
    #m <- lm(f, d.bs)
    bs.coef <- rbind(bs.coef, coef(m.bs))
    setTxtProgressBar(pb, i)
  }
  ci.lwr <- apply(bs.coef, 2, quantile, 0.05)
  ci.upr <- apply(bs.coef, 2, quantile, 0.95)
  avemag0 <- 1:150 # log(avemag) = 0 when avemag = 1
  eneg.lo <- quantile(cg.old$eneg, 0.10)
  eneg.hi <- quantile(cg.old$eneg, 0.90)
  uppertier0 <- 0
  proximity0 <- 0
  enpres0 <- 0
  X.lo <- cbind(1, eneg.lo, log(avemag0), uppertier0, enpres0, proximity0,
                eneg.lo*log(avemag0), eneg.lo*uppertier0, enpres0*proximity0)
  X.hi <- cbind(1, eneg.hi, log(avemag0), uppertier0, enpres0, proximity0,
                eneg.hi*log(avemag0), eneg.hi*uppertier0, enpres0*proximity0)
  
  med.y.hi.bs <- bc.inv(X.hi%*%t(bs.coef), lambda)
  med.y.lo.bs <- bc.inv(X.lo%*%t(bs.coef), lambda)
  fd.bs <- med.y.hi.bs - med.y.lo.bs
  sd.bs <- fd.bs[14,] - fd.bs[1, ]
  fd.ci <- apply(fd.bs, 1, quantile, probs = c(0.05, 0.95))
  sd.ci <- quantile(sd.bs, c(0.05, 0.95))
  med.y.hi <- bc.inv(X.hi%*%coef(m), lambda)
  med.y.lo <- bc.inv(X.lo%*%coef(m), lambda)
  fd <- med.y.hi - med.y.lo
  sd <- fd[14] - fd[1]
  ret <- list(m = m, coef = coef(m), bs.coef = bs.coef, ci.upr = ci.upr, ci.lwr = ci.lwr, med.y.hi.bs = med.y.hi.bs,
              med.y.lo.bs = med.y.lo.bs, med.y.hi = med.y.hi, med.y.lo = med.y.lo, fd = fd, fd.ci = fd.ci, sd = sd, sd.ci = sd.ci)
}

set.seed(1347013)
bs.lm.none <- boot()
bs.lm.bc <- boot(lambda = -1/3)
bs.bw.none <- boot(method = "bw")
bs.bw.bc <- boot(method = "bw", lambda = -1/3)

## Coefficient Tables
texreg(file = "doc/tabs/cg-coef-tab.tex", 
       scriptsize = TRUE,
       table = FALSE,
       # single.row = TRUE,
       list(bs.lm.none$m, bs.bw.none$m, bs.lm.bc$m, bs.bw.bc$m),
          override.ci.low = list(bs.lm.none$ci.lwr, bs.bw.none$ci.lwr, 
                                 bs.lm.bc$ci.lwr, bs.bw.bc$ci.lwr),
          override.ci.up = list(bs.lm.none$ci.upr, bs.bw.none$ci.upr, 
                                 bs.lm.bc$ci.upr, bs.bw.bc$ci.upr),
          include.rsquared = FALSE, include.adjrs = FALSE,
       custom.coef.names = c("Constant",
                             "ENEG",
                             "log(Magnitude)",
                             "Upper-Tier Seats",
                             "Presidential Candidates",
                             "Proximity",
                             "ENEG $\\times$ log(Magnitude)",
                             "ENEG $\\times$ Upper-Tier Seats",
                             "Presidential Candidates $\\times$ Proximity"),
       custom.model.names = c("\\specialcell{Least Squares\\\\w/ No Transformation}", 
                              "\\specialcell{Biweight\\\\w/ No Transformation}",
                              "\\specialcell{Least Squares\\\\w/ Box-Cox Transformation}",
                              "\\specialcell{Biweight\\\\w/Box-Cox Transformation}"))

pg.ci <- function(bs) {
  polygon(c(1:150, rev(1:150)), c(bs$fd.ci[1, ], rev(bs$fd.ci[2, ])), 
          col = "grey70", border = NA)
}

## FD plots
pdf("doc/figs/cg-fd-plots.pdf", height = 4, width = 6)
par(mfrow = c(2, 2), mar = c(1/2, 1/2, 1, 1/2), oma = c(3, 3, 1, 1))
eplot(xlim = c(1, 150), ylim = c(-2, 15),
      xlab = "District Magnitude",
      ylab = "Effect of ENEG",
      main = "Least Squares, No Transformation")
pg.ci(bs.lm.none)
lines(1:150, bs.lm.none$fd, lwd = 3)
abline(h = 0, lty = 2)

aplot("Least Squares, Box-Cox Transformation")
pg.ci(bs.lm.bc)
lines(1:150, bs.lm.bc$fd, lwd = 3)
abline(h = 0, lty = 2)

aplot("Biweight, No Transformation")
pg.ci(bs.bw.none)
lines(1:150, bs.bw.none$fd, lwd = 3)
abline(h = 0, lty = 2)

aplot("Biweight, Box-Cox Transformation")
pg.ci(bs.bw.bc)
lines(1:150, bs.bw.bc$fd, lwd = 3)
abline(h = 0, lty = 2)
dev.off()

format.est <- function(est, digits = 2) {
  paste("     ", format(round(est, digits), nsmall = digits), " ", sep = "")
}
format.ci <- function(ci, digits = 2) {
  ul <- format(round(ci, digits), nsmall = digits)
  ci <- paste("[", ul[1], "; ", ul[2], "]", sep = "")
}

qitab <- matrix(NA, nrow = 4, ncol = 6)
## M = 1
qitab[1, 1] <- format.est(bs.lm.none$fd[1])
qitab[1, 2] <- format.ci(bs.lm.none$fd.ci[, 1])
qitab[2, 1] <- format.est(bs.bw.none$fd[1])
qitab[2, 2] <- format.ci(bs.bw.none$fd.ci[, 1])
qitab[3, 1] <- format.est(bs.lm.bc$fd[1])
qitab[3, 2] <- format.ci(bs.lm.bc$fd.ci[, 1])
qitab[4, 1] <- format.est(bs.bw.bc$fd[1])
qitab[4, 2] <- format.ci(bs.bw.bc$fd.ci[, 1])
## M = 4
qitab[1, 3] <- format.est(bs.lm.none$fd[14])
qitab[1, 4] <- format.ci(bs.lm.none$fd.ci[, 14])
qitab[2, 3] <- format.est(bs.bw.none$fd[14])
qitab[2, 4] <- format.ci(bs.bw.none$fd.ci[, 14])
qitab[3, 3] <- format.est(bs.lm.bc$fd[14])
qitab[3, 4] <- format.ci(bs.lm.bc$fd.ci[, 14])
qitab[4, 3] <- format.est(bs.bw.bc$fd[14])
qitab[4, 4] <- format.ci(bs.bw.bc$fd.ci[, 14])
## SD
qitab[1, 5] <- format.est(bs.lm.none$sd)
qitab[1, 6] <- format.ci(bs.lm.none$sd.ci)
qitab[2, 5] <- format.est(bs.bw.none$sd)
qitab[2, 6] <- format.ci(bs.bw.none$sd.ci)
qitab[3, 5] <- format.est(bs.lm.bc$sd)
qitab[3, 6] <- format.ci(bs.lm.bc$sd.ci)
qitab[4, 5] <- format.est(bs.bw.bc$sd)
qitab[4, 6] <- format.ci(bs.bw.bc$sd.ci)

rownames(qitab) <- rep(c("Least Squares", "Biweight"), 2)
colnames(qitab) <- rep(c("Est.", "90\\% CI"), 3)

quantreg::latex.table(qitab,
                      rowlabel = "",
                      rowlabel.just = "l",
                      file = "doc/tabs/cg-qi",
                      table.env = FALSE,
                      cgroup = c("First-Difference When \\textit{M} = 1", 
                                 "First-Difference When \\textit{M} = 14",
                                 "Second-Difference"),
                      rgroup = c("No Transformation",
                                 "Box-Cox Transformation"),
                      n.rgroup = c(2, 2))


bc.inv <- function(y, lambda) {
  if (lambda != 0) {
    y.trans <- (lambda*y + 1)^(1/lambda)
  }
  if (lambda == 0) {
    y.trans <- exp(y)
  }
  return(y.trans)
}

r3 <- residuals(m3)
r4 <- residuals(m4)

pdf("doc/figs/cg-residuals-compare.pdf", height = 5, width = 6.5)
par(mfrow = c(1, 1), mar = c(1/2, 1/2, 1, 1/2), oma = c(4, 4, 1, 1))
eplot(xlim = mm(r3), ylim = mm(r4),
      xlab = "Residuals from Least Squares Estimates\nwith Box-Cox Transformation",
      ylab = "Residuals from Biweight Estimates\nwith Box-Cox Transformation",
      xlabpos = 2.3,
      ylabpos = 2.3)
points(r3, r4)
text(r3["785"], r4["785"], cg.old["785", "country.year"], pos = 4, cex = 0.8)
dev.off()

pdf("doc/figs/cg-weights.pdf", height = 7.5, width = 5)
par(mfrow = c(1, 1), mar = c(3, 10, 1, 1), oma = c(0, 0, 0, 0))
wt <- m4$w
names(wt) <- names(r4)
n.keep <- 35
low.wts <- sort(wt)[1:n.keep]
eplot(xlim = mm(low.wts), ylim = c(1, n.keep),
      xlab = "Weights",
      yat = 1:n.keep,
      yticklab = cg.old[names(low.wts), "country.year"])
for (i in 1:n.keep) {
  points(low.wts[i], i, pch = 19)
  lines(c(0, low.wts[i]), c(i, i))
}
dev.off()

## scatterplot
pdf("doc/figs/cg-scatter.pdf", height = 5, width = 8)
par(mfrow = c(1, 1), mar = c(1, 1, 1, 3), oma = c(3, 3, 2, 1))
eplot(xlim = mm(log(cg$avemag)),
      ylim = mm(cg$eneg),
      xat = log(c(1, 2, 5, 20, 50, 150)),
      xticklab = c(1, 2, 5, 20, 50, 150),
      xlab = "District Magnitude",
      ylab = "Effective Number of Ethnic Groups")
col <- rgb(.3, .3, .3, .3)
points(log(cg$avemag), cg$eneg, cex = sqrt(cg$enep1), 
       pch = 21, col = col, bg = col)

label.it <- function(name, pos = 4) {
  year <- cg$year[cg$country == name]
  enep <- round(cg$enep1[cg$country == name], 2)
  label <- paste(name, " (",year , ")", " - ", enep, sep = "")
  text(log(cg$avemag[cg$country == name]), 
       cg$eneg[cg$country == name], 
       labels = label, 
       pos = pos, cex = 0.8)
}
#text(log(cg$avemag), cg$eneg, paste(cg$country, cg$year))
label.it("Ghana")
label.it("Uganda")
label.it("Somalia")
label.it("Indonesia", pos = 3)
text(log(22.22), 8.304, "South Africa (1994, 1999) - 2.24, 2.16", cex = 0.8, pos = 4)
legend(x = 1.03*par("usr")[2], y = 10, #mean(c(par("usr")[4], par("usr")[3])), 
       pch = 21, pt.cex = sqrt(c(2, 5, 10)), col = col, 
       pt.bg = col, legend = c(2, 5, 10), bty = "n", title = "ENEP", 
       xjust = 0, yjust = 1,
       y.intersp = c(1, 1, 1.1), xpd = NA)
dev.off()
