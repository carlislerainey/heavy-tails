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

