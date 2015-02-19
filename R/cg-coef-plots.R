## clear workspace
rm(list = ls())

## set working directory
setwd("~/Dropbox/projects/heavy-tails")

## load packages
library(arm)  # a variety of useful functions\
library(compactr)
library(sandwich)
library(car)

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

## replicate Clark and Golder's models
f <- enep1 ~ st.eneg*st.logavemag + st.eneg*st.uppertier + st.enpres*st.proximity1
### col 3, table 2
ls.90s <- lm(f, data = cg, subset = nineties == 1)
mm.90s <- rlm(f, data = cg, subset = nineties == 1, method = "MM", maxit = 200)
### col 4, table 2
ls.90s.old <- lm(f, data = cg, subset = nineties == 1 & old == 1)
mm.90s.old <- rlm(f, data = cg, subset = nineties == 1 & old == 1, method = "MM", maxit = 200)
### col 3, table 2
ls.whole <- lm(f, data = cg)
mm.whole <- rlm(f, data = cg, method = "MM", maxit = 200)
### col 4, table 2
ls.old <- lm(f, data = cg, subset = old == 1)
mm.old <- rlm(f, data = cg, subset = old == 1, method = "MM", maxit = 200)

## plot coefficients
### a function to plot the points and lines
plot.coefs <- function(m, d = 0, col = "black") {
  abline(v = 0, col = "grey70", lty = 3)
  d <- d*1.5
  for (i in 1:n.coef) {
    est <- coef(m)[i]
    se <- sqrt(diag(vcov(m)))[i]
    lines(c(est + 1.64*se, est - 1.64*se), c(i + d, i + d), lwd = 1, col = col)
    points(est, i + d, pch = 19, cex = 0.8, col = col)
  }
}
### do the plotting
n.coef <- length(coef(ls.90s))
pdf("doc/figs/cg-coef-plots.pdf", height = 3, width = 8)
par(mfrow = c(1, 4), oma = c(3, 12, 2, 1), mar = c(1, 1, 1, 1))
eplot(xlim = c(-5, 7), ylim = c(n.coef + 0.5, 0.5),
      yat = 1:n.coef,
      yticklab = c("Constant",
                   "ENEG",
                   "ln(Magnitude)",
                   "Upper-tier Seats",
                   "Presidential Candidates",
                   "Proximity",
                   "ENEG x ln(Magnitude)",
                   "ENEG x Upper-tier Seats",
                   "Presidential Candidates x Proximity"),
      xlab = "Coefficient",
      main = "1990s\nWhole Sample")
plot.coefs(ls.90s, d = -0.1)
plot.coefs(mm.90s, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
aplot("1990s\nEstablished Democracies")
plot.coefs(ls.90s.old, d = -0.1)
plot.coefs(mm.90s.old, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
aplot("1946-2000\nWhole Sample")
plot.coefs(ls.whole, d = -0.1)
plot.coefs(mm.whole, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
aplot("1946-2000\nEstablished Democracies")
plot.coefs(ls.old, d = -0.1)
plot.coefs(mm.old, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
dev.off()

## qq-plots
### a function to plot the points 
plot.qq <- function(e) {
  qn <- qqnorm(e, plot.it = FALSE)
  points(qn)
  #abline(a = 0, b = 1)
  qqline(e)
}
### do the plotting
pdf("doc/figs/cg-qq-plots.pdf", height = 2.5, width = 10)
par(mfrow = c(1, 5), mar = c(1, 1, 1, 1), oma = c(3, 3, 2, 1))
eplot(xlim = c(-4, 4), ylim = c(-4, 10),
      xlab = "Normal (Theoretical) Quantiles",
      ylab = "Data Quantiles",
      main = "Assumed Distribution")
n <- length(residuals(ls.90s))
sd <- sd(residuals(ls.90s))
e <- rnorm(n, 0, sd)
plot.qq(e)
aplot("1990s\nWhole Sample")
plot.qq(residuals(ls.90s))
text(.7, 7.9, "Benin (1995)", cex = 0.8)
aplot("1990s\nEstablished Democracies")
plot.qq(residuals(ls.90s.old))
text(.6, 3.7, "Brazil (1994)", cex = 0.8)
aplot("1946-2000\nWhole Sample")
plot.qq(residuals(ls.whole))
lines(c(3.1, 2), c(9.0, 9.5))
text(2.2, 9.5, "Benin (1995)", pos = 2, cex = 0.8)
lines(c(2.5, 2), c(8.3, 8.3))
text(2.2, 8.3, "Poland (1991)", pos = 2, cex = 0.8)
lines(c(2.4, 2.0), c(7.55, 7.1))
text(2.2, 7.1, "Chile (1953)", pos = 2, cex = 0.8)
aplot("1946-2000\nEstablished Democracies")
plot.qq(residuals(ls.old))
text(1.5, 7.8, "Brazil (1962)", cex = 0.8)
dev.off()

## marginal effect plots
### function to plot marginal effects and confidence intervals
plot.me <- function(m, red = 0) {
  ## store the coefficient estimates
  beta.hat <- coef(m)
  Sigma <- vcov(m)

  log.avemag0 <- 0:5 # log(avemag) = 0 when avemag = 1
  uppertier0 <- 0
  dy.deneg <- beta.hat["eneg"] + 
    beta.hat["eneg:log(avemag)"]*log.avemag0 + 
    beta.hat["eneg:uppertier"]*uppertier0
  se.dy.deneg <- sqrt(Sigma["eneg", "eneg"] + 
                        log.avemag0^2*Sigma["eneg:log(avemag)", "eneg:log(avemag)"] +
                        uppertier0^2*Sigma["eneg:uppertier", "eneg:uppertier"] + 
                        2*log.avemag0*Sigma["eneg", "eneg:log(avemag)"] + 
                        2*uppertier0*Sigma["eneg", "eneg:uppertier"] + 
                        2*log.avemag0*uppertier0*Sigma["eneg:log(avemag)", "eneg:uppertier"])
  ## calculate the lower and bounds of the 90% confidence interval
  lwr90 <- dy.deneg - 1.64*se.dy.deneg
  upr90 <- dy.deneg + 1.64*se.dy.deneg
  abline(h = 0)  # add horizontal line at zero
  polygon(c(log.avemag0, rev(log.avemag0)), c(lwr90, rev(upr90)), col = rgb(red, 0, 0, .2), border = NA)
  #lines(log.avemag0, lwr90, lty = 3)
  #lines(log.avemag0, upr90, lty = 3)
  lines(log.avemag0, dy.deneg, lwd = 3, col = rgb(red, 0, 0, 1))
}
### replicate Clark and Golder's models with unstandardized variables
f <- enep1 ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1
#### col 3, table 2
ls.90s <- lm(f, data = cg, subset = nineties == 1)
mm.90s <- rlm(f, data = cg, subset = nineties == 1, method = "MM", maxit = 200)
#### col 4, table 2
ls.90s.old <- lm(f, data = cg, subset = nineties == 1 & old == 1)
mm.90s.old <- rlm(f, data = cg, subset = nineties == 1 & old == 1, method = "MM", maxit = 200)
#### col 3, table 2
ls.whole <- lm(f, data = cg)
mm.whole <- rlm(f, data = cg, method = "MM", maxit = 200)
#### col 4, table 2
ls.old <- lm(f, data = cg, subset = old == 1)
mm.old <- rlm(f, data = cg, subset = old == 1, method = "MM", maxit = 200)
## plot the marginal effects and confidence intervals
par(mfrow = c(2, 4), mar = c(1, 1, 1, 1), oma = c(3, 3, 2, 1))
eplot(xlim = c(0, 5), ylim = c(-2, 5),
      xlab = "log(Average District Magnitude)",
      ylab = "Marginal Effect of ENEG on ENEP",
      ylabpos = 2.5,
      main = "1990s\nWhole Sample")
plot.me(ls.90s)
aplot("1990s\nEstablished Democracies")
plot.me(ls.90s.old)
aplot("1946-2000\nWhole Sample")
plot.me(ls.whole)
aplot("1946-2000\nEstablished Democracies")
plot.me(ls.old)
aplot()
plot.me(mm.90s, red = 1)
aplot()
plot.me(mm.90s.old, red = 1)
aplot()
plot.me(mm.whole, red = 1)
aplot()
plot.me(mm.old, red = 1)

par(mfrow = c(1,2))
eplot(xlim = c(0, 5), ylim = c(-1, 2),
      xlab = "log(Average District Magnitude)",
      ylab = "Marginal Effect of ENEG on ENEP",
      ylabpos = 2.5,
      main = "Least-Squares Estimator")
plot.me(ls.old)
aplot("MM Estimator")
plot.me(mm.old, red = 1)

## scatterplot
par(mfrow = c(1, 1))
eplot(xlim = mm(log(cg$avemag)),
      ylim = mm(cg$eneg),
      xlab = "Log of the Average District Magnitude",
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
       pos = pos, cex = 0.9)
}
#text(log(cg$avemag), cg$eneg, paste(cg$country, cg$year))
label.it("Ghana")
label.it("Uganda")
label.it("Somalia")
label.it("Indonesia", pos = 3)
text(log(22.22), 8.304, "South Africa (1994, 1999) - 2.24, 2.16", cex = 0.9, pos = 4)


legend(x = par("usr")[2], y = par("usr")[4], 
       pch = 21, pt.cex = sqrt(c(1, 2, 5, 10)), col = col, 
       pt.bg = col, legend = c(2, 5, 10), bty = "b", title = "ENEP", xjust = 1)

## dfbetas
dfbeta <- dfbetas(ls.old)[, "eneg:log(avemag)"]
order <- order(dfbeta)
est <- coef(ls.old)["eneg:log(avemag)"]; est
numeric.cut <- 2/sqrt(length(residuals(ls.old)))
dfd <- data.frame(country = cg[names(dfbeta), "country.year"], dfbeta = round(dfbeta, 2))
dfd <- dfd[order(dfd$dfbeta), ]
dfd2 <- dfd[abs(dfd$dfbeta) > numeric.cut, ]
dfd2$sign <- sign(dfd2$dfbeta)

par(mfrow = c(1, 1), mar = c(3, 1, 1, 1), oma = c(0, 0, 0, 0))
eplot(xlim = mm(dfd2$dfbeta), ylim = c(1, length(dfd2$country)),
      anny = FALSE,
      xlab = "dfbeta")
upr <- .999*par("usr")[4]
lwr <- .999*par("usr")[3]
polygon(x = c(-est, est, est, -est), 
        y = c(lwr, lwr, upr, upr),
        col = "grey95", border = NA)
polygon(x = c(-numeric.cut, numeric.cut, numeric.cut, -numeric.cut), 
        y = c(lwr, lwr, upr, upr),
        col = "grey85", border = NA)
points(dfd2$dfbeta, 1:length(dfd2$country) + dfd2$sign, pch = 19, cex = 0.5)
text(0, 1:length(dfd2$country) + dfd2$sign, dfd2$country, cex = 0.5, pos = 3 - dfd2$sign)
for (i in 1:length(dfd2$country)) {
  lines(c(0, dfd2$dfbeta[i]), c(i + dfd2$sign[i], i + dfd2$sign[i]))
}
text(0, sum(dfd2$sign == -1) + c(0, 0.5, 1), ".")

par(mfrow = c(1, 4), mar = c(1, 1, 1, 1), oma = c(3, 3, 2, 1))
eplot(xlim = c(0, 5), ylim = c(-2, 5),
      xlab = "log(Average District Magnitude)",
      ylab = "Marginal Effect of ENEG on ENEP",
      ylabpos = 2.5,
      main = "1990s\nWhole Sample")
m <- lm(f, data = cg)
plot.me(m)
aplot("Drop Uganda (1980)")
m <- lm(f, data = cg, subset = !(country == "Uganda" & year == 1980))
plot.me(m)
aplot("Drop Uganda (1980),\nIsrael (1999), and Brazil (1962)")
m <- lm(f, data = cg, subset = !((country == "Uganda" & year == 1980) | 
                                 (country == "Israel" & year == 1999) | 
                                 (country == "Brazil" & year == 1962)))
plot.me(m)
aplot("Drop All Influential Cases")
m <- lm(f, data = cg, subset = !(rownames(cg) %in% rownames(dfd2)))
plot.me(m)



