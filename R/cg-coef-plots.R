## clear workspace
rm(list = ls())

## set seed 
set.seed(4897350)

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

vars <- c("enep1", "eneg", "avemag", "uppertier", 
          "enpres", "proximity1", "st.eneg", "st.logavemag", 
          "st.uppertier", "st.enpres", "st.proximity1", "country", 
          "year", "country.year")

## subsets of data
cg.90s <- na.omit(cg[cg$nineties == 1, vars])
cg.90s.old <- na.omit(cg[cg$nineties == 1 & cg$old == 1, vars])
cg.whole <- na.omit(cg[, vars])
cg.old <- na.omit(cg[cg$old == 1, vars])

## replicate Clark and Golder's models
f <- enep1 ~ st.eneg*st.logavemag + st.eneg*st.uppertier + st.enpres*st.proximity1
# ### col 3, table 2
ls.90s <- lm(f, data = cg.90s)
m.90s <- rlm(f, data = cg.90s, method = "M", psi = psi.bisquare)
# ### col 4, table 2
ls.90s.old <- lm(f, data = cg.90s.old)
m.90s.old <- rlm(f, data = cg.90s.old, method = "M", psi = psi.bisquare)
### col 3, table 2
ls.whole <- lm(f, data = cg.whole)
m.whole <- rlm(f, data = cg.whole, method = "M", psi = psi.bisquare)
### col 4, table 2
ls.old <- lm(f, data = cg.old)
m.old <- rlm(f, data = cg.old, method = "M", psi = psi.bisquare)


## plot coefficients
### a function to plot the points and lines
plot.coefs <- function(m, d = 0, col = "black") {
  abline(v = 0, col = "grey70", lty = 3)
  d <- d*1.5
  for (i in 1:n.coef) {
    est <- coef(m)[i]
    se <- sqrt(diag(vcov(m)))[i]
    lines(c(est + se, est - se), c(i + d, i + d), lwd = 1, col = col)
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
plot.coefs(m.90s, d = 0.1, col = "red")
text(4, 0.82, "least squares", pos = 2, cex = 0.7)
text(3.5, 1.17, "biweight", pos = 2, col = "red", cex = 0.7)
abline(h = c(6.5, 7.5), col = "grey80")
aplot("1990s\nEstablished Democracies")
plot.coefs(ls.90s.old, d = -0.1)
plot.coefs(m.90s.old, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
aplot("1946-2000\nWhole Sample")
plot.coefs(ls.whole, d = -0.1)
plot.coefs(m.whole, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
aplot("1946-2000\nEstablished Democracies")
plot.coefs(ls.old, d = -0.1)
plot.coefs(m.old, d = 0.1, col = "red")
abline(h = c(6.5, 7.5), col = "grey80")
dev.off()
