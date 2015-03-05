
# set working directory
setwd("~/Dropbox/projects/heavy-tails")

# clear workspace
rm(list = ls())

# load packages
library(robustbase)
library(galts)

# parameters
n <- 50
n.sims <- 10000
df.t <- 3

# perform simulation
set.seed(89132)
x <- rnorm(n)
a <- 0
b <- 1
b.hat.ls <- numeric(n.sims)
b.hat.lts <- numeric(n.sims)
pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
for (i in 1:n.sims) {
  e <- rt(n, df.t)
  y <- a + b*x + e
  m.lts <- ga.lts(y ~ x, h = 45, lower = -10, upper = 10)
  b.hat.lts[i] <- coef(m.lts)[2]
  m.ls <- lm(y ~ x)
  b.hat.ls[i] <- coef(m.ls)[2]
  setTxtProgressBar(pb, i)
}

# descriptives
mean(b.hat.ls); mean(b.hat.lts)
sd(b.hat.ls); sd(b.hat.lts)



# plot
pdf("doc/figs/lts-illustration.pdf", height = 3.5, width = 8)
par(mfrow = c(1, 2), oma = c(2, 2, 1/2, 1/2), mar = c(1, 1, 1, 1))
h.ls <- hist(b.hat.ls, plot = FALSE)
h.lts <- hist(b.hat.lts, plot = FALSE)
breaks.at <- seq(from = min(c(h.ls$breaks, h.lts$breaks)),
                 to = max(c(h.ls$breaks, h.lts$breaks)),
                 length.out = 60)
h.ls <- hist(b.hat.ls, breaks = breaks.at, plot = FALSE)
h.lts <- hist(b.hat.lts, breaks = breaks.at, plot = FALSE)

eplot(xlim = mm(c(h.ls$breaks, h.lts$breaks)),
      ylim = 1.05*mm(c(h.ls$density, h.lts$density)),
      ylabpos = 2.0,
      xlab = "Coefficient Estimates",
      ylab = "Counts",
      main = "Least Squares")
plot(h.ls, freq = FALSE, add = TRUE, border = "black", col = "grey70")
#lines(density(b.hat.ls), lwd = 2)
text(par("usr")[2], .8*par("usr")[4], paste("True = 1.00\nMean = ", format(round(mean(b.hat.ls), 2), nsmall = 2), 
                 "\nSD = ", format(round(sd(b.hat.ls), 2), nsmall = 2),
                 "\nMSE = ", format(round(sum((b.hat.ls - b)^2), 2), nsmall = 2), sep = ""), 
     pos = 2, cex = 0.7)
aplot("Least Trimmed Squares")
plot(h.lts, freq = FALSE, add = TRUE, border = "black", col = "grey70")
#lines(density(b.hat.lts), lwd = 2)
text(par("usr")[2], .8*par("usr")[4], paste("True = 1.00\nMean = ", format(round(mean(b.hat.lts), 2), nsmall = 2), 
                             "\nSD = ", format(round(sd(b.hat.lts), 2), nsmall = 2),
                             "\nMSE = ", format(round(sum((b.hat.lts - b)^2), 2), nsmall = 2), sep = ""), 
     pos = 2, cex = 0.7)
dev.off()

