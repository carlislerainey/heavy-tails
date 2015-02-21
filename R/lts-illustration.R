
# clear workspace
rm(list = ls())

# load packages
library(robustbase)
library(galts)

# parameters
n <- 50
n.sims <- 1000
df.t <- 2.5

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
par(mfrow = c(1, 2), oma = c(3, 3, 1, 1), mar = c(1, 1, 1, 1))
h.ls <- hist(b.hat.ls)
h.lts <- hist(b.hat.lts)
breaks.at <- seq(from = min(c(h.ls$breaks, h.lts$breaks)),
                 to = max(c(h.ls$breaks, h.lts$breaks)),
                 length.out = 50)
h.ls <- hist(b.hat.ls, breaks = breaks.at)
h.lts <- hist(b.hat.lts, breaks = breaks.at)

eplot(xlim = mm(c(h.ls$breaks, h.lts$breaks)),
      ylim = mm(c(h.ls$density, h.lts$density)),
      xlab = "Coefficient Estimates",
      ylab = "Counts",
      main = "Least Squares")
plot(h.ls, freq = FALSE, add = TRUE, border = NA, col = "grey50")
lines(density(b.hat.ls), lwd = 3)
text(par("usr")[2], .8*par("usr")[4], paste("True = 1.00\nMean = ", format(round(mean(b.hat.ls), 2), nsmall = 2), 
                 "\nSD = ", format(round(sd(b.hat.ls), 2), nsmall = 2),
                 "\nMSE = ", format(round(sum((b.hat.ls - b)^2), 2), nsmall = 2), sep = ""), 
     pos = 2, cex = 0.7)
aplot("Least Trimmed Squares")
plot(h.lts, freq = FALSE, add = TRUE, border = NA, col = "grey50")
lines(density(b.hat.lts), lwd = 3)
text(par("usr")[2], .8*par("usr")[4], paste("True = 1.00\nMean = ", format(round(mean(b.hat.lts), 2), nsmall = 2), 
                             "\nSD = ", format(round(sd(b.hat.lts), 2), nsmall = 2),
                             "\nMSE = ", format(round(sum((b.hat.lts - b)^2), 2), nsmall = 2), sep = ""), 
     pos = 2, cex = 0.7)


