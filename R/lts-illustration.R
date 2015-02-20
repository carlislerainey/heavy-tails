library(robustbase)

# parameters
n <- 50
n.sims <- 10000
df.t <- 3

# perform simulation
x <- rnorm(n)
a <- 0
b <- 1
b.hat.ls <- numeric(n.sims)
b.hat.lts <- numeric(n.sims)
pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
for (i in 1:n.sims) {
  e <- rt(n, df.t)
  y <- a + b*x + e
  m.lts <- ltsReg(y ~ x, alpha = 0.8)
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
h.ls <- hist(b.hat.ls, breaks = 50)
h.lts <- hist(b.hat.lts, breaks = 50)

eplot(xlim = mm(c(h.ls$breaks, h.lts$breaks)),
      ylim = mm(c(h.ls$density, h.lts$density)),
      xlab = "Coefficient Estimates",
      ylab = "Counts",
      main = "Least Squares")
plot(h.ls, freq = FALSE, add = TRUE, border = NA, col = "grey50")
lines(density(b.hat.ls), lwd = 3)
text(par("usr")[2], 2, paste("true = 1.000\nmean = ", format(round(mean(b.hat.ls), 3), nsmall = 3), 
                 "\nvariance = ", format(round(var(b.hat.ls), 3), nsmall = 3), sep = ""), 
     pos = 2, cex = 0.7)
aplot("Least Trimmed Squares")
plot(h.lts, freq = FALSE, add = TRUE, border = NA, col = "grey50")
lines(density(b.hat.lts), lwd = 3)
text(par("usr")[2], 2, paste("true = 1.000\nmean = ", format(round(mean(b.hat.lts), 3), nsmall = 3), 
                             "\nvariance = ", format(round(var(b.hat.lts), 3), nsmall = 3), sep = ""), 
     pos = 2, cex = 0.7)


