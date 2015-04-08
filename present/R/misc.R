## set working directory
setwd("~/Dropbox/projects/heavy-tails")

# load packages
library(compactr)

### normal and t10 plots

# calculate quantities
x <- seq(-4, 4, length.out = 1000)
norm <- dnorm(x)
t10 <- dt(x, df = 10)

# plots
pdf("present/figs/norm.pdf", width = 4, height = 3)
par(mfrow = c(1, 1),
    mar = c(3, 4, 1, 1),
    oma = c(0, 0, 0, 0))
eplot(xlim = mm(x), ylim = mm(c(norm, t10, 0)),
      xlab = expression(epsilon[i]),
      ylab = "Density", ylabpos = 2.3)
lines(x, norm, lwd = 3)
dev.off()

pdf("present/figs/norm-t10.pdf", width = 4, height = 3)
par(mfrow = c(1, 1),
    mar = c(3, 4, 1, 1),
    oma = c(0, 0, 0, 0))
eplot(xlim = mm(x), ylim = mm(c(norm, t10, 0)),
      xlab = expression(epsilon[i]),
      ylab = "Density", ylabpos = 2.3)
lines(x, norm, lwd = 3)
lines(x, t10, lwd = 3, col = "red")
dev.off()

### biweight plots

x <- seq(-8, 8, length.out = 1000)
bw <- robustbase::Mpsi(x, psi = "bisquare", cc = 4.685, deriv = -1)
abs <- abs(x)
square <- x^2

pdf("present/figs/rho-square.pdf", width = 4, height = 3)
par(mfrow = c(1, 1),
    mar = c(3, 4, 2, 1),
    oma = c(0, 0, 0, 0))
eplot(xlim = mm(x), ylim = mm(c(0, square)),
      xlab = expression(epsilon[i]),
      ylab = expression(rho(epsilon[i])),
      main = "Square")
lines(x, square, lwd = 3)
dev.off()

pdf("present/figs/rho-abs.pdf", width = 4, height = 3)
par(mfrow = c(1, 1),
    mar = c(3, 4, 2, 1),
    oma = c(0, 0, 0, 0))
eplot(xlim = mm(x), ylim = mm(c(0, abs)),
      xlab = expression(epsilon[i]),
      ylab = expression(rho(epsilon[i])),
      main = "Absolute Value")
lines(x, abs, lwd = 3)
dev.off()

pdf("present/figs/rho-bw.pdf", width = 4, height = 3)
par(mfrow = c(1, 1),
    mar = c(3, 4, 2, 1),
    oma = c(0, 0, 0, 0))
eplot(xlim = mm(x), ylim = mm(c(0, bw)),
      xlab = expression(epsilon[i]),
      ylab = expression(rho(epsilon[i])),
      main = "Biweight")
lines(x, bw, lwd = 3)
dev.off()
