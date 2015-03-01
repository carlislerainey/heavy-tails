library(compactr)
library(MASS)

# example 1: two lines
x <- 1:10
y <- 0.5*x + (2 + 2*x*(x %in% c(5, 6, 8))) + rnorm(10, sd = 0.25)

# example 2: unusual and extremely unusual cases
y2 <- y
y2[8] <- 40

# plot examples
shaded.color <- "grey50"
pdf("doc/figs/best-fit-illustration.pdf", height = 3, width = 6)
par(mfrow = c(1, 2), mar = c(1/2, 1/2, 1/2, 1/2), oma = c(3, 3, 1, 1))
## Example #1
eplot(xlim = c(1, 10), ylim = c(0, 40),
     xlab = "Explanatory Variable",
     ylab = "Outcome Variable",
     main = "Example #1")
points(x, y, pch = 19)
m1 <- lm(y ~ x)
rm1 <- rlm(y ~ x, method = "M", psi = psi.bisquare, init = "lts")
abline(m1, lwd = 3)
abline(rm1, lwd = 3, lty = 2)
text(10, predict(m1)[10], "A", pos = 3, cex = 0.8)
text(8, predict(rm1)[8], "B", pos = 1, cex = 0.8)


## Example #2
aplot("Example #2")
points(x, y2, pch = 19)
m2 <- lm(y2 ~ x)
rm2 <- rlm(y2 ~ x, method = "M", psi = psi.bisquare, init = "lts")
abline(m2, lwd = 3)
abline(rm2, lwd = 3, lty = 2)
abline(m1, lty = 3, col = shaded.color)
arrows(x = 8, x1 = 8, y0 = 1.05*y[8], y1 = 0.97*y2[8],
       length = .05, code = 2, col = shaded.color)
points(8, y[8], col = shaded.color, pch = 19)
arrows(x = 9, x1 = 9, y0 = 1.05*predict(m1)[9], y1 = 0.97*predict(m2)[9],
       length = .05, code = 2, col = shaded.color)
text(10, predict(m2)[10], "A", pos = 3, cex = 0.8)
text(8, predict(rm2)[8], "B", pos = 1, cex = 0.8)
dev.off()
