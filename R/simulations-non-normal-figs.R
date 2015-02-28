
rm(list = ls())
#load packages
library(quantreg)
library(VGAM)
library(xtable)

# simulation parameters
pdf("doc/figs/mc-sims.pdf", height = 2, width = 8)
par(mfrow = c(1, 4), mar = c(1/2, 1/2, 1/2, 1/2), oma = c(3, 3, 1, 1))

nv <- c(25, 100, 500, 2000)
for (n in nv) {
  n.sims <- 10000
  df <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30)
  
  # simulation data
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  f <- y ~ x1 + x2 + x3
  
  # function to compute mse
  mse <- function(x) {
    sum((x - 1)^2)
  }
  
  # simulation
  res <- list()
  n.models <- 3
  model.names <- c("Least Squares",
                   "LAD",
                   "Biweight")
  res$mse <- res$mad <- res$mean <- matrix(NA, nrow = length(df), ncol = n.models)
  pb <- txtProgressBar(min = 0, max = length(df), style = 3)
  for (i in 1:length(df)) {
    temp.res <- matrix(NA, nrow = n.sims, ncol = n.models)
    for (j in 1:n.sims) {
      y <- x1 + x2 + x3 + rt(n, df = df[i])
      m1 <- lm(f)
      m2 <- rq(f)
      m3 <- rlm(f, method = "M", psi = psi.bisquare, maxit = 1000, init = "lts")
      temp.res[j, ] <- c(coef(m1)[2], 
                         coef(m2)[2], 
                         coef(m3)[2])
    }
    res$mse[i, ] <- apply(temp.res, 2, mean)
    res$mad[i, ] <- apply(temp.res, 2, sd)
    res$mean[i, ] <- apply(temp.res, 2, mse)
    setTxtProgressBar(pb, i)
  }
  
  # plot all
  eplot(xlim = mm(df), ylim = c(0.2, 1.6),
        xlab = expression(paste("df for ", italic(t), " Distributed Errors")),
        ylab = "Relative Std. Deviation",
        main = paste("N = ", n, sep = ""))
#   for (i in 1:n.models) {
#     lines(df, res$mad[, i], col = i, lwd = 2)
#   }
abline(h = 1)
lines(df, res$mad[, 3]/res$mad[, 1], col = 1, lwd = 3)
lines(df, res$mad[, 2]/res$mad[, 1], col = 2, lwd = 3, lty = 3)


}
legend(par("usr")[2], par("usr")[3], xjust = 1, yjust = 0,
       col = 1:2, lty = c(1, 3), lwd = 3,
       legend = c("BW/LS", "LAD/LS"),
       bty = "n")
dev.off()




