rm(list = ls())
#load packages
library(quantreg)
library(VGAM)

# function to compute mse
mse <- function(x) {
  sum((x - 1)^2)
}

mc.sims <- function(n.sims = 10000, n = 100, rdist) {
  ## simulation data
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  f <- y ~ x1 + x2 + x3
  ## simulation
  pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
  temp.res <- matrix(NA, nrow = n.sims, ncol = 3)
  for (j in 1:n.sims) {
    y <- x1 + x2 + x3 + rdist(n)
    m1 <- lm(f)
    m2 <- rq(f)
    m3 <- rlm(f, method = "M", psi = psi.bisquare, maxit = 1000, init = "lts")
    temp.res[j, ] <- c(coef(m1)[2], 
                       coef(m2)[2], 
                       coef(m3)[2])
    setTxtProgressBar(pb, j)
  }
  ### resulsts
  c1 <- apply(temp.res, 2, mean)
  c2 <- apply(temp.res, 2, sd)
  c3 <- apply(temp.res, 2, mse)
  return(list(c1 = c1, c2 = c2, c3 = c3))
}

### table 1
mc.lap <- mc.sims(rdist = function(n) rlaplace(n))
mc.t2 <- mc.sims(rdist = function(n) rt(n, df = 2))
mc.t10 <- mc.sims(rdist = function(n) rt(n, df = 10))
mc.n <- mc.sims(rdist = function(n) rnorm(n))


ta1 <- cbind(mc.lap$c1, mc.t2$c1, mc.t10$c1, mc.n$c1)
ta2 <- cbind(mc.lap$c2, mc.t2$c2, mc.t10$c2, mc.n$c2)
ta3 <- cbind(mc.lap$c3, mc.t2$c3, mc.t10$c3, mc.n$c3)

ta <- cbind(ta1, ta2, ta3)
ta <- rbind(ta, ta[2, ]/ta[1, ])
ta <- rbind(ta, ta[3, ]/ta[1, ])

# ta <- format(round(ta, 2), nsmall = 2)
rownames(ta) <- c("Least Squares", "Least Absolute Deviation", "Tukey's Biweight", 
                 "LAD/LS", "BW/LS")
colnames(ta) <- rep(c("Lapl.", "$t_2$", "$t_{10}$", "Norm."), 3)

quantreg::latex.table(ta, dec = 3,
                      rowlabel = "",
                      file = "doc/tabs/mc-sims-100",
                      table.env = FALSE,
                      cgroup = c("Mean", 
                                 "Standard Deviation", "Mean Squared Error"),
                      rgroup = c("Absolute Performance",
                                 "Relative Performance"),
                      n.rgroup = c(3, 2),
                      label = "tab:mc-sims-100")

### table 2
mc.lap <- mc.sims(n = 1000, rdist = function(n) rlaplace(n))
mc.t2 <- mc.sims(n = 1000, rdist = function(n) rt(n, df = 2))
mc.t10 <- mc.sims(n = 1000, rdist = function(n) rt(n, df = 10))
mc.n <- mc.sims(n = 1000, rdist = function(n) rnorm(n))


ta1 <- cbind(mc.lap$c1, mc.t2$c1, mc.t10$c1, mc.n$c1)
ta2 <- cbind(mc.lap$c2, mc.t2$c2, mc.t10$c2, mc.n$c2)
ta3 <- cbind(mc.lap$c3, mc.t2$c3, mc.t10$c3, mc.n$c3)

ta <- cbind(ta1, ta2, ta3)
ta <- rbind(ta, ta[2, ]/ta[1, ])
ta <- rbind(ta, ta[3, ]/ta[1, ])

# ta <- format(round(ta, 2), nsmall = 2)
rownames(ta) <- c("Least Squares", "Least Absolute Deviation", "Tukey's Biweight", 
                  "LAD/LS", "BW/LS")
colnames(ta) <- rep(c("Lapl.", "$t_2$", "$t_{10}$", "Norm."), 3)

quantreg::latex.table(ta, dec = 3,
                      rowlabel = "",
                      file = "doc/tabs/mc-sims-1000",
                      table.env = FALSE,
                      cgroup = c("Mean", 
                                 "Standard Deviation", "Mean Squared Error"),
                      rgroup = c("Absolute Performance",
                                 "Relative Performance"),
                      n.rgroup = c(3, 2),
                      label = "tab:mc-sims-1000")
