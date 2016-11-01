
# clear workspace
rm(list = ls())

set.seed(8749310)

#load packages
library(MASS)
library(VGAM)
library(quantreg)
library(galts)

# function to compute mse
mse <- function(x) {
  mean((x - 1)^2)
}

mc.sims <- function(n.sims = 10000, n = 100, rdist) {
  ## simulation data
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  f <- y ~ x1 + x2 + x3
  ## simulation
  pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
  temp.res <- matrix(NA, nrow = n.sims, ncol = 4)
  for (j in 1:n.sims) {
    y <- x1 + x2 + x3 + rdist(n)
    m1 <- lm(f)
    m2 <- rlm(f, method = "M", psi = psi.bisquare, maxit = 1000, init = c(0, 1, 1, 1))
    m3 <- rq(f)
    m4 <- ga.lts(f, h = floor(.9*length(y)), lower = -2, upper = 2)
    temp.res[j, ] <- c(coef(m1)[2], 
                       coef(m2)[2], 
                       coef(m3)[2], 
    									 coef(m4)[2])
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
mc.logistic <- mc.sims(rdist = function(n) rlogis(n))
mc.unif <- mc.sims(rdist = function(n) runif(n, -1, 1))
mc.n <- mc.sims(rdist = function(n) rnorm(n))


ta3 <- cbind(mc.lap$c3, mc.t2$c3, mc.t10$c3, mc.logistic$c3, mc.unif$c3, mc.n$c3)


ta <- rbind(ta3, ta3[2, ]/ta3[1, ], ta3[3, ]/ta3[1, ], ta3[4, ]/ta3[1, ])

# ta <- format(round(ta, 2), nsmall = 2)
rownames(ta) <- c("Least Squares", "Tukey's Biweight", "Least Absolute Deviation", 
									"Least Trimmed Squares",
									"BW/LS", "LAD/LS", "LTS/LS")
colnames(ta) <- c("Laplace", "$t_2$", "$t_{10}$", "Logistic", "Uniform", "Normal")

quantreg::latex.table(ta, dec = 2,
                      rowlabel = "",
                      file = "doc/tabs/mc-sims-100",
                      table.env = FALSE,
                      rgroup = c("Absolute Performance",
                                 "Relative Performance"),
                      n.rgroup = c(4, 3),
                      label = "tab:mc-sims-100")
