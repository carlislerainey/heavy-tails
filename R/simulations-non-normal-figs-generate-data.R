
# clear workspace
rm(list = ls())

#load packages
library(robustbase)
library(galts)
library(quantreg)
library(VGAM)
library(MASS)
library(foreach)
library(doParallel)

sim_results <- NULL

# simulation parameters
n.sims <- 10000
df <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30)
nv <- c(25, 100, 500, 2000)

# simulation data
x1_ <- rnorm(max(nv))
x2_ <- rnorm(max(nv))
x3_ <- rnorm(max(nv))

# set up clusters
cl <- makeCluster(4)
registerDoParallel(cl)

# create file to track progress
file.create("log2-office.txt")

start_time <- Sys.time()
cat("\n*****************************************", file = "log2-office.txt", append = TRUE)
cat(paste("\nStart time: ", start_time, "\n"), file = "log2-office.txt", append = TRUE)
cat("*****************************************\n", file = "log2-office.txt", append = TRUE)

# perform simulations
system.time(
sim_results <- foreach (n = nv, .combine = rbind, .packages = c("quantreg", "MASS", "galts")) %:%
	foreach (i = 1:length(df), .combine = rbind) %dopar% {
		for (j in 1:n.sims) {
			x1 <- x1_[1:n] 
			x2 <- x2_[1:n] 
			x3 <- x3_[1:n] 
		  y <- x1 + x2 + x3 + rt(n, df = df[i])
			# formula for regression models
			f <- y ~ x1 + x2 + x3
			m1 <- lm(f)
			df1 <- data.frame(method = "ls", n = n, df = df[i], est = coef(m1)[2])
			m2 <- rq(f)
			df2 <- data.frame(method = "lad", n = n, df = df[i], est = coef(m2)[2])
			m3 <- rlm(f, method = "M", psi = psi.bisquare, maxit = 1000, init = c(0, 1, 1, 1))
			df3 <- data.frame(method = "biweight", n = n, df = df[i], est = coef(m3)[2])
			m4 <- ga.lts(f, h = floor(.9*length(y)), lower = -2, upper = 2)
			df4 <- data.frame(method = "lts", n = n, df = df[i], est = coef(m4)[2])
			# combine results
			sim_results <- rbind(sim_results, df1, df2, df3, df4)
		}
	  # update log file
	  cat(paste("\nFinished with df = ", df[i], " and n = ", n, "... ", round(as.numeric(difftime(Sys.time(), start_time, units = "hours")), 2), " hours in.\n", sep = ""), file = "log2-office.txt", append = TRUE)
	  # return to foreach
	  sim_results
	}
)

cat("\n*****************************************\n", file = "log2-office.txt", append = TRUE)
cat(paste("Finish time: ", Sys.time(), "\n"), file = "log2-office.txt", append = TRUE)
cat("*****************************************\n", file = "log2-office.txt", append = TRUE)


# stop cluster
stopCluster(cl)

# save simulations to csv
rownames(sim_results) <- NULL
readr::write_csv(sim_results, "doc/figs/sim-results-data.csv")
