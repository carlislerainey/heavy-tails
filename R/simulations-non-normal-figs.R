
# clear workspace
rm(list = ls())

#load packages
library(compactr)
library(dplyr)
library(magrittr)
library(tidyr)

sim_results <- readr::read_csv("doc/figs/sim-results-data.csv")

# function to compute mse
mse <- function(x) {
	mean((x - 1)^2)
}

tsr <- sim_results %>%
	group_by(n, df, method) %>%
	summarize(mse = sum((est - 1)^2)) %>%
	spread(method, mse) %>%
	transmute(lad = lad/ls, 
				 biweight = biweight/ls,
				 lts = lts/ls) 

tsr_tall <- tsr %>%
	gather(method, rel_mse, lad:lts)



pdf("doc/figs/mc-sims.pdf", height = 2, width = 8)
par(mfrow = c(1, 4), mar = c(1/2, 1/2, 1/2, 1/2), oma = c(3, 3, 1, 1))
n_vals <- sort(unique(sim_results$n))
for (n0 in n_vals) {
	tsr0 <- dplyr::filter(tsr, n == n0)
	# plot all
	eplot(xlim = mm(tsr_tall$df), ylim = mm(tsr_tall$rel_mse),
				xlab = expression(paste("df for ", italic(t), " Distributed Errors")),
				ylab = "Relative MSE",
				main = paste("N = ", n0, sep = ""))
	abline(h = 1)
	lines(tsr0$df, tsr0$biweight, col = 1, lwd = 1.5)
	lines(tsr0$df, tsr0$lad, col = 2, lwd = 1.5, lty = 3)
	lines(tsr0$df, tsr0$lts, col = 3, lwd = 1.5, lty = 5)
	legend(par("usr")[2], par("usr")[3], xjust = 1, yjust = 0,
				 col = 1:3, lty = c(1, 3, 5), lwd = 1.5,
				 legend = c("BW/LS", "LAD/LS", "LTS/LS"),
				 bty = "n", seg.len = 3)
}

dev.off()




