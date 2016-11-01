## set working directory
setwd("~/Dropbox/projects/heavy-tails")

## run scripts

source("R/lts-illustration.R")  # least trimmed squared example

source("R/simulations-non-normal-figs-generate-data.R") # perform mc simulations
source("R/simulations-non-normal-figs.R")  # non-normal simulation in figures
source("R/simulations-non-normal-tabs.R")  # non-normal simulations in tables

source("R/cg-analysis.R")  # clark and golder analysis
source("R/cg-coef-plots.R")  # plots of clark and golder analysis


