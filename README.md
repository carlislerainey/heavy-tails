This repository contains the manuscript and code for paper "When BLUE Is Not Best: Non-Normal Errors and the Linear Model."

You can find the latest version [here](http://www.carlislerainey.com/papers/heavy-tails.pdf).

To replicate the results, simply run the file `do-all.R`. If you prefer to replicate only some of the results, this file points to the relevant files in the directory `R`. Note that the output of `R/simulations-non-normal-figs-generate-data.R` is too big to keep on GitHub, so you must run this script to create the simulations.

Here's the abstract:

> Researchers in political science often estimate linear models of continuous outcomes using least squares. While it is well-known that least-squares estimates are sensitive to single, unusual data points, this knowledge has not led to careful practices when using least-squares estimators. Using statistical theory, Monte Carlo simulations, and an applied example, we highlight the importance of using more robust estimators along with variable transformations. We also discuss several approaches to detect, summarize, and communicate the influence of particular data points. We conclude with a reanalysis of Clark and Golder (2006), showing that the residuals are highly non-normal under their model specification and that an alternative, robust estimator leads to richer substantive results.
