library(data.table)

# Loading programs
source("analysis/fitters.R")

# Loading data -----------------------------------------------------------------

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/univariate_1.rds")
univariate_2 <- readRDS("analysis/univariate_2.rds")

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))

# Program for general model fitting (all combinations)
vars1 <- c(vars1, "Size")
models1 <- combn(vars1, 2, simplify = FALSE)

vars2 <- c(vars2, "Size")
models2 <- combn(vars2, 2, simplify = FALSE)

# Analysis for CIF_T1 ----------------------------------------------------------

fit1 <- analyze_models("CIF_T1", as.data.frame(univariate_1), models1)
saveRDS(fit1, "analysis/multivariate_step1_counts_time1.rds")

tabulate_counts(
  model.       = fit1$model,
  significant. = fit1$significant,
  file.        = "analysis/multivariate_step1_counts_time1.tex",
  caption.     = "\\label{tab:top1-1}Top 10 variables with 2 predictors for CI in time 1."
)

# Analysis for CIF_T2 ----------------------------------------------------------
fit2 <- analyze_models("CIF_T2", as.data.frame(univariate_2), models2)
saveRDS(fit2, "analysis/multivariate_step1_counts_time2.rds")

tabulate_counts(
  model.       = fit2$model,
  significant. = fit2$significant,
  file.        = "analysis/multivariate_step1_counts_time2.tex",
  caption.     = "\\label{tab:top1-2}Top 10 variables with 2 predictors for CI in time 2."
)

