library(data.table)

# Loading programs
source("analysis/fitters.R")

# Loading data -----------------------------------------------------------------

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/univariate_1.rds")
univariate_2 <- readRDS("analysis/univariate_2.rds")

# Loading results from step1 (top 100 per each that)
step1_1 <- readRDS("analysis/multivariate_step1_counts_time1.rds")
step1_2 <- readRDS("analysis/multivariate_step1_counts_time2.rds")

# step1_1 <- subset(step1_1, nsignificant ==  3)
step1_1 <- step1_1[order(step1_1$LOO_rmse),][1:min(200, nrow(step1_1)),]$model

# step1_2 <- subset(step1_2, nsignificant ==  3)
step1_2 <- step1_2[order(step1_2$LOO_rmse),][1:min(200, nrow(step1_2)),]$model

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group"))

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group"))

# Program for general model fitting (all combinations)
models1 <- grow_a_model(step1_1, vars1, size = 3)
models2 <- grow_a_model(step1_2, vars2, size = 3)

# Analysis for CIF_T1 ----------------------------------------------------------

fit1 <- analyze_models("CIF_T1", as.data.frame(univariate_1), models1)
saveRDS(fit1, "analysis/multivariate_step2_counts_time1.rds")

tabulate_counts(
  model.       = fit1$model,
  significant. = fit1$significant,
  file.        = "analysis/multivariate_step2_counts_time1.tex",
  caption.     = "\\label{tab:top3-1}Top 20 variables with 3 predictors for CI in time 1."
)

# Analysis for CIF_T2 ----------------------------------------------------------
fit2 <- analyze_models("CIF_T2", as.data.frame(univariate_2), models2)
saveRDS(fit2, "analysis/multivariate_step2_counts_time2.rds")

tabulate_counts(
  model.       = fit2$model,
  significant. = fit2$significant,
  file.        = "analysis/multivariate_step2_counts_time2.tex",
  caption.     = "\\label{tab:top3-2}Top 20 variables with 3 predictors for CI in time 2."
)
