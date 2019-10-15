library(data.table)

# Loading programs
source("analysis/fitters.R")

# Loading data -----------------------------------------------------------------

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/univariate_1.rds")
univariate_2 <- readRDS("analysis/univariate_2.rds")

# Loading results from step1 (top 100 per each that)
step3_1 <- readRDS("analysis/multivariate_step3_counts_time1.rds")
step3_2 <- readRDS("analysis/multivariate_step3_counts_time2.rds")

# step2_1 <- subset(step2_1, nsignificant ==  4)
step3_1 <- step3_1[order(step3_1$LOO_rmse),][1:min(200, nrow(step3_1)),]$model

# step2_2 <- subset(step2_2, nsignificant ==  4)
step3_2 <- step3_2[order(step3_2$LOO_rmse),][1:min(200, nrow(step3_2)),]$model

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group"))

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group"))

# Program for general model fitting (all combinations)
models1 <- grow_a_model(step3_1, vars1, size = 5)
models2 <- grow_a_model(step3_2, vars2, size = 5)

# Analysis for CIF_T1 ----------------------------------------------------------

fit1 <- analyze_models("CIF_T1", as.data.frame(univariate_1), models1)
saveRDS(fit1, "analysis/multivariate_step4_counts_time1.rds")

tabulate_counts(
  model.       = fit1$model,
  significant. = fit1$significant,
  file.        = "analysis/multivariate_step4_counts_time1.tex",
  caption.     = "\\label{tab:top5-1}Top 10 variables with 5 predictors for CI in time 1."
)

# Analysis for CIF_T2 ----------------------------------------------------------
fit2 <- analyze_models("CIF_T2", as.data.frame(univariate_2), models2)
saveRDS(fit2, "analysis/multivariate_step4_counts_time2.rds")

tabulate_counts(
  model.       = fit2$model,
  significant. = fit2$significant,
  file.        = "analysis/multivariate_step4_counts_time2.tex",
  caption.     = "\\label{tab:top5-2}Top 10 variables with 5 predictors for CI in time 1."
)
