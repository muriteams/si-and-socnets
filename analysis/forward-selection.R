library(data.table)

# Loading programs
source("analysis/fitters.R")

# Loading data -----------------------------------------------------------------

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/univariate_1.rds")
univariate_2 <- readRDS("analysis/univariate_2.rds")

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group", "Size", "GPA0 Avg."))

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))

# Program for general model fitting (all combinations)
vars1 <- c(vars1, "Size")
models1 <- combn(vars1, 2, simplify = FALSE)

vars2 <- c(vars2, "Size")
models2 <- combn(vars2, 2, simplify = FALSE)

# Analysis for CIF_T1 ----------------------------------------------------------
library(olsrr)

model1 <- lm(CIF_T1 ~ ., data = subset(univariate_1, select = c("CIF_T1", vars1)))
model1 <- suppressMessages(stepAIC(model1, direction = "forward"))

model11 <- lm(
  CIF_T1 ~ `Female Avg.` + `Age0 Min` + `RME0 Min` +
    `TIPI_OpennesstoExperiences Avg.` + `SocialInfoProcessing Avg.` +
    `SocialSkills Avg.` + `SocialAwareness Avg.` +
    `InterpersonalRelationship Avg. sqrt.` +
    `SI3Fac20 Min`, data = univariate_1
  )
