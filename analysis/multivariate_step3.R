library(data.table)

# Loading programs
source("analysis/fitters.R")

# Loading data -----------------------------------------------------------------

# Variables labels
vars <- c(
  # Socio-demographic controls
  Female                       = "Female",
  "Non-white"                  = "nonwhite",
  Age                          = "Age0",
  GPA                          = "GPA0",
  # Psychological measurements
  RME                          = "RME0",
  Extraversion                 = "TIPI_Extraversion",
  Agreeableness                = "TIPI_Agreeableness",
  Conscientiousness            = "TIPI_Conscientiousness",
  "Emotional Stability"        = "TIPI_EmotionalStability",
  "Openness to Experiences"    = "TIPI_OpennesstoExperiences",
  "Social Info. Processing"    = "SocialInfoProcessing",
  "Social Skills"              = "SocialSkills",
  "Social Awareness"           = "SocialAwareness",
  "Social Responsibility"      = "SocialResponsibility",
  "Empathy"                    = "Empathy",
  "Interpersonal Relationship" = "InterpersonalRelationship",
  "SI Factor 1"                = "SI3Fac10",
  "SI Factor 2"                = "SI3Fac20",
  "SI Factor 3"                = "SI3Fac30"
)

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/uniariate_1.rds")
univariate_2 <- readRDS("analysis/uniariate_2.rds")

# Loading results from step1 (top 100 per each that)
step2_1 <- readRDS("analysis/multivariate_step2_counts_time1.rds")
step2_2 <- readRDS("analysis/multivariate_step2_counts_time2.rds")

step2_1 <- subset(step2_1, nsignificant ==  3)
step2_1 <- step2_1[order(step2_1$rmse),][1:100,]$model
step2_1 <- do.call(rbind, step2_1)

step2_2 <- subset(step2_2, nsignificant ==  3)
step2_2 <- step2_2[order(step2_2$rmse),][1:100,]$model
step2_2 <- do.call(rbind, step2_2)

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))


# Program for general model fitting (all combinations)

vars1 <- c(vars1, "Size")
models1 <- expand.grid(idx = 1:100, var3 = vars1, stringsAsFactors = FALSE)
models1 <- cbind(step2_1[models1$idx, ], models1$var3)
models1 <- as.matrix(models1)

vars2 <- c(vars2, "Size")
models2 <- expand.grid(idx = 1:100, var3 = vars2, stringsAsFactors = FALSE)
models2 <- cbind(step2_2[models2$idx, ], models2$var3)
models2 <- as.matrix(models2)

# Filtering out cases
models1 <- models1[
  (models1[,1] != models1[,4]) & (models1[,2] != models1[,4]) & (models1[,3] != models1[,4]),
  ]
models2 <- models2[
  (models2[,1] != models2[,4]) & (models2[,2] != models2[,4]) & (models2[,3] != models2[,4]),
  ]


# # Filtering out cases
# models_main_vars1 <- models1
# models_main_vars1[] <- gsub("(Min|Max|Avg\\.|Geom\\.|Range).+$", "" , models_main_vars1)
# 
# test              <- which(models_main_vars1[,3] != models_main_vars1[,4])
# models_main_vars1 <- models_main_vars1[test,]
# models1           <- models1[test, ]
# 
# test              <- which(models_main_vars1[,2] != models_main_vars1[,4])
# models_main_vars1 <- models_main_vars1[test,]
# models1           <- models1[test, ]
# 
# test              <- which(models_main_vars1[,1] != models_main_vars1[,4])
# models_main_vars1 <- models_main_vars1[test,]
# models1           <- models1[test, ]
# 
# models_main_vars2 <- models2
# models_main_vars2[] <- gsub("(Min|Max|Avg\\.|Geom\\.|Range).+$", "" , models_main_vars2)
# 
# test              <- which(models_main_vars2[,1] != models_main_vars2[,4])
# models_main_vars2 <- models_main_vars2[test,]
# models2           <- models2[test, ]
# 
# test              <- which(models_main_vars2[,2] != models_main_vars2[,4])
# models_main_vars2 <- models_main_vars2[test,]
# models2           <- models2[test, ]
# 
# test              <- which(models_main_vars2[,3] != models_main_vars2[,4])
# models_main_vars2 <- models_main_vars2[test,]
# models2           <- models2[test, ]

# Analysis for CIF_T1 ----------------------------------------------------------

fit1 <- analyze_models("CIF_T1", as.data.frame(univariate_1), models1)
saveRDS(fit1, "analysis/multivariate_step3_counts_time1.rds")

tabulate_counts(
  model.       = fit1$model,
  significant. = fit1$significant,
  file.        = "analysis/multivariate_step3_counts_time1.tex",
  caption.     = "Top 20 variables with 4 predictors for CI in time 1."
)

# Analysis for CIF_T2 ----------------------------------------------------------
fit2 <- analyze_models("CIF_T2", as.data.frame(univariate_2), models2)
saveRDS(fit2, "analysis/multivariate_step3_counts_time2.rds")

tabulate_counts(
  model.       = fit2$model,
  significant. = fit2$significant,
  file.        = "analysis/multivariate_step3_counts_time2.tex",
  caption.     = "Top 20 variables with 4 predictors for CI in time 1."
)
