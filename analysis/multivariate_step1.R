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

# Group sizes
group_size <- fread("data-raw/Study1_Group sizes.csv")
group_size[, Group := sprintf("%02d", Group)]
colnames(group_size)[1] <- "group"

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/uniariate_1.rds")
univariate_2 <- readRDS("analysis/uniariate_2.rds")

univariate_1 <- merge(univariate_1, group_size, by = "group")
univariate_2 <- merge(univariate_2, group_size, by = "group")

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group", "groupSize"))
# vars1 <- setdiff(vars1, vars1[grepl("^(GPA|SI3F)", vars1)])

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group", "groupSize"))
# vars2 <- setdiff(vars2, vars2[grepl("^(GPA|SI3F)", vars2)])

# Adding an interaction effect with the group size -----------------------------
for (i in vars1) {
  univariate_1[[paste0(i," x Size")]] <- univariate_1[[i]] * univariate_1[["groupSize"]]
  vars1 <- c(vars1, paste0(i," x Size"))
}
  
for (i in vars2) {
  univariate_2[[paste0(i," x Size")]] <- univariate_2[[i]] * univariate_2[["groupSize"]]
  vars2 <- c(vars2, paste0(i," x Size"))
}

# Rescaling variables ----------------------------------------------------------
for (i in seq_len(ncol(univariate_1))) {
  if (is.numeric(univariate_1[[i]]))
    univariate_1[[i]][] <- (univariate_1[[i]][] - mean(univariate_1[[i]][], na.rm=TRUE))/
      (1e-15 + sd(univariate_1[[i]][], na.rm=TRUE))
}

for (i in seq_len(ncol(univariate_2))) {
  if (is.numeric(univariate_2[[i]]))
    univariate_2[[i]][] <- (univariate_2[[i]][] - mean(univariate_2[[i]][], na.rm=TRUE))/
      (1e-15 + sd(univariate_2[[i]][], na.rm=TRUE))
}

# Program for general model fitting (all combinations)
vars1 <- c(vars1, "groupSize")
models1 <- expand.grid(vars1, vars1)
models1 <- as.matrix(models1)

vars2 <- c(vars2, "groupSize")
models2 <- expand.grid(vars2, vars2)
models2 <- as.matrix(models2)

# Filtering out cases
models_main_vars1 <- models1
models_main_vars1[] <- gsub("(Min|Max|Avg\\.|Geom\\.|Range).+$", "" , models_main_vars1)
models1 <- models1[models_main_vars1[,1] != models_main_vars1[,2], ]

models_main_vars2 <- models2
models_main_vars2[] <- gsub("(Min|Max|Avg\\.|Geom\\.|Range).+$", "" , models_main_vars2)
models2 <- models2[models_main_vars2[,1] != models_main_vars2[,2], ]

# # All models should include group size
# models1 <- cbind(models1, "groupSize")
# models2 <- cbind(models2, "groupSize")

# Analysis for CIF_T1 ----------------------------------------------------------

fit1 <- analyze_models("CIF_T1", as.data.frame(univariate_1), models1)
saveRDS(fit1, "analysis/multivariate_step1_counts_time1.rds")

tabulate_counts(
  model.       = fit1$model,
  significant. = fit1$significant,
  file.        = "analysis/multivariate_step1_counts_time1.tex"
)

# Analysis for CIF_T2 ----------------------------------------------------------
fit2 <- analyze_models("CIF_T2", as.data.frame(univariate_2), models2)
saveRDS(fit2, "analysis/multivariate_step1_counts_time2.rds")

tabulate_counts(
  model.       = fit2$model,
  significant. = fit2$significant,
  file.        = "analysis/multivariate_step1_counts_time2.tex"
)

