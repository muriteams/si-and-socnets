library(texreg)

source("analysis/fitters.R")

dat1  <- readRDS("analysis/univariate_1.rds")
dat2  <- readRDS("analysis/univariate_2.rds")

varnames <- c(
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
  "Openness to Exp."    = "TIPI_OpennesstoExperiences",
  "Soc. Info. Proc."    = "SocialInfoProcessing",
  "Soc. Skills"              = "SocialSkills",
  "Soc. Awareness"           = "SocialAwareness",
  "Soc. Resp."      = "SocialResponsibility",
  "Empathy"                    = "Empathy",
  "Interper. Rel." = "InterpersonalRelationship",
  "SI Factor 1"                = "SI3Fac10",
  "SI Factor 2"                = "SI3Fac20",
  "SI Factor 3"                = "SI3Fac30",
  "Geom."                  = "Geometric"
)

# Function to label groups
add_groups <- function(x., name., at.) {
  
  y <- readLines(x.)
  y[grepl(at., y)] <- sprintf("{\\textit{%s}}\\\\ \\cmidrule(r){1-1}\n%s", name., y[grepl(at., y)])
  writeLines(y, x.)
  
}

# We swap this b/c of how texreg works
varnames <- structure(names(varnames), names = unname(varnames))
cnames <- sort(unique(c(colnames(dat1), colnames(dat2))))
varnames <- structure(
  c("Size", "(Intercept)", stringr::str_replace_all(cnames, varnames)),
  names = c("Size", "(Intercept)", paste0("`", cnames, "`"))
  )

# Best model for time 1 --------------------------------------------------------

model1a_1 <- lm(CIF_T1 ~ Size + `Age0 Min` + `SocialAwareness Avg.`, data = dat1)
model1a_2 <- lm(CIF_T1 ~ Size + `Age0 Min` + `SocialAwareness Avg.` + 
                   `InterpersonalRelationship Avg.`, data = dat1)
model1a_3 <- lm(CIF_T1 ~ Size + `Age0 Min` + `SocialAwareness Avg.` +
                   `RME0 Range`, data = dat1)
model1a_4 <- lm(CIF_T1 ~ Size + `Age0 Min` + `SocialAwareness Avg.` +
                  `SocialInfoProcessing Avg.`, data = dat1)
model1a_5 <- lm(CIF_T1 ~ Size + `Age0 Min` + `SocialAwareness Avg.` +
                   `InterpersonalRelationship Avg.` + `RME0 Range` +
                  `SocialInfoProcessing Avg.`, data = dat1)
model1a_6 <- lm(CIF_T1 ~ `Age0 Min` + `SocialAwareness Avg.` +
                  `InterpersonalRelationship Avg.` + `RME0 Range` , data = dat1)

# Model with the 3 factors
model1a_7 <- lm(CIF_T1 ~ `Age0 Min` + `RME0 Range` + `SI3Fac10 Avg.`, data = dat1)

model1a_8 <- lm(CIF_T1 ~ `Age0 Min` + `RME0 Range` + `SI3Fac20 Avg.`, data = dat1)

model1a_9 <- lm(CIF_T1 ~ `Age0 Min` + `RME0 Range` + `SI3Fac30 Avg.`, data = dat1)

model1a_10 <- lm(CIF_T1 ~ `Age0 Min` + `RME0 Range` + `SI3Fac10 Avg.` + 
                  `SI3Fac20 Avg.` + `SI3Fac30 Avg.`, data = dat1)

texreg(
  list(model1a_1, model1a_2, model1a_3, model1a_4, model1a_5, model1a_6,
       model1a_7, model1a_8, model1a_9, model1a_10),
  custom.coef.map = as.list(varnames[c(
    "Size",
    "`Age0 Min`",
    "`RME0 Range`",
    # "(Intercept)",
    "`SocialAwareness Avg.`",
    "`InterpersonalRelationship Avg.`",
    "`SocialInfoProcessing Avg.`",
    "`SI3Fac10 Avg.`",
    "`SI3Fac20 Avg.`",
    "`SI3Fac30 Avg.`"
  )]), stars = c(.01, .05, .10),
  file  = "analysis/multivariate_stepfinal_time1.tex",
  caption = "Final set of best performing models with variables selected from the model selection algorithm for CI in time 1.",
  label = "tab:final1",
  scalebox = .8,
  use.packages = FALSE,
  booktabs = TRUE
)

add_groups(
  "analysis/multivariate_stepfinal_time1.tex",
  name. = "SI factors",
  at.   = "SI Factor 1"
)

add_groups(
  "analysis/multivariate_stepfinal_time1.tex",
  name. = "Subscales",
  at.   = "Awareness"
)


# Best model for time 2 --------------------------------------------------------

model2a_1 <- lm(CIF_T2 ~ Size + `RME0 Range`, data = dat2)

model2a_2 <- lm(CIF_T2 ~ Size + `RME0 Range` + `TIPI_Agreeableness Avg.`, data = dat2)

model2a_3 <- lm(CIF_T2 ~ Size + `RME0 Range` + `TIPI_EmotionalStability Avg.`, data = dat2)

model2a_4 <- lm(CIF_T2 ~ Size + `RME0 Range` + `TIPI_Extraversion Range`, data = dat2)

model2a_5 <- lm(CIF_T2 ~ Size + `TIPI_Agreeableness Avg.` +
                  `TIPI_EmotionalStability Avg.` + `RME0 Range` + 
                  `TIPI_Extraversion Range`, data = dat2)

model2a_6 <- lm(CIF_T2 ~ Size + `RME0 Range` + `SI3Fac10 Avg.`, data = dat2)

model2a_7 <- lm(CIF_T2 ~ Size + `RME0 Range` + `SI3Fac20 Range`, data = dat2)

model2a_8 <- lm(CIF_T2 ~ Size + `RME0 Range` + `SI3Fac30 Avg.`, data = dat2)

model2a_9 <- lm(CIF_T2 ~ Size + `RME0 Range` + `SI3Fac10 Avg.` +
                  `SI3Fac20 Range` + `SI3Fac30 Avg.`, data = dat2)
# model2a_4 <- lm(CIF_T2 ~ Size + `RME0 Range` + `SI3Fac20 Range`, data = dat2)


texreg(
  list(model2a_1, model2a_2, model2a_3, model2a_4, model2a_5, model2a_6,
       model2a_7, model2a_8, model2a_9),
  custom.coef.map = as.list(varnames[c(
    "Size",
    "`RME0 Range`",
    # "(Intercept)",
    "`TIPI_Agreeableness Avg.`",
    "`TIPI_EmotionalStability Avg.`",
    "`TIPI_Extraversion Range`",
    "`SI3Fac10 Avg.`",
    "`SI3Fac20 Range`",
    "`SI3Fac30 Avg.`"
  )]), stars = c(.01, .05, .10),
  file  = "analysis/multivariate_stepfinal_time2.tex",
  caption = "Final set of best performing models with variables selected from the model selection algorithm for CI in time 2.",
  label = "tab:final2",
  scalebox = .8,
  use.packages = FALSE,
  booktabs = TRUE
)

add_groups(
  "analysis/multivariate_stepfinal_time2.tex",
  name. = "SI factors",
  at.   = "SI Factor 1"
  )

add_groups(
  "analysis/multivariate_stepfinal_time2.tex",
  name. = "Subscales",
  at.   = "Agreeableness"
)
