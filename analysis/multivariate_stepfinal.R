library(texreg)
dat1  <- readRDS("analysis/univariate_1.rds")
dat2  <- readRDS("analysis/univariate_2.rds")
time1 <- readRDS("analysis/multivariate_step4_time1.rds")
time2 <- readRDS("analysis/multivariate_step4_time2.rds")
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
# We swap this b/c of how texreg works
varnames <- structure(names(varnames), names = unname(varnames))
cnames <- sort(unique(c(colnames(dat1), colnames(dat2))))
varnames <- structure(c("Size", stringr::str_replace_all(cnames, varnames)),
                      names = c("Size", paste0("`", cnames, "`")))
# Best model for time 1 --------------------------------------------------------
# Not using the factors
model1a_1 <- glm(CIF_T1 ~ Size + `Age0 Min`, data = dat1)
model1a_2 <-
  glm(CIF_T1 ~ Size + `Age0 Min` + `RME0 Min`, data = dat1)
model1a_3 <- glm(CIF_T1 ~ Size + `SocialSkills Min` +
                   `InterpersonalRelationship Avg.` + `RME0 Min`,
                 data = dat1)
model1a_4 <- glm(
  CIF_T1 ~ Size + `Age0 Min` + `SocialSkills Min` +
    `InterpersonalRelationship Avg.` + `RME0 Min`,
  data = dat1
)
model1a_5 <-
  glm(CIF_T1 ~ Size + `Age0 Min` + `RME0 Min` + `SI3Fac20 Avg.`, data = dat1)
