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

# Plots ------------------------------------------------------------------------

# Normality assumptions
model1a_6$call <- quote(` `)
model2a_5$call <- quote(` `)

graphics.off()

png("analysis/multivariate_stepfinal_gof-time1.png", width = 500, height = 300)
op <- par(mfrow=c(2,2), mar = c(4,4,2,1))
plot(model1a_6, which = c(1, 2, 5))
dev.off()
par(op)

png("analysis/multivariate_stepfinal_gof-time2.png", width = 500, height = 300)
par(mfrow=c(2,2), mar = c(4,4,2,1))
plot(model2a_5, which = c(1, 2, 5))
dev.off()
par(op)

# Distribution of the data
library(ggplot2)
library(gridExtra)

# Plot for time 1
p1 <- ggplot(dat1, aes(y = CIF_T1, x = `Age0 Min`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 1", x = "Age Min")

p2 <- ggplot(dat1, aes(y = CIF_T1, x = `RME0 Range`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 1", x = "RME Range")

p3 <- ggplot(dat1, aes(y = CIF_T1, x = `SocialAwareness Avg.`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 1", x = "Social Awareness Avg.")

p4 <- ggplot(dat1, aes(y = CIF_T1, x = `InterpersonalRelationship Avg.`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 1", x = "Interp. Rel. Avg.")

p <- marrangeGrob(list(p1, p2, p3, p4), nrow=2, ncol=2, top="",
             layout_matrix = matrix(c(1,2,3,4), nrow=2, byrow = TRUE))
ggsave(plot = p, filename = "analysis/multivariate_stepfinal_time1.png", 
       height = 4, width = 6, units="in")

# Plot for time 2
p1 <- ggplot(dat2, aes(y = CIF_T2, x = `Size`)) +
  geom_jitter(width=0) +
  geom_smooth() +
  labs(y = "CI in time 2", x = "Size")

p2 <- ggplot(dat2, aes(y = CIF_T2, x = `RME0 Range`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 2", x = "RME Range")

p3 <- ggplot(dat2, aes(y = CIF_T2, x = `TIPI_Agreeableness Avg.`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 2", x = "Agreeableness Avg.")

p4 <- ggplot(dat2, aes(y = CIF_T2, x = `TIPI_EmotionalStability Avg.`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 2", x = "Emotional Stability Avg.")

p5 <- ggplot(dat2, aes(y = CIF_T2, x = `TIPI_Extraversion Range`)) +
  geom_jitter() +
  geom_smooth() +
  labs(y = "CI in time 2", x = "Extraversion Range")

p <- marrangeGrob(list(p1, p2, p3, p4, p5), nrow=3, ncol=2, top="",
                  layout_matrix = matrix(c(1,2,3,4,5,6), nrow=3, byrow = TRUE))
ggsave(plot = p, filename = "analysis/multivariate_stepfinal_time2.png", 
       height = 5, width = 6, units="in")
