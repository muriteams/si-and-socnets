library(data.table)
library(magrittr)

nodal_data <- readRDS("data/nodal_data.rds")
nodal_data <- data.table(nodal_data)

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
  "Interpersonal Relationship" = "InterpersonalRelationship"
)

# Aggregating
aggregator <- function(v, f, pf="all", dat.) {
  
  ans <- dat.[
    ,
    c(list(CIF_T1 = max(CIF_T1), CIF_T2 = max(CIF_T2)),
      lapply(.SD, f)), by = "group", .SDcols = v]
  
  colnames(ans)[4:(4+length(v) - 1)] <- paste0(v, pf)
  ans
}

# Test 1: Association of each variable with the full set

to_keep <- c(
  "statistic",
  "p.value",
  "conf.int",
  "estimate"
  )

# Ideal print:
# Corr [95% Corr value CI] & Statistic value & p.value

as_asterisk <- function(x) {
  
  ifelse(x < .001, "***",
         ifelse(x < .01, "**",
                ifelse(x < .05, "*", "")))
    
}

cor_test <- function(dat., vars., name.) {
  ans <- lapply(vars., function(v) {
    with(
      # Correlation test
      cor.test(dat.[[v]], dat.$CIF_T1),
      
      # Distilling the results
      data.frame(
        Variable = v,
        Estimate = sprintf(
          "%.2f %s", estimate, # conf.int[1], conf.int[2],
          as_asterisk(p.value)
        ),
        check.names = FALSE
      )
    )
  }) %>% rbindlist()
  
  if (missing(name.))
    return(ans)
  
  colnames(ans)[2] <- name.
  ans
}

# Computing tests --------------------------------------------------------------

# On the average -----
dat_means <- aggregator(vars, mean, "", nodal_data)
dat_means <- cor_test(dat_means, vars, "Avg.")

# On the min ---------
dat_min <- aggregator(vars, min, "", nodal_data)
dat_min <- cor_test(dat_min, vars, "Min")

# On the max ---------
dat_max <- aggregator(vars, max, "", nodal_data)
dat_max <- cor_test(dat_max, vars, "Max")

# On the range -------
dat_range <- aggregator(vars, function(x.) diff(range(x.)), "", nodal_data)
dat_range <- cor_test(dat_range, vars, "Range")

# Product
dat_prod <- aggregator(vars, prod, "", nodal_data)
dat_prod <- cor_test(dat_prod, vars, "Product")

# Sum
dat_sum <- aggregator(vars, sum, "", nodal_data)
dat_sum <- cor_test(dat_sum, vars, "Sum")

# Merging 
tab <- merge(dat_means, dat_min, all = TRUE) %>%
  merge(dat_max, all = TRUE) %>%
  merge(dat_range, all = TRUE) %>%
  merge(dat_prod, all = TRUE) %>%
  merge(dat_sum, all = TRUE)

# Modifying the names
tab$Variable <- names(vars)[match(tab$Variable, vars)]

# Visuals ----------------------------------------------------------------------

plotdat <- rbind(
  cbind(aggregator(vars, mean, "", nodal_data), Type = "mean"),
  cbind(aggregator(vars, min, "", nodal_data), Type = "min"),
  cbind(aggregator(vars, max, "", nodal_data), Type = "max"),
  cbind(aggregator(vars, function(x.) diff(range(x.)), "", nodal_data), Type = "function"),
  cbind(aggregator(vars, prod, "", nodal_data), Type = "prod"),
  cbind(aggregator(vars, sum, "", nodal_data), Type = "sum")
)

plotdat <- 
  melt(plotdat, id.vars = c("Type", "CIF_T1", "CIF_T2"), measure.vars = vars)

library(ggplot2)

ggplot(plotdat, aes(x = value, y = CIF_T1)) +
  geom_jitter() +
  facet_wrap(variable ~ Type)
