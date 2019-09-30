library(data.table)
library(magrittr)
library(xtable)

nodal_data <- readRDS("data/nodal_data.rds")
nodal_data <- as.data.table(nodal_data)

# Variables to summarize
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

# Report mean, missigness, and sd
meaner <- function(x) sprintf(
  "%.2f (%.2f)",
  mean(x, na.rm = TRUE),
  sd(x, na.rm = TRUE)
)

tab <- sapply(vars, function(x.) meaner(nodal_data[[x.]])) %>%
  cbind

# For collective intelligence, we need to compute the aggregate per group
group_data <- nodal_data[
  ,
  list(
    CIF_T1 = unique(CIF_T1),
    CIF_T2 = unique(CIF_T2)
    ) , by="group"]

# Outcome of interest
ci_vars <- c("CI in time 1" = "CIF_T1", "CI in time 2" = "CIF_T2")
tab <- rbind(
  tab, {
    sapply(ci_vars, function(x.) meaner(nodal_data[[x.]])) %>%
      cbind
})

tab <- data.frame(
  Variable = rownames(tab),
  "Mean (sd)" = tab[,1], check.names = FALSE
)
rownames(tab) <- seq_len(nrow(tab))

tab <- xtable(tab)
print(
  tab,
  file             = "figures/descriptive_stats.tex",
  booktabs         = TRUE, 
  include.rownames = FALSE
  )

tab <- readLines("figures/descriptive_stats.tex")

tab[grepl("Female", tab)] <- paste0(
  "\\multicolumn{3}{c}{\\textit{Socio-demographics}} \\\\ \n",
  tab[grepl("Female", tab)]
)

tab[grepl("RME", tab)] <- paste0(
  "\\multicolumn{3}{c}{\\textit{RME and Social Intelligence}} \\\\ \n",
  tab[grepl("RME", tab)]
)
tab[grepl("CI in time 1", tab, fixed = TRUE)] <- paste0(
  "\\multicolumn{3}{c}{\\textit{Collective Intelligence}} \\\\ \n",
  tab[grepl("CI in time 1", tab, fixed = TRUE)]
)

# Modifying the dimension of the table
tab <- strsplit(paste(tab, collapse="\n"), "\n")[[1]]
tab[grepl("\\begin{tabular}", tab, fixed=TRUE)] <- "\\begin{tabular}{lll}"
tab[!grepl("^(\\t*|\\s*)[\\%]", tab)] <-
  paste("&", tab[!grepl("^(\\t*|\\s*)[\\%]", tab)])

# Put this on the paper
writeLines(tab, con = "figures/descriptive_stats.tex")

# Now, at the group level, the question is, are groups balanced?
# We can look at this looking at the distribution of each sample.
lapply(split(nodal_data, nodal_data$group), function(x..) {
  sapply(vars, function(x.) meaner(x..[[x.]]))
}) %>% 
  lapply(function(x.) data.frame(matrix(x., nrow=1))) %>%
  rbindlist()
