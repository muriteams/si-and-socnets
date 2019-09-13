library(data.table)
library(magrittr)
library(xtable)
library(ggplot2)

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

counts <- new.env(parent = emptyenv())
cor_test <- function(dat., vars., name.) {
  ans <- lapply(vars., function(v) {
    
    assign(v, sum(is.na(dat.[[v]])), envir = counts)
    
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
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }) %>% rbindlist()
  
  if (missing(name.))
    return(ans)
  
  colnames(ans)[2] <- name.
  ans
}

# Computing tests --------------------------------------------------------------

transformations <- list(
  `Avg.`  = mean,
  Min     = min,
  Max     = max,
  Range   = function(x.) diff(range(x.)),
  Product = prod,
  Sum     = sum #,
  # `Avg. sq.`    = function(x.) mean(x.)^2,
  # `Min sq.`     = function(x.) min(x.)^2,
  # `Max sq.`     = function(x.) max(x.)^2,
  # `Range sq.`   = function(x.) diff(range(x.)),
  # `Product sq.` = function(x.) prod(x.)^2,
  # `Sum sq.`     = function(x.) sum(x.)^2
)

ans <- vector("list", length(transformations))
excluded <- NULL
for (i in seq_along(transformations)) {
  
  res <- aggregator(vars, transformations[[i]], "", nodal_data)
  ans[[i]] <-  cor_test(res, vars, names(transformations)[i])
  
  excluded <- cbind(excluded, unlist(as.list(counts)))
  
}


# Merging 
tab <- merge(ans[[1]], ans[[2]], all = TRUE)
for (i in 3:length(ans))
  tab <- merge(tab, ans[[i]], all = TRUE) 

# Modifying the names
tab$`Obs. Excluded` <- rowMeans(excluded)[tab$Variable]
tab$Variable <- names(vars)[match(tab$Variable, vars)]

tab <- as.matrix(tab)

tab[grepl("NA NA", tab)] <- " - "

# Highlighting the highest most significant correlatio levels
three <- t(apply(tab, 1, function(t.) grepl("\\s+[*]{3}$", t.)))
two   <- t(apply(tab, 1, function(t.) grepl("\\s+[*]{2}$", t.)))
one   <- t(apply(tab, 1, function(t.) grepl("\\s+[*]{1}$", t.)))

for (i in seq_len(nrow(tab))) {
  
  tab_i <- abs(as.numeric(gsub("\\s+|[*]+", "" ,tab[i,])))
  
  if (any(three[i,])) {
    
    # Which is the highest, of the ones with three
    idx <- which(three[i,])
    idx <- idx[which.max(tab_i[idx])]
    tab[i, idx] <- sprintf("\\textbf{%s}", tab[i, idx])
    
  } else if (any(two[i,])) {
    
    # Which is the highest, of the ones with three
    idx <- which(two[i,])
    idx <- idx[which.max(tab_i[idx])]
    tab[i, idx] <- sprintf("\\textbf{%s}", tab[i, idx])
    
  } else if (any(one[i,])) {
    
    # Which is the highest, of the ones with three
    idx <- which(one[i,])
    idx <- idx[which.max(tab_i[idx])]
    tab[i, idx] <- sprintf("\\textbf{%s}", tab[i, idx])
    
  }
  
}

# Exporting
tab <- xtable(
  tab,
  caption = paste(
    "Correlation levels between group level predictors and CI in time 1.",
    "For each variable (row), the most significant value is highlighted.",
    "The last column of the table indicates the number of groups droped from",
    "the analysis due to missigness."
  )
  )

print(
  tab, include.rownames = FALSE, booktabs = TRUE,
  sanitize.text.function = function(i) i,
  file = "analysis/univariate.tex"
  )

# Visuals ----------------------------------------------------------------------

# Using base R
tmp <- aggregator(vars, transformations[[1]], pf = "", dat. = nodal_data)

op <- par(mfrow = c(4, 4), mai = rep(.2, 4), oma = c(4,4,4,0))
for (v in vars) {
  plot(y = tmp[["CIF_T1"]], x = tmp[[v]])
  abline(lm(tmp[["CIF_T1"]] ~ tmp[[v]]))
}
par(op)
title(names(transformations)[1])

# With ggplot

plotdat <- NULL
for (i in seq_along(transformations)) 
  plotdat <- rbind(
    plotdat,
    cbind(
      aggregator(vars, transformations[[i]], "", nodal_data),
      Type = names(transformations)[i]
      )
  )

plotdat <- 
  melt(plotdat, id.vars = c("Type", "CIF_T1", "CIF_T2"), measure.vars = vars)


# Rscaling to get a better measurement (nicer on the plot)
plotdat[
  ,
  value := (value - min(value, na.rm = TRUE))/diff(range(value, na.rm = TRUE)),
  by = c("Type", "variable")
  ]

plotdat$variable <- names(vars)[match(plotdat$variable, vars)]

# For the mean first
for (i in seq_along(transformations)) {
  subset(plotdat, Type == names(transformations)[i]) %>%
    ggplot(aes(x = value, y = CIF_T1)) +
    geom_point() +
    ylim(c(-3, 3)) +
    facet_wrap(variable ~ .) +
    geom_smooth() +
    labs(x = paste("Group", names(transformations)[i]), y = "Collective Intelligence")
  
  ggsave(
    filename = sprintf(
      "analysis/univariate_%s.png",
      gsub("[._ ]+", "_", tolower(names(transformations)[i]))
      )
    )
}

