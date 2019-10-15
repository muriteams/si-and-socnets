library(data.table)
library(magrittr)
library(xtable)
library(ggplot2)

nodal_data <- readRDS("data/nodal_data.rds")
nodal_data <- data.table(nodal_data)

group_size <- fread("data-raw/Study1_Group sizes.csv")
group_size[, group := sprintf("%02d", Group)]
group_size <- subset(group_size, select = c(-Group))
group_size <- setNames(group_size, c("Size", "group"))


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

# Aggregating
aggregator <- function(v, f, pf="all", dat.) {
  
  
  if (is.list(f)) {
    
    ans. <- aggregator(v = v, f = f[[1]], pf = pf[1], dat. = dat.)
    if (length(f) > 1)
      for (i in 2:length(f))
        ans. <- merge(
          ans., 
          aggregator(v = v, f = f[[i]], pf = pf[i], dat. = dat.)
        )
      
    return(ans.)
      
  }

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
cor_test <- function(yvar, dat., vars., name.) {
  ans <- lapply(vars., function(v) {
    
    assign(v, sum(is.na(dat.[[v]])), envir = counts)
    
    with(
      # Correlation test
      cor.test(dat.[[v]], dat.[[yvar]]),
      
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

impute <- function(i) {
  # i[is.na(i)] <- mean(i, na.rm = TRUE)
  i
}

transformations <- list(
  `Avg.`  = function(x.) mean(impute(x.), na.rm = FALSE),
  Min     = function(x.) min(impute(x.), na.rm = FALSE),
  Max     = function(x.) max(impute(x.), na.rm = FALSE),
  Range   = function(x.) diff(range(impute(x.), na.rm = FALSE)),
  # Sum     = function(x.) sum(x., na.rm = TRUE),
  # Product = function(x.) prod(x., na.rm = TRUE),
  # `Geometric Mean` = function(x.) prod(x., na.rm=FALSE)^(1/sum(!is.na(x.))),
  `Avg. sq.`    = function(x.) mean(impute(x.), na.rm = FALSE)^2,
  `Avg. sqrt.`    = function(x.) mean(impute(x.), na.rm = FALSE)^(1/2),
  # `Min sq.`     = function(x.) min(x.)^2,
  # `Max sq.`     = function(x.) max(x.)^2,
  `Range sq.`   = function(x.) diff(range(impute(x.), na.rm = FALSE)),
  `Range sqrt.`   = function(x.) diff(range(impute(x.), na.rm = FALSE)) 
  # `Geom. Mean sq.` = function(x.) prod(x., na.rm = TRUE)^2
  # `Sum sq.`     = function(x.) sum(x.)^2
)

# Computing tests --------------------------------------------------------------

for (time. in 1:2) {
  
  yvar <- paste0("CIF_T", time.)
  
  ans <- vector("list", length(transformations))
  excluded <- NULL
  for (i in seq_along(transformations)) {
    
    res <- aggregator(vars, transformations[[i]], "", nodal_data)
    ans[[i]] <-  cor_test(yvar, res, vars, names(transformations)[i])
    
    excluded <- cbind(excluded, unlist(as.list(counts)))
    
  }
  
  
  # Merging 
  tab <- merge(ans[[1]], ans[[2]], all = TRUE)
  for (i in 3:length(ans))
    tab <- merge(tab, ans[[i]], all = TRUE) 
  
  # Modifying the names
  # tab$`Obs. Excluded` <- apply(excluded, 1, paste, collapse="/")[tab$Variable]
  tab$Variable <- names(vars)[match(tab$Variable, vars)]
  
  # Adding interaction effects with group size
  tab <- as.matrix(tab)
  
  tab[grepl("NA NA", tab)] <- " - "
  
  # Highlighting the highest most significant correlatio levels
  three <- t(apply(tab, 1, function(t.) grepl("\\s+[*]{3}$", t.)))
  two   <- t(apply(tab, 1, function(t.) grepl("\\s+[*]{2}$", t.)))
  one   <- t(apply(tab, 1, function(t.) grepl("\\s+[*]{1}$", t.)))
  
  # Significance
  aggregations_to_keep <- structure(vector("list", nrow(tab)), names = vars)
  for (i in seq_len(nrow(tab))) {
    
    tab_i <- abs(as.numeric(gsub("\\s+|[*]+", "" ,tab[i,])))
    
    tmp_top2 <- NULL
    if (any(three[i,])) {
      
      # Which is the highest, of the ones with three
      idx <- which(three[i,])
  
      # We will save the top 2 (if any) per variable
      tmp_top2 <- sort(idx, decreasing = TRUE)
  
      idx <- idx[which.max(tab_i[idx])]
      tab[i, idx] <- sprintf("\\textbf{%s}", tab[i, idx])
      
    } else if (any(two[i,])) {
      
      # Which is the highest, of the ones with three
      idx <- which(two[i,])
      
      # We will save the top 2 (if any) per variable
      tmp_top2 <- sort(idx, decreasing = TRUE)
      
      idx <- idx[which.max(tab_i[idx])]
      tab[i, idx] <- sprintf("\\textbf{%s}", tab[i, idx])
      
    } else if (any(one[i,])) {
      
      # Which is the highest, of the ones with three
      idx <- which(one[i,])
      
      # We will save the top 2 (if any) per variable
      tmp_top2 <- sort(idx, decreasing = TRUE)
      
      idx <- idx[which.max(tab_i[idx])]
      tab[i, idx] <- sprintf("\\textbf{%s}", tab[i, idx])
      
    }
    
    # Storing the significant association
    aggregations_to_keep[[vars[tab[i, "Variable"]]]] <-
      transformations[colnames(tab)[which((three + two + one)[i,] == 1)]]
    
    # If no transformation is kept, then keep the average only
    if (!length(aggregations_to_keep[[vars[tab[i, "Variable"]]]]))
      aggregations_to_keep[[vars[tab[i, "Variable"]]]] <-
      transformations["Avg."]
    
  }
  
  # Exporting
  tab <- xtable(
    tab,
    caption = paste0(
      "\\label{tab:cor",time.,"}Correlation levels between group level predictors and CI in time ", time., ". ",
      "For each variable (row), the most significant value is highlighted. ",
      "The last column of the table indicates the number of groups droped from",
      "the analysis due to missigness."
    )
    )
  
  align(tab) <- c(
    c("l", "l"),
    sprintf("m{%.2f\\linewidth}<\\centering ", rep(.95/(ncol(tab)), ncol(tab)-1))
  )
  
  print(
    tab, include.rownames = FALSE, booktabs = TRUE,
    sanitize.text.function = function(i) i,
    file = sprintf("analysis/univariate_%i.tex", time.), scalebox = .8
    )
  
  # Now, we save the top associations. First, we need to sort the aggregations
  # according to the order in which they show up in the vars vector

  new_data <- vector("list", length(aggregations_to_keep))
  for (i in seq_along(aggregations_to_keep)) {
    
    # We only select varaibles as a function of whether one was significant at this
    # stage
    if (!length(aggregations_to_keep[[i]]))
      next
    
    # Computing aggregations (the top ones)
    new_data[[i]] <- aggregator(
      names(aggregations_to_keep)[i],
      aggregations_to_keep[[i]],
      paste0(" ", names(aggregations_to_keep[[i]])),
      nodal_data
      )
  }
  
  # Merging all the dataset
  univariate <- NULL
  for (i in new_data) {
    if (length(univariate) & length(i))
      univariate <- merge(univariate, i, by = c("group", "CIF_T1", "CIF_T2"))
    else if (length(i))
      univariate <- i
  }
  
  # Merging group size information
  univariate <- merge(univariate, group_size, by = "group")
  
  # Creating interaction with group size and rescaling
  vars_ii <- setdiff(colnames(univariate), c("group", "CIF_T1", "CIF_T2", "Size"))
  for (ii in vars_ii) {
    
    # # Interaction
    # univariate[[paste(ii,"x Size")]] <- univariate[[ii]] * univariate$Size
    
    # Rescaling both
    univariate[[ii]] <- univariate[[ii]]/
      (1e-15 + sd(univariate[[ii]], na.rm = TRUE))
    # univariate[[paste(ii,"x Size")]] <- univariate[[paste(ii,"x Size")]]/
    #   (1e-15 + sd(univariate[[paste(ii,"x Size")]], na.rm = TRUE))
    
  }
  
  # # Adding an intercept
  # univariate[["(Intercept)"]] <- 1.0
  
  saveRDS(univariate, sprintf("analysis/univariate_%d.rds", time.))
}

# stop()
# 
# # Visuals ----------------------------------------------------------------------
# 
# # Using base R
# tmp <- aggregator(vars, transformations[[1]], pf = "", dat. = nodal_data)
# 
# op <- par(mfrow = c(4, 4), mai = rep(.2, 4), oma = c(4,4,4,0))
# for (v in vars) {
#   plot(y = tmp[["CIF_T1"]], x = tmp[[v]])
#   abline(lm(tmp[["CIF_T1"]] ~ tmp[[v]]))
# }
# par(op)
# title(names(transformations)[1])
# 
# # With ggplot
# 
# plotdat <- NULL
# for (i in seq_along(transformations)) 
#   plotdat <- rbind(
#     plotdat,
#     cbind(
#       aggregator(vars, transformations[[i]], "", nodal_data),
#       Type = names(transformations)[i]
#       )
#   )
# 
# plotdat <- 
#   melt(plotdat, id.vars = c("Type", "CIF_T1", "CIF_T2"), measure.vars = vars)
# 
# 
# # Rscaling to get a better measurement (nicer on the plot)
# plotdat[
#   ,
#   value := (value - min(value, na.rm = TRUE))/diff(range(value, na.rm = TRUE)),
#   by = c("Type", "variable")
#   ]
# 
# plotdat$variable <- names(vars)[match(plotdat$variable, vars)]
# 
# # For the mean first
# for (i in seq_along(transformations)) {
#   subset(plotdat, Type == names(transformations)[i]) %>%
#     ggplot(aes(x = value, y = CIF_T1)) +
#     geom_point() +
#     ylim(c(-3, 3)) +
#     facet_wrap(variable ~ .) +
#     geom_smooth() +
#     labs(x = paste("Group", names(transformations)[i]), y = "Collective Intelligence")
#   
#   ggsave(
#     filename = sprintf(
#       "analysis/univariate_%s.png",
#       gsub("[._ ]+", "_", tolower(names(transformations)[i]))
#       )
#     )
# }
# 
# 
# # Most significant per factor ---------------------------------------------------
# 
# 
# # Now, we save the top associations
# new_data <- vector("list", length(aggregations_to_keep))
# for (i in seq_along(aggregations_to_keep)) {
#   
#   # We only select varaibles as a function of whether one was significant at this
#   # stage
#   if (!length(aggregations_to_keep[[i]]))
#     next
#   
#   # Computing aggregations (the top ones)
#   new_data[[i]] <- aggregator(
#     vars[i],
#     transformations[aggregations_to_keep[[i]][1]],
#     "",
#     nodal_data
#   )
#   
#   
# }
# 
# # Merging all the dataset
# plotdat <- NULL
# for (i in new_data) {
#   if (length(plotdat) & length(i))
#     plotdat <- merge(plotdat, i, by = c("group", "CIF_T1", "CIF_T2"))
#   else 
#     plotdat <- i
# }
# 
# # Rscaling to get a better measurement (nicer on the plot)
# plotdat <- 
#   melt(plotdat, id.vars = c("CIF_T1", "CIF_T2"), measure.vars = vars)
# plotdat[
#   ,
#   value := (value - min(value, na.rm = TRUE))/(diff(range(value, na.rm = TRUE)) + 1e-20),
#   by = c("variable")
#   ]
# 
# plotdat$variable <- names(vars)[match(plotdat$variable, vars)]
# 
# 
# ggplot(plotdat, aes(x = value, y = CIF_T1)) +
#   geom_point() +
#   ylim(c(-3, 3)) +
#   facet_wrap(variable ~ .) +
#   geom_smooth() +
#   labs(x = "Aggregated by most\nsignificant transformation", y = "Collective Intelligence")
# 
# ggsave("analysis/univariate_best.png")
