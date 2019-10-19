library(data.table)
library(magrittr)
library(xtable)

# Loading programs
source("analysis/fitters.R")

# Loading data -----------------------------------------------------------------

# Loading the aggregated data in 
univariate_1 <- readRDS("analysis/univariate_1.rds")
univariate_2 <- readRDS("analysis/univariate_2.rds")

# Applying transformations -----------------------------------------------------

vars1 <- setdiff(colnames(univariate_1), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))

vars2 <- setdiff(colnames(univariate_2), c("CIF_T1", "CIF_T2", "PID", "group", "Size"))

# Program for general model fitting (all combinations)
vars1 <- c(vars1, "Size")
models1 <- combn(vars1, 2, simplify = FALSE)

vars2 <- c(vars2, "Size")
models2 <- combn(vars2, 2, simplify = FALSE)

# Analysis for CIF_T1 ----------------------------------------------------------

varnames <- structure(names(VARNAMES), names=unname(VARNAMES))

summarizer <- function(x, dat) {
  cbind(
    Name   = x,
    `Min.` = sprintf("%.2f", min(dat[[x]], na.rm = TRUE)),
    `Max.` = sprintf("%.2f", max(dat[[x]], na.rm = TRUE)),
    `Avg.` = sprintf("%.2f", mean(dat[[x]], na.rm = TRUE)),
    # `Sd.`  = sprintf("%.2f", sd(dat[[x]], na.rm = TRUE)),
    `Miss.` = sprintf("%d", sum(is.na(dat[[x]])))
  )
  
}



tab1 <- vars1 %>%
  lapply( summarizer, dat = as.data.frame(univariate_1)) %>%
  do.call(rbind, .)

tab1[,1] <- stringr::str_replace_all(tab1[,1], varnames)
tab1 <- tab1[order(tab1[,1]),]


tab1 <- xtable(tab1, align = c("l", "l","c","c","c","c"))
caption(tab1) <- paste(
  "\\label{tab:descstats1}Descriptive statistics of initial set of candidate",
  "variables for modeling Collective Intelligence in time 1. The standard error",
  "was not included since all variables are scaled such that they have a santard",
  "error equal to one."
  )

print(tab1, include.rownames = FALSE, booktabs = TRUE,
      file = "analysis/multivariate_step0-time1.tex", scalebox = .7)


tab2 <- vars2 %>%
  lapply( summarizer, dat = as.data.frame(univariate_2)) %>%
  do.call(rbind, .)

tab2[,1] <- stringr::str_replace_all(tab2[,1], varnames)
tab2 <- tab2[order(tab2[,1]),]


tab2 <- xtable(tab2, align = c("l","l","c","c","c","c"))
caption(tab2) <- paste(
  "\\label{tab:descstats2}Descriptive statistics of initial set of candidate",
  "variables for modeling Collective Intelligence in time 2. The standard error",
  "was not included since all variables are scaled such that they have a santard",
  "error equal to one."
)

print(tab2, include.rownames = FALSE, booktabs = TRUE,
      file = "analysis/multivariate_step0-time2.tex", scalebox = .7)
