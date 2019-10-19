library(ggplot2)
library(data.table)

as_asterisk <- function(x) {
  
  ifelse(x < .001, "$^{***}$",
         ifelse(x < .01, "$^{**}$",
                ifelse(x < .05, "$^*$", "")))
  
}

varnames <- VARNAMES

# We swap this b/c of how stringr::str_replace_all works
varnames <- structure(names(varnames), names = unname(varnames))

step1_1 <- readRDS("analysis/multivariate_step1_counts_time1.rds")
step1_2 <- readRDS("analysis/multivariate_step1_counts_time2.rds")
step2_1 <- readRDS("analysis/multivariate_step2_counts_time1.rds")
step2_2 <- readRDS("analysis/multivariate_step2_counts_time2.rds")
step3_1 <- readRDS("analysis/multivariate_step3_counts_time1.rds")
step3_2 <- readRDS("analysis/multivariate_step3_counts_time2.rds")
step4_1 <- readRDS("analysis/multivariate_step4_counts_time1.rds")
step4_2 <- readRDS("analysis/multivariate_step4_counts_time2.rds")

all_data <- mget(ls(pattern = "^step"))

# Groupong up and selecting the top performing models
all_data <- lapply(names(all_data), function(dat.) {
  dat. <- cbind(data.frame(Group = dat., stringsAsFactors = FALSE), all_data[[dat.]])
  dat.
})

all_data <- do.call(rbind, all_data)
all_data$nvars <- sapply(all_data$model, length) 
all_data$Time  <- paste("CI in Time", gsub(".+_", "", all_data$Group))

ggplot(tibble::as_tibble(all_data), aes(x = nvars + 1, y = LOO_rmse)) +
  geom_jitter(aes(color = factor(nsignificant, levels = 0:6)), height = 0) +
  scale_color_viridis_d(alpha = .7) +
  scale_x_continuous(breaks = 2:6) +
  facet_grid(cols = vars(Time)) +
  labs(
    color = "# of significant\npredictors",
    x     = "# predictors",
    y     = "CV-RMSE"
    )

ggsave("analysis/multivariate_step5.png", width=8, height = 6)

# Selecting the 20 best models -------------------------------------------------

#' This function takes whatever set of observations was passed to it and creates
#' a nice-looking latex table with the output from the model summarized.
make_regression_table <- function(dat., file., caption.) {
  vars <- c(unique(unlist(dat.$model)), "(Intercept)")
  
  n <- nrow(dat.)
  res <- matrix(nrow = length(vars), ncol = n, dimnames = list(vars, 1:n))
  for (i in seq_len(n)) {
    
    vars_i <- rownames(dat.$beta[[i]])
    
    res[vars_i, i] <- sprintf(
      "\\makecell{%.2f%s \\\\ (%.2f)}", dat.$beta[[i]], 
      as_asterisk(dat.$pval[[i]]),
      dat.$sd[[i]]
    )
  }
  res <- rbind(
    res,
    `CV-RMSE`  = sprintf("%.2f", dat.$rmse),
    `CV-R2adj` = sprintf("%.2f", dat.$LOO_r2adj)
  )
  
  # Correcting rownmaes
  rownames(res) <- stringr::str_replace_all(rownames(res), varnames)
  
  # Printing out the fancy tabnle 
  res <- xtable::xtable(res, caption = caption.)
  xtable::align(res) <- c(
    "m{.25\\linewidth}",
    rep(sprintf(" m{%.2f\\linewidth}<\\centering", .8/(ncol(res))), ncol(res))
  )
  
  print(res, file = file., booktabs = TRUE, scalebox=.75,
        sanitize.text.function = function(e) e)

}

# Preparing to select and rerun the models -------------------------------------

# Time 1
time1 <- subset(all_data, Time == "CI in Time 1")
time1 <- subset(time1, nsignificant >= (nvars))
time1 <- time1[order(time1$rmse, decreasing = FALSE),][1:10,]
saveRDS(time1, "analysis/multivariate_step5_time1.rds")

make_regression_table(
  time1,
  "analysis/multivariate_step5_time1.tex",
  caption. = "\\label{tab:top10_1}Top 10 models predicting CI in time 1. RMSE and R2adj reported from the leave-one-out cross-validation."
  )


# Time 2
time2 <- subset(all_data, Time == "CI in Time 2")
time2 <- subset(time2, nsignificant >= (nvars))
time2 <- time2[order(time2$rmse, decreasing = FALSE),][1:10,]
saveRDS(time2, "analysis/multivariate_step5_time2.rds")

make_regression_table(
  time2,
  "analysis/multivariate_step5_time2.tex",
  caption. = "\\label{tab:top10_2}Top 10 models predicting CI in time 2. RMSE and R2adj reported from the leave-one-out cross-validation."
  )

