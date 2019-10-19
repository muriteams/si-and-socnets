

fastlm <- function(y, x, check_nas = TRUE) {
  
  # # Adding the intercept
  # x <- cbind("(Intercept)" = 1, x)
  
  # Checking for NAs
  if (check_nas) {
    idx <- which(complete.cases(y, x))
    
    x <- cbind(as.matrix(x)[idx, ])
    y <- y[idx]
    n <- length(idx)
    k <- ncol(x)
  } else {
    n <- nrow(x)
    k <- ncol(x)
  }
  
  # Computing the OLS estimates and returning the statistics
  xtx  <- crossprod(x)
  beta <- solve(xtx) %*% crossprod(x, y)
  y_pred <- x %*% beta
  err  <- (y - y_pred)^2
  rmse <- sqrt(mean(err))
  
  vcov <- sum(err)/(n - k) * solve(xtx)
  y_mean <- mean(y)
  list(
    beta  = beta,
    rmse  = rmse,
    vcov  = vcov,
    pval  = (1 - pt(abs(beta)/sqrt(diag(vcov)), n - k))*2,
    n     = n,
    r2adj = 1 - (n - 1)/(n - k)*(1 - sum((y_pred - y_mean)^2)/sum((y - y_mean)^2))
  )
}


# Leave-one-out
LOO <- function(y, x, add_intercept = TRUE) {
  
  if (add_intercept)
    x <- cbind(`(Intercept)` = 1, as.matrix(x))
  else
    x <- as.matrix(x)
  
  # Trying estimates first
  estimates <- tryCatch(fastlm(y, x), error = function(e) e)
  if (inherits(estimates, "error"))
    return(estimates)
  
  pred <- vector("numeric", length(y))
  for (i in seq_along(y)) {
    
    # If any of the ith is missing, then next
    if (any(is.na(c(x[i,], y[i])))) {
      pred[i] <- NA_real_
      next
    }
    
    tmp <- tryCatch(fastlm(y[-i], x[-i,,drop=FALSE]), error = function(e) e)
    
    # If is error, we cannot do anything
    if (inherits(tmp, "error"))
      pred[i] <- NA_real_
    else
      pred[i] <- x[i,,drop=TRUE] %*% tmp$beta 
    
    
  }
  
  idx    <- which(!is.na(pred))
  n      <- length(idx)
  k      <- ncol(x)
  y_mean <- mean(y[idx])
  
  c(
    estimates,
    list(
      LOO_rmse  = sqrt(mean((y - pred)^2, na.rm = TRUE)),
      LOO_r2adj = 1 - (n - 1)/(n - k)*(1 - sum((pred[idx] - y_mean)^2)/sum((y[idx] - y_mean)^2))
    )
  )
  
}

# sol <- LOO(dat$CIF_T1, dat[,models1[3,]])

library(parallel)

analyze_models <- function(depvar, dat., models., mc.cores = 4L) {
  
  # Applying the Leave one out cross validation
  # Notice that LOO will add the intercept by default!
  fit <- mclapply(seq_len(length(models.)), function(i) {
    tryCatch(LOO(dat.[[depvar]], dat.[,models.[[i]], drop = FALSE]), error = function(e) e)
  }, mc.cores = mc.cores)
  
  # Checking how many went OK (most failures are due to colieanirity)
  are_ok <- !sapply(fit, inherits, what = "error")
  
  # Creating a data.frame with it
  fit <- mclapply(fit[are_ok], function(f) {
    ans <- with(f, data.frame(
      rmse = rmse, LOO_rmse = LOO_rmse, r2adj = r2adj, LOO_r2adj = LOO_r2adj, n = n
    ))
    
    # Vectors
    ans$beta <- list(f$beta)
    ans$pval <- list(f$pval)
    ans$sd   <- list(sqrt(diag(f$vcov)))
    
    ans
  }, mc.cores = mc.cores)
  
  fit <- do.call(rbind, fit)
    
  # Excluding the intercept from the model (because we added it during the LOO)
  # step!
  fit$model        <- mclapply(fit$pval, function(f.) {
    setdiff(rownames(f.), "(Intercept)")
    }, mc.cores = mc.cores)
  
  fit$nsignificant <- unlist(mclapply(fit$pval, function(p) sum(p < .05), mc.cores = mc.cores))
  fit$significant  <- mclapply(fit$pval, function(p) rownames(p)[which(p < .05)], mc.cores =mc.cores)
  
  fit
  
}

VARNAMES <- c(
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

#' This function takes a list with the set of variables included in the model
#' and a nother list with the set of variables that showed up to be significant
#' in that model and creates a table reporting the number and proportion of time
#' that the variable showed up to be significant.
#' # We swap this b/c of how stringr::str_replace_all works
tabulate_counts <- function(model., significant., file., caption. = NULL, n = 10L) {

  varnames <- VARNAMES
  varnames <- structure(names(varnames), names = unname(varnames))
    
  tab <- as.data.frame(
    table(Variable = unlist(significant.)),
    responseName = "Count"
  )
  
  tab <- merge(x = tab, y = as.data.frame(
    table(Variable = unlist(model.)),
    responseName = "Total"
  ))
  
  tab$Proportion <- with(tab, Count/Total)
  
  tab <- tab[order(-tab$Proportion),]
  
  if (n > 0)
    tab <- tab[1L:n,,drop=FALSE]
  
  # Replacing names
  tab$Variable <- stringr::str_replace_all(tab$Variable, varnames)
  
  print(
    xtable::xtable(tab, caption=caption.),
    file = file.,
    include.rownames = FALSE,
    tabular.environment = "longtable",
    floating = FALSE,
    booktabs = TRUE
  )
  
}


grow_a_model <- function(l., v., size = NULL, mc.cores = 4L) {
  
  newmodels <- expand.grid(l., v., stringsAsFactors = FALSE)
  newmodels <- mcMap(function(a,b) {
    unique(sort(c(a,b)))
    }, a = newmodels[,1], b = newmodels[,2], mc.cores = mc.cores)
  
  if (!is.null(size))
    newmodels <- newmodels[sapply(newmodels, length) == size]
  
  unique(newmodels)
  
}
