# Linear decay
#
# @param yvar the variable for which to compute time since event.
# @param d the decay cut-point.
# @author Zachary M. Jones
# @source Zachary M. Jones, 
# "Some Time-Series Functions for Panels with Missingness", 
# \url{http://www.zmjones.com/panel-ts.html}
# @keywords internal
decay <- function(yvar, d) {
  yvar[is.na(yvar)] <- 0
  run <- cumsum(yvar)
  tvar = seq_along(yvar)
  run = 0; sum = 0
  for(i in 1:length(tvar)) {
    if(yvar[i] == 1)
      run = run + 1
    
    if(run != 0) {
      event.idx <- which(yvar == 1)
      for(j in 1:length(event.idx)) {
        if(i == (d + event.idx[j])) {
          run = run - 1
        }}}
    sum[i] = run
  }
  return(sum)
}

# Time since event
#
# Time since event function adapted from the \code{\link[doBy]{doBy}} package by 
# Zachary M. Jones, and modified to understand the \code{\link{xtdata}} 
# attribute.
# 
# @param yvar the variable for which to compute time since event.
# @param tvar the time sequence.
# @author Zachary M. Jones
# @source Zachary M. Jones, 
# "Some Time-Series Functions for Panels with Missingness", 
# \url{http://www.zmjones.com/panel-ts.html}
# @keywords internal
panel.tse <- function(yvar, tvar = seq_along(yvar)) {
  if (!(is.numeric(yvar) | is.logical(yvar)))
    stop("yvar must be either numeric or logical")
  
  yvar[is.na(yvar)] <- 0
  event.idx <- which(yvar == 1)
  run <- cumsum(yvar)
  un <- unique(run)
  tlist <- list()
  for (i in 1:length(un)) {
    v <- un[[i]]
    y <- yvar[run == v]
    t <- tvar[run == v]
    t <- t - t[1]
    tlist[[i]] <- t
  }
  
  timeAfterEvent <- unlist(tlist)
  timeAfterEvent[run == 0] <- NA
  run[run == 0] <- NA
  
  return(timeAfterEvent)
}

# Quantize a variable
# Cut a variable to its quantiles, with error correction for the quantiles argument.
# 
# @param x variable
# @param q quantiles
# @keywords internal
quantize <- function(x, q, levels = FALSE) {
  stopifnot(q > 0 & length(x) > 0)
  if(q >= length(unique(x))) {
    q = length(unique(x)) - 1
    warning("only ", q + 1, " values exist in the data")
  }
  y = cut(x,
          quantile(x, 
                   probs = seq(0, 1, by = 1/q), 
                   na.rm = TRUE),
          include.lowest = TRUE)
  if(levels)
    levels(y) = paste(tapply(x, y, min), tapply(x, y, max), sep = "-")
  return(y)
}

# Standardize a variable
# Standardize a variable to (0,1).
# 
# @param x variable
# @keywords internal
std01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Quietly try to require a package
# Quietly require a package, returning an error message if that package is not installed.
# Code snippet lifted from \code{\link[ggplot2]{ggplot2}}.
# 
# @param name of package
# @author Hadley Wickham
# @source \url{https://github.com/hadley/ggplot2/blob/master/R/utilities.r}
# @keywords internal
try_require <- function(package) {
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]
  
  if (length(missing) > 0) 
    stop(paste(package, collapse=", "), " package required for this functionality.  Please install and try again.", call. = FALSE)
}
