bootstrap_t_interval <- function(S, a, confidence, B, D) {
  ## Inputs: S = an n element array containing the variate values in the
  ## sample a = a scalar-valued function that calculates the attribute a()
  ## of interest confidence = a value in (0,1) indicating the confidence
  ## level B = a numeric value representing the outer bootstrap count of
  ## replicates (used to calculate the lower and upper limits) D = a
  ## numeric value representing the inner bootstrap count of replicates
  ## (used to estimate the standard deviation of the sample attribute for
  ## each (outer) bootstrap sample)
  Pstar <- S
  aPstar <- a(Pstar)
  sampleSize <- length(S)
  ## get (outer) bootstrap values
  bVals <- sapply(1:B, FUN = function(b) {
    Sstar <- sample(Pstar, sampleSize, replace = TRUE)
    aSstar <- a(Sstar)
    ## get (inner) bootstrap values to estimate the SD
    Pstarstar <- Sstar
    SD_aSstar <- sd(sapply(1:D, FUN = function(d) {
      Sstarstar <- sample(Pstarstar, sampleSize, replace = TRUE)
      ## return the attribute value
      a(Sstarstar)
    }))
    z <- (aSstar - aPstar)/SD_aSstar
    ## Return the two values
    c(aSstar = aSstar, z = z)
  })
  SDhat <- sd(bVals["aSstar", ])
  zVals <- bVals["z", ]
  ## Now use these zVals to get the lower and upper c values.
  cValues <- quantile(zVals, probs = c((1 - confidence)/2, (confidence +
                                                              1)/2), na.rm = TRUE)
  cLower <- min(cValues)
  cUpper <- max(cValues)
  interval <- c(lower = aPstar - cUpper * SDhat, middle = aPstar, upper = aPstar -
                  cLower * SDhat)
  return(interval)
}