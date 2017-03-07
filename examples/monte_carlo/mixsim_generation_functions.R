#these functions are slightly adapted (or largely just documented from the MixSim package
#tweaked to let me separate the simulation and noising processes.

simdata <- function (n, Pi, Mu, S, n.noise = 0, n.out = 0, alpha = 0.001, 
    max.out = 1e+05, int = NULL, lambda = NULL) 
{
  if (n < 1) 
    stop("Wrong sample size n...\n")
  if (sum((Pi <= 0) | (Pi >= 1)) != 0) 
    stop("Wrong vector of mixing proportions Pi...\n")
  if (n.noise < 0) 
    stop("Wrong value of n.noise...\n")
  if (n.out < 0) 
    stop("Wrong value of n.out...\n")
  if ((alpha >= 1) | (alpha <= 0)) 
    stop("Wrong value of alpha...\n")
  if (max.out < 1) 
    stop("Wrong value of max.out...\n")
  K <- dim(Mu)[1]
  p <- dim(Mu)[2]
  X <- NULL
  if (n >= K) {
    Nk <- rep(1, K) + drop(rmultinom(1, n - K, Pi))
  }
  else {
    stop("sample size (n) cannot be less than the number of clusters")
  }
  id <- NULL
  for (k in 1:K) {
    id <- c(id, rep(k, Nk[k]))
    X <- rbind(X, mvrnorm(n = Nk[k], mu = Mu[k, ], Sigma = S[, 
                , k]))
  }
  if (n.out != 0) {
    O <- getOutliers(n.out, Pi, Mu, S, alpha, max.out, int)
    if (O$fail == 1) {
      stop("Cannot generate outliers in ", max.out, " trials...\n")
    }
    else {
      X <- rbind(X, O$X.out)
      id <- c(id, rep(0, n.out))
    }
  }
  if (n.noise != 0) {
    if (is.null(int)) {
      L <- min(Mu)
      U <- max(Mu)
    }
    else {
      L <- int[1]
      U <- int[2]
    }
    X.noise <- matrix(runif(n.noise * (n + n.out), min = L, 
            max = U), ncol = n.noise)
    X <- cbind(X, X.noise)
  }
  if (!is.null(lambda)) {
    if (length(lambda) == p + n.noise) {
      for (j in 1:(p + n.noise)) {
        X[, j] <- (lambda[j] * X[, j] + 1)^(1/lambda[j]) - 
            1
        if (sum(is.nan(X[, j])) != 0) 
          warning("NaNs were produced during transformation\n")
      }
    }
    else {
      stop("The number of transformation coefficients lambda should be equal to n.dimensions + n.noise")
    }
  }
  return(list(X = X, id = id))
}


genOutliers <- function (n.out, Pi, Mu, S, alpha = 0.001, max.out = 1e+05, int = NULL) 
{
  fail <- 0
  K <- dim(Mu)[1]
  p <- dim(Mu)[2]

  #generate outliers that fall outside of the 95% CI in each dimension.
  
  crit.val <- qchisq(1 - alpha, df = p)
  if (is.null(int)) {
    L <- min(Mu)
    U <- max(Mu)
  }
  else {
    L <- int[1]
    U <- int[2]
  }
  X.out <- matrix(rep(NA, n.out * p), ncol = p)
  Sinv <- S
  for (k in 1:K) {
    Sinv[, , k] <- solve(S[, , k])
  }
  i <- 1
  s <- 1
  while (i <= n.out) {
    X.out[i, ] <- runif(p, min = L, max = U)
    Z <- sweep(Mu, 2, X.out[i, ])
    k <- 1
    while (k <= K) {
      if (t(Z[k, ]) %*% Sinv[, , k] %*% Z[k, ] < crit.val) {
        break
      }
      else {
        k <- k + 1
      }
    }
    if (k == K + 1) 
      i <- i + 1
    if (s == max.out) {
      fail <- 1
      break
    }
    s <- s + 1
  }
  return(list(X.out = X.out, fail = fail))
}
