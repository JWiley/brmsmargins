#' @describeIn integratemvn Pure \code{R} implementation of \code{integratemvn}
integratemvnR <- function(X, k, sd, chol) {
  n <- length(sd)
  Z <- matrix(rnorm(k * n, mean = 0, sd = 1), nrow = k, ncol = n)
  if (n > 1) {
    Z <- Z %*% chol
  }
  for (i in seq_len(n)) {
    Z[, i] <- Z[, i] * sd[i]
  }
  X %*% t(Z)
}

#' @describeIn tab2mat Pure \code{R} implementation of \code{tab2mat}
tab2matR <- function(X) {
  X <- as.vector(X)
  dims <- sqrt(length(X))
  matrix(X, dims, dims, byrow = TRUE)
}

#' @describeIn integratere Pure \code{R} implementation of \code{integratere}
integratereR <- function(d, sd, L, k, yhat, backtrans) {
  M <- nrow(yhat)
  N <- ncol(yhat)
  J <- length(sd)
  yhat2 <- matrix(0, M, N)
  for (i in seq_len(M)) {
    Z <- vector("list", J)
    for (re in seq_len(J)) {
      cholmat <- tab2matR(L[[re]][i, ])
      dmat <- d[[re]]
      Z[[re]] <- integratemvnR(dmat, k, sd[[re]][i, ], cholmat)
    }
    Zall <- Z[[1]]
    if (J > 1) {
      for (re in 2:J) {
        Zall <- Zall + Z[[re]]
      }
    }
    for (nsamp in seq_len(k)) {
      Zall[, nsamp] <- Zall[, nsamp] + t(yhat[i, ])
    }
    if (backtrans == 0) {
      Zall <- 1 / (1 + exp(-Zall))
    } else if (backtrans == 1) {
      Zall <- exp(Zall)
    } else if (backtrans == 2) {
      Zall <- Zall^2
    }
    zm <- rowMeans(Zall)
    yhat2[i, ] <- t(zm)
  }
  return(yhat2)
}
