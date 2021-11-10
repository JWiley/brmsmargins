

is.integer(n)
isSymmetric(x, tol = sqrt(.Machine$double.eps), check.attributes = FALSE)



object <- JWileymisc::readRDSfst("../mixedlogit.RDS")
object1 <- brm(y ~ 1 + x + (1 | ID),
               family = bernoulli(),
               data = model.frame(object),
               iter = 2000, warmup = 1000, seed = 1234,
               chains = 4, cores = 4, backend = "cmdstanr")
object2 <- JWileymisc::readRDSfst("../../Projects/CodeRepos/mega-pri/m_pri_isi.RDS")
object3 <- JWileymisc::readRDSfst("../mixedlogitnocor.RDS")



createCholesky <- function() {
}

## https://knausb.github.io/2017/08/header-files-in-rcpp/

## model is object
object

check.brmsfit(object)
check.brmsfit(object1)
check.brmsfit(object2)

is.random(object)
is.random(object1)
is.random(object2)

check.gaussian(object)
check.gaussian(object1)
check.gaussian(object2)

tmp <- as_draws_df(object2)

(r1 <- tmp[1, grepl("Cor_", names(tmp))])

chol(matrix(unlist(r1),2))

(l1 <- tmp[1, grepl("L_", names(tmp))])


## single level model
isTRUE(identical(
  nrow(object$ranef),
  0L))

## multi level model

## MVN work

x <- matrix(rnorm(100000 * 2, mean = 0, sd = 1), ncol = 2)
sd <- c(5, 1)
mu <- c(-3, 2)
sSigma <- matrix(c(1, .5, .5, 1), 2)

cor(x)
sqrt(diag(cov(x)))
colMeans(x)

xt <- x %*% chol(sSigma)
xt[, 1] <- xt[, 1] * sd[1] + mu[1]
xt[, 2] <- xt[, 2] * sd[2] + mu[2]

cor(xt)
sqrt(diag(cov(xt)))
colMeans(xt)


x <- matrix(1:12, ncol = 4, byrow = TRUE)

x

array(x, dim = c(2, 2, 3))
matrix(x[1, ], 2)

object2$ranef

Xm <- model.frame(object2)
Xm2 <- model.matrix(object2)
x <- make_standata(formula(object2), data = model.frame(object2))

d <- list(
  Block1 = cbind(x$Z_1_1),
  Block2 = cbind(x$Z_2_1, x$Z_2_2, x$Z_2_3))

sd <- list(
  Block1 = as.matrix(tmp[, "sd_idp__Intercept", drop = FALSE]),
  Block2 = as.matrix(tmp[, c("sd_idt__Intercept", "sd_idt__Time", "sd_idt__Time:Tx"),
                         drop = FALSE]))

L <- list(
  Block1 = matrix(1L, nrow = nrow(sd$Block1), ncol = 1),
  Block2 = as.matrix(tmp[,
                         sprintf("L_2[%s]",
        apply(expand.grid(Col = 1:3, Row = 1:3)[, c("Row", "Col")], 1, paste,
              collapse = ","))]))

yhat <- fitted(object2, newdata = model.frame(object2),
               summary = FALSE, re_formula = NA)

yhat2 <- matrix(NA_real_, nrow = nrow(yhat), ncol = ncol(yhat))


k <- 100L  ## points for integration
M <- 8000L ## posterior draws
N <- 7092L ## data for predictions / observations
J <- 2L    ## RE blocks

start <- proc.time()
for (m in seq_len(M)) {
  Z <- vector("list", J)
  as.vector(sd[[j]][m, ])

  for (j in seq_len(J)) {
    Z[[j]] <- integratemvn(
      X = d[[j]],
      k = k,
      sd = as.vector(sd[[j]][m, ]),
      chol = tab2mat(L[[j]][m, , drop = FALSE], 0))
  }

  Zall <- Z[[1]] + Z[[2]]

  for (i in seq_len(k)) {
    Zall[, i] <- Zall[, i] + yhat[m, ]
  }

  yhat2[m, ] <- rowMeans(exp(Zall))
}
stop <- proc.time()

stop - start
