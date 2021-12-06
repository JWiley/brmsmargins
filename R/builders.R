#' @title Internal functions build the variable names or data objects for estimation
#'
#' @description
#' These are a set of internal utility functions.
#' They are not intended for general use.
#'
#' @details
#' \itemize{
#'   \item{\code{.namesL}}{Generate names of an L matrix from \code{brms}. Create the variable names for the Cholesky decomposition of the random effects correlation matrix in \code{brms}. Note that \code{brms} returns the lower triangular matrix and we want the upper triangular matrix, so the names are transposed. The results can then be passed to the \code{tab2mat} function to convert the row vector into a matrix.}
#'   \item{\code{.buildL}}{Returns the L matrix object. Rows are posterior draws.}
#'   \item{\code{.namesSD}}{Create the names of random effect standard deviation estimates.}
#'   \item{\code{.buildSD}}{Return matrix of random effect standard deviation estimates. Rows are posterior draws.}
#'   \item{\code{.namesZ}}{Create the names of random effects data for predictions.}
#'   \item{\code{.buildZ}}{Return matrix of data for random effect predictions.}
#' }
#'
#' @param data A data object. For example the result of [make_standata()]
#'   for [.buildZ()], which is a list,
#'   or a dataset of the posterior draws such as from [as_draws_df()]
#'   for [.buildL()] and [.buildSD()].
#' @param ranef A data set with information about the model object random effects.
#'   Only used for \code{.namesSD} and \code{.buildSD}.
#' @param block Which random effect block to use. An integer.
#' @param number The number of elements in that random effect block. An integer.
#' @param dpar Which dpar to use. Does not apply to the L matrix.
#' @return A character vector for all \code{.names} functions or a matrix
#'   for all \code{.build} functions.
#' @keywords internal
#' @name builders
NULL

## make Rcmd check happy
utils::globalVariables(c("Block", "Row", "Col"))

#' @rdname builders
#' @importFrom data.table as.data.table
#' @examples
#' brmsmargins:::.namesL(1, 3)
#' brmsmargins:::tab2matR(matrix(brmsmargins:::.namesL(1, 3), 1))
.namesL <- function(block, number) {
  n <- expand.grid(Block = block,
                   Row = seq_len(number),
                   Col = seq_len(number))
  n <- as.data.table(n)
  n[, sprintf("L_%d[%d,%d]",
              Block, Row, Col)]
}

## make Rcmd check happy
utils::globalVariables(c("..n"))

#' @rdname builders
.buildL <- function(data, block, number, dpar) {
  stopifnot(is.data.table(data))
  n <- .namesL(block, number)
  if (isTRUE(number == 1)) {
    out <- matrix(1, nrow = nrow(data), ncol = 1)
    colnames(out) <- n
  } else {
    out <- as.matrix(data[, ..n])
  }
  return(out)
}

## make Rcmd check happy
utils::globalVariables(c("group", "coef", "id"))

#' @rdname builders
.namesSD <- function(ranef, block, dpar) {
  stopifnot(is.data.table(ranef))
  n <- ranef[id == block]
  if (isTRUE(is.null(dpar)) || isFALSE(nzchar(dpar))) {
    n[, sprintf("sd_%s__%s", group, coef)]
  } else if (isTRUE(nzchar(dpar))) {
    n[, sprintf("sd_%s__%s_%s", group, dpar, coef)]
  }
}

#' @rdname builders
.buildSD <- function(data, ranef, block, dpar) {
  stopifnot(is.data.table(data))
  n <- .namesSD(ranef, block, dpar)
  as.matrix(data[, ..n])
}

## make Rcmd check happy
utils::globalVariables(c("Number"))

#' @rdname builders
#' @examples
#' brmsmargins:::.namesZ(1, 3, NULL)
.namesZ <- function(block, number, dpar) {
  n <- expand.grid(Block = block,
                   Number = seq_len(number))
  n <- as.data.table(n)

  if (isTRUE(is.null(dpar)) || isFALSE(nzchar(dpar))) {
    n[, sprintf("Z_%d_%d", Block, Number)]
  } else if (isTRUE(nzchar(dpar))) {
    n[, sprintf("Z_%d_%s_%d", Block, dpar, Number)]
  }
}

#' @rdname builders
.buildZ <- function(data, block, number, dpar) {
  n <- .namesZ(block, number, dpar)
  as.matrix(do.call(cbind, data[n]))
}
