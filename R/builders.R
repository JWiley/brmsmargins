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
#' @return A character vector for all \code{.names} functions or a matrix
#'   for all \code{.build} functions.
#' @keywords internal
#' @name builders
NULL

#' @rdname builders
#' @importFrom data.table as.data.table
#' @examples
#' brmsmargins:::.namesL(1, 3)
#' tab2matR(matrix(brmsmargins:::.namesL(1, 3), 1))
.namesL <- function(block, number) {
  n <- expand.grid(Block = block,
                   Row = seq_len(number),
                   Col = seq_len(number))
  n <- as.data.table(n)
  n[, sprintf("L_%d[%d,%d]",
              Block, Row, Col)]
}

#' @rdname builders
.buildL <- function(data, block, number) {
  n <- .namesL(block, number)
  as.matrix(data[, n, drop = FALSE])
}

#' @rdname builders
.namesSD <- function(ranef, block) {
  n <- subset(ranef, id == block)
  n <- as.data.table(n)
  n[, sprintf("sd_%s__%s", group, coef)]
}

#' @rdname builders
.buildSD <- function(data, ranef, block) {
  n <- .namesSD(ranef, block)
  as.matrix(data[, n, drop = FALSE])
}

#' @rdname builders
#' @examples
#' brmsmargins:::.namesZ(1, 3)
.namesZ <- function(block, number) {
  n <- expand.grid(Block = block,
                   Number = seq_len(number))
  n <- as.data.table(n)
  n[, sprintf("Z_%d_%d", Block, Number)]
}

#' @rdname builders
.buildZ <- function(data, block, number) {
  n <- .namesZ(block, number)
  as.matrix(do.call(cbind, data[n]))
}
