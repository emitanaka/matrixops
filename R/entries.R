#' @title Matrix operations
#'
#' @description
#' This adds a value to a particular entry of a matrix.
#'
#' @param M A matrix.
#' @param entry An entry, which is a vector of size.
#' @param x The value to add.
#' @name matrix-ops
#' @examples
#' add_entry(matrix(0, 2, 2), e(1, 1), 3)
#'
NULL

#' @rdname matrix-ops
#' @export
entry_add <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] + x
  M
}

#' @rdname matrix-ops
#' @export
entry_subtract <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] -  x
  M
}

#' @rdname matrix-ops
#' @export
entry_multiply_by <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] *  x
  M
}

#' @rdname matrix-ops
#' @export
entry_divide_by <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] /  x
  M
}

#' @export
e <- function(i, j) {
  if(i < 0) stop("Row entry is not a positive integer.")
  if(j < 0) stop("Column entry is not a positive integer.")
  c(i, j)
}
