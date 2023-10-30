#' @title This adds a number to an entry in matrix
#'
#' @description More details...
#'
#' @param M A matrix
#' @param entry An entry created using function [e]
#' @param x A single nunber.
#' @examples
#' M <- matrix(1:4, 2, 2)
#' entry_add(M, e(2, 2), 3)
#'
#' @export
entry_add <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] +  x
  M
}

#' @rdname entry_add
#' @export
entry_subtract <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] -  x
  M
}


#' Entry multipled
#'
#' Multiply matrix.... bla bla
#'
#' @inheritParams entry_add
#'
#' @export
entry_multiply_by <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] *  x
  M
}

entry_divide_by <- function(M, entry, x) {
  M[entry[1], entry[2]] <- M[entry[1], entry[2]] /  x
  M
}

#' A helper function to specify entry
#'
#' @param i,j The row and column entry of the matrix.
#' @export
e <- function(i, j) {
  if(i < 0) stop("Row entry is not a positive integer.")
  if(j < 0) stop("Column entry is not a positive integer.")
  c(i, j)
}
