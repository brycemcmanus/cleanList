list_depth <- function(lst) {
#' Find list depth
#'
#' Iterates \code{\link{purrr::vec_depth}} over each object in a list.
#'
#' @param lst A list object.
#'
#' @importFrom purrr vec_depth
#' @return Returns a numeric vetor of list depths.

  vapply(lst, purrr::vec_depth, numeric(1))
}


list_length <- function(lst) {
#' Find list length
#'
#' Iterates \code{length} over each object in a list
#'
#' @param lst A list object.
#'
#' @return Returns a numeric vector of list lengths.

  vapply(lst, length, numeric(1))
}


is_leaf_table <- function(x) {
#' Detect potential tables in list node
#'
#' Determine whether an object in a list can ve converted into a dataframe.
#' @param x An object in a list.
#' @importFrom purrr vec_depth
#' @return Returns \code{TRUE} if leaf node can be converted to a dataframe,
#'   \code{FALSE} if not.

  dep <- purrr::vec_depth(x)
  len <- list_length(x)

  if (length(len) == 1) {

    len <- c(len, len)
  }

  if (dep == 2) {

  var(len) == 0
  }
}


is_leaf <- function(x) {
#'Is list object a leaf node
#'
#'Determines whether an object in a list is a leaf node.
#'
#'A leaf node is an entry in a list that does not contain other lists. It is an
#'endpoint in a list.
#'
#'@param x An object in a list.
#'@importFrom purrr vec_depth
#'@return Returns \code{TRUE} if list object is a leaf node, \code{FALSE} if not.

  dep <- purrr::vec_depth(x)

  dep == 1 | is.data.frame(x)
}


collaspe_leaf <- function(x) {
  #' Convert vector to a scalar
  #'
  #' Collapses a vector to a scalar.
  #'
  #' Converting a vector to a scalar is useful for converting a list in R to an
  #' html list.
  #'
  #' @param x A vector or object in a list.
  #' @return Returns a character vector of length 1.

  if (is_leaf(x)) {

    if (!is.matrix(x) & !is.data.frame(x) & length(x) > 1) {

      paste(x, collapse = ", ")

    } else x

  } else x
}



collaspe_leaf_r <- function(lst) {
#' Recursively apply \code{collapse_leaf}
#'
#' Recursively applies \code{collapse_leaf} to a list object.
#'
#' @param lst A list object.
#' @importFrom purrr map2
#' @return Returns a list.

  lst <- lapply(lst, collapse_leaf)

  leaf <- vapply(lst, is_leaf, logical(1))

  purrr::map2(lst, leaf, function(x, y) {

    if (y) x

    else collpase_leaf_r(x)
  })
}


lame_list <- function(lst) {
#' Detect an unnecessary neste list
#'
#' If a list object contains an unnamed list containing another list or a single
#' object it is a "lame list". For example: \code{list(list(1:10))}. The nested
#' list in this in this example is unnecessary since it contains a single vector,
#' therefore can be removed. Pruning lame list can help simplify a messy list.
#'
#' @param lst A list object
#' @seealso \code{\link{clean_list}}
#' @return Returns \code{TRUE} if list object is a lame list, \code{FALSE} if not.
#' @examples
#' lame_list(list(list(1:10)))
#'
#' lame_list(list(A = list(1:10)))
#'
#' lame_list(list(list(1:10, list(11:20))))

  len <- length(lst)
  nm <- names(lst)
  is_lst <- is.list(lst) #is_bare_list(lst)?
  len == 1 & is.null(nm) & is_lst
}

simplify_list <- function(lst) {
#' Remove unnecessary nested lists
#'
#' Removes unnecessary lists (or "lame lists") from a list. A lame list is a
#' unnamed list contain a single list object(see \code{\link{lame_list}}).
#'
#' @param lst A list object
#' @importFrom purrr vec_depth
#' @seealso \code{\link{simplify_list}} \code{\link{lame_list}}
  dep <- purrr::vec_depth(lst)

  if (dep > 1) {

    if (lame_list(lst)) unlist(lst, recursive = FALSE)

    else lapply(lst, simplify_list)

  } else lst
}


list_to_df <- function(lst) {
#' Convert leaf node to a dataframe
#'
#' Determines whether a leaf node contains an object that can be converted to a
#' dataframe.
#'
#' @param lst A list object.
#' @importFrom purrr vec_depth
#' @seealso \code{\link{is_leaf_table}}, \code{\link{simplify_list}}

  dep <- purrr::vec_depth(lst)

  if (dep > 1) {

    if (is_leaf_table(lst)) as.data.frame(lst)

    else lapply(lst, list_df)

  } else lst
}


clean_list <- function(lst, format = FALSE) {
#' Clean and simplify a list
#'
#' Cleans a list by pruning (removing) unnecessary nested lists and by simplifying
#' leaf nodes into dataframes when appropriate.
#'
#' @param lst A list object.
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom pander pander
#' @importFrom rlang is_bare_list
#' @return Returns a simplified list. It is possible that \code{lst} will be returned
#'   unmodified if no lame lists or leaf tables are found.
#' @example
#' clean_list(list(A = 1:10, B = 11:20))
#'
#' clean_list(list(A = list(X = 1:10, Y = letters[1:10])))
#'
#' clean_list(list(A = 1:10, B = "TEXT", C = c("FOO", "BAR")))
#'

  if (rlang::is_bare_list(lst)) {

    out <- simplify_list(lst)

    len <- list_length(out)
    dep <- list_depth(out)

    if (!is.na(var(len)) & !is.na(var(dep))) {

      if (var(len) == 0 & var(dep) == 0) { # list entries have same length and depth

        out <- dplyr::bind_rows(out)

      } else {

        out <- list_df(out)
      }

    } else {

      out <- list_df(out)
    }

  } else out <- lst

  # TODO: make pander optional package
  if (format) pander::pander(out)
  else out
}
