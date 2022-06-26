to_html_table <- function(x, rownames = FALSE) {

  #' Convert dataframe/matrix to html table
  #'
  #' @param x A vector or list object.
  #' @param rownames Logical, whether to show rownames.
  #' @export
  #' @importFrom htmltools HTML
  #' @importFrom shiny renderTable
  #'
  #' @return Returns an HTML table or (if the object is not a matrix or
  #'   dataframe) the orignal object unmodified.
  #'
  #'

  if (is.matrix(x)) {

    if (is.null(colnames(x))) {

      colnames(x) <- paste0("col_", seq(ncol(x))) # use empty names?
    }

    x <- as.data.frame(x)
  }

  if (is.data.frame(x)) {

    htmltools::HTML(shiny::renderTable(x, rownames = rownames)())

  } else x
}

list_html <- function(lst) {
  #' Create an HTML list item
  #'
  #' Converts a list object to an HTML list item. If an object is a
  #' dataframe or matrix it is convert to an HTML table first.
  #'
  #' @param lst A list object.
  #' @importFrom shiny tags
  #' @export
  #' @return Returns an HTML string of a list item.
  #'
  #'

  is_html_tab <- is.data.frame(lst) | is.matrix(lst)

  lst <- to_html_table(lst)

  if (is_html_tab) {

    shiny::tags$li(lst)

  } else {

    shiny::tags$li(as.character(lst))
  }
}


list_html_r <- function(lst) {

  #' Create an un-ordered HTML list recursively
  #'
  #' Applies \code{\link{list_html}} to every entry of a list object.
  #'
  #' @param lst A list object.
  #' @importFrom purrr map2
  #' @importFrom shiny tags
  #' @return Returns a list of HTML list item strings.
  #'
  #'

  nms <- names(lst) # what if no names?

  purrr::map2(lst, nms, function(x, y) {

    if (is_leaf(x)) {

      shiny::tags$li(y, shiny::tags$ul(list_html(x)))

    } else {

      shiny::tags$li(y, shiny::tags$ul(list_html_r(x)))
    }
  })
}


list_to_html <- function(lst) {

  #' Convert list to HTML
  #'
  #' Converts a list to an un-ordered HTML list.
  #'
  #' @param lst A list object.
  #' @importFrom shiny tags
  #' @importFrom rlang is_bare_list
  #' @export
  #' @return Returns an un-ordered HTML list.
  #'
  #' @examples
  #' lst <- list(A = "some text", B = list(b1 = "b1 text", b2 = "b2 text),
  #'             C = list(data.frame(A = 1:10, b = letters[1:10])))
  #'
  #' list_to_html(lst)
  #'
  #'
  #'

  if (rlang::is_bare_list(lst)) {

    lst <- clean_list(lst)

    lst <- collapse_leaf_r(lst)

    lst <- list_html_r(lst)

    shiny::tags$ul(lst)

  } else {

    warning("object is not a list. Cannot convert to an HTML list.")
  }
}
