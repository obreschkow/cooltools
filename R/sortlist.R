#' Sort List Entries Alphabetically
#'
#' @description This function sorts the entries of a structured list in alphabetical order, including all nested sublists
#' recursively, making it easier to navigate nested list structures and compare different lists. Unnammed items are listed last.
#' Attributes of the original list are preserved.
#'
#' @param lst list to be sorted.
#' @param recursive logical flag indicating whether to apply sorting recursively to all sublists.
#'   Defaults to \code{TRUE}. If \code{FALSE}, only the top-level entries in the list will be sorted.
#' @param convert.data.frames logical flag. If \code{TRUE}, data frames are converted to lists.
#' @return Structured list with alphabetically sorted entries. If \code{recursive = TRUE}, all sublists
#'   are also sorted alphabetically. If the list contains unnamed elements, they are left unsorted.
#' @examples
#' # Create a nested list
#' my_list = list(
#' b = list(d = 4, a = 2),
#' empty = list(),
#' 'unnamed',
#' a = 1,
#' f = list(b = 3, a = list(d = seq(5), 'unnamed', c = 2))
#' )
#'
#' # Sort the list recursively
#' sorted_list_recursive = sortlist(my_list)
#' print(sorted_list_recursive)
#'
#' @export
sortlist <- function(lst, recursive=TRUE, convert.data.frames=FALSE) {
  if (length(lst) > 0) {
    # Check for names in the list, assign empty names if NULL
    lst_names <- names(lst)
    if (is.null(lst_names)) lst_names <- rep("", length(lst))

    # Separate named and unnamed elements
    named_elements <- lst[lst_names != ""]
    unnamed_elements <- lst[lst_names == ""]

    # Sort named elements by name if there are any named elements
    if (length(named_elements) > 0) {
      sorted_named_elements <- named_elements[order(names(named_elements))]
    } else {
      sorted_named_elements <- named_elements
    }

    # Apply sorting recursively to each element in the list
    if (recursive) {
      sorted_named_elements <- lapply(sorted_named_elements, function(x) {
        if (is.list(x)) sortlist(x, recursive=TRUE, convert.data.frames=convert.data.frames) else x
      })
      unnamed_elements <- lapply(unnamed_elements, function(x) {
        if (is.list(x)) sortlist(x, recursive=TRUE, convert.data.frames=convert.data.frames) else x
      })
    }

    # Combine named and unnamed elements
    output <- c(sorted_named_elements, unnamed_elements)

  } else {
    output <- list()
  }

  # Copy custom attributes of list itself
  user_attr <- names(attributes(lst))
  for (attr_name in user_attr) {
    if (attr_name!='names') { # avoids wrong permutations of dataset names; the attribute names still exists, but is assigned automatically to lists
      attr(output, attr_name) <- attributes(lst)[[attr_name]]
    }
  }

  if (convert.data.frames && is.data.frame(output)) output <- as.list(output)

  return(output)
}


