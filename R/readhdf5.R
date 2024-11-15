#' Read data from an HDF5 file
#'
#' @importFrom hdf5r H5File h5attributes
#' @importFrom bit64 as.integer64
#'
#' @description Reads data from an HDF5 file, including attributes, into a nested list that preserves the hierarchical structure of the HDF5 data.
#'
#' @param file Character string specifying the file name of the input HDF5 file.
#' @param subtree A structure specifying the HDF5 groups and datasets to be read. Use an asterisk \code{"*"} (default) to read the entire file. To read only part of the file, provide a named list reflecting the hierarchy of groups, subgroups, and datasets. For (sub)groups, use nested lists containing the items to read, or use \code{'*'} to load everything in the group. An empty list \code{list()} reads only the attributes of a group. For datasets, use \code{NULL} to read only attributes, or any other content to read the full data.
#' @param group.attr.as.data Logical flag. If \code{TRUE}, group attributes are converted to datasets, which is useful for formats where parameters are stored as attributes (e.g., Gadget simulation outputs).
#' @param empty Logical flag. If \code{TRUE}, only names of groups and datasets are returned, with all data equal to NA. This is a fast way of reading the hierarchical structure.
#'
#' @details This function, based on the \code{hdf5r} package, recursively parses and reads HDF5 files into nested lists that preserve the original hierarchy. Attributes in groups and datasets are included in the output.
#'
#' @return Nested list representing the contents of the HDF5 file. Groups are nested sublists, datasets are represented by their data. Attributes of groups and datasets are attached as attributes to the corresponding sublists and data elements.
#'
#' @seealso \code{\link{writehdf5}} for examples.
#'
#' @export

readhdf5 <- function(file, subtree = "*", group.attr.as.data = FALSE, empty = FALSE) {

  # Recursive helper function to read each element in the HDF5 structure
  read_data <- function(item, subtree_node) {

    # Read attributes
    attributes <- hdf5r::h5attributes(item)

    if (inherits(item, "H5Group")) { # Item is a group

      item_names <- if (is.list(subtree_node)) intersect(names(item), names(subtree_node)) else names(item)

      # Recursively read sub-items in the group based on subtree_node
      result <- list()
      if (identical(subtree_node, "*") || is.list(subtree_node)) {
        for (name in item_names) {
          result[[name]] <- read_data(item[[name]], if (is.list(subtree_node)) subtree_node[[name]] else "*")
        }
      } else if (!is.null(subtree_node)) {
        stop("Argument 'subtree' must be '*' or a valid list structure.")
      }

      # Convert attributes to data if desired
      if (group.attr.as.data) {
        if (empty) {
          attr_names = names(attributes)
          for (attr_name in attr_names) {
            attribute = item$attr_open(attr_name)
            result[[attr_name]] = sprintf('dtype=%s, size=%s, dims=(%s)',attribute$get_type()$get_class(),attribute$get_space()$get_simple_extent_npoints(),paste0(attribute$get_space()$get_simple_extent_dims()$dims, collapse=','))
          }
        } else {
          result = c(result, attributes)
        }
        attributes = NULL # to name sure that attributes are not also attached as attributes
      }

    } else if (inherits(item, "H5D")) { # Item is a dataset

      if (empty) {
        result = sprintf('dtype=%s, size=%s, dims=(%s)',item$get_type()$get_class(),item$get_space()$get_simple_extent_npoints(),paste0(item$dims,collapse=','))
      } else {
        result = if (is.null(subtree_node)) NULL else item$read()
        if (!is.null(result) && item$key_info$type$get_class() == "H5T_INTEGER" && item$key_info$type$get_size() == 8) {
          result = as.integer64(result)
        }
      }

    } else {

      stop("Unsupported object type encountered in HDF5 file.")

    }

    # Attach attributes
    if (!empty) {
      for (attr_name in names(attributes)) {
        if (attr_name != "names") {
          attr(result, attr_name) <- attributes[[attr_name]]
        }
      }
    }

    return(result)
  }

  # Check file
  if (!file.exists(file)) stop('File does not exist: ',file)
  if (file.info(file)$isdir) stop('Provide a filename instead of director: ',file)
  if (file.access(file,4)!=0) stop('You do not have read permission for file: ',file)

  # Open the HDF5 file in read-only mode
  h5file <- hdf5r::H5File$new(file, mode = "r")
  on.exit(h5file$close_all())

  # Prepare the root-level subtree structure
  root_subtree <- if (identical(subtree, '*')) names(h5file) else if (is.list(subtree)) intersect(names(h5file), names(subtree)) else stop("Argument 'subtree' must be '*' or a list.")

  # Read the file starting from the root group
  out <- list()
  for (name in root_subtree) {
    if (name %in% names(h5file)) {
      out[[name]] = read_data(h5file[[name]], if (identical(subtree, '*')) "*" else subtree[[name]])
    }
  }

  return(out)
}
