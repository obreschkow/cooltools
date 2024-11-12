#' Read data from an HDF5 file
#'
#' @importFrom hdf5r H5File h5attributes
#' @importFrom bit64 as.integer64
#'
#' @description Reads data from an HDF5 file, including attributes, into a nested list that preserves the hierarchical structure of the HDF5 data.
#'
#' @param file Character string specifying the file name of the input HDF5 file.
#' @param subtree Optional hierarchical list, specifying groups and datasets to read from the HDF5 file. For example, to read group1-dataset3, group5-dataset7, group5-dataset8, and group7-subgroup3-dataset9, use \code{subtree = list(group1 = list(dataset3 = ""), group5 = list(dataset7 = "", dataset8 = ""), group7 = list(subgroup3 = list(dataset9 = "")))}.
#'   If \code{subtree} is NULL (default), the entire file hierarchy is read.
#' @param group.attr.as.data logical flag. If \code{TRUE}, group attributes are converted to datasets. This is useful, for example, for Gadget simulation outputs in HDF5 format, where the simulation parameters are stored as attributes in the `Header' group.
#'
#' @details This function, built on the \code{hdf5r} package, reads the contents of an HDF5 file into a nested list structure in R. Groups and datasets are recursively parsed, preserving the original hierarchy.
#'
#' @return Nested list representing the contents of the HDF5 file. Groups are represented as nested sublists, and datasets are represented by their data. If groups and datasets have attributes in the HDF5 data, these are added attributes to the corresponding sublists and data elements. Use the function \code{\link{userattributes}} to retrieve these custom arguments, or the base R function \code{\link{attributes}} to access all attributes, including inherent ones (e.g. `dim` for arrays).
#'
#' @seealso \code{\link{writehdf5}} providing a code example
#'
#' @export

readhdf5 <- function(file, subtree = NULL, group.attr.as.data = FALSE) {

  # Recursive helper function to read each element based on subtree
  read_data <- function(group, subtree_node = NULL) {
    result <- list()

    # Read attributes
    attributes <- hdf5r::h5attributes(group)
    has_attributes <- length(attributes) > 0

    # Check if it's a group or dataset
    if (inherits(group, "H5Group")) {
      # List the names of all sub-items in the group
      item_names <- names(group)

      # If a subtree is provided, filter item_names by subtree keys
      if (!is.null(subtree_node)) {
        item_names <- intersect(item_names, names(subtree_node))
      }

      # If the group has sub-items, recursively read each item
      for (name in item_names) {
        # Proceed only if the name is in the subtree (if subtree is defined)
        if (is.null(subtree_node) || !is.null(subtree_node[[name]])) {
          result[[name]] <- read_data(group[[name]], subtree_node[[name]])
        }
      }

      # Add group attributes if there are any
      if (group.attr.as.data) {
        result <- c(result, attributes)
      } else {
        for (attr_name in names(attributes)) {
          if (attr_name!='names') { # avoids wrong permutations of dataset names; the attribute names still exists, but is assigned automatically to lists
            attr(result, attr_name) <- attributes[[attr_name]]
          }
        }
      }
    } else if (inherits(group, "H5D")) {
      # It's a dataset, read data if in subtree or subtree is NULL
      data.is.int64 = group$key_info$type$get_class()=="H5T_INTEGER" & group$key_info$type$get_size() == 8
      if (data.is.int64) {
        data <- as.integer64(group$read()) # avoids an automatic conversion of 64bit integers to 32bit integers in some cases
      } else {
        data <- group$read()
      }
      result <- data
      # Add dataset attributes if there are any
      for (attr_name in names(attributes)) {
        attr(result, attr_name) <- attributes[[attr_name]]
      }
    } else {
      stop("Unsupported object type in HDF5 file.")
    }

    return(result)
  }


  # Open the HDF5 file in read-only mode
  h5file <- hdf5r::H5File$new(file, mode = "r")

  # Start reading from the root group, using the subtree structure if provided
  data <- list()
  root_subtree <- if (is.null(subtree)) names(h5file) else names(subtree)

  for (name in root_subtree) {
    # Proceed only if name exists in h5file and is in subtree
    if (name %in% names(h5file)) {
      data[[name]] <- read_data(h5file[[name]], subtree[[name]])
    }
  }

  # Close the file
  h5file$close_all()

  return(data)
}
