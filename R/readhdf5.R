#' Read data from an HDF5 file
#'
#' @importFrom hdf5r H5File h5attributes
#' @importFrom bit64 as.integer64
#'
#' @description Recursively reads data from an HDF5 file, preserving the structure of groups, datasets, and attributes. The function supports groups and datasets containing only attributes, treating these attributes as the main data for those objects (e.g. as used in the Header group of HDF5 outputs of the Gadget simulation code).
#'
#' @param file A character string specifying the path to the input HDF5 file.
#'
#' @details This function reads the contents of an HDF5 file into a nested list structure in R. Groups and datasets are recursively parsed, preserving the original hierarchy.
#' If a group or dataset contains only attributes and no other data, these attributes are returned as the main data for that object. Attributes for objects with actual data are ignored, except for groups, where they are stored under the `_attributes` key.
#'
#' @return A nested list representing the contents of the HDF5 file. Groups are represented as lists, datasets are represented by their data, and attributes are included either as the main data (if no other data is present) or under the `_attributes` key for groups.
#'
#' @seealso \code{\link[hdf5r]{H5File}}, \code{\link{writehdf5}}
#'
#' @examples
#'
#' \dontrun{
#' # make test data
#' input = list(
#'   group1 = list(
#'     dataset1 = matrix(1:10, nrow = 2),
#'     dataset2 = c("A", "B", "C")
#'   ),
#'   group2 = list(
#'     dataset3 = c(3.14, 2.71),
#'     subgroup1 = list(
#'       dataset3 = array(runif(8), dim = c(2, 2, 2))
#'     )
#'   ),
#'   group3 = list(id64 = bit64::as.integer64(123487918235335756), id32 = 5756)
#' )
#'
#' # write list to HDF5 file
#' writehdf5(example_data, "example_data.h5")
#'
#' # read HDF5 into a new list
#' output = readhdf5(filename)
#'
#' # Test if input and output lists are identical
#' print(all.equal(input,output))
#' }
#'
#' @export

readhdf5 <- function(file) {
  # Open the HDF5 file in read-only mode
  h5file <- hdf5r::H5File$new(file, mode = "r")

  # Recursive helper function to read each element
  read_data <- function(group) {
    result <- list()

    # Read attributes
    attributes <- hdf5r::h5attributes(group)
    has_attributes <- length(attributes) > 0

    # Check if it's a group or dataset
    if (inherits(group, "H5Group")) {
      # List the names of all sub-items in the group
      item_names <- names(group)

      if (length(item_names) == 0 && has_attributes) {
        # If the group has no sub-items but has attributes, treat attributes as data
        result <- attributes
      } else {
        # If the group has sub-items, recursively read each item
        for (name in item_names) {
          result[[name]] <- read_data(group[[name]])
        }
        # Include group attributes if there are any
        if (has_attributes) {
          result[["_attributes"]] <- attributes
        }
      }

    } else if (inherits(group, "H5D")) {
      # It's a dataset
      data <- group$read()

      if (length(data) == 0 && has_attributes) {
        # If the dataset is empty and only contains attributes, treat attributes as data
        result <- attributes
      } else {
        # Otherwise, store the actual data and ignore attributes
        result <- data
      }
    } else {
      stop("Unsupported object type in HDF5 file.")
    }

    return(result)
  }

  # Start reading from the root group contents
  data <- list()
  for (name in names(h5file)) {
    data[[name]] <- read_data(h5file[[name]])
  }

  # Close the file
  h5file$close()

  return(data)
}
