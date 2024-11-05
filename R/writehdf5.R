#' Write data to an HDF5 file
#'
#' @importFrom hdf5r H5File h5types
#' @importFrom bit64 is.integer64 as.integer64
#'
#' @description Recursively writes data to an HDF5 file, creating groups and datasets as needed. The routine supports writing lists, 64-bit integers, character data, and numeric arrays.
#'
#' @param data A list containing the data to be written. Nested lists are interpreted as groups within the HDF5 file. If an element is `NULL`, it creates an empty dataset that can hold only attributes.
#' @param file A character string specifying the path to the output HDF5 file.
#' @param level An integer specifying the compression level for datasets, typically between 0 (no compression) and 9 (maximum compression). Not all dataset types support compression.
#' @param overwrite A logical value indicating whether to overwrite an existing file.
#'
#' @details The function creates the necessary subdirectories for the output file if they do not exist.
#' It uses 64-bit integer format (\code{H5T_NATIVE_INT64}) for \code{integer64} data and automatically detects types for character and numeric data.
#' Attributes can be added to groups or datasets that contain no other data, allowing for flexible metadata storage.
#'
#' @return This function does not return any value. It writes data directly to the specified HDF5 file.
#'
#' @seealso \code{\link[hdf5r]{H5File}}, \code{\link{readhdf5}}
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

writehdf5 = function(data, file, level = 6, overwrite = TRUE) {

  # Create or overwrite the HDF5 file
  if (file.exists(file)) {
    if (overwrite) {
      file.remove(file)
    } else {
      stop(sprintf('File %s already exists',file))
    }
  } else {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  }

  h5file = hdf5r::H5File$new(file, mode = "w")

  write_data = function(group, data, name = NULL) {
    if (is.list(data)) {
      # Handle lists (groups) recursively
      subgroup = if (!is.null(name)) group$create_group(name) else group
      for (subname in names(data)) {
        write_data(subgroup, data[[subname]], subname)
      }
    } else if (is.null(data)) {
      # Create an empty dataset for NULL values with specified dtype
      group$create_dataset(name, dims = c(0), dtype = hdf5r::h5types$H5T_NATIVE_DOUBLE)
    } else {
      # Write atomic data types directly within the group
      if (bit64::is.integer64(data)) {
        group[[name]] = bit64::as.integer64(data)
      } else if (is.integer(data)) {
        group[[name]] = data
      } else if (is.double(data)) {
        group[[name]] = data
      } else if (is.character(data)) {
        group[[name]] = data
      } else if (is.logical(data)) {
        group[[name]] = as.integer(data)
      } else {
        stop("Unsupported data type")
      }
    }
  }

  write_data(h5file, data)
  h5file$close()
}
