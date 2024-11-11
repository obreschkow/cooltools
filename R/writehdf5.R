#' Write structured list to an HDF5 file
#'
#' @importFrom hdf5r H5File h5types
#' @importFrom bit64 as.integer64
#'
#' @description Writes a named list, including its hierarchy of nested sublists, to an HDF5 file with HDF5 groups, subgroups and datasets preserving the hierarchical structure.
#' The routine supports standard HDF5 data types, including double-precision floating point numbers (64 bit), integers (32 bit), characters (8 bit), and booleans, as well as vectors and arrays of these types. It also supports
#' 64-bit integers (\code{H5T_NATIVE_INT64}), available in R via the \code{bit64} package. Custom attributes of sublists and data, assigned in R via \code{\link{attr}}),
#' are automatically transcribed to group and dataset attributes in the HDF5 file. Such attributes can also be provided for empty groups and datasets.
#'
#' @param obj List containing the data to be written. Nested sublists are interpreted as sub-groups within the HDF5 file. If a sublist is empty or if an element inside a list is `NULL`, it creates an empty group/dataset that can hold only attributes.
#' @param file Character string specifying the file name of the output HDF5 file.
#' @param inherent.attributes Logical flag indicating whether to include inherent attributes that some R-objects possess, such as `dim` for matrices.
#' @param level Integer specifying the compression level for datasets, typically between 0 (no compression) and 9 (maximum compression). Not all dataset types support compression.
#' @param overwrite Logical value indicating whether to overwrite an existing file.
#'
#' @details The function relies on the \code{hdf5r} package and on the \code{bit64} package.\cr\cr
#' The nested list \code{obj} should only contain data types available to HDF5. The only exception are data frames, which, if included, are automatically converted to lists. Normally, the list \code{obj} and its nested sublists, should all be named lists, i.e. \code{list(a=1, b=2)} rather than \code{list(1,2)}. Unnamed elements are automatically assigned a name `unnamed_#` in the HDF5 file.\cr\cr
#' Some data types in R have inherent attributes, such as `names` for data frames and `dim` for arrays. By default, these inherent attributes are not written to the HDF5 file. They are, however, automatically regenerated when the HDF5 file is loaded back via \code{\link{readhdf5}}. The argument \code{inherent.attributes} can be used to force writing all attributes, including the inherent ones, to the HDF5 file.\cr\cr
#' If a structured R list is saved with \code{writehdf5} and then reloaded using \code{\link{readhdf5}}, the recovered list is identical to the input list up to the ordering of list elements, which is alphabetic by default in HDF5. The only other difference between the input and recovered data occurs when the input data contain data frames, as these are automatically converted do lists. A work around is to set \code{inherent.attributes=TRUE} when writing the HDF5 file. In this case, the reloaded HDF5 data becomes a data frame again.
#'
#' @return None
#'
#' @seealso \code{\link{readhdf5}}
#'
#' @examples
#'
#' # Create example data
#' input = list(
#'   people = data.frame(
#'     ID = as.integer(1:3),
#'     Name = c("Alice", "Bob", "Charlie"),
#'     Salary = c(2341.2, 3534.2, 4541.9),
#'     Employed = c(TRUE, TRUE, FALSE)
#'   ),
#'   empty = list(),
#'   header = list(),
#'   number = 4,
#'   groupa = list(
#'     header = 'test header',
#'     subgroup = list(
#'       dataset1 = c("A", "B", "C", "xxx"),
#'       dataset3 = array(runif(30), dim = c(2, 5, 3)),
#'       dataset2 = matrix(as.integer(1:10), nrow = 2)
#'     ),
#'     dataset4 = c(pi)
#'   ),
#'   groupb = list(int32 = as.integer(123),
#'                 int64 = bit64::as.integer64(123),
#'                 vector = bit64::as.integer64(seq(3)+12345678912345))
#' )
#'
#' # Add attributes to some datasets
#' attr(input$groupa$subgroup$dataset3,'Type') = '3D array'
#' attr(input$groupb$vector,'Comment') = 'Large integer'
#' attr(input$groupb$vector,'Package') = 'bit64'
#'
#' # Add attributes to some groups
#' attr(input$people,'Company branch') = 'Sales'
#' attr(input$groupb,'Comment') = 'Integer group'
#' attr(input$header,'Timestamp') = date()
#' attr(input$header,'Working directory') = getwd()
#'
#' # Write list to HDF5 file
#' filename = tempfile()
#' writehdf5(input, filename)
#'
#' # Read HDF5 file into a new list
#' output = readhdf5(filename)
#'
#' # Check if input and output lists are identical
#' # (up to alphabetic ordering and data frame-to-list conversion)
#' print(all.equal(sortlist(input, convert.data.frames = TRUE), output))
#'
#' # Write list to HDF5 file again, this time with inherent attributes, allowing
#' # to keep track of the data frames
#' filename = tempfile()
#' writehdf5(input, filename, inherent.attributes = TRUE)
#'
#' # Read HDF5 file into a new list
#' output = readhdf5(filename)
#'
#' # Check if input and output lists are identical
#' # (up to alphabetic ordering only)
#' print(all.equal(sortlist(input), output))
#'
#' @export

writehdf5 <- function(obj, file, inherent.attributes = FALSE, level = 6, overwrite = TRUE) {

  # Create or overwrite the HDF5 file
  if (file.exists(file)) {
    if (overwrite) {
      file.remove(file)
    } else {
      stop(sprintf('File %s already exists', file))
    }
  } else {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  }

  h5file <- hdf5r::H5File$new(file, mode = "w")

  write_obj <- function(group, obj, name = NULL) {

    if (is.list(obj)) {

      # Handle lists (groups) recursively
      subgroup <- if (!is.null(name)) group$create_group(name) else group
      names_obj <- names(obj)
      if (is.null(names_obj)) names_obj <- rep("", length(obj))

      counter <- 0
      for (i in seq_along(obj)) {
        subname <- names_obj[i]
        if (subname == "" || is.null(subname)) {
          counter <- counter + 1
          subname <- sprintf("unnamed_%d", counter)
        }
        write_obj(subgroup, obj[[i]], subname)
      }

      # Add group attributes if they exist
      if (!is.null(attributes(obj))) {
        if (inherent.attributes) {
          attrs <- attributes(obj)
        } else {
          attrs <- userattributes(obj)
        }
        for (attr_name in names(attrs)) {
          subgroup$create_attr(attr_name, robj = attrs[[attr_name]])
        }
      }

    } else {

      if (is.null(obj)) {

        # Create an empty dataset for NULL values with specified dtype and add attributes if they exist
        dataset <- group$create_dataset(name, dims = c(0), dtype = hdf5r::h5types$H5T_NATIVE_DOUBLE)

      } else {

        # Write atomic data types with specified compression level in one line
        if (inherits(obj[1], c('numeric','integer','integer64','character','logical'))) {
          dataset <- group$create_dataset(name, robj = obj, gzip_level = level)
        } else {
          stop("Unsupported input data (class=", paste0(class(obj), collapse='/'), ", type=", typeof(obj),').\n  Consider converting to numeric or character type.')
        }

      }

      # Add dataset attributes if they exist
      if (!is.null(attributes(obj))) {
        if (inherent.attributes) {
          attrs <- attributes(obj)
        } else {
          attrs <- userattributes(obj)
        }
        for (attr_name in names(attrs)) {
          dataset$create_attr(attr_name, robj = attrs[[attr_name]])
        }
      }
    }
  }

  write_obj(h5file, obj)
  h5file$close_all()
}
