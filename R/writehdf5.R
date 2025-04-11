#' Write structured list to an HDF5 file
#'
#' @importFrom hdf5r H5File h5types
#' @importFrom bit64 as.integer64
#'
#' @description Writes a named list, including its hierarchy of nested sublists, to an HDF5 file with HDF5 groups, subgroups and datasets preserving the hierarchical structure.
#' The routine supports standard HDF5 data types, including double-precision floating point numbers (64 bit), integers (32 bit), characters (8 bit), and booleans, as well as vectors and arrays of these types. It also supports
#' 64-bit integers (\code{H5T_NATIVE_INT64}), available via the \code{bit64} package, as well as 32-bit floating points, available via the \code{float} package. Custom attributes of sublists and data, assigned in R via \code{\link{attr}},
#' are automatically transcribed to group and dataset attributes in the HDF5 file. Such attributes can also be provided for empty groups (produced by \code{list()}) and datasets (produced by \code{numeric(0)}).
#'
#' @param obj List containing the data to be written. Nested sublists are interpreted as sub-groups within the HDF5 file. If a sublist is empty or if an element inside a list is `NULL`, it creates an empty group/dataset that can hold only attributes.
#' @param file Character string specifying the file name of the output HDF5 file.
#' @param inherent.attributes Logical flag indicating whether to include inherent attributes that some R-objects possess, such as `dim` for matrices or `names` for named vectors and arrays.
#' @param level Integer specifying the compression level for datasets, typically between 0 (no compression) and 9 (maximum compression). Not all dataset types support compression.
#' @param overwrite Logical value indicating whether to overwrite an existing file.
#'
#' @details The function relies on the \code{hdf5r} package and on the \code{bit64} package.\cr\cr
#' The nested list \code{obj} should only contain data types available to HDF5. The only exception are data frames, which, if included, are automatically converted to lists. Normally, the list \code{obj} and its nested sublists, should all be named lists, i.e. \code{list(a=1, b=2)} rather than \code{list(1,2)}. Unnamed elements are automatically assigned a name `unnamed_#` in the HDF5 file.\cr\cr
#' Some data types in R have inherent attributes, such as `names` for data frames and `dim` for arrays. By default, these inherent attributes are not written to the HDF5 file. They are, however, automatically regenerated when the HDF5 file is loaded back via \code{\link{readhdf5}}. The argument \code{inherent.attributes} can be used to force writing all attributes, including the inherent ones, to the HDF5 file.\cr\cr
#' If a structured R list is saved with \code{writehdf5} and then reloaded using \code{\link{readhdf5}}, the recovered list is identical to the input list up to the ordering of list elements, which is alphabetic by default in HDF5. The only other difference between the input and recovered data occurs when the input data contain data frames, as these are automatically converted do lists. A workaround is to set \code{inherent.attributes=TRUE} when writing the HDF5 file. In this case, the reloaded HDF5 data becomes a data frame again.
#'
#' @return None
#'
#' @seealso \code{\link{readhdf5}}
#'
#' @examples
#'
#' # Create example data
#' input = list(
#'   group_empty = list(),
#'   dataset_empty = numeric(0),
#'   group_dataframe = data.frame(
#'     ID = as.integer(1:3),
#'     Name = c("Alice", "Bob", "Charlie"),
#'     Salary = c(2341.2, 3534.2, 4541.9),
#'     Employed = c(TRUE, TRUE, FALSE)
#'   ),
#'   dataset_parameters = 1.234,
#'   group_integers = list(int32 = as.integer(123),
#'                         int64 = bit64::as.integer64(123),
#'                         vector = bit64::as.integer64((1:3)+12345678912345)),
#'   dataset_nonnumeric = c(NA, NaN, Inf, -Inf),
#'   group_mixed = list(
#'     header = 'test header',
#'     subgroup1 = list(
#'       dataset1 = c("A", "%}~&^", "x1y2z3", "", " "),
#'       dataset2 = matrix(as.integer(1:10), nrow = 2),
#'       dataset3 = array(runif(30), dim = c(2, 5, 3))
#'     ),
#'     subgroup2 = list(date = as.character(as.Date("2025-01-01")),
#'                      location = 'Perth')
#'   )
#' )
#'
#' # Add attributes to some datasets
#' attr(input$dataset_empty,'Comment') = 'This is a test file.'
#' attr(input$group_mixed$subgroup1$dataset3,'Type') = '3D array'
#' attr(input$group_integers$vector,'Comment') = 'Vector of long integers'
#' attr(input$group_integers$vector,'Package') = 'bit64'
#'
#' # Add attributes to some groups
#' attr(input$group_dataframe,'Company branch') = 'Sales'
#' attr(input$group_integers,'Comment') = 'Testing different integers'
#' attr(input$group_empty,'Timestamp') = date()
#' attr(input$group_empty,'Working directory') = getwd()
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

writehdf5 = function(obj, file, inherent.attributes = FALSE, level = 6, overwrite = TRUE) {

  write_obj = function(group, obj, name = NULL) {

    if (is.list(obj)) { # make group

      # Create new group (if obj is not the root list)
      element = if (!is.null(name)) group$create_group(name) else group

      # Recursively create subgroups
      counter = 0
      for (i in seq_along(obj)) {
        obj_name = names(obj)[i]
        if (obj_name == "" || is.null(obj_name)) {
          counter = counter + 1
          obj_name = sprintf("unnamed_%d", counter)
        }
        write_obj(element, obj[[i]], obj_name)
      }

    } else { # make dataset

      # Write atomic data types with specified compression level in one line
      if (inherits(obj[1], c('numeric','integer','integer64','character','logical'))) {
        if (is.array(obj)) {
          element = group$create_dataset(name, robj = aperm(obj,length(dim(obj)):1), gzip_level = level)
        } else {
          element = group$create_dataset(name, robj = obj, gzip_level = level)
        }
      } else if (inherits(obj[1], 'float32')) {
        if (is.array(obj)) {
          element = group$create_dataset(name, robj = t(as.numeric(obj)), dtype = hdf5r::h5types$H5T_IEEE_F32LE, gzip_level = level)
        } else {
          element = group$create_dataset(name, robj = as.numeric(obj), dtype = hdf5r::h5types$H5T_IEEE_F32LE, gzip_level = level)
        }
      } else {
        stop("Unsupported input data (class=", paste0(class(obj), collapse='/'), ", type=", typeof(obj),').\n  Consider converting to numeric or character type.')
      }

    }

    # Add attributes, if they exist, to group/dataset
    if (!is.null(attributes(obj))) {
      if (inherits(obj[1], 'float32')) {
        if (inherent.attributes) {
          attrs = attributes(as.numeric(obj))
        } else {
          attrs = userattributes(as.numeric(obj))
        }
      } else {
        if (inherent.attributes) {
          attrs = attributes(obj)
        } else {
          attrs = userattributes(obj)
        }
      }
      for (attr_name in names(attrs)) {
        element$create_attr(attr_name, robj = attrs[[attr_name]])
      }
    }
  }

  # Remove existing file and create directory, as required
  if (file.exists(file)) {
    if (overwrite) {
      file.remove(file)
    } else {
      stop(sprintf('File %s already exists', file))
    }
  } else {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  }

  # Write new HDF5 file
  h5file = hdf5r::H5File$new(file, mode = "w")
  write_obj(h5file, obj)
  h5file$close_all()

}
