#' Write structure list to an HDF5 file
#'
#' @importFrom hdf5r H5File h5types
#' @importFrom bit64 is.integer64 as.integer64
#'
#' @description Recursively writes named lists, including nested named sublists, to an HDF5 file, creating HDF5 groups and datasets as needed to preserve the hierarchical structure.
#' The routine supports standard data types, including double floats, integers, characters, and logical values, as well as vectors and arrays of these types. It also supports
#' 64-bit integers (\code{H5T_NATIVE_INT64}) if such input data is provided via the \code{bit64} package. Custom attributes of sublists and data, included in the R-data,
#' are automatically transcribed to group and dataset attributes in the HDF5 file. Such attributes can also be provided for empty groups and datasets.
#'
#' @param obj List containing the data to be written. Nested sublists are interpreted as sub-groups within the HDF5 file. If a sublist is empty or if an element inside a list is `NULL`, it creates an empty group/dataset that can hold only attributes.
#' @param file Character string specifying the file name of the output HDF5 file.
#' @param inherent.attributes Logical flag indicating whether to include inherent attributes that some R-objects possess, such as `dim` for matrices.
#' @param level Integer specifying the compression level for datasets, typically between 0 (no compression) and 9 (maximum compression). Not all dataset types support compression.
#' @param overwrite Logical value indicating whether to overwrite an existing file.
#'
#' @details The function relies on the \code{hdf5r} package and on the \code{bit64} package.\cr
#' If the argument `obj` is a data frame or contains a data frame in a nested sublist, then this data frame is automatically converted into a list with matching names.\cr
#' If the input data contain some unnamed elements, e.g. \code{obj=list(a='named','unnamed')}, the unnamed groups/datasets are called `unnamed_#` in the HDF5 file.\cr
#' Some data types in R have inherent attributes, such as `names` for data frames and `dim` for arrays. By default, these inherent attributes are not written to the HDF5 file. They are, however, automatically regenerated when the HDF5 file is loaded back via \code{\link{readhdf5}}. The argument \code{inherent.attributes} can be used to force writing all attributes, including the inherent ones, to the HDF5 file.
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
#' # (up to ordering, as HDF5 uses alphabetic default order)
#' print(all.equal(sortlist(input, convert.data.frames = TRUE), output))
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

      counter <- 0
      for (i in seq_along(obj)) {
        subname <- names_obj[i]
        if (subname == "" || is.null(subname)) {
          counter <- counter + 1
          subname <- sprintf("unnamed_%d", counter)
        }
        write_obj(subgroup, obj[[i]], subname)
      }

    } else if (is.null(obj)) {
      # Create an empty dataset for NULL values with specified dtype and add attributes if they exist
      dataset <- group$create_dataset(name, dims = c(0), dtype = hdf5r::h5types$H5T_NATIVE_DOUBLE)

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

    } else {
      # Define chunk dimensions for compression if the data has more than one element
      chunk_dims <- if (length(obj) > 1) dim(obj) else NULL

      # Write atomic data types with specified compression level in one line
      dataset <- if (bit64::is.integer64(obj)) {
        group$create_dataset(name, robj = bit64::as.integer64(obj), dtype = hdf5r::h5types$H5T_NATIVE_LLONG, chunk_dims = chunk_dims, gzip_level = level)
      } else if (is.integer(obj)) {
        group$create_dataset(name, robj = obj, chunk_dims = chunk_dims, gzip_level = level)
      } else if (is.double(obj)) {
        group$create_dataset(name, robj = obj, chunk_dims = chunk_dims, gzip_level = level)
      } else if (is.character(obj)) {
        group$create_dataset(name, robj = obj, chunk_dims = chunk_dims, gzip_level = level)
      } else if (is.logical(obj)) {
        group$create_dataset(name, robj = obj, chunk_dims = chunk_dims, gzip_level = level)
      } else if (is.factor(obj)) {
        stop("Factors are not supported by HDF5. Convert all factor data to regular data, e.g. using as.character(...).")
      } else {
        stop("Unsupported data type: Class=", class(obj), ", Type=", typeof(obj))
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
