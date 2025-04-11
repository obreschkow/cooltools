#' Retrieve Custom Object Attributes
#'
#' @importFrom bit64 as.integer64
#' @importFrom float as.float
#' @importFrom stats lm ts
#'
#' @description This function takes a generic R object and returns the custom attributes
#' added by the user, distinguishing them from the object's inherent attributes.
#'
#' @param obj R object whose custom attributes to extract.
#'
#' @return Named list of custom attributes, mimicking the output of the base function \code{\link{attributes}}, but only including custom attributes
#'
#' @seealso \code{attributes}
#'
#' @examples
#' # Create a list containing objects of different classes
#' example = list(1,
#'                as.integer(1:3),
#'                bit64::as.integer64(1),
#'                diag(5),
#'                array(0,c(2,3,5)),
#'                list(a=1,b='c'),
#'                data.frame(x=1, y=1:10, char=LETTERS[1:10])
#' )
#'
#' # Loop over all the objects to check if the custom attribute is correctly extracted.
#' for (i in seq_along(example)) {
#'   x = example[[i]]
#'   attr(x,'MyAttribute') = 'Test'
#'     cat(sprintf('Class = %s, Custom attribute(s) = %s\n',class(x)[1],
#'                 paste(names(userattributes(x)), collapse = ', ')))
#' }
#' @export
userattributes <- function(obj) {

  # Get all the attributes of the object
  obj_attrs <- attributes(obj)

  if (is.null(obj_attrs)) {

    return(NULL)

  } else {

    # Create a baseline object of the same type
    baseline <- switch(class(obj)[1],
                       # Common object types
                       numeric = numeric(),
                       integer = integer(),
                       character = character(),
                       logical = logical(),

                       # Factors and tables
                       factor = factor(c("a", "b")),
                       table = table(c(1, 2, 2, 3)),

                       # Complex objects
                       matrix = matrix(0, nrow = 1, ncol = 1),
                       array = array(0, dim = c(1, 1, 1)),
                       data.frame = data.frame(x = 0),
                       list = list(x = 0),

                       # Date and datetime
                       Date = as.Date("2023-01-01"),
                       POSIXct = as.POSIXct("2025-01-01 00:00:00"),
                       POSIXlt = as.POSIXlt("2025-01-01 00:00:00"),

                       # Classes defined in packages
                       ts = stats::ts(1:2),
                       lm = stats::lm(y ~ x, data = data.frame(x = 1:2, y = 1:2)),
                       integer64 = bit64::as.integer64(1),
                       float32 = float::as.float(1),

                       # Default case if object type isn't covered
                       0)

    # Get the attributes of baseline
    baseline_attrs <- attributes(baseline)

    # Identify custom attributes
    custom_attr_names <- setdiff(names(obj_attrs), c('names',names(baseline_attrs)))

    # add attribute values
    if (length(custom_attr_names) == 0) {

      return(NULL)

    } else {

      custom_attrs <- lapply(custom_attr_names, function(attr) obj_attrs[[attr]])
      names(custom_attrs) <- custom_attr_names
      return(custom_attrs)

    }

  }
}
