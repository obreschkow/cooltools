#' Read binary data into array
#'
#' @description Reads binary data using the base function \code{\link[base]{readBin}} and recasts it into an array of custom dimensions.
#'
#' @param filename path of the file to be loaded
#' @param dim vector specifying the dimensions of the array
#' @param bytes number of bytes per number in the binary file
#' @param type  character vector of length describing the data type: "numeric" (default), "double", "integer", "int", "logical", "complex", "character", "raw"
#' @param signed logical. Only used for integers of sizes 1 and 2, when it determines if the quantity on file should be regarded as a signed or unsigned integer.
#' @param endian endian-type ("big" or "little") of the file
#'
#' @return Returns an array of dimension dim.
#'
#' @author Danail Obreschkow
#'
#' @export

loadbin <- function(filename, dim, bytes=4, type='numeric', signed=FALSE, endian='little') {
  n = prod(dim)
  dat = readBin(filename,what=type,size=bytes,n=n,signed=signed,endian=endian)
  return(array(dat,dim))
}
