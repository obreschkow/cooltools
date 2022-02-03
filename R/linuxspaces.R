#' Handle spaces in Linux filenames
#'
#' @description Convert spaces in filenames (" ") to linux-type spaces "\ ", needed when calling system() on macOS.
#'
#' @param txt filename, which may contain ordinary spaces, e.g. "my file 1.txt"
#'
#' @return filename with modified spaces, e.g. "my\\ file\\ 1.txt"
#'
#' @examples
#' filename = '~/Desktop/my file 1.txt'
#' command = sprintf('ls -l %s',linuxspaces(filename))
#' \dontrun{
#' system(command)
#' }
#'
#' @author Danail Obreschkow
#'
#' @export

linuxspaces = function(txt) gsub(' ','\\\\ ',txt)
