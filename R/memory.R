#' List of object sizes
#' 
#' Returns the object sizes of all objects in memory.
#' 
#' @return NULL
#' 
#' @importFrom utils object.size
#' 
#' @examples
#' \dontrun{
#' WindowsMT.memory() 
#' }
#' 
#' @export

WindowsMT.memory <- function() {
  
  return(sort(sapply(ls(), function(x) {object.size(get(x))})))
  
}
