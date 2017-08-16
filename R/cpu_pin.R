#' Windows CPU Pinning
#' 
#' Pins CPU on Windows depending on the number of cores requested.
#' 
#' CPU pinning forces the first group of appropriate cores to be pinned. It handles hyperthreaded cores correctly by trying to avoid them. It has a parameter for the sockets, but it is currently unused.
#' 
#' @param n_request Type: integer. Number of logical cores to pin.
#' @param n_cores Type: integer. Number of logical cores on your machine. Defaults to \code{as.integer(Sys.getenv("NUMBER_OF_PROCESSORS"))}.
#' @param n_sockets Type: integer. Number of sockets on your machine. Defaults to \code{1}.
#' @param n_hyperthread Type: logical. Whether hyperthreading is activated. Defaults to \code{TRUE}.
#' @param verbose Type: logical. Whether to print debug information. Defaults to \code{TRUE}.
#' @param test Type: logical. Whether to just print the pinning without performing the pinning (placebo if \code{TRUE}). Defaults to \code{FALSE}.
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#' # Pin 1 core
#' WindowsMT.cpu_pin(1)
#' 
#' # Pin 2 core
#' WindowsMT.cpu_pin(2)
#' 
#' # Pin all cores (reset pinning)
#' WindowsMT.cpu_pin(as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")))
#' 
#' # Crazy test, do not run
#' for (i in 1:4) {
#' for (j in c(FALSE, TRUE)) {
#'   for (k in 1:10) {
#'     for (l in 1:(i * k * (j + 1))) {
#'       WindowsMT.cpu_pin(l, i * k * (j + 1), i, j, TRUE, FALSE)
#'     }
#'   }
#' }
#' }
#' 
#' }
#' 
#' @export

WindowsMT.cpu_pin <- function(n_request,
                              n_cores = as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")),
                              n_sockets = 1,
                              n_hyperthread = TRUE,
                              verbose = TRUE,
                              test = FALSE) {
  
  core_steal <- logical(n_cores)
  
  if (n_cores >= n_request) {
    
    if (n_hyperthread == TRUE) {
      
      # Ignore balancing rules for cores, see here for demo on 128 threads: https://github.com/szilard/ml-x1 https://github.com/szilard/GBM-multicore
      core_number <- c((2 * (1:(n_cores / 2)) - 1), (2 * (1:(n_cores / 2))))[1:n_request]
      
    } else {
      
      # Ignore balancing rules for cores, see here for demo on 64 cores: https://github.com/szilard/ml-x1 https://github.com/szilard/GBM-multicore
      core_number <- (1:n_cores)[1:n_request]
      
    }
    
    core_steal[core_number] <- TRUE
    core_affinity <- sum(core_steal * 2 ^ ((1:length(core_steal)) - 1))
    
    if (test == FALSE) {
      cat("Pinning [1-", n_cores, "] performed on ", sum(core_steal), " cores {", paste(which(core_steal), collapse = ", "), "} (", core_affinity, ").  \n", sep = "")
    }
    
    # Windows-only version!
    shell(paste0("PowerShell -Command \"& {(Get-Process -id ", Sys.getpid(), ").ProcessorAffinity = ", core_affinity, "}\""))
    
  } else {
    
    # User requested too many cores!
    cat("You requested too many cores!  \n")
    
  }
  
}



