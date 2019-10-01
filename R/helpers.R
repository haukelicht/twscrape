#' Wait until
#'
#' @param until.time POSIXlt time stamp
#' @param pause.secs integer. seconds to pause between checking if \code{until.time} has been reached
#'
#' @return \code{NULL}
wait_until <- function(until.time, pause.secs = 5) {
  message("waiting until ", until.time)
  while (Sys.time() < until.time) {
    Sys.sleep(pause.secs)
  }
  NULL
}
