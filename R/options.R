# Interpreted GRASS 7 interface functions
# Copyright (c) 2015 Roger S. Bivand

#' @rdname gmeta
#' @order 12
#' @param value logical value for setting options on `ignore.stderr` set by
#'   default on package load to FALSE, `stop_on_no_flags_params` set by default
#'   on package load to TRUE, `echoCmd` set by default on package load to FALSE.
#'   `useIntern` sets the intern argument globally; `legacyExec` sets the
#'   legacyExec option globally, but is initialized to FALSE on unix systems
#'   (all but Windows) and TRUE on Windows; `defaultFlags` is initialized to
#'   NULL, but may be a character vector with values from c("quiet", "verbose")
#'   `suppressEchoCmdInFunc` default TRUE suppresses the effect of echoCmd
#'   within package functions, maybe set FALSE for debugging.
#' @export
set.ignore.stderrOption <- function(value) {
  if (!is.logical(value)) stop("logical argument required")
  res <- get("ignore.stderr", envir = .GRASS_CACHE)
  assign("ignore.stderr", value, envir = .GRASS_CACHE)
  res
}

#' @rdname gmeta
#' @order 5
#' @export
get.ignore.stderrOption <- function() {
  get("ignore.stderr", envir = .GRASS_CACHE)
}

#' @rdname gmeta
#' @order 13
#' @export
set.stop_on_no_flags_parasOption <- function(value) {
  if (!is.logical(value)) stop("logical argument required")
  res <- get("stop_on_no_flags_paras", envir = .GRASS_CACHE)
  assign("stop_on_no_flags_paras", value, envir = .GRASS_CACHE)
  res
}

#' @rdname gmeta
#' @order 6
#' @export
get.stop_on_no_flags_parasOption <- function() {
  get("stop_on_no_flags_paras", envir = .GRASS_CACHE)
}

#' @rdname gmeta
#' @order 14
#' @export
set.echoCmdOption <- function(value) {
  if (!is.logical(value)) stop("logical argument required")
  res <- get("echoCmd", envir = .GRASS_CACHE)
  assign("echoCmd", value, envir = .GRASS_CACHE)
  res
}

#' @rdname gmeta
#' @order 7
#' @export
get.echoCmdOption <- function() {
  get("echoCmd", envir = .GRASS_CACHE)
}

#' @rdname gmeta
#' @order 15
#' @export
set.useInternOption <- function(value) {
  if (!is.logical(value)) stop("logical argument required")
  res <- get("useIntern", envir = .GRASS_CACHE)
  assign("useIntern", value, envir = .GRASS_CACHE)
  res
}

#' @rdname gmeta
#' @order 8
#' @export
get.useInternOption <- function() {
  get("useIntern", envir = .GRASS_CACHE)
}

#' @rdname gmeta
#' @order 16
#' @export
set.legacyExecOption <- function(value) {
  if (!is.logical(value)) stop("logical argument required")
  res <- get("legacyExec", envir = .GRASS_CACHE)
  assign("legacyExec", value, envir = .GRASS_CACHE)
  res
}

#' @rdname gmeta
#' @order 9
#' @export
get.legacyExecOption <- function() {
  get("legacyExec", envir = .GRASS_CACHE)
}

#' @rdname gmeta
#' @order 17
#' @export
set.defaultFlagsOption <- function(value) {
  res <- get("defaultFlags", envir = .GRASS_CACHE)
  if (is.null(value)) {
    assign("defaultFlags", value, envir = .GRASS_CACHE)
  } else {
    if (!is.character(value)) stop("character argument required")
    stopifnot(length(value) > 0)
    stopifnot(all(value %in% c("quiet", "verbose")))
    assign("defaultFlags", value, envir = .GRASS_CACHE)
  }
  res
}

#' @rdname gmeta
#' @order 10
#' @export
get.defaultFlagsOption <- function() {
  get("defaultFlags", envir = .GRASS_CACHE)
}

#' @rdname gmeta
#' @order 18
#' @export
set.suppressEchoCmdInFuncOption <- function(value) {
  if (!is.logical(value)) stop("logical argument required")
  res <- get("suppressEchoCmdInFunc", envir = .GRASS_CACHE)
  assign("suppressEchoCmdInFunc", value, envir = .GRASS_CACHE)
  res
}

#' @rdname gmeta
#' @order 11
#' @export
get.suppressEchoCmdInFuncOption <- function() {
  get("suppressEchoCmdInFunc", envir = .GRASS_CACHE)
}
