sanitize_layername <- function(name, type, mapsets, ignore.stderr) {
  stopifnot(
    is.character(type),
    length(type) == 1L,
    type %in% c("raster", "vector")
  )
  stopifnot(
    is.character(name),
    length(name) == 1L
  )
  vca <- unlist(strsplit(name, "@"))
  if (length(vca) == 1L) {
    exsts <- execGRASS("g.list",
                       type = type, pattern = vca[1],
                       intern = TRUE, ignore.stderr = ignore.stderr
    )
    if (length(exsts) > 1L) {
      stop(
        "multiple layers named ", vca[1],
        " found in in mapsets in search path: ",
        paste(mapsets, collapse = ", "),
        " ; use full path with @ to choose the required raster"
      )
    }
    if (length(exsts) == 0L || exsts != vca[1]) {
      stop(
        name, " not found in mapsets in search path: ",
        paste(mapsets, collapse = ", ")
      )
    }
  } else if (length(vca) == 2L) {
    exsts <- execGRASS("g.list",
                       type = type, pattern = vca[1],
                       mapset = vca[2], intern = TRUE, ignore.stderr = ignore.stderr
    )
    if (length(exsts) == 0L || exsts != vca[1]) {
      stop(name, " not found in mapset: ", vca[2])
    }
  } else {
    stop(name, " incorrectly formatted")
  }
  vca
}

get_mapsets <- function() {
  unlist(strsplit(
    execGRASS("g.mapsets", flags = "p", intern = TRUE),
    " "
  ))
}
