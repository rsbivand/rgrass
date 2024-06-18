gdal_has_grassraster_driver <- function() {
  if (!(requireNamespace("terra", quietly = TRUE))) {
    stop("terra is required to get the GDAL drivers list")
  }
  drv <- terra::gdal(drivers = TRUE)
  "GRASS" %in% drv[drv$raster, ]$name
}

gdal_has_grassvector_driver <- function() {
  if (!(requireNamespace("terra", quietly = TRUE))) {
    stop("terra is required to get the GDAL drivers list")
  }
  drv <- terra::gdal(drivers = TRUE)
  "OGR_GRASS" %in% drv[drv$vector, ]$name
}

generate_header_path <- function(name, type, ...) {
  stopifnot(
    is.character(type),
    length(type) == 1L,
    type %in% c("raster", "vector")
  )
  stopifnot(
    is.character(name),
    length(name) == 1L
  )
  element <- ifelse(type == "vector", "vector", "cellhd")
  path <- execGRASS(
    "g.findfile",
    element = element,
    file = name,
    intern = TRUE,
    ...
  )[4]
  path <- regmatches(
    path,
    regexpr("(?<==['\"]).+(?=['\"]$)", path, perl = TRUE)
  )
  if (type == "vector") {
    path <- file.path(path, "head")
  }
  path
}
