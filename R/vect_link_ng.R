# Interpreted GRASS interface functions
# Copyright (c) 2022 Roger S. Bivand
#
read_VECT <- function(
    vname, layer = "", proxy = FALSE, use_gdal_grass_driver = TRUE, type = NULL,
    flags = "overwrite", Sys_ignore.stdout = FALSE,
    ignore.stderr = get.ignore.stderrOption()) {
  if (!(requireNamespace("terra", quietly = TRUE))) {
    stop("terra required for SpatVector output")
  }
  stopifnot(is.logical(ignore.stderr), !is.na(ignore.stderr))
  stopifnot(is.logical(use_gdal_grass_driver), !is.na(use_gdal_grass_driver))
  if (get.suppressEchoCmdInFuncOption()) {
    inEchoCmd <- set.echoCmdOption(FALSE)
  }
  stopifnot(length(layer) == 1L)
  if (!missing(layer)) layer <- as.character(layer)
  vinfo <- vInfo(vname)
  types <- names(vinfo)[which(vinfo > 0)]
  if (is.null(type)) {
    if (length(grep("points", types)) > 0) type <- "point"
    if (length(grep("lines", types)) > 0) type <- "line"
    if (length(grep("areas", types)) > 0) type <- "area"
    if (is.null(type)) stop("Vector type not found")
  }
  msp <- get_mapsets()
  # in the v.out.ogr case we won't use vca, but this is done to run the checks
  # on vname anyway:
  vca <- sanitize_layername(
    name = vname,
    type = "vector",
    mapsets = msp,
    ignore.stderr = ignore.stderr
  )
  has_grassraster_drv <- gdal_has_grassraster_driver()

  if (has_grassraster_drv && use_gdal_grass_driver) {
    args <- list(name = vca[1], type = "vector")
    if (length(vca) == 2L) args <- c(args, mapset = vca[2])
    tf <- do.call(generate_header_path, args)
    if (layer == "" && type == "area") {
      layers <- terra::vector_layers(tf)
      # Remove this condition once GDAL-GRASS driver issue
      # has been solved (https://github.com/OSGeo/gdal-grass/issues/46).
      # Then also move the type assignment code (from vInfo) to the
      # v.out.ogr case, where it is used as an argument
      layer <- layers[2]
    }
    # message(
    #   "Will get data source ",
    #   tf,
    #   " (layername ",
    #   ifelse(layer == "", "unknown, will get first layer", layer),
    #   ")"
    # )
    res <- getMethod("vect", "character")(tf, layer, proxy = proxy)

  } else {
    if (layer == "") layer <- "1"
    tf <- tempfile(fileext = ".gpkg")
    execGRASS("v.out.ogr",
              flags = flags, input = vname, type = type,
              layer = layer, output = tf, output_layer = vname,
              format = "GPKG", Sys_ignore.stdout = Sys_ignore.stdout,
              ignore.stderr = ignore.stderr
    )
    # message("Reading ", tf)
    res <- getMethod("vect", "character")(tf, proxy = proxy)
  }
  if (!all(getMethod("is.valid", "SpatVector")(res))) {
    res <- getMethod("makeValid", "SpatVector")(res)
  }
  if (get.suppressEchoCmdInFuncOption()) {
    tull <- set.echoCmdOption(inEchoCmd)
  }
  res
}

write_VECT <- function(x, vname, flags = "overwrite",
                       ignore.stderr = get.ignore.stderrOption()) {
  if (!(requireNamespace("terra", quietly = TRUE))) {
    stop("terra required for SpatVector input")
  }
  stopifnot(is.logical(ignore.stderr), !is.na(ignore.stderr))
  if (get.suppressEchoCmdInFuncOption()) {
    inEchoCmd <- set.echoCmdOption(FALSE)
  }
  srcs <- getMethod("sources", "SpatVector")(x)
  if (length(srcs) == 1L) {
    tf <- srcs
  } else {
    tf <- ""
  }
  # exit when the source is a GRASS database layer already:
  if (grepl("[/\\\\]head::[^/\\\\]+$", tf)) {
    grass_layername <- regmatches(
      tf,
      regexpr("(?<=[/\\\\]head::)[^/\\\\]+$", tf, perl = TRUE)
    )
    grass_dsn <- regmatches(
      tf,
      regexpr("(?<=[/\\\\])[^/\\\\]+(?=[/\\\\]head::)", tf, perl = TRUE)
    )
    stop(
      "This SpatVector already links to layer '",
      grass_layername,
      "' of the data source '",
      grass_dsn,
      "' in the GRASS GIS database."
    )
  }

  if (!file.exists(tf)) {
    tf <- tempfile(fileext = ".gpkg")
    getMethod("writeVector", c("SpatVector", "character"))(x, filename = tf,
    filetype = "GPKG", overwrite = TRUE)
  }

  type <- NULL
  if (getMethod("geomtype", "SpatVector")(x) == "points") type <- "point"
  if (getMethod("geomtype", "SpatVector")(x) == "lines") type <- "line"
  if (getMethod("geomtype", "SpatVector")(x) == "polygons") type <- "boundary"
  if (is.null(type)) stop("Unknown data class")

  execGRASS("v.in.ogr",
    flags = flags, input = tf, output = vname, type = type,
    ignore.stderr = ignore.stderr
  )
  if (get.suppressEchoCmdInFuncOption()) {
    tull <- set.echoCmdOption(inEchoCmd)
  }
}
