suppressPackageStartupMessages(library(rgrass))
## IGNORE_RDIFF_BEGIN
old <- readRDS(system.file("etc/XML.r.out.gdal.rds", package = "rgrass"))
if (nchar(Sys.getenv("GISRC")) > 0) {
  new <- parseGRASS("r.out.gdal")
  stopifnot(isTRUE(all.equal(old, new)))
}
old <- readRDS(system.file("etc/res_r.water.outlet.rds", package = "rgrass"))
if (nchar(Sys.getenv("GISRC")) > 0) {
  new <- parseGRASS("r.water.outlet")
  stopifnot(isTRUE(all.equal(old, new)))
}
## IGNORE_RDIFF_END
