library(rgrass)
old <- readRDS(system.file("etc/XML.r.out.gdal.rds", package="rgrass"))
if (nchar(Sys.getenv("GISRC")) > 0) {
  new <- parseGRASS("r.out.gdal")
  all.equal(old, new)
}
old <- readRDS(system.file("etc/res_r.water.outlet.rds", package="rgrass"))
if (nchar(Sys.getenv("GISRC")) > 0) {
  new <- parseGRASS("r.water.outlet")
  all.equal(old, new)
}
