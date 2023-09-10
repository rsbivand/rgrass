library(rgrass)
if (requireNamespace("terra", quietly=TRUE)) {
  f <- system.file("ex/elev.tif", package="terra")
  SG <- terra::rast(f)
  if (packageVersion("terra") < "1.7.46") {
    bb <- getMethod("ext", "SpatRaster")(SG)@ptr$vector
    all.equal(bb, as.vector(getMethod("ext", "SpatRaster")(SG)))
  } else {
    bb <- as.vector(getMethod("ext", "SpatRaster")(SG))
  }
}
