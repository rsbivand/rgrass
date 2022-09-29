\name{rgrass-package}
\alias{rgrass-package}
\alias{rgrass}
\docType{package}
\title{
Interface between GRASS geographical information system and R
}
\description{
Interpreted interface between GRASS geographical
information system, versions 7 and 8, and R, based on starting R from within the
GRASS environment, or on running R stand-alone and creating a throw-away GRASS environment from within R. The interface uses classes defined in the sp package
to hold spatial data.
}

\details{
Index:
\preformatted{
read_RAST              read GRASS raster files
write_RAST             write GRASS raster files
read_VECT              read GRASS vector object files
write_VECT             write GRASS vector object files
gmeta                  read GRASS metadata from the current LOCATION
getLocationProj        return a WKT2 string of projection information
gmeta2grd              create a GridTopology object from the GRASS region
vInfo                  return vector geometry information
vColumns               return vector database columns information
vDataCount             return count of vector database rows
vect2neigh             return area neighbours with shared boundary length
}
Note that the examples now use the smaller subset North Carolina location: \url{https://grass.osgeo.org/sampledata/north_carolina/nc_basic_spm_grass7.tar.gz}
}
\author{
Roger Bivand

Maintainer: Roger Bivand <Roger.Bivand@nhh.no>

}

\keyword{ package }
\keyword{ spatial }


\examples{
run <- FALSE
if (nchar(Sys.getenv("GISRC")) > 0 &&
  read.dcf(Sys.getenv("GISRC"))[1,"LOCATION_NAME"] == "nc_basic_spm_grass7") run <- TRUE
run <- run && require("terra", quietly=TRUE) && require("sp", quietly=TRUE)
if (run) {
  grd <- gmeta2grd(ignore.stderr=TRUE)
  grd
}
if (run) {
  elevation <- read_RAST("elevation", ignore.stderr=TRUE)
}
if (run) {
  set.seed(1)
  smple <- spatSample(ext(elevation), lonlat=is.lonlat(elevation), size=200,
    method="random", as.points=TRUE)
  values(smple) <- extract(elevation, smple)[,2]
  smple
 }
if (run) {
  write_VECT(smple, "sp_dem", flags=c("overwrite", "o"), ignore.stderr=TRUE)
}
if (run) {
  schoolsDF <- read_VECT("schools", ignore.stderr=TRUE)
  summary(schoolsDF)
}
if (run) {
  vInfo("streams", ignore.stderr=TRUE)
}
if (run) {
  vColumns("streams", ignore.stderr=TRUE)
}
if (run) {
  vDataCount("streams", ignore.stderr=TRUE)
}
if (run) {
  streams <- read_VECT("streams", type="line", ignore.stderr=TRUE)
  summary(streams)
}
}
