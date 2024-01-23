% Copyright (C) 2005-2022 Roger S. Bivand
\name{readRAST}
\alias{read_RAST}
\alias{write_RAST}
\title{Read and write GRASS raster files}
\description{
Read GRASS raster files from GRASS into R \pkg{terra} \code{"SpatRaster"} or \pkg{sp} \code{"SpatialGridDataFrame"} objects, and write single columns of \pkg{terra} \code{"SpatRaster"} or \pkg{sp} \code{"SpatialGridDataFrame"} objects to GRASS. When \code{return_format="terra"}, temporary binary files and r.out.bin and r.in.bin are used for speed reasons. \code{read_RAST()} and \code{write_RAST()} by default use \code{"RRASTER"} files written and read by GDAL. 
}



\usage{
read_RAST(vname, cat=NULL, NODATA=NULL, ignore.stderr=get.ignore.stderrOption(),
 return_format="terra", close_OK=return_format=="SGDF", flags=NULL)
write_RAST(x, vname, zcol = 1, NODATA=NULL, flags=NULL, 
 ignore.stderr = get.ignore.stderrOption(), overwrite=FALSE, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vname}{A vector of GRASS raster file names in mapsets in the current search path, as set by \dQuote{g.mapsets}; the file names may be given as fully-qualified map names using \dQuote{name@mapset}, in which case only the mapset given in the full path will be searched for the existence of the raster; if more than one raster with the same name is found in mapsets in the current search path, an error will occur, in which case the user should give the fully-qualified map name. If the fully-qualified name is used, \code{@} will be replaced by underscore in the output object.}
  \item{cat}{default NULL; if not NULL, must be a logical vector matching vname, stating which (CELL) rasters to return as factor}
  \item{return_format}{default \code{"terra"}, optionally \code{"SGDF"}}
  \item{ignore.stderr}{default taking the value set by \code{set.ignore.stderrOption}; can be set to TRUE to silence \code{system()} output to standard error; does not apply on Windows platforms}
  \item{close_OK}{default TRUE - clean up possible open connections used for reading metadata; may be set to FALSE to avoid the side-effect of other user-opened connections being broken}
  \item{x}{A \pkg{terra} \code{"SpatRaster"} or \pkg{sp} \code{"SpatialGridDataFrame"} object}
  \item{zcol}{Attribute column number or name}
  \item{NODATA}{by default NULL, in which case it is set to one less than \code{floor()} of the data values for FCELL rasters or the range maximum for CELL Byte, UInt16 and Uint32 rasters (with no negative values), and an attempt is made to set NODATA to the upper Int16 and Int32 range if the lower range is occupied; otherwise an integer NODATA value (required to be integer by GRASS r.out.bin)}
  \item{overwrite}{default FALSE, if TRUE inserts \code{"overwrite"} into the value of the \code{flags} argument if not already there to allow existing GRASS rasters to be overwritten}
  \item{flags}{default NULL, character vector, for example \code{"overwrite"}}
  \item{verbose}{default TRUE, report how writing to GRASS is specified}
}

\value{\code{read_RAST} by default returns a SpatRaster object, but may return a legacy SpatialGridDataFrame object if \code{return_format="SGDF"}. \code{write_RAST} silently returns the object being written to GRASS.}

\author{Roger S. Bivand, e-mail: \email{Roger.Bivand@nhh.no}}

\examples{
run <- FALSE
if (nchar(Sys.getenv("GISRC")) > 0 &&
  read.dcf(Sys.getenv("GISRC"))[1,"LOCATION_NAME"] == "nc_basic_spm_grass7") run <- TRUE
GV <- Sys.getenv("GRASS_VERBOSE")
Sys.setenv("GRASS_VERBOSE"=0)
ois <- get.ignore.stderrOption()
set.ignore.stderrOption(TRUE)
if (run) {
unlist(strsplit(execGRASS("g.mapsets", flags="p", intern=TRUE), " "))
}
if (run) {
execGRASS("g.list", type="raster", pattern="soils", flags="m", intern=TRUE)
}
if (run) {
execGRASS("g.list", type="raster", pattern="soils@PERMANENT", mapset=".", flags="m", intern=TRUE)
}
if (run) {
execGRASS("g.list", type="raster", pattern="soils", mapset="PERMANENT", flags="m", intern=TRUE)
}
run <- run && require("terra", quietly=TRUE)
if (run) {
  v1 <- read_RAST("landuse", cat=TRUE, return_format="terra")
  v1
  inMemory(v1)
}
if (run) {
  write_RAST(v1, "landuse1", flags=c("o", "overwrite"))
  execGRASS("r.stats", flags="c", input="landuse1")
  execGRASS("g.remove", flags="f", name="landuse1", type="raster")
}
Sys.setenv("_SP_EVOLUTION_STATUS_"="2")
run <- run && require("sp", quietly=TRUE)
if (run) {
  nc_basic <- read_RAST(c("geology", "elevation"), cat=c(TRUE, FALSE),
    return_format="SGDF")
  print(table(nc_basic$geology))
}
if (run) {
  execGRASS("r.stats", flags=c("c", "l", "quiet"), input="geology")
}
if (run) {
  boxplot(nc_basic$elevation ~ nc_basic$geology)
}
if (run) {
  nc_basic$sqdem <- sqrt(nc_basic$elevation)
}
if (run) {
  write_RAST(nc_basic, "sqdemSP", zcol="sqdem", flags=c("quiet", "overwrite"))
  execGRASS("r.info", map="sqdemSP")
}
if (run) {
  print(system.time(sqdemSP <- read_RAST(c("sqdemSP", "elevation"),
    return_format="SGDF")))
}
if (run) {
  print(system.time(sqdem <- read_RAST(c("sqdemSP", "elevation"),
    return_format="terra")))
}
if (run) {
names(sqdem)
}
if (run) {
  try(sqdem1 <- read_RAST(c("sqdemSP@rsb", "elevation@PERMANENT")))
}
if (run) {
names(sqdem1)
}
if (run) {
  execGRASS("g.remove", flags="f", name="sqdemSP", type="raster")
  execGRASS("r.mapcalc", expression="basins0 = basins - 1", flags="overwrite")
  execGRASS("r.stats", flags="c", input="basins0")
}
if (run) {
  basins0 <- read_RAST("basins0", return_format="SGDF")
  print(table(basins0$basins0))
}
if (run) {
  execGRASS("g.remove", flags="f", name="basins0", type="raster")
}
if (run) {
  execGRASS(cmd = "r.mapcalc", expression="test_t=66000", flags="overwrite")
  execGRASS("r.info", flags = "r", map = "test_t", intern = TRUE) # #82
}
if (run) {
  (tt = read_RAST(vname = "test_t"))
}
if (run) {
  execGRASS("g.remove", flags="f", name="test_t", type="raster")
}
Sys.setenv("GRASS_VERBOSE"=GV)
set.ignore.stderrOption(ois)
}
\keyword{spatial}

