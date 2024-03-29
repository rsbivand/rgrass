\name{initGRASS}
\alias{initGRASS}
\alias{get.GIS_LOCK}
\alias{set.GIS_LOCK}
\alias{unset.GIS_LOCK}
\alias{unlink_.gislock}
\alias{remove_GISRC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initiate GRASS session}
\description{
  Run GRASS interface in an R session not started within GRASS. In general, most users will use \code{initGRASS} in throwaway locations, to use GRASS modules  on R objects without the need to define and populate a location. The function initializes environment variables used by GRASS, the .gisrc used by GRASS for further environment variables, and a temporary location.

On Windows, if OSGeo4W GRASS is being used, the R session must be started in the OSGeo4W shell. If not, the non-standard placing of files and of environment variables confuses the function. If \code{toupper(gisBase)} contains \dQuote{OSGEO4W64/APPS/GRASS} or \dQuote{OSGEO4W/APPS/GRASS} (and after converting \dQuote{\\} to \dQuote{/}), but the environment variable \code{OSGEO4W_ROOT} is not defined, \code{initGRASS()} will exit with an error before confusion leads to further errors. For further details, see \url{https://github.com/rsbivand/rgrass/issues/16} and \url{https://lists.osgeo.org/pipermail/grass-stats/2018-November/001800.html}.

The same restriction applies to use of GRASS with QGIS Windows standalone installations, which may be used with \code{initGRASS} only if the R session is started from the OSGeo4W shell shipped as part of the standalone installer (see \url{https://github.com/rsbivand/rgrass/issues/87}). The function will exit with an error if R was not started from the QGIS OSGeo4W shell before confusion leads to further errors.

The locking functions are used internally, but are exposed for experienced R/GRASS scripters needing to use the GRASS module \dQuote{g.mapset} through \code{initGRASS} in an existing GRASS location. In particular, \dQuote{g.mapset} may leave a \code{.gislock} file in the current MAPSET, so it may be important to call \code{unlink_.gislock} to clean up before quitting the R session. \code{remove_GISRC} may be used to try to remove the file given in the \dQuote{GISRC} environment variable if created by \code{initGRASS} with argument \code{remove_GISRC=} TRUE.
}
\usage{
initGRASS(gisBase=NULL, home, SG, gisDbase, addon_base, location, mapset,
  override = FALSE, use_g.dirseps.exe = TRUE, pid, remove_GISRC=FALSE,
  ignore.stderr=get.ignore.stderrOption())
get.GIS_LOCK()
set.GIS_LOCK(pid)
unset.GIS_LOCK()
unlink_.gislock()
remove_GISRC()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gisBase}{The directory path to GRASS binaries and libraries, containing bin and lib subdirectories among others; if NULL, set from environment variable GRASS_INSTALLATION if found, if not found, \code{system("grass --config path")} is tried}
  \item{home}{The directory in which to create the .gisrc file; defaults to \code{$HOME} on Unix systems and to \code{USERPROFILE} on Windows systems; can usually be set to \code{tempdir()}}
  \item{SG}{An optional \code{SpatRaster} or \code{SpatialGrid} object to define the \code{DEFAULT_WIND} of the temporary location.}
  \item{gisDbase}{if missing, \code{tempdir()} will be used; GRASS GISDBASE directory for the working session}
  \item{addon_base}{if missing, assumed to be \dQuote{$HOME/.grass7/addons} on Unix-like platforms, on MS Windows \dQuote{\%APPDATA\%\\GRASS7\\addons}, and checked for existence}
  \item{location}{if missing, \code{basename(tempfile())} will be used; GRASS location directory for the working session}
  \item{mapset}{if missing, \code{basename(tempfile())} will be used; GRASS mapset directory for the working session}
  \item{override}{default FALSE, set to TRUE if accidental trashing of GRASS .gisrc files and locations is not a problem}
  \item{use_g.dirseps.exe}{default TRUE; when TRUE appears to work for WinGRASS Native binaries, when FALSE for QGIS GRASS binaries; ignored on other platforms.}
  \item{pid}{default \code{as.integer(round(runif(1, 1, 1000)))}, integer used to identify GIS_LOCK; the value here is arbitrary, but probably should be set correctly}
  \item{remove_GISRC}{default FALSE; if TRUE, attempt to unlink the temporary file named in the \dQuote{GISRC} environment variable when the R session terminates or when this package is unloaded}
  \item{ignore.stderr}{default taking the value set by \code{set.ignore.stderrOption}; can be set to TRUE to silence \code{system()} output to standard error; does not apply on Windows platforms}
}
\details{
  The function establishes an out-of-GRASS working environment providing GRASS commands with the environment variable support required, and may also provide a temporary location for use until the end of the running R session if the \code{home} argument is set to \code{tempdir()}, and the \code{gisDbase} argument is not given. Running \code{gmeta} shows where the location is, should it be desired to archive it before leaving R.
}
\value{
  The function runs \code{gmeta6} before returning the current values of the running GRASS session that it provides.
}
\note{If any package command fails with a UTF-8 error from the XML package, try using \code{setXMLencoding} to work around the problem that GRASS modules declare --interface-description output as UTF-8 without ensuring that it is.}

\author{Roger S. Bivand, e-mail: \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{gmeta}}}
\examples{
GRASS_INSTALLATION <- Sys.getenv("GRASS_INSTALLATION")
run <- FALSE
if (nzchar(GRASS_INSTALLATION)) run <- file.info(GRASS_INSTALLATION)$isdir
run <- run && require(terra, quietly=TRUE)
if (run) {
f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
plot(r, col=grDevices::terrain.colors(50))
}
if (run) {
(loc <- initGRASS(GRASS_INSTALLATION, home=tempdir(), SG=r, override=TRUE))
}
if (run) {
write_RAST(r, "elev", flags="overwrite")
execGRASS("r.info", map="elev")
}
if (run) {
s <- rast(r)
values(s) <- values(r)
write_RAST(s, "elev1", flags="overwrite")
execGRASS("r.info", map="elev1")
}
if (run) {
execGRASS("r.slope.aspect", flags="overwrite", elevation="elev", slope="slope", aspect="aspect")
}
if (run) {
u1 <- read_RAST(c("elev", "slope", "aspect"), return_format="terra")
plot(u1[["elev"]], col=grDevices::terrain.colors(50))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

