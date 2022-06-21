% Copyright (C) 2005-2022 Roger S. Bivand
%
\name{readVECT}
\alias{read_VECT}
\alias{write_VECT}
\alias{vInfo}
\alias{vColumns}
\alias{vDataCount}
\alias{vect2neigh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read and write GRASS vector object files}
\description{
  \code{read_VECT} moves one GRASS vector object file with attribute data through a temporary GeoPackage file to a \pkg{terra} \code{"SpatVector"} object; \code{write_VECT} moves a \pkg{terra} \code{"SpatVector"} object through a temporary GeoPackage file to a GRASS vector object file. \code{vect2neigh} returns neighbour pairs with shared boundary length as described by Markus Neteler, in \url{https://stat.ethz.ch/pipermail/r-sig-geo/2005-October/000616.html}. \code{cygwin_clean_temp} can be called to try to clean the GRASS mapset-specific temporary directory under cygwin.
}
\usage{
read_VECT(vname, layer, type=NULL, flags="overwrite",
    ignore.stderr = NULL)
write_VECT(x, vname, flags="overwrite", ignore.stderr = NULL)
vInfo(vname, layer, ignore.stderr = NULL)
vColumns(vname, layer, ignore.stderr = NULL)
vDataCount(vname, layer, ignore.stderr = NULL)
vect2neigh(vname, ID=NULL, ignore.stderr = NULL, remove=TRUE, vname2=NULL,
 units="k")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vname}{A GRASS vector file name}
  \item{layer}{a layer name (string); if missing set to default of \dQuote{1}}
  \item{type}{override type detection when multiple types are non-zero, passed to v.out.ogr}
  \item{ignore.stderr}{default the value set by \code{set.ignore.stderrOption}; NULL, taking the value set by \code{set.ignore.stderrOption}, can be set to TRUE to silence \code{system()} output to standard error; does not apply on Windows platforms}
  \item{x}{A \code{"SpatVector"} object moved to GRASS}
  \item{flags}{Character vector containing additional optional flags and/or options for v.in.ogr, particularly "o" and "overwrite"}
  \item{ID}{A valid DB column name for unique identifiers (optional)}
  \item{remove}{default TRUE, remove copied vectors created in \code{vect2neigh}}
  \item{vname2}{If on a previous run, remove was FALSE, the name of the temporary vector may be given to circumvent its generation}
  \item{units}{default "k"; see GRASS 'v.to.db' manual page for alternatives}
}
\value{
   \code{read_VECT} imports a GRASS vector object into a \code{"SpatVector"} object.

\code{vect2neigh} returns a data frame object with left and right neighbours and boundary lengths, also given class GRASSneigh and spatial.neighbour (as used in spdep). The incantation to retrieve the neighbours list is \code{sn2listw(vect2neigh())$neighbours}, and to retrieve the boundary lengths: \code{sn2listw(vect2neigh())$weights}. The GRASSneigh object has two other useful attributes: external is a vector giving the length of shared boundary between each polygon and the external area, and total giving each polygon's total boundary length.
}

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
  execGRASS("v.info", map="schools", layer="1")
}
if (run) {
  print(vInfo("schools"))
  schs <- read_VECT("schools")
  print(summary(schs))
}
if (run) {
  write_VECT(schs, "newsch", flags=c("o", "overwrite"))
  execGRASS("v.info", map="newsch", layer="1")
}
if (run) {
  nschs <- read_VECT("newsch")
  print(summary(nschs))
}
if (run) {
  print(all.equal(names(nschs), as.character(vColumns("newsch")[,2])))
}
if (run) {
  print(vInfo("roadsmajor"))
}
if (run) {
  roads <- read_VECT("roadsmajor")
  print(summary(roads))
}
if (run) {
  cen_neig <- vect2neigh("census")
  str(cen_neig)
}
if (run) {
  execGRASS("g.remove", flags="f", name=c("newsch", "newsch1"), type="vector")
}
Sys.setenv("GRASS_VERBOSE"=GV)
set.ignore.stderrOption(ois)
}
\keyword{spatial}

