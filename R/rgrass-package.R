#' rgrass: Interface between GRASS geographical information system and R
#'
#' Interpreted interface between GRASS geographical information
#' system, versions 7 and 8, and R, based on starting R from within the GRASS
#' environment, or on running R stand-alone and creating a throw-away GRASS
#' environment from within R. The interface uses classes defined in the sp
#' package to hold spatial data.
#'
#' @details
#' Index:
#'
#' \preformatted{
#' read_RAST              read GRASS raster files
#' write_RAST             write GRASS raster files
#' read_VECT              read GRASS vector object files
#' write_VECT             write GRASS vector object files
#' gmeta                  read GRASS metadata from the current LOCATION
#' getLocationProj        return a WKT2 string of projection information
#' gmeta2grd              create a GridTopology object from the GRASS region
#' vInfo                  return vector geometry information
#' vColumns               return vector database columns information
#' vDataCount             return count of vector database rows
#' vect2neigh             return area neighbours with shared boundary length
#' }
#'
#' Note that the examples now use the smaller subset North Carolina location:
#' \url{https://grass.osgeo.org/sampledata/north_carolina/nc_basic_spm_grass7.tar.gz}
#'
#' @name rgrass
#' @keywords internal package spatial
"_PACKAGE"
