# **rgrass** version 0.4-3 (development)

- see #87 - Windows QGIS standalone installations of GRASS GIS can be used only if R is started in the OSGeo4W shell bundled with the installation

- `write_VECT()`: when the `SpatVector` object already refers to a source file, an intermediate temporary file is no longer written to get the data into the GRASS GIS database (#93).
A similar shortcut was already in place for `write_RAST()`.

- `read_VECT()`: provide access to the standalone GDAL-GRASS driver to read vector data, which skips the step of writing a intermediate file (#93).
Note that this standalone driver needs to be set up separately.
More information is in the [driver's README](https://github.com/OSGeo/gdal-grass/blob/main/README.md).

- `read_VECT()`: support reading as `SpatVectorProxy` class of `{terra}`, by providing a `proxy` argument (#93).

# **rgrass** version 0.4-2 (2024-03-17)

- see #84 - handling fully-qualified map names

# **rgrass** version 0.4-1 (2024-01-08)

- replace `XML` with `xml2` see #72

# **rgrass** version 0.3-9 (2023-09-10)

- reinstate `grass-stable` https://github.com/OSGeo/grass-website/issues/357

- fix #79 thanks to Adam B. Smith, use `terra::as.vector` method rather than internal slot name

# **rgrass** version 0.3-8 (2023-03-17)

- #73 guess `gisBase=` in `initGRASS()`

- added _SP_EVOLUTION_STATUS_ 2 to examples

- #66 re-examining to protect from UInt maxing out; add stop for required manual NODATA

- #68, #69 improvements to vignettes, thanks to Floris Vanderhaeghe and Veronica Andreo

# **rgrass** version 0.3-6 (2022-10-11)

- macOS vignette issue

# **rgrass** version 0.3-5 (2022-09-29)

- #63 and #64, detection of GRASS path for `initGRASS()` semi-automated if `grass --config path` works or if environment variable `GRASS_INSTALLATION` set to path, thanks to Robin Lovelace

- correct NODATA logic in `read_RAST()` for unsigned rasters #66 thanks to Laura Poggio

# **rgrass** version 0.3-3 (2022-08-08)

- correct vignette logic error triggered on M1 when **stars** not installed

# **rgrass** version 0.3-2 (2022-07-21)

- first release to replace deprecated **rgrass7**

- remove suggested packages planned for retirement (**rgdal**)

- add vignettes

- remove old code working around earlier use by GRASS of DBF rather than SQLite for storing vector attributes

- remove `use_sp()` and `use_sf()` for file transfer

- remove code using GDAL GRASS plugin
