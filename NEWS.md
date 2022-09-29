# **rgrass** version 0.3-5 (development)

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
