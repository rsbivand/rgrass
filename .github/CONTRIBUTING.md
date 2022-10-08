### Contributing to rgrass

This outlines how to propose a change to `rgrass` as we move to support [GRASS 8.0](https://github.com/OSGeo/grass). The `rgrass` package is the successor to the `rgrass7` package. `rgrass` supports both GRASS 7 and 8 and is available from CRAN; `rgrass7` will be retired at the latest at the end of 2023 when `rgdal` retires.

### How to propose a change to rgrass

To propose a change to `rgrass`, please consider the roles assigned to the different branches in this repository.

* The `main` branch is the development branch for `rgrass`. If you would like to contribute to `rgrass` ("new" package), please target your PRs and contributions to this branch.

* The `rgrass7` branch is the "target" maintenance source for the package `rgrass7` ("old" package). If you would like to contribute to `rgrass7`, please target your PRs and contributions to this branch, but note that onlly essential PRs will be considered. 

* The next release of `rgrass7` has been updated to give a startup message advising users to switch to `rgrass` and to deprecate all functionality.
