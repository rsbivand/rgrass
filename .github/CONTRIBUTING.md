### Contributing to rgrass

This outlines how to propose a change to `rgrass` as we move to support [GRASS 8.0](https://github.com/OSGeo/grass). The `rgrass` package is the successor to the `rgrass7` package. The current plan is for `rgrass` to support both GRASS 7 and 8, but for `rgrass7` to be retired.

### How to propose a change to rgrass

To propose a change to `rgrass`, please consider the roles assigned to the different branches in this repository.

* The `rgrass7` branch is the "target" maintenance source for the package `rgrass7` ("old" package). If you would like to contribute to `rgrass7`, please target your PRs and contributions to this branch.

* The `rgrass` branch is the development branch for moving to support GRASS 8.0. If you would like to contribute to `rgrass` ("new" package), please target your PRs and contributions to this branch.

* The `main` branch remains the branch for maintenance updates (bug fixes), until support for GRASS 8.0 is achieved (changes on `rgrass7` are merged into `main`).

* Until the `rgrass` branch is ready for release, `main` and `rgrass7` are the same (changes on `rgrass7` are merged into `main`).

* When `rgrass` is ready for submission to CRAN, `rgrass` is merged with `main`, and `rgrass7` will be updated to give a startup message advising users to switch to `rgrass`.
