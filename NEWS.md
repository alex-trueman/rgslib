# 0.0.1 Early Release

* Initial functions for reading and writing GeoEase format.

# 0.0.2 New interfaces to CCG Fortran programs.

* Interfaces to `ADDCOORD`, `DECLUS`, `UNSCORE`, `VARCALC`, `VARMAP`, `VARMODEL`, and `VL_LMC`.
* Improvments to read and write functions.
* New function `read_gslib_mvario` imports GSLIB variogram model formet files as `gstat` variogramModel class data frames.
* `read_gslib` and `write_gslib` now use `data.table` `fread` and `fwrite` for better performance.
* Improved documentation and package compliance.

