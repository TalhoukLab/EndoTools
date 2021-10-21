# EndoTools 0.3.2

* Add optional parameter for residual disease in `assign_esmo2016()` for high and advanced risk groups
* Microscopic residual disease is regarded as no residual disease
* Add residual disease condition for non-endometrioids in ESMO 2016 "high" case
* Remove non-endometrioid condition in ESMO 2020 "intermediate" case for Stage IA, no myometrial invasion
* In ESMO 2020 "high" case Stage I-IVA, endometrioid, >0% myometrial invasion, p53abn, (no residual disease if known), allow residual disease to also be missing if stage is low (I-II). If high stage, residual disease must be explicitly none.
* Return value of all `assign_esmo*()` functions are now factors with levels appropriately ordered

# EndoTools 0.3.1

* Fix conditions for residual disease

# EndoTools 0.3.0

* Add parameter validation using standardized vocabulary, (#3)
* Use standardized variable names, (#7)
* Add optional parameter for residual disease in `assign_esmo2020()`, (#6)
* Remove `hist` input to `assign_esmo2020()`, (#5)

# EndoTools 0.2.0

* Add examples in documentation
* Document simulated data `emdb`
* Store factor levels for variables in `emdb`
* Update README example

# EndoTools 0.1.0

* Helper tools to assign ESMO risk groups, ProMisE classifications, and MMR status
* Added a `NEWS.md` file to track changes to the package.
