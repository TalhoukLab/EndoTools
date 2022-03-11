# EndoTools 0.3.6

* Add `assign_ace_vars()` to get numeric scores assigned for each ACE variable
* Rename ACE-27 score labels depending on `separate_nos` condition

# EndoTools 0.3.5

* Fix internal input validation function

# EndoTools 0.3.4

* Add parameter `separate_nos` to handle NOS cases in ACE comorbidities

# EndoTools 0.3.3

* Add ACE variables to `emdb`
* Add `assign_ace27()` to assign ACE-27 score
* Randomly add 10% missing data to each variable in `emdb`

# EndoTools 0.3.2

* Change criteria "Stage I–IVA p53abn endometrial carcinoma with myometrial invasion, with no residual disease" to be irrespective of histotype
* Add optional parameter for residual disease in `assign_esmo2016()` for high and advanced risk groups
* Microscopic residual disease is regarded as no residual disease
* Add residual disease condition for non-endometrioids in ESMO 2016 "high" case
* Remove non-endometrioid condition in ESMO 2020 "intermediate" case for Stage IA, no myometrial invasion
* In all ESMO 2020 risk groups involving residual disease, allow residual disease to also be missing if stage is low (I-II). If high stage, residual disease must be explicitly none.
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
