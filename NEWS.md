# ManyEcoEvo (development version)

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- Add very brief software example for JOSS to manuscript, setup manuscript as pkgdown article #158, #167
- Make existing manuscript a package vignette instead #59
- set graphics device and math-rendering format for pkgdown
- update citation using bibentry format
- Rewrite README introduction to better highlight package features, workflows, and infrastructure, following updated manuscript
- Update installation instructions to recommend using `pak`. #169
- Revise the instructions for reproducing the dataset and manuscript analysis pipeline. #167
- Update package citation to version 2.7.8, add the Zenodo DOI, and reference the published BMC Biology 2025 article. #169
- Fix target resources by passing an explicit integer (`6L`) to `nthreads` in `_targets.R` following upgrade to qs2 and targets version increment
- inline code for package name in article
- Add references for packages #167
- Update scaling up section and add code examples to ms_revised vignette
- docs: rm function from index due to error
* bug: fix threshold checking for multivariate model fitting #147
* docs: rearrange headings in NEWS.md
* bug: ensure outlier subset creation occurs on all `exclusion_set` values in Zr #144
* bug: exclude analysis with non-count-based dependent variable from `yi` analysis #145
* feat: #146 add function for excluding extreme estimates based on a multiplier threshold for population parameter estimates
* fix typo in article #146
* docs!: #146 `devtools::document()`
* bug: #146 export function

# ManyEcoEvo 2.7.6

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->
* Increment version number to 2.7.6

- #153 Revert back to ReviewerID random effect for Deviation ~ Categorical Ratings model
- [docs]!: `devtools::document()`
- [bug]!: #153 update deviation ~ peer-rating model structures
- [docs]: #102 wrap equations in `\eqn{}` instead of `$ $`
- [docs]!: `devtools::document()`
- #116 add argument check
- #97 rename `ManyEcoEvo` arg as `data`
- fix #75 ensure columns not rm in rating subset generation, and that review data cols are all re-nested
- add argument checking for summary fns #116 and update roxygen imports #102
- #151 rm reprex and investigation script
- fix!: #151 revert to taking sd() from normalised distribution prior to back-transformation and assigning as SE
- analysis #151 add additional reprex chunk to quiet output
- docs:! `devtools::document()`
- analysis #151 add reprex chunk options to quiet messages, add headings
- analysis: #151 investigate extreme precision after changing back-transformation fns
- bug, refactor!: extract SD in addition to SE for analysis #151
- refactor!: rm unnecessary arguments to `back_transform_response_vars_yi()`, update call of function within `prepare_response_variables_yi()` #97
- refactor!: change method for renaming prediction columns (generalise beyond BT / Euc)
- docs!: #102 add roxygen imports to function doc
- #151 retain sample size
- bug fix in arg checking #116
- #97 rename first data argument #102 update roxygen doc, add imports
* build!: #146 apply exclusion function to Eucalyptus dataset in targets pipeline
* #146 increment dev version and news before rebuilding package and targets pipeline
* - build!: don't forget to filter the corresponding diversity data after exclusions!
* - build!: fix #146 regenerate yi data after excluding extreme values
- build!: fix #146 regenerate yi data after excluding extreme values
- build!: don't forget to filter the corresponding diversity data after exclusions!
* Increment version number to 2.7.5
* docs: update changelog
*  bug: #146 export function
*  docs!: #146 `devtools::document()`
* feat: #146 add function for excluding extreme estimates based on a multiplier threshold for population parameter estimates
* bug: exclude analysis with non-count-based dependent variable from `yi` analysis #145
* bug: ensure outlier subset creation occurs on all `exclusion_set` values in Zr #144
* bug: fix threshold checking for multivariate model fitting #147
- build!: `usethis::use_data()` update results of `make_viz()`

# ManyEcoEvo 2.7.5

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- build!: `usethis::use_data()` update results of `make_viz()`
* - build!: force `tar_make()` for #140
* - build!: force `tar_make()` for #140, add targets meta

# ManyEcoEvo 2.7.4

- docs: Update function documentation #140
  - add `@detail` explaining functions applied and which output list-column they map onto
  - update `@return` since grouped columns are no longer output
  - add `@seealso`
  - add `@importFrom` call for `broom::glance()`
  - Add example
- feat: add `broom::glance()` back into `make_viz()` workflow #140

# ManyEcoEvo 2.7.3

- Fix #136 generate `Zr` outlier subsets on `exclusion_set == "complete"`, not on `exclusion_set == "partial"`

# ManyEcoEvo 2.7.2

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- separated column creation to occur under three conditions: NULL outcome_variable supplied, character string supplied, and expression argument supplied
- separated subset creation to occur separately on results of conditional evaluation
- Added conditional behaviour for when character vector supplied
- feat!: added arg checks #116 and cli output for when this condition is triggered
- explicitly supply `outcome_variable` and `outcome_SE` args for Zr
- #118 docs: Add explanation about updated behaviour when `estimate_type` is missing in `ManyEcoEvo` dataframe
- #118 build: devtools::document()

# ManyEcoEvo 2.6.0

- Update arg supply to targets call on prepare_response_variables() after #118 updates
- #118 add pmap internal helper function for differential application of transformation / standardisation in `standardise_response()`
- #118 delete old pmap helper function
- `dat` to `data` to help with auto-matching in pmap within prepare_response_variables() wrapper #118
- ensure all family fns have ... arg for pmap application in prepare_response_variables() since all fns have different argument lengths and names
- accidentally deleted when upgrading for #118, have added creation of transform_datasets tibbles for all cases now, and then these will apply the appropriate functions in final code chunk at end
- #118 ensure application of Z_VZ_preds takes the generalised colnames yi, yi_se instead of using hard-coded dataset application #97
- #118 call new arg `dataset_log_transform` in fn to log-transform outcomes for euc yi analysis
- #118 add log-transformation equivalent to `standardise_response()` and `process_resonse()`
- #102 add function documentation, including examples
- #118 extract `lower` and `upper` transformed vals in line with addition of `log_transform_response()` / changes to `standardise_response()`
- #116 check appropriate required variable (i.e. function needs `back_transformed_data`, but checked for `augmented_data` in `dat` arg, wouldn't throw required error because `augmented_data` was present in `dat`
- #102 add import, return, and see also roxygen doc tags, replace note with details tag, rename fn doc title
- #116 update argument checks conditional expression
- #118 match output to `log_transform_yi()` (now returns additional cols `lower` and `upper`, not only `c("Z","VZ")`)
- #118 match process to `log_transform_yi()` and #97 generalise processing to both euc/bt datasets without hard-coding dataset names in fns, and remove associated dataset-specific argument checking #116
- #118 adapt response variable preparation to accept additional argument `dataset_log_transform` apply argument checks #116, add roxygen param #102
- #118 adapt response variable processing to accept either/or/none for dataset standardisation/log-transformation.
- equivalent to `pred_to_z()`
- #102 write documentation
- #102 add import tags for `log_transform()` and link to equivalent functions, apply default argument values / checks
- #97 rename out argument
<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- Add very brief software example for JOSS to manuscript, setup manuscript as pkgdown article #158, #167
- Make existing manuscript a package vignette instead #59
- set graphics device and math-rendering format for pkgdown
- update citation using bibentry format
- Rewrite README introduction to better highlight package features, workflows, and infrastructure, following updated manuscript
- Update installation instructions to recommend using `pak`. #169
- Revise the instructions for reproducing the dataset and manuscript analysis pipeline. #167
- Update package citation to version 2.7.8, add the Zenodo DOI, and reference the published BMC Biology 2025 article. #169
- Fix target resources by passing an explicit integer (`6L`) to `nthreads` in `_targets.R` following upgrade to qs2 and targets version increment
- inline code for package name
- Add references for packages #167
- Update scaling up section and add code examples to ms_revised vignette
- docs: rm function from index due to error
* Increment version number to 2.7.5.9000
* bug: fix threshold checking for multivariate model fitting #147
* docs: rearrange headings in NEWS.md
* bug: ensure outlier subset creation occurs on all `exclusion_set` values in Zr #144
* bug: exclude analysis with non-count-based dependent variable from `yi` analysis #145
* feat: #146 add function for excluding extreme estimates based on a multiplier threshold for population parameter estimates
* fix typo #146
* docs!: #146 `devtools::document()`
* bug: #146 export function
* build!: #146 apply exclusion function to Eucalyptus dataset in targets pipeline
* #146 increment dev version and news before rebuilding package and targets pipeline
* - build!: don't forget to filter the corresponding diversity data after exclusions!
* - build!: fix #146 regenerate yi data after excluding extreme values
* Increment version number to 2.7.6


# ManyEcoEvo (development version)

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- Refactor package dependencies: move heavy packages to Suggests and use rlang::check_installed() (#169).
- Use tidy dependency list / tidy up DESCRIPTION (#169).
- Update formatting and build vignettes (#59).
- Fix cross-references, update missing .bib entries, remove old vignette files (#158, #167, #59).
- Update article citations and .bib (#158, #167, #59).
- Add data cleaning code (new helper/data-cleaning scripts).
- Reorganise vignettes and manuscripts; add new example code (thesis edits).
- Update manuscript with software-design section and .bib (#167).
- Add Research Impact section and references (#166).
- Organise pkgdown / roxygen documentation (#65).
- Revise manuscript to match JOSS author guidelines / article structure (#167).
- General upkeep for ManyEcoEvo (2026) — minor maintenance (#169).
- pkgdown: link to locally installed packages when website down (linking guidance) (#166, #169).

- #153 Revert back to ReviewerID random effect for Deviation ~ Categorical Ratings model
- [docs]!: `devtools::document()`
- [bug]!: #153 update deviation ~ peer-rating model structures
- [docs]: #102 wrap equations in `\\eqn{}` instead of `$ $`
- [docs]!: `devtools::document()`
- #116 add argument check
- #97 rename `ManyEcoEvo` arg as `data`
- fix #75 ensure columns not rm in rating subset generation, and that review data cols are all re-nested
- add argument checking for summary fns #116 and update roxygen imports #102
- #151 rm reprex and investigation script
- fix!: #151 revert to taking sd() from normalised distribution prior to back-transformation and assigning as SE
- analysis #151 add additional reprex chunk to quiet output
- docs:! `devtools::document()`
- analysis #151 add reprex chunk options to quiet messages, add headings
- analysis: #151 investigate extreme precision after changing back-transformation fns
- bug, refactor!: extract SD in addition to SE for analysis #151
- refactor!: rm unnecessary arguments to `back_transform_response_vars_yi()`, update call of function within `prepare_response_variables_yi()` #97
- refactor!: change method for renaming prediction columns (generalise beyond BT / Euc)
- docs!: #102 add roxygen imports to function doc
- #151 retain sample size
- bug fix in arg checking #116
- #97 rename first data argument #102 update roxygen doc, add imports

# ManyEcoEvo 2.7.6

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- build!: fix #146 regenerate yi data after excluding extreme values
- build!: don't forget to filter the corresponding diversity data after exclusions!

* Increment version number to 2.7.5
* docs: update changelog
*  bug: #146 export function
*  docs!: #146 `devtools::document()`
* feat: #146 add function for excluding extreme estimates based on a multiplier threshold for population parameter estimates
* bug: exclude analysis with non-count-based dependent variable from `yi` analysis #145
* bug: ensure outlier subset creation occurs on all `exclusion_set` values in Zr #144
* bug: fix threshold checking for multivariate model fitting #147
- build!: `usethis::use_data()` update results of `make_viz()`

# ManyEcoEvo 2.7.5

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- build!: `usethis::use_data()` update results of `make_viz()`
* - build!: force `tar_make()` for #140
* - build!: force `tar_make()` for #140, add targets meta

# ManyEcoEvo 2.7.4

- docs: Update function documentation #140
  - add `@detail` explaining functions applied and which output list-column they map onto
  - update `@return` since grouped columns are no longer output
  - add `@seealso`
  - add `@importFrom` call for `broom::glance()`
  - Add example
- feat: add `broom::glance()` back into `make_viz()` workflow #140

# ManyEcoEvo 2.7.3

- Fix #136 generate `Zr` outlier subsets on `exclusion_set == \"complete\"`, not on `exclusion_set == \"partial\"`

# ManyEcoEvo 2.7.2

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- separated column creation to occur under three conditions: NULL outcome_variable supplied, character string supplied, and expression argument supplied
- separated subset creation to occur separately on results of conditional evaluation
- Added conditional behaviour for when character vector supplied
- feat!: added arg checks #116 and cli output for when this condition is triggered
- explicitly supply `outcome_variable` and `outcome_SE` args for Zr
- #118 docs: Add explanation about updated behaviour when `estimate_type` is missing in `ManyEcoEvo` dataframe
- #118 build: devtools::document()

# ManyEcoEvo 2.6.0

- Update arg supply to targets call on prepare_response_variables() after #118 updates
- #118 add pmap internal helper function for differential application of transformation / standardisation in `standardise_response()`
- #118 delete old pmap helper function
- `dat` to `data` to help with auto-matching in pmap within prepare_response_variables() wrapper #118
- ensure all family fns have ... arg for pmap application in prepare_response_variables() since all fns have different argument lengths and names
- accidentally deleted when upgrading for #118, have added creation of transform_datasets tibbles for all cases now, and then these will apply the appropriate functions in final code chunk at end
- #118 ensure application of Z_VZ_preds takes the generalised colnames yi, yi_se instead of using hard-coded dataset application #97
- #118 call new arg `dataset_log_transform` in fn to log-transform outcomes for euc yi analysis
- #118 add log-transformation equivalent to `standardise_response()` and `process_resonse()`
- #118 call new arg `dataset_log_transform` in fn to log-transform outcomes for euc yi analysis
- #118 add log-transformation equivalent to `standardise_response()` and `process_resonse()`
- #102 add function documentation, including examples
- #118 extract `lower` and `upper` transformed vals in line with addition of `log_transform()` / changes to `standardise_response()`
- #116 check appropriate required variable (i.e. function needs `back_transformed_data`, but checked for `augmented_data` in `dat` arg, wouldn't throw required error because `augmented_data` was present in `dat`
- #102 add import, return, and see also roxygen doc tags, replace note with details tag, rename fn doc title
- #116 update argument checks conditional expression
- #118 match output to `log_transform_yi()` (now returns additional cols `lower` and `upper`, not only `c(\"Z\",\"VZ\")`)
- #118 match process to `log_transform_yi()` and #97 generalise processing to both euc/bt datasets without hard-coding dataset names in fns, and remove associated dataset-specific argument checking #116
- #118 adapt response variable preparation to accept additional argument `dataset_log_transform` apply argument checks #116, add roxygen param #102
- #118 adapt response variable processing to accept either/or/none for dataset standardisation/log-transformation.
- equivalent to `pred_to_z()`
- #102 write documentation
- #102 add import tags for `log_transform()` and link to equivalent functions, apply default argument values / checks
- #97 rename out argument
