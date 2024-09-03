# ManyEcoEvo (development version)

<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

- build!: `usethis::use_data()` update results of `make_viz()`
* - build!: force `tar_make()` for #140
* - build!: force `tar_make()` for #140, add targets meta


# ManyEcoEvo 2.7.5

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
