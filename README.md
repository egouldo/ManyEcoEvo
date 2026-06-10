

<!-- README.md is generated from README.qmd. Please edit that file -->

# ManyEcoEvo <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10046153.svg)](https://doi.org/10.5281/zenodo.10046153)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
<!-- badges: end -->

`ManyEcoEvo` provides a reusable workflow infrastructure for analysing
many-analyst studies - where multiple independent analyst teams answer
the same research questions using the same dataset. The package
implements a reproducible pipeline for executing meta-analyses across
multiple datasets and systematically varied analytical subsets, making
sensitivity analyses tractable and auditable.

Key features include:

- **Modular analysis functions** for standardising effect sizes, fitting
  multilevel meta-analytic models, fitting univariate regression models
  for analysing the effect of different analysis features on variation
  in effect sizes, extracting and visualising associated statistics
- A **Dataset-agnostic pipeline** that scales from single datasets to
  multiple datasets, and data subsets
- **Curated datasets** from the ‘Same Data, Many Analysts’ study in
  ecology and evolutionary biology[^1]
- **Reproducible infrastructure** built on `targets` and `renv` for
  transparent computational dependencies

While designed for many-analyst studies, the package also generalises to
conventional meta-analyses and sensitivity analyses.

## Installation

You can install the development version of ManyEcoEvo from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("egouldo/ManyEcoEvo")
```

# Relationship to the ManyEcoEvo manuscript

Raw, intermediate, and final analysis datasets in this package are
produced by a reproducible
[`targets::`](https://github.com/ropensci/targets) package pipeline
(`data-raw/tar_make.R`) and exported as package data objects. The
datasets and analysis functions from v2.7.6 of the package are used
directly by the Quarto manuscript of Gould et al. (2025) at
<https://egouldo.github.io/ManyAnalysts/>. To reproduce the
data-generation and analyses locally:

1.  Clone or download [this
    repository](https://github.com/egouldo/ManyEcoEvo)
2.  Run `renv::restore()` to load the packages used in the analysis
    pipeline locally on your machine (see
    [`renv::`](https://rstudio.github.io/renv/index.html) for details)
3.  Run `targets::tar_destroy("local")` to remove any record and caches
    of existing targets
4.  Run `targets::tar_make()` in your console, depending on your
    machine’s ahrdware, the analysis pipeline will take between 2 and 10
    minutes to execute
5.  You can view a table of all targets in the pipeline by running
    `targets::tar_meta()`
6.  To interact with objects or ‘targets’ within the analysis pipeline,
    call `targets::tar_load()` or `targets::tar_read()`:

``` r
targets::tar_read("ManyEcoEvo")
#> # A tibble: 2 × 4
#>   dataset    data                diversity_data      estimate_type
#>   <chr>      <list>              <named list>        <chr>        
#> 1 blue tit   <tibble [174 × 38]> <tibble [174 × 54]> Zr           
#> 2 eucalyptus <tibble [128 × 38]> <tibble [128 × 61]> Zr
```

Please see the documentation at <https://docs.ropensci.org/targets/> for
further detail.

## License

This software is licensed with the GNU GPL 3 license.

## Contributing

This package is released with a [Contributor Code of
Conduct](https://github.com/egouldo/ManyEcoEvo/blob/aa2b9dcb6462f35ce873418e0b9c9697cf0b2f24/CODE_OF_CONDUCT.md).

### Testing

This package uses the `{testthat}` framework for unit testing. You can
run the test suite locally using `devtools::test()`.

## Citation

To cite the package ‘ManyEcoEvo’ in publications use:

> Gould E, Fraser H, Nakagawa S, Parker T (2026). *ManyEcoEvo:
> Meta-analyse data from ‘Many-Analysts’ style studies*. R package
> version 2.7.8, <https://github.com/egouldo/ManyEcoEvo>.

A BibTeX entry for LaTeX users is

``` bibtex
@Manual{,
  title = {ManyEcoEvo: Meta-analyse data from 'Many-Analysts' style studies},
  author = {Elliot Gould and Hannah S. Fraser and Shinichi Nakagawa and Timothy H. Parker},
  year = {2025},
  note = {R package version 2.7.8},
  url = {https://github.com/egouldo/ManyEcoEvo},
  doi = {https://doi.org/10.5281/zenodo.13690949}
}
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Gould2025" class="csl-entry">

Gould, Elliot, Hannah S. Fraser, Timothy H. Parker, et al. 2025. “Same
Data, Different Analysts: Variation in Effect Sizes Due to Analytical
Decisions in Ecology and Evolutionary Biology.” *BMC Biology* 23 (1):
35. <https://doi.org/10.1186/s12915-024-02101-x>.

</div>

</div>

[^1]: Gould, E., Fraser, H. S., Parker, T. H., Nakagawa, S., Griffith,
    S. C., Vesk, P. A., Fidler, F., … Zitomer, R. A. (2025). Same data,
    different analysts: Variation in effect sizes due to analytical
    decisions in ecology and evolutionary biology. BMC Biology, 23(1),
    35. https://doi.org/10.1186/s12915-024-02101-x
