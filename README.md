
# ManyEcoEvo <img src="man/figures/ManyEcoEvoHex.jpg" align="right" width="120"/>

The `ManyEcoEvo` package provides a suite of functions for:

- Summarising, analysing and visualising the ManyEcoEvo dataset from
  *Gould et al.*[^1]
- Tidying and cleaning many-analyst style data for further analysis
- Reproducing the analysis in *Gould et al.* using your own many-analyst
  style data

> Note that the manuscript source-code for *Gould et al.* is located in
> a separate repository at <https://github.com/egouldo/ManyAnalysts>,
> which can be viewed at <https://egouldo.github.io/ManyAnalysts/>.

## Installation

`ManyEcoEvo::` can be installed using
[`devtools::`](https://devtools.r-lib.org) from GitHub with:

``` r
devtools::install_github("egouldo/ManyEcoEvo")
```

# Regenerating the ManyEcoEvo dataset

The data processing and analysis can be freely reproduced with the help
of the [`targets::`](https://github.com/ropensci/targets) package.
Please see the documentation at <https://docs.ropensci.org/targets/> for
further detail.

Should you wish to completely reproduce the dataset generation and
analysis in *Gould et al.*, complete the following steps:

1.  Clone or download [https://github.com/egouldo/this
    repository](https://github.com/egouldo/ManyEcoEvo)
2.  Run `renv::restore()` to load the packages used in the analysis
    pipeline locally on your machine (see
    \[`renv::`\]https://rstudio.github.io/renv/index.html) for details)
3.  Run `tar_destroy()` to remove any record and caches of existing
    targets
4.  Run `targets::tar_make()` in your console, depending on the power of
    your machine, the analysis pipeline will take between 2 and 7
    minutes to execute (plus or minus some!)
5.  You can view a table of all targets in the pipeline by running
    `targets::tar_meta()`
6.  To interact with objects or ‘targets’ within the analysis pipeline,
    call `targest::tar_load()` or `targets::tar_read()`:

``` r
targets::tar_read("ManyEcoEvo")
```

    # A tibble: 2 × 4
      dataset    data                diversity_data      estimate_type
      <chr>      <list>              <named list>        <chr>        
    1 blue tit   <tibble [174 × 38]> <tibble [174 × 54]> Zr           
    2 eucalyptus <tibble [128 × 38]> <tibble [128 × 61]> Zr           

## License

This software is licensed with the GNU GPL 3 license [![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Contributing

This package is released with a [Contributor Code of
Conduct](https://github.com/egouldo/ManyEcoEvo/blob/aa2b9dcb6462f35ce873418e0b9c9697cf0b2f24/CODE_OF_CONDUCT.md)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)

## Citation

    To cite package ‘ManyEcoEvo’ in publications use:

      Gould E, Fraser H, Nakagawa S, Parker T (2023). _ManyEcoEvo: Meta-analyse
      data from 'Many-Analysts' style studies_. R package version 1.0.0,
      <https://github.com/egouldo/ManyEcoEvo>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {ManyEcoEvo: Meta-analyse data from 'Many-Analysts' style studies},
        author = {Elliot Gould and Hannah S. Fraser and Shinichi Nakagawa and Timothy H. Parker},
        year = {2023},
        note = {R package version 1.0.0},
        url = {https://github.com/egouldo/ManyEcoEvo},
      }

[^1]: Gould, E., Fraser, H., Parker, T. *et al.* (2023). Same data,
    different analysts: Variation in effect sizes due to analytical
    decisions in ecology and evolutionary biology \[Preprint\]. Ecology
    and Evolutionary Biology. https://doi.org/10.32942/X2GG62
