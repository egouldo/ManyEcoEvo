#' Negative Value Matching
#'
#' See \code{base::[](`%in%`)} for details. `%nin%` is a binary operator, 
#' returning a logical vector indicating if there is a negative match or not.
#'
#' @name %nin%
#' @rdname NotIn
#' @keywords internal
#' @export
#' @usage lhs \%in\% rhs
#' @param lhs vector or NULL: the values to be matched. [Long vectors](http://127.0.0.1:59782/help/library/base/help/Long%20vectors) are supported.
#' @param rhs vector or NULL: the values to be matched. [Long vectors](http://127.0.0.1:59782/help/library/base/help/Long%20vectors) are supported.
#' @return A logical vector indicating which value of `lhs` are *not* matched in `rhs`
#' @examples
#' "A" %nin% LETTERS[1:10]
#' "A" %in% LETTERS[1:10]
`%nin%` <- Negate(`%in%`)

#' Subsetting Functions for effect-size meta-analysis
#'
#' @description Generates a list of functions that are used to subset the processed [ManyEcoEvo::] dataset
#'
#' @return A named list of `lambda` functions
#' @export
subset_fns_Zr <- function() { # TODO update calling of this fn (switch to fn rather than object)
  out <- list(
    subset_complete = rlang::as_function(~ .x %>%
      filter(
        exclusions_all == "retain",
        exclusions_effect_analysis %in%
          c("retain", "exclude_partial")
      )),
    subset_partial = rlang::as_function(~ .x %>%
      filter(
        exclusions_all == "retain",
        exclusions_effect_analysis %in%
          c("retain")
      ))
  )
  return(out)
}

#' Subsetting Functions for out-of-sample predictions meta-analysis
#'
#' @description Generates a list of functions that are used to subset the 
#' processed ManyEcoEvo dataset (e.g.`data(ManyEcoEvo_results`) containing 
#' out-of-sample predictions $y_i$. 
#' 
#' @details 
#' The subset functions are used to filter out predictions that are not 
#' to be included in the meta-analysis.
#'
#' @return A named list of `lambda` functions
#' @export
subset_fns_yi <- function() {
  out <- list( # TODO: which dataset and variable are the prediction exclusions contained??
    subset_complete = rlang::as_function(~ .x %>%
      filter(exclusions_all == "retain"))
  )

  return(out)
}

#' Capitalise Words
#'
#' @param s character string containing words to be captalised
#' @param strict whether to capitalise all words or not
#'
#' @return a character string whose words are now capitalised
#' @export
#'
#' @examples
#' capwords("bah, bah, black sheep")
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "",
      collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' Removes infinite and NA values from a dataframe of standardised effects
#'
#' @param effects_analysis A dataframe containing the standardised effects
#' @param Z_colname unquoted or bare column name with the $Z$ or $Z_r$ estimates
#' @param VZ_colname unquoted or bare column name containing the $VZ$ or $\\text{VZ}_r$ estimates
#'
#' @return a dataframe without
#' @export
#' @examples
#' data(ManyEcoEvo_results)
#' ManyEcoEvo_results %>% 
#' pluck("effects_analysis", 1) %>% 
#' rm_inf_na(beta_estimate, beta_SE)
rm_inf_na <- function(effects_analysis, Z_colname, VZ_colname) {
  effects_analysis %>%
    filter(
      !is.na({{ Z_colname }}),
      !is.na({{ VZ_colname }}),
      !is.infinite({{ Z_colname }}),
      !is.infinite({{ VZ_colname }})
    )
}

#' Split data frame by groups and name elements
#'
#' @param .data A tbl.
#' @param grouping_variable Unquoted variable name to group columns by
#' @details
#' Function will fail if character string is provided to `grouping_variable` instead of bare variable name.
#'
#' @return A named list of tibbles. Each tibble contains the rows of .tbl for the associated group and all the columns, including the grouping variables. Note that this returns a list_of which is slightly stricter than a simple list but is useful for representing lists where every element has the same type.
#' @export
#' @importFrom dplyr group_split
#' @examples
#' data(euc_data)
#' data(blue_tit_data)
#' named_group_split(euc_data, Property)
#' named_group_split(blue_tit_data, hatch_Area)
named_group_split <- function(.data, grouping_variable) {
  .data %>%
    group_by({{ grouping_variable }}) %>%
    group_split(.keep = TRUE) %>%
    set_names(., {
      map_chr(
        .,
        ~ pluck(.x, paste0(rlang::ensym(grouping_variable))) %>%
          unique()
      )
    }) %>%
    map(., ~ select(.x, -{{ grouping_variable }}))
}
