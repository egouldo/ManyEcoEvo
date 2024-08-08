#' Anonymise ManyEcoEvo Data
#'
#' @param df A dataframe containing the column `id_col`
#' @param lookup A dataframe containing the columns `TeamIdentifier` and `New_Identifier`
#'
#' @return A `df` with anonymised values of `id_col` based on the `New_Identifier` colum of `lookup`
#' @export
#' @importFrom pointblank col_vals_not_null
#' @import tidyr
#' @import dplyr
anonymise_teams <- function(df, lookup) { # TODO actually... this is anonymise_id_col()
  df %>%
    separate(id_col,
      into = c("TeamIdentifier", "id_string"), # but what if TeamIdentifier exists??
      sep = "-",
      extra = "merge",
      remove = FALSE
    ) %>%
    left_join(lookup, by = "TeamIdentifier") %>%
    col_vals_not_null(columns = c(TeamIdentifier, New_Identifier)) %>%
    unite(col = "id_col_anon", New_Identifier, id_string, sep = "-", remove = FALSE) %>%
    select(-TeamIdentifier, -id_string, -id_col) %>%
    rename(
      TeamIdentifier = New_Identifier,
      id_col = id_col_anon
    )
}
