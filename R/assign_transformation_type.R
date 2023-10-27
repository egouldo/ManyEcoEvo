#' Assign back-transformation type to be applied to analysis point-estimates
#'
#' @param response_transformation Character vector of length 1L containing the analysis response transformation
#' @param link_fun Character vector of length 1L containing the analysis link function
#'
#' @return A character vector of length 1L 
#' @export
#' @importFrom dplyr case_when
#' @importFrom rlang is_na
#' @importFrom rlang na_chr
assign_transformation_type <- function(response_transformation = character(1L),
                                       link_fun = character(1L)) {
  
  # # Link-Fun: Set back.transformed to "identity"
  # link_fun <- dplyr::case_when(
  #   link_fun == "back.transformed" ~ "identity",
  #   TRUE ~ link_fun
  # )
  # 
  # # Response Variable Transformation: Set back.transformed to NA
  # response_transformation <- dplyr::case_when(
  #   response_transformation == "back.transformed" ~ NA,
  #   TRUE ~ response_transformation
  # )
  transformation_type <- case_when(
    response_transformation %in% "back.transformed" & link_fun %in% "back.transformed" ~ "identity",
    response_transformation %in% "back.transformed" & link_fun %in% "identity" ~ "identity",
    response_transformation %in% "back.transformed" & is_na(link_fun) ~ "identity",
    response_transformation %in% "back.transformed" & !is_na(link_fun) & link_fun %nin% "identity" & link_fun %nin% "back.transformed" ~ paste(link_fun),
    is_na(response_transformation) & link_fun %in% "back.transformed" ~ "identity",
    is_na(response_transformation) & link_fun %in% "identity" ~ "identity",
    is_na(response_transformation) & is_na(link_fun) ~ "identity",
    is_na(response_transformation) & !is_na(link_fun) & link_fun %nin% "back.transformed" & link_fun %nin% "identity" ~ paste(link_fun),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & link_fun %in% "back.transformed" ~ paste(response_transformation),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & link_fun %in% "identity" ~ paste(response_transformation),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & is_na(link_fun) ~ paste(response_transformation),
    !is_na(response_transformation) & response_transformation %nin% "back.transformed" & is_na(link_fun) & link_fun %nin% "back.transformed" & link_fun %nin% "identity" ~ "double_transformation",
    TRUE ~ na_chr
  )
  
  return(transformation_type)
}