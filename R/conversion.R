#' Apply back-transformation to beta estimates
#' @description Conditionally apply back-transformation functions depending on the value of `transformation`.
#'
#' @param beta Beta estimate, numeric vector of length 1.
#' @param se Standard error of the `beta` estimate, numeric vector of length 1
#' @param transformation Character string describing transformation
#' @param sim Number of simulations to use during back-transformation. Defaults to $10000$.
#'
#' @return The outputs of a back-transformation function, see family back-transformations
#' @details `transformation` character strings may take the values:
#' * "log"
#' * "logit"
#' * "probit"
#' * "square"
#' * "probit"
#' * "double_transformation"
#' * "cube"
#' * "inverse"
#' * "square_root"
#' * "powerX", where `X` is a numeric
#' * "divided.by.X", where `X` is a numeric
#' @export
#' @import dplyr
#' @importFrom purrr discard is_scalar_vector pluck
#' @importFrom cli cli_alert_danger cli_alert_warning cli_ol cli_alert_success
#' @importFrom rlang is_na
#' @importFrom stringr str_detect str_split
#' @family Back-transformation
#' @seealso [conversion_2()], [back()]
conversion <- function(beta, se, transformation, sim = 10000) {
  # ----- Argument Checking -----
  na_args <- purrr::discard(c(beta, se, transformation), is.na) %>%
    length()
  
  if (na_args < 3) {
    cli::cli_alert_danger("Required values for back-transformation missing:")
    cli::cli_alert_warning("Returning {.val NA} for tupple:")
    cli::cli_ol(c(
      "beta {.val {beta}},",
      "se {.val {se}},",
      "with {.val {transformation}} transformation."
    ))
    return(NA)
  }
  
  stopifnot(
    purrr::is_scalar_vector(sim),
    is.numeric(beta),
    is.numeric(se),
    is.character(transformation)
  )
  
  # ----- Apply Back Transformations -----
  if (transformation == "log") {
    log_back(beta, se, sim)
  } else if (transformation == "logit") {
    logit_back(beta, se, sim)
  } else if (transformation == "probit") {
    probit_back(beta, se, sim)
  } else if (transformation == "square") {
    square_back(beta, se, sim)
  } else if (transformation == "cube") {
    cube_back(beta, se, sim)
  } else if (transformation == "inverse") {
    inverse_back(beta, se, sim)
  } else if (transformation == "square_root") {
    square_root_back(beta, se, sim)
  } else if (transformation == "(power3)/100") {
    x100 <- divide_back(beta, se, sim, 100)
    cube_back(x100$mean_origin, x100$se_origin, sim = 1000)
  } else if (stringr::str_detect(transformation, "power")) {
    n <- str_split(transformation, "power") %>%
      pluck(1, 2) %>%
      as.numeric()
    if (rlang::is_na(n)) {
      return(data.frame(mean_origin = NA, m_est = NA, se_origin = NA, lower = NA, upper = NA))
    } else {
      power_back(beta, se, sim, n)
    }
  } else if (stringr::str_detect(transformation, "divided")) { # divided by n
    n <- stringr::str_split(string = transformation, pattern = "[.]") %>%
      pluck(1, 3) %>%
      as.numeric()
    if (rlang::is_na(n)) {
      return(data.frame(mean_origin = NA, m_est = NA, se_origin = NA, lower = NA, upper = NA))
    } else {
      divide_back(beta, se, sim, n)
    }
  } else if (transformation == "double_transformation") {
    return(data.frame(mean_origin = NA, m_est = NA, se_origin = NA, lower = NA, upper = NA))
  } else {
    identity_back(beta, se, sim) # TODO change conditional logic to ensure strange transformations not put through here
  }
}
