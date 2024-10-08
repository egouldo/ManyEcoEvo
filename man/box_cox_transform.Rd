% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_cox_transform.R
\name{box_cox_transform}
\alias{box_cox_transform}
\title{Box-cox transform absolute deviation from the meta-analytic mean scores}
\usage{
box_cox_transform(data, dataset, outcome_SE_colname)
}
\arguments{
\item{data}{Dataset for model fitting, must contain columns \code{"abs_deviation_score_estimate"} and standard error}

\item{dataset}{character string of length 1 either "blue tit" or "eucalyptus"}
}
\value{
data with additional columns of box-cox transformed deviation scores and variance
}
\description{
Box-cox transform absolute deviation from the meta-analytic mean scores
}
\seealso{
\code{\link[=variance_boxcox]{variance_boxcox()}}, \code{\link[=folded_params]{folded_params()}}

Other Box-Cox transformation: 
\code{\link{folded_params}()},
\code{\link{variance_box_cox}()}

Other Analysis-level functions: 
\code{\link{Z_VZ_preds}()},
\code{\link{apply_sorensen_calc}()},
\code{\link{back_transform_response_vars_yi}()},
\code{\link{convert_predictions}()},
\code{\link{est_to_zr}()},
\code{\link{folded_params}()},
\code{\link{pmap_wrap}()},
\code{\link{rename_prediction_cols}()},
\code{\link{variance_box_cox}()}
}
\concept{Analysis-level functions}
\concept{Box-Cox transformation}
