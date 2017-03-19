# (c) Jim Vine
# Author: Jim Vine
# Confint documentation



#' Confidence Intervals for Objects' Parameters
#'
#' Calculates confidence intervals for one or more parameters in an effect size
#' measure object. Package \code{\link{rctsimple}} adds methods for
#' \code{\link{prevalence_difference}} and \code{\link{prevalence_ratio}}
#' objects.
#'
#' \code{\link{confint}} is a generic function in the package \code{stats}.
#'
#' These \code{confint} methods calculate the relevant confidence intervals
#' for the objects created in the \code{rctsimple} package.
#'
#' The \code{...} argument is included for consistency with the \code{confint}
#' definition in  \code{stats} (and \code{MASS}). No additional arguments are
#' used by the methods provided in this package.
#'
#' @param object
#'   an object containing an effect size estimate (and possibly other
#'   parameters). Methods currently exist for the classes
#'   \code{prevalence_difference} and \code{prevalence_ratio}.
#'
#' @param parm
#'   Vector of names specifying which parameters confidence intervals,
#'   are to be calculated for. Defaults to just calculating the confidence
#'   interval for the effect size estimate. Valid options are c("estimate",
#'   "p_group1", "p_group2")
#'
#' @param level
#'   Confidence level for confidence intervals to be calculated at.
#'
#' @param ...
#'   additional argument(s) for methods (provided for consistency with generic)
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence
#'   limits for each parameter. These will be labelled as (1 - level) / 2 and
#'   1 - (1 - level) / 2 in \% (by default 2.5\% and 97.5\%).
#' @examples
#' dummy_rct_data_list   <- list(group1_t = 48, group1_f = 52,
#'                               group2_t = 64, group2_f = 36)
#' pd <- prevalence_difference(outcome_data = dummy_rct_data_list,
#'                       groups = c("Control group", "Intervention group"),
#'                       outcomes = c("Outcome occurred", "Outcome did not occur"))
#' pr <- prevalence_ratio(outcome_data = dummy_rct_data_list,
#'                       groups = c("Control group", "Intervention group"),
#'                       outcomes = c("Outcome occurred", "Outcome did not occur"))
#' confint(pd, c("estimate","p_group1","p_group2"))
#' confint(pr, c("estimate","p_group1","p_group2"))
#' @name confint-rctsimple
NULL
