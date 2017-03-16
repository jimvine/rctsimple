# (c) Jim Vine
# Author: Jim Vine
# Some documentation for the package as a whole


#' rctsimple: A package for analysing data from randomised controlled trials.
#'
#' The rctsimple package provides functions in two broad categories:
#' functions for calculating effect size measures and methods for calculating
#' confidence intervals based on these measures.
#'
#' @section Effect size measure functions:
#' The following functions are provided to calculate effect size measures:
#' \itemize{
#'   \item \code{\link{prevalence_difference}}, an absolute measure
#'   \item \code{\link{prevalence_ratio}}, a relative measure
#' }
#'
#' @section Confidence intervals:
#' Methods are provided for the \code{confint} function for the objects created
#' by the effect size measure functions. See \code{\link{confint-rctsimple}}.
#'
#' @docType package
#' @name rctsimple
NULL
