# (c) Jim Vine
# Author: Jim Vine
# Some documentation for the package as a whole


#' rctsimple: A package for analysing data from randomised controlled trials.
#'
#' The rctsimple package provides functions in three broad categories:
#' functions for calculating effect size measures, methods for calculating
#' confidence intervals based on these measures, and a function for extracting
#' qualitative conclusions from confidence intervals.
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
#' by the effect size measure functions.
#'
#' @section Conclusions from confidence intervals:
#' A function, \code{\link{ci_conclusion}}, is provided to output simple
#' qualitative conclusions from confidence intervals, based on the type of
#' tests being conducted.
#'
#' Qualitative conclusions from confidence intverals, with sharp cut-offs
#' defined at particular boundaries, are susceptible to many of the same
#' concerns as hypothesis testing based on p-values. In considering the point
#' estimate alongside the confidence interval as a measure of uncertainty,
#' there is some improvement over simply identifying an effect size estimate
#' and a measure of 'statistical significance'.
#'
#' @docType package
#' @name rctsimple
NULL