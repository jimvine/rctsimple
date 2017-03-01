# (c) Jim Vine
# Author: Jim Vine
# Function to derive qualitative conclusions from confidence intervals




# =============================================================================
# Conclusion from Confidence Interval
# =============================================================================

#' Descriptive conclusions from confidence intervals.
#'
#' Produces a descriptive conclusion from a confidence interval, depending
#' on the type of test specified (superiority, non-inferiority or equivalence).
#'
#' @param ci A single row from a matrix of the type returned by confint(),
#'   containing the confidence interval for the parameter estimate. The two
#'   columns provide the lower and upper confidence limits.
#' @param groups
#'   Character vector of length 2 containing descriptive labels for the
#'   the interventions received by each group, with the control group label
#'   first and the intervention group label second.
#' @param beneficial_outcome Is the outcome to be treated as beneficial
#'   (i.e., a higher value of the outcome is superior)?
#' @param test_type
#'   Type of test to be conducted (superiority, noninferiority or equivalence).
#' @param margin Numerical value specifying the non-inferiority or equivalence
#'    margin to be used if conducting those test types.
#' @param actual_null The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. For superiority tests
#'   this is the point value that the confidence interval is compared at. For
#'   non-inferiority / equivalence tests it is the starting point that the
#'   margin is applied to in order to establish the point / region for
#'   comparison.
#' @return A list object with elements stating the conclusion in different
#'   formats.
#' @examples
#' # Establish a test confidence interval
#' ci_test <- matrix(c(-0.1,0.1),
#'                   nrow = 1, dimnames = list("estimate",
#'                                             c("2.5 %","97.5 %")))
#' conc <- ci_conclusion(ci_test)
#' conc <- ci_conclusion(ci_test, test_type = "noninferiority", margin=0.15)
#' conc <- ci_conclusion(ci_test, test_type = "equivalence", margin=0.15)
#' conc <- ci_conclusion(ci_test, test_type = "equivalence", margin=0.05)

ci_conclusion <- function(ci,
                          groups = c("Control intervention",
                                     "Test intervention"),
                          beneficial_outcome = TRUE,
                          test_type = c("superiority",
                                        "noninferiority",
                                        "equivalence"),
                          margin = 0,
                          actual_null = 0) {


  # Validate the input: ci ----------------------------------------------------

  arg_check_ci(ci)

  # Check that ci[2] contains the upper confidence limit.
  # If not, swap them and throw a warning.
  if(ci[2] < ci[1]) {
    ci[] <- ci[c(2,1)]
    warning(paste("The lower and upper confidence limits seem to be the",
                  "wrong way round. They have been swapped. Check that you",
                  "have not passed an unexpected type of object."))
  }


  # Validate the input: groups ------------------------------------------------

  # Check the groups argument
  arg_check_groups_2(groups)


  # Validate the input: beneficial_outcome ------------------------------------

  if (!is.logical(beneficial_outcome)) {
    stop("beneficial_outcome must be logical (TRUE or FALSE)")
  }


  # Validate the input: test_type ---------------------------------------------

  # Check that test_type is one of the valid options
  #   and set to first (i.e. default) option if not set.
  test_type <- match.arg(test_type)


  # Validate the input: margin ------------------------------------------------

  args_check_margin__test_type(margin, test_type)


  # Validate the input: actual_null -------------------------------------------

  arg_check_actual_null(actual_null)


  # Test according to test_type -----------------------------------------------

  if (test_type == "superiority") {

    is_superior <- ifelse (beneficial_outcome,
                           ci[1] > actual_null,
                           ci[2] < actual_null)

    is_inferior <- ifelse (beneficial_outcome,
                           ci[2] < actual_null,
                           ci[1] > actual_null)

    if (is_superior) {
      conclusion_short <- "Superior"
      conclusion       <- paste(groups[2], "superior to", groups[1])
      conclusion_md    <- paste(groups[2], "**superior** to", groups[1])
    } else if (is_inferior) {
      conclusion_short <- "Inferior"
      conclusion       <- paste(groups[2], "inferior to", groups[1])
      conclusion_md    <- paste(groups[2], "**inferior** to", groups[1])
    } else {
      conclusion_short <- "Inconclusive"
      conclusion       <- paste("Inconclusive:", groups[2],
                                "not shown to be superior to", groups[1])
      conclusion_md    <- paste("**Inconclusive**:", groups[2],
                                "not shown to be superior to", groups[1])
    }


  } else if (test_type == "noninferiority") {
    margin_value <- ifelse (beneficial_outcome,
                            actual_null - margin,
                            actual_null + margin)

    is_noninferior <- ifelse (beneficial_outcome,
                              ci[1] > margin_value,
                              ci[2] < margin_value)

    is_superior <- ifelse (beneficial_outcome,
                           ci[1] > actual_null,
                           ci[2] < actual_null)

    is_not_superior <- ifelse (beneficial_outcome,
                               ci[2] < actual_null,
                               ci[1] > actual_null)

    is_inferior <- ifelse (beneficial_outcome,
                           ci[2] < margin_value,
                           ci[1] > margin_value)

    if (is_superior) {
      conclusion_short <- "Superior"
      conclusion       <- paste(groups[2], "superior to", groups[1])
      conclusion_md    <- paste(groups[2], "**superior** to", groups[1])
    } else if (is_inferior) {
      conclusion_short <- "Inferior"
      conclusion       <- paste(groups[2], "inferior to", groups[1])
      conclusion_md    <- paste(groups[2], "**inferior** to", groups[1])
    } else if (is_noninferior) {
      if (is_not_superior) {
        conclusion_short <- "Non-inferior"
        conclusion       <- paste(groups[2], "non-inferior to", groups[1])
        conclusion_md    <- paste(groups[2], "**non-inferior** to", groups[1])
      } else {
        conclusion_short <- "Non-inferior"
        conclusion       <- paste(groups[2], "non-inferior to", groups[1],
                                  "but not shown to be superior")
        conclusion_md    <- paste(groups[2], "**non-inferior** to", groups[1],
                                  "but not shown to be superior")
      }
    } else {
      if (is_not_superior) {
        conclusion_short <- "Inconclusive"
        conclusion       <- paste("Inconclusive:", groups[2],
                                  "not shown to be non-inferior to",
                                  groups[1])
        conclusion_md    <- paste("**Inconclusive**:", groups[2],
                                  "not shown to be non-inferior to",
                                  groups[1])
      } else {
        conclusion_short <- "Inconclusive"
        conclusion       <- paste("Inconclusive:", groups[2],
                                  "not shown to be superior or non-inferior to",
                                  groups[1])
        conclusion_md    <- paste("**Inconclusive**:", groups[2],
                                  "not shown to be superior or non-inferior to",
                                  groups[1])
      }
    }

  } else {

    margin_top <- actual_null + margin

    margin_bottom <- actual_null - margin

    is_equivalent <- (ci[1] > margin_bottom) && (ci[2] < margin_top)

    is_superior <- ifelse (beneficial_outcome,
                           ci[1] > margin_top,
                           ci[2] < margin_bottom)

    is_inferior <- ifelse (beneficial_outcome,
                           ci[2] < margin_bottom,
                           ci[1] > margin_top)

    if (is_superior) {
      conclusion_short <- "Non-equivalent"
      conclusion       <- paste("Non-equivalent:", groups[2],
                                "superior to", groups[1])
      conclusion_md    <- paste("**Non-equivalent**:", groups[2],
                                "superior to", groups[1])
    } else if (is_inferior) {
      conclusion_short <- "Non-equivalent"
      conclusion       <- paste("Non-equivalent:", groups[2],
                                "inferior to", groups[1])
      conclusion_md    <- paste("**Non-equivalent**:", groups[2],
                                "inferior to", groups[1])
    } else if (is_equivalent) {
      conclusion_short <- "Equivalent"
      conclusion       <- paste(groups[2], "equivalent to", groups[1])
      conclusion_md    <- paste(groups[2], "**equivalent** to", groups[1])
    } else {
      # is_not_superior <- ifelse (beneficial_outcome,
      #                            ci[2] < margin_top,
      #                            ci[1] > margin_bottom)
      #
      # is_not_inferior <- ifelse (beneficial_outcome,
      #                            ci[1] < margin_bottom,
      #                            ci[2] > margin_top)

      conclusion_short <- "Inconclusive"
      conclusion       <- paste("Inconclusive:", groups[2],
                                "not shown to be equivalent to", groups[1])
      conclusion_md    <- paste("**Inconclusive**:", groups[2],
                                "not shown to be equivalent to", groups[1])
    }
  }


  concs <- list(conclusion = conclusion,
                conclusion_short = conclusion_short,
                conclusion_md = conclusion_md)

  # class(concs) <- "conclusion_ci"
}
