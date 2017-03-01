# (c) Jim Vine
# Author: Jim Vine
# Prevalence ratio and related functions



# =============================================================================
# Generic
# =============================================================================


#' Prevalence ratio of an outcome between groups.
#'
#' Calculates the prevalence ratio of a binary / dichotomous outcome
#' between two or more groups, from individual or aggregate data.
#'
#' @param outcome_data
#'   Data providing the outcomes against the groups. Can be provided as raw
#'   (i.e., participant-level) data, detailing row-by-row the the group and
#'   outcome for each participant. Can also be provided as aggregate data.
#'   Valid input types are:
#'     data.frame with 2 columns and a row for each participant, where the
#'       first column contains the participant's group, second column
#'       contains the participant's outcome;
#'     matrix containing a contingency table of the groups against the
#'       outcomes, with 2 columns (one for the 'outcome occurred' state and
#'       one for the 'outcome did not occur' state), and a named row for each
#'       group;
#'     list with 4 named elements, group1_t, group1_f, group2_t and group2_f,
#'       giving the number of participants in each category, where group1 is
#'       the control group and group2 is the intervention group, and t is the
#'       'outcome occurred' state and f is the 'outcome did not occur' state.
#' @param groups
#'   Character vector of the groups in the order in which they should be
#'   analysed, starting with the control group. For list data the parameter
#'   will define the labels to be applied in the output, and default values
#'   will be used if not specified.
#' @param outcomes
#'   Character vector of length 2 specifying the labels used in the data for
#'   the two outcome states, with the 'outcome occurred' state listed first and
#'   the 'outcome did not occur' state second. For list data the parameter will
#'   define the labels to the be applied in the output, and default values
#'   will be used if not specified.
#' @return The prevalence ratio, indicating the prevalence in group 2, relative
#'   to group 1.
#' @examples
#' dummy_rct_data_list   <- list(group1_t = 48, group1_f = 52,
#                               group2_t = 64, group2_f = 36)
#' prevalence_ratio(outcome_data = dummy_rct_data_list,
#'                       groups = c("Control group", "Intervention group"),
#'                       outcomes = c("Outcome occurred", "Outcome did not occur"))
#' # Data will be reordered if necessary.
#' dummy_rct_data_matrix <- matrix(data=c(36, 64,
#'                                        52, 48),
#'                                   ncol=2, byrow=TRUE,
#'                                   dimnames = list(c("Group 1 - new treatment",
#'                                                     "Control group"),
#'                                                   c("No","Yes")))
#' prevalence_ratio(outcome_data = dummy_rct_data_matrix,
#'                       groups = c("Control group", "Intervention group"),
#'                       outcomes = c("Yes", "No"))

prevalence_ratio <- function(outcome_data,
                                  groups,
                                  outcomes,
                                  data_name = NULL) {
  UseMethod("prevalence_ratio", outcome_data)
}



# =============================================================================
# Default method
# =============================================================================
# Used to give an error if an un-handled data type is passed.

prevalence_ratio.default <- function(outcome_data, ...) {
  stop(paste("Unknown type. This function has methods for taking raw data as",
             "a data.frame or processed data as a table (matrix) or named",
             "list."))
}



# =============================================================================
# Data.frame method
# =============================================================================
# Takes raw data.
# First column must be the Groups. Second must be the outcomes.
# (If other columns etc., re-jig it first.)

prevalence_ratio.data.frame <- function(outcome_data,
                                             groups,
                                             outcomes,
                                             data_name = NULL) {

  # Validate the input --------------------------------------------------------

  # Check for a 2-column data.frame.
  # Could just take the first 2 columns, but safer to force the user to
  #   specifically drop surplus columns to ensure that we use the right ones.
  if(ncol(outcome_data)!=2){
    stop(paste("When passing a data.frame you must provide exactly 2 columns,",
               "one containing the participants' groups and one containing",
               "their outcomes. The object received had too",
               ifelse(ncol(outcome_table)>2,"many","few"), "columns."))}

  # Create the contingency table from the outcomes data.frame
  outcome_table <- table(outcome_data, useNA = "ifany")

  # This is going to be checked when passed to the table-based method anyway,
  # but helpful to check here to give a more meaningful error message.
  if(ncol(outcome_table)!=2){
    stop(paste("The outcomes column of the data must be dichotomous. Exactly",
               "2 values must exist across all rows, one for each participant",
               " in the 'outcome occurred' state and one for each in the",
               "'outcome did not occur' state. The object received had too",
               ifelse(ncol(outcome_table)>2,"many","few"), "values."))}


  if(is.null(data_name)) {
    data_name <- deparse(substitute(outcome_data))
  }

  pr <- prevalence_ratio(outcome_table,
                         groups = groups,
                         outcomes = outcomes,
                         data_name = data_name)
}





# TODO: How to document these for the list data type?
# @param group1_t
#   The number of participants in group 1 for whom the outcome occured.
# @param group1_f
#   The number of participants in group 1 for whom the outcome did not occur.
# @param group2_t
#   The number of participants in group 2 for whom the outcome occured.
# @param group2_f
#   The number of participants in group 2 for whom the outcome did not occur.


# =============================================================================
# List method
# =============================================================================
# Works on list objects containing 4 elements.
# Elements are group1_t, group1_f, group2_t, group2_f

prevalence_ratio.list <- function(outcome_data,
                                  groups = c("Control group",
                                             "Intervention group"),
                                  outcomes = c("Outcome occurred",
                                               "Outcome did not occur"),
                                  data_name = NULL) {

  # Validate the input: outcome_data list -------------------------------------

  # Check the outcome_data argument
  arg_check_outcome_data_list_dichotomous(outcome_data)

  # Validate the input: groups ------------------------------------------------

  # Check the groups argument (special version for exactly 2 groups required)
  arg_check_groups_2(groups)

  # Create data_name if not set -----------------------------------------------

  if(is.null(data_name)) {
    data_name <- deparse(substitute(outcome_data))
    # data_name <- if (is.null(data_name)) {
    # paste("Contingency table comprising values",
    #       group1_t,group1_f,group2_t,group2_f)}
  }

  # Set up matrix to pass to workhorse function -------------------------------

  outcome_matrix <- matrix(data=c(outcome_data$group1_t, outcome_data$group1_f,
                                  outcome_data$group2_t, outcome_data$group2_f),
                           ncol=2, byrow=TRUE,
                           dimnames = list(groups, outcomes))

  pr <- prevalence_ratio(outcome_matrix,
                              groups = groups,
                              outcomes = outcomes,
                              data_name = data_name)

  return(pr)

}








# =============================================================================
# Matrix method
# =============================================================================
# Works on matrix objects containing a contingency table.
# Columns are outcomes (ncol must = 2)
# Rows are groups (nrow must be >=2)
# Items assembled using matrix() are TRUE for is.matrix but FALSE for is.table.
# Whereas contingency tables made with table() are TRUE for both.
# This is the workhorse function. All other methods end up here.

prevalence_ratio.matrix <- function(outcome_data,
                                         groups,
                                         outcomes,
                                         data_name = NULL) {

  # Validate the input: rows / groups -----------------------------------------

  # Check the groups (rows) of the outcome_data argument
  arg_check_outcome_data_matrix_groups(outcome_data)
  # Check the groups argument
  arg_check_groups(groups)
  # Check the groups argument against outcome_data
  args_check_groups__outcome_data_matrix(groups,outcome_data)


  # Validate the input: columns / outcomes ------------------------------------

  # Check the outcomes (columns) of the outcome_data argument
  arg_check_outcome_data_matrix_outcomes_dichotomous(outcome_data)
  # Check the outcomes argument
  #   (These will be run in the list method anyway, but checking here ensures
  #   the relevant meaningful error would be thrown, rather than getting a less
  #   meaningful error if the code tries to do something with it.)
  arg_check_outcomes_dichotomous(outcomes)
  # Check the outcomes argument against outcome_data
  args_check_outcomes__outcome_data_matrix(outcomes,outcome_data)

  # Validate the input: count data --------------------------------------------

  # Check every cell contains valid count data (zero or positive integer)
  for(row_num in 1:(nrow(outcome_data))){
    arg_check_count_data(outcome_data[row_num,1])
    arg_check_count_data(outcome_data[row_num,2])
  }


  # Validate the input: outcomes ----------------------------------------------

  # Check the outcomes argument
  arg_check_outcomes_dichotomous(outcomes)




  # Create data_name if not set -----------------------------------------------

  if(is.null(data_name)) {
    data_name <- deparse(substitute(outcome_data))
    }


  # Reorder the dataset -------------------------------------------------------

  # Sort the rows based on the groups argument
  outcome_data <- outcome_data[order(match(rownames(outcome_data),groups)),]

  # Sort the columns based on the outcomes argument
  outcome_data <- outcome_data[,order(match(colnames(outcome_data),outcomes))]


  # Two arm or multi-arm? -----------------------------------------------------
  if(nrow(outcome_data)==2){
    # 2 arm so can just run the calculations


    # Extract the counts from the matrix

    group1_t <- outcome_data[1,1]
    group1_f <- outcome_data[1,2]
    group2_t <- outcome_data[2,1]
    group2_f <- outcome_data[2,2]


    # Calculate prevalence in each group ----------------------------------------

    p_group1 <- group1_t / (group1_t + group1_f)
    p_group2 <- group2_t / (group2_t + group2_f)


    # Calculate ratio of prevalences ---------------------------------------

    pr <- p_group2 / p_group1



    # Assemble elements for output ----------------------------------------------

    names(pr) <- "prevalence ratio"


    pr_details <- list(estimate = pr,
                       p_group1 = p_group1,
                       p_group2 = p_group2,
                       method="Prevalence ratio estimate",
                       data.name = data_name,
                       data = outcome_data)

    class(pr_details) <- "prevalence_ratio"

    # TODO: Consider alternative ways of holding these.
    #       ? As part of $data.name
    #       ? As a new named element within the htest list
    #       ? In the $method text
    #       ? In names( $estimate)
    # Attaching the group labels to the returned htest object as a "groups" attr.
    attr(pr_details, "groups") <- groups

    return(pr_details)



  }
  else {
    # Multi-arm. Compare row 1 to each subsequent row in turn
    # First initiate an empty list. This permits list objects to be put as
    #   items of the list.
    prlist <- list(NULL)

    # Iterate through comp_num (comparison number).
    for(comp_num in 1:(nrow(outcome_data)-1)){

      # Comparing row 1 (control group row) to each other row in turn.
      outcome_data_2 <- outcome_data[c(1,comp_num+1),]

      # Extract the 2 relevant groups
      groups_2 <- groups[c(1, comp_num+1)]

      # Assemble the results into a list.
      prlist[[comp_num]] <-
        prevalence_ratio(outcome_data_2,
                              groups = groups_2,
                              outcomes = outcomes,
                              data_name = data_name)
    }
    return(prlist)
  }
}









# =============================================================================
# Confidence interval : Prevalence ratio
# =============================================================================
# Provide a confint method for prevalence_ratio objects.
# confint is from ::stats

# Uses the common approximation, implicilyt including assumptions of
# normality. Will not work well for small sample sizes. Also, does not check
# validity of bounds. For example, with an absolute measure where the only
# valid values are in the region -1 to +1 (prevalence_ratio) with an
# estimate close to 1, the returned CI may extend past 1.


## TODO: Do I need to provide any documentation when providing a method for
##       an existing generic?
## Yes, seems useful. See this, f.e.:
###   https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/confint.html

# @param level
#   Confidence level for confidence intervals to be calculated at.


confint.prevalence_ratio <- function(object,
                                     parm = "estimate",
                                     level = 0.95, ...) {


  # Validate the input: parm ---------------------------------------------

  # Check that parm is from the list of valid options.
  parm <- match.arg(arg = parm,
                    choices = c("estimate",
                                "p_group1",
                                "p_group2"),
                    several.ok = TRUE)

  # Validate the input: level --------------------------------------

  arg_check_confidence_level(level)


  # Extract the values from the object

  group1_t <- object$data[1,1]
  group1_f <- object$data[1,2]
  group2_t <- object$data[2,1]
  group2_f <- object$data[2,2]

  estimate <- object$estimate
  p_group1 <- object$p_group1
  p_group2 <- object$p_group2

  # Also need them in a single named vector of numbers
  values <- c(object$estimate, object$p_group1, object$p_group2)
  names(values) <- c("estimate", "p_group1", "p_group2")


  # Calculate standard errors -------------------------------------------------
  # (Needed for establishing confidence interval)

  se_ln_pr <- sqrt((1 / group1_t) + (1 / group2_t) -
                   (1 / (group1_t + group1_f)) -
                   (1 / (group2_t + group2_f)))


  # Actual formula for SE in a given sample includes 1-f term, the fraction
  # of the population sampled. We assume an arbitrarily large population, so
  # f tends to 0, and 1-f tends to 1.
  # http://www.fao.org/wairdocs/ILRI/x5436E/x5436e07.htm#5.2.3 estimating a population proportion or rate from a simple random sample

  se_p1 <- sqrt((p_group1 * (1 - p_group1)) / (group1_t + group1_f))
  se_p2 <- sqrt((p_group2 * (1 - p_group2)) / (group2_t + group2_f))


  # Calculate confidence intervals --------------------------------------------

  # For a given CI, 1/2 of (1-level) is above the top of the CI and
  #                 1/2 of it is below.
  # E.g. For 0.95 confidence level, 2.5% is above the top and 2.5% is below the
  #    bottom. Or conversely, the top boundary is the 97.5% quantile, so
  #    convert 0.95 to 0.975 and get the 97.5% quantile on the normal distrib
  #    (=~1.96). Similarly for 2.5% quantile as ~=-1.96.

  level_end <- (1 - level) / 2
  level_ends_pc <- c(paste(level_end * 100, "%"),
                     paste((1-level_end) * 100, "%"))
  z <- qnorm(c(level_end, 1 - level_end))

  ci <- matrix(data = NA,
               nrow = length(parm),
               ncol = 2,
               dimnames = list(parm, level_ends_pc))

  # A prevalence ratio must be >= 0
  if ("estimate" %in% rownames(ci)) {
    ci["estimate",]  <- exp((se_ln_pr * z) + log(estimate))
    ci["estimate",1] <- max(ci["estimate", 1], 0)
  }


  # A prevalence must be in the range [0,1]
  if ("p_group1" %in% rownames(ci)) {
    ci["p_group1",]  <- p_group1 + se_p1 * z
    ci["p_group1",1] <- max(ci["p_group1", 1], 0)
    ci["p_group1",2] <- min(ci["p_group1", 2], 1)
  }

  if ("p_group2" %in% rownames(ci)) {
    ci["p_group2",]  <- p_group2 + se_p2 * z
    ci["p_group2",1] <- max(ci["p_group2", 1], 0)
    ci["p_group2",2] <- min(ci["p_group2", 2], 1)
  }

  return(ci)
}


