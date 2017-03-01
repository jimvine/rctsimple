# (c) Jim Vine
# Author: Jim Vine
# Checker functions to test various arguments etc. are correct


# Some functions to check various arguments being passed to main functions.
#
# Naming convention: use double underscore __ between two arg names in args
# checks, to distinguish from a single underscore in the middle of an arg
# name or just between other bits.

# Outcomes ---------------------------------------------------------------------

# Argument checker function for outcomes where it should be dichotomous
arg_check_outcomes_dichotomous <- function(outcomes) {
  # Check that outcomes is specified
  if(is.null(outcomes)){
    stop(paste("The outcomes argument must be provided, specifying the label",
               "for the 'outcome occurred' state then the one for the",
               "'outcome did not occur' state."))
  }

  # Check exactly two elements in the outcomes argument.
  if(length(outcomes)!=2){
    stop(paste("Exactly 2 elements required in outcomes, one for the 'outcome",
               "occurred' label and one for the 'outcome did not occur'",
               "label. The object received had too",
               ifelse(length(outcomes)>2,"many","few"), "elements."))
  }

  # Check no duplicated elements in the outcomes argument.
  if(outcomes[1] == outcomes[2]){
    stop(paste0("The two elements in outcome must be unique. Both contain: ",
                outcomes[1]))
  }
}


# Argument checker function for outcome_data as a matrix where it should be
# dichotomous
arg_check_outcome_data_matrix_outcomes_dichotomous <- function(outcome_data){
  # Check exactly two columns (outcomes) in the outcome_data table.
  if(ncol(outcome_data)!=2){
    stop(paste("If outcome_data is a contingency table (table or matrix data",
               "type) it must have exactly 2 columns, one for the 'outcome",
               "occurred' counts and one for the 'outcome did not occur'",
               "counts. The object received had too",
               ifelse(ncol(outcome_data)>2,"many","few"), "columns."))
  }

  # Check the columns are named.
  if(is.null(colnames(outcome_data))){
    stop(paste0("outcome_data needs to have column names. Object received ",
                "has NULL names. Depending on which way round your data are ",
                "you could try naming them with something like these:\n",
                "  colnames(outcome_data) <- c('Outcome occurred','Outcome did not occur')\n",
                "  colnames(outcome_data) <- c('Outcome did not occur','Outcome occurred')"))
  }

  # Check colnames are unique in the outcome_data dataset.
  if(colnames(outcome_data)[1] == colnames(outcome_data)[2]){
    stop(paste0("The two columns in outcome_data share the name:\n  ",
                colnames(outcome_data)[1]))
  }
}

# Arguments checker for outcomes against outcome_data (as a matrix)
args_check_outcomes__outcome_data_matrix <- function(outcomes,outcome_data){
  # Check outcomes argument contains the same elements as cols in outcome_data.
  if(!setequal(outcomes, colnames(outcome_data))){
    stop(paste0("The outcomes specified in the outcomes argument do not ",
                "match the outcomes in the outcome_data. The outcomes found ",
                "in outcome_data are:\n  ",
                paste0(colnames(outcome_data), collapse = "\n  ")))
  }
}


# Groups ----------------------------------------------------------------------


# Argument checker function for groups
arg_check_groups <- function(groups){

  # Check that groups is specified.
  #   (Would fail a subsequent check anyway but this provides a more helpful
  #    error msg.)
  if(is.null(groups)){
    stop(paste("The groups argument must be provided, specifying the order",
               "for analysis with the control group first."))
  }

  # Check no duplicated names in the groups argument.
  if(anyDuplicated(groups)>0){
    stop(paste0("Multiple groups with the same name. At least two of the ",
                "elements in groups argument share the following name(s):\n  ",
                paste0(unique(groups[duplicated(groups)]), collapse = "\n  ")))
  }

  # Check at least two elements in the groups argument.
  if(length(groups)<2){
    stop(paste("At least 2 elements required in groups, one for each group to",
               "be compared. The object received had too few rows."))}
}

# Argument checker for groups where only 2 groups are allowed.
arg_check_groups_2 <- function(groups){
  # All the normal checks.
  arg_check_groups(groups)


  # Check exactly two elements in the groups argument.
  if(length(groups)!=2){
    stop(paste("Exactly 2 elements expected in groups for this method.",
               "The object received had too",
               ifelse(length(groups)>2,"many","few"), "elements."))
  }
}


# Argument checker function for the groups in outcome_data as a matrix
arg_check_outcome_data_matrix_groups <- function(outcome_data) {
  # Check at least two rows (groups) in the outcome_data table.
  if(nrow(outcome_data)<2){
    stop(paste("If outcome_data is a contingency table (table or matrix data",
               "type) it must have at least 2 rows, one for each group to be",
               "compared. The object received had too few rows."))}

  # Check the rows are named.
  if(is.null(rownames(outcome_data))){
    stop(paste("Rows of outcome_data matrix are not named. rownames should",
               "contain the name of each group."))
  }

  # Check rownames are unique in the outcome_data dataset.
  if(anyDuplicated(rownames(outcome_data))>0){
    stop(paste0("Multiple groups with the same name. At least two of the ",
                "rows in outcome_data share the following name(s):\n  ",
                paste0(unique(rownames(outcome_data)[
                  duplicated(rownames(outcome_data))]), collapse = "\n  ")))
  }
}


# Arguments checker for groups against outcome_data (as a matrix)
args_check_groups__outcome_data_matrix <- function(groups,outcome_data){

  # Check groups argument contains the same elements as rows in outcome_data.
  if(!setequal(groups, rownames(outcome_data))){
    stop(paste0("The groups specified in the groups argument do not match ",
                "the groups in the outcome_data. The groups found in ",
                "outcome_data are:\n  ",
                paste0(rownames(outcome_data), collapse = "\n  ")))
  }
}


# Data as list ----------------------------------------------------------------

arg_check_outcome_data_list_dichotomous <- function(outcome_data){
  # Check 4 elements in the list
  if(length(outcome_data)!=4) {
    stop(paste("Exactly 4 elements required in outcome_data when calling the",
               "list method. The object received had too",
               ifelse(length(outcome_data)>4,"many","few"), "elements."))
  }

  # Check the 4 elements are called the right things
  valid_list_names <- c("group1_t", "group1_f", "group2_t", "group2_f")
  if(!setequal(names(outcome_data), valid_list_names)){
    stop(paste0("The elements specified in the outcome_data list argument ",
                "do not match the list of elements expected. ",
                "The elements found in outcome_data are:\n  ",
                paste0(names(outcome_data), collapse = "\n  "),
                "\nThe expected elements are:\n  ",
                paste0(valid_list_names, collapse = "\n  ")))
  }


}


# Single number args ----------------------------------------------------------

arg_check_count_data <- function(value){
  # For testing data that are 'count data' - i.e., counts of outcomes.
  # So only integer or zero is valid.
  # Check that the passed value is a single finite positive or zero number.
  # Check the value is an integer (within tolerance).

  if (missing(value) ||
      (length(value) != 1) ||
      !is.finite(value) ||
      (value < 0)) {
    stop(paste("The values of count data passed must",
               "all be present and each must be a single finite numerical",
               "value, greater than or equal to zero."))
  }

  # Integer check uses approach from one of the answers here:
  # http://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
  if (min(abs(c(value %% 1, value %% 1 - 1))) > 0.0001){
    warning(paste("The values of count data passed must",
                  "all be integers."))
  }

}



arg_check_confidence_level <- function(confidence_level){

  if (missing(confidence_level) ||
      (length(confidence_level) != 1) ||
      !is.finite(confidence_level) ||
      (confidence_level <= 0) ||
      (confidence_level >= 1)) {
    stop(paste("The confidence_level must be present and must be a single",
               "finite numerical value. It must be greater than 0",
               "(typically at least 0.9) and less than 1.",
               if (confidence_level >= 90 && confidence_level < 100){
                 paste("Please express as a decimal rather than percentage,",
                       "for example 0.95 instead of 95.")
               }))
  }

  # In practice, a confidence_level below, say, 0.9 is unlikely to be chosen.
  # Can't throw a STOP error, since it is technically acceptable, if unlikely,
  # but can give a WARNING hint.

  if (confidence_level < 0.9) {
    warning(paste("The confidence_level is < 0.9. Are you sure you want",
                  "that? More typical values for confidence_level include",
                  "0.90, 0.95 and 0.99."))
  }
}



# Argument checker function for margin for absolute measures
# NOT currently in use, since don't have a way of identifying absolute
# measures.
# Most of this is checked in the version with test_type included,
# just without the requirement for <1.
# arg_check_margin_abs <- function(margin){
#
#   # For absolute measures, margin has to be >0 and <1, since prevalence is
#   # always between 0 and 1, so difference between 2 prevalences must also be
#   # between 0 and 1 (or -1, depending on which way round).
#   # A margin of more than 1 definitely means accepting all values.
#   #
#   # This won't quite apply to relative measures - can technically have
#   # meaningful ratios of more than 1, though above certain levels will
#   # still want to throw a warning.
#
#   if (missing(margin) ||
#       (length(margin) != 1) ||
#       !is.finite(margin) ||
#       (margin < 0) ||
#       (margin >= 1)) {
#     stop(paste("The margin must be present and must be a single",
#                "finite numerical value. It must be at least 0",
#                "and less than 1 (typically less than 0.2).",
#                if (margin <= 20 && margin >= 1){
#                  paste("Please express as a decimal rather than percentage,",
#                        "for example 0.02 instead of 2.")
#                }))
#   }
#
#   # In practice, a margin above, say, 0.2 (20%) is less likely to be chosen.
#   # It might indicate the user was meaning to specify, for example,
#   # 0.5%, which should be entered as 0.005, but accidentally put 0.5
#   # (which means 50%). Can't throw a STOP error, since it is technically
#   # acceptable, if unlikely, but can give a WARNING hint.
#
#   if (margin > 0.2) {
#     warning(paste("The margin is > 0.2. Are you sure you want that?",
#                   "Note that margin is expressed in decimals,",
#                   "so 0.5 means 50%, not 0.5%"))
#   }
# }


# Argument checker function for margin and test_type
args_check_margin__test_type <- function(margin, test_type){

  # Check margin appropriate to test_type. For superiority should be 0 or NULL.
  if (test_type == "superiority") {
    if (!is.null(margin) &
        (margin != 0 ||
         length(margin) != 1)
        ) {
      stop(paste("The argument margin has been set as something other than",
                 "zero but the test_type has been specified as superiority."))
    }
  }

  # Check margin appropriate to test_type. For types other than superiority
  # should be 0 or NULL.
  if (test_type != "superiority") {
    if (margin == 0) {
      stop(paste("The argument margin has been set to 0",
                 "but the specified test_type requires a margin > 0."))
    }

    if (missing(margin) ||
        (length(margin) != 1) ||
        !is.finite(margin) ||
        (margin < 0)) {
      stop(paste("The margin must be present and must be a single",
                 "finite numerical value. It must be at least 0.",
                 "It is typically less than 0.2."))
      }

    # In practice, a margin above, say, 0.2 (20%) is less likely to be chosen.
    # It might indicate the user was meaning to specify, for example,
    # 0.5%, which should be entered as 0.005, but accidentally put 0.5
    # (which means 50%). Can't throw a STOP error, since it is technically
    # acceptable, if unlikely, but can give a WARNING hint.

    if (margin > 0.2) {
      warning(paste("The margin is > 0.2. Are you sure you want that?",
                    "Note that margin is expressed in decimals,",
                    "so 0.5 means 50%, not 0.5%"))
    }
  }
}


# Argument checker function for actual_null
arg_check_actual_null <- function(actual_null){

  if (missing(actual_null) ||
      (length(actual_null) != 1) ||
      !is.finite(actual_null)) {
    stop(paste("The actual_null must be present and must be a single",
               "finite numerical value."))
  }


  if (actual_null != 0 & actual_null != 1) {
    warning(paste("The actual_null is not 0 or 1. Are you sure you want that?",
                  "Note that the actual_null is the value for a",
                  "mathematically zero result, and is typically 0 in absolute",
                  "measures or 1 in relative measures. Use the margin",
                  "parameter to specify a margin of comparison away from",
                  "the actual null point."))
  }
}


# Confidence Interval ---------------------------------------------------------

arg_check_ci <- function(ci) {
  # Check that ci is specified
  if(is.null(ci)){
    stop(paste("The ci argument must be provided."))
  }

  # Check exactly two elements in the ci argument.
  if(length(ci)!=2){
    stop(paste("Exactly 2 elements required in ci, one for the lower",
               "confidence limit, the other for the upper confidence limit.",
               "The object received had too",
               ifelse(length(ci)>2,"many","few"), "elements."))
  }

  # Check no duplicated elements in the ci argument.
  if(ci[1] == ci[2]){
    stop(paste0("The two elements in ci must be unique. Both contain: ",
                ci[1]))
  }
}
