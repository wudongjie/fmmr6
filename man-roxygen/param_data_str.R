#' @param data_str (`character(1)`)\cr
#' Desired data structure.
#' Available data structure are listed as follows:
#' 
#' `default`: Default data structure, the DataModel object will identify
#' the dependent variable `Y` and the independent variable `X` based on 
#' the provided formula.
#' 
#' `freq_tab`: the frequency table. The data must contain a `freq` variable.
#' 
#' `long`: Data in the long format. Normally used for a longitudinal/panel data.
#' The data must contain a variable to identify the changes within observation, 
#' e.g. the observations made at different time `t`. One should also define a variable
#' for identifying each observation, e.g. `obs`.
#' 
#' `wide`: Data in the wide format. This data format collapses a longitudinal data
#' into one row for one observation.
#' 
#' `longc`: Data in the long format used in a choice model. Variable `alt` represents
#' the alternative for each individual. Variable `chosen` shows which `alt` is chosen.
#' 
#' `widec` Data in the wide format used in a choice model.
