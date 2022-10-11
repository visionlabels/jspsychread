#' Wrapper for possible null objects
#'
#' @param x Object to check
#'
#' @return The same object or NA if the object is null.
#' @export
#'
#' @examples
#' single_value(1)
#' single_value(NA)
#' single_value(NULL)
#' single_value(1:3)
single_value <- function(x) { if (is.null(x)) NA else x }


#' Convert list of lists into a matrix
#'
#' We expect the same representation as in serial-reaction-time-mouse:
#' Each inner array represents a single row.
#' The entries in the inner arrays represent the columns.
#'
#' @param x List of lists, expecting all inner-lists of the same length
#' @param byrow logical. If TRUE (the default) the matrix is filled by rows,
#' otherwise the matrix is filled by columns.
#'
#' @return Matrix
#' @export
#'
#' @examples
#' l <- list(list(1:3), list(4:6))
#' list2_to_matrix(l)
list2_to_matrix <- function(x, byrow = T) {
  nrows <- length(x)
  values <- unlist(x)
  n <- length(values)
  matrix(values, nrow = nrows, ncol = n / nrows, byrow = byrow)
}

#' Demo files
#'
#' Returns a path for example data.
#'
#' @param fn File to read from the demo folder
#'
#' @return File path for given demo file
#'
#' The list of available demo files:
#'
#' - jspsych-animation.json
#' - jspsych-audio-button-response.json
#' - jspsych-audio-keyboard-response.json
#' - jspsych-audio-slider-response.json
#' - jspsych-call-function.json
#' - jspsych-canvas-button-response.json
#' - jspsych-canvas-keyboard-response.json
#' - jspsych-canvas-slider-response.json
#' - jspsych-free-sort.json
#' - jspsych-fullscreen.json
#' - jspsych-html-button-response.json
#' - jspsych-html-keyboard-response.json
#' - jspsych-html-slider-response.json
#' - jspsych-image-button-response.json
#' - jspsych-image-keyboard-response.json
#' - jspsych-image-slider-response.json
#' - jspsych-instructions.json
#' - jspsych-serial-reaction-time-mouse.json
#' - jspsych-serial-reaction-time.json
#' - jspsych-survey-html-form.json
#' - jspsych-survey-likert.json
#' - jspsych-survey-multi-choice.json
#' - jspsych-survey-multi-select.json
#' - jspsych-survey-text.json
#' - jspsych-video-button-response.json
#' - jspsych-video-keyboard-response.json
#' - jspsych-video-slider-response.json
#'
#' @export
#'
#' @examples
#' demo_file("jspsych-html-button-response.json")
demo_file <- function(fn) {
  fnx <- system.file("testdata", fn, package = "jspsychread")
  stopifnot(file.exists(fnx))
  fnx
}

# note how to generate file list
#   dir("inst/testdata/") %>% str_c(collapse = "\n#' - ") %>% cat()
