#' Process raw jsPsych data
#'
#' @param x Dataset with jsPsych records
#' @param .using Parser function to use on each record. Use `NULL` for auto-detection.
#' @param auto List of trial types and parser functions used for auto-detection.
#'
#' @return Original dataset with new column `processed` with nested results.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-html-button-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$html_button_response) %>%
#'   process_records(.using = parse_html_button_response) %>%
#'   unnest(processed)
#' # auto-detected
#' d %>%
#'   filter(trial_type == trial_types$html_button_response) %>%
#'   process_records() %>%
#'   unnest(processed)
#' }
process_records <- function(x, .using = NULL, auto = default_parsers()) {
  if (is.null(.using)) {
    # auto-detection
    # check if all trial types are same
    types <- x %>% pull("trial_type")
    if (length(unique(types)) > 1) stop("All trials must be of the same type. \n  Here: ", paste(unique(types), collapse = ", "))
    .using <- auto[[types[1]]]
    if (is.null(.using)) stop("Parser for", types[1], " not defined")
    # continue as if specified
  }
  # parser provided by user (.using)
  x %>%
    mutate(
      processed = map(raw, ~.using(.x))
    )
}
