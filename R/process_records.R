#' Process raw jsPsych data
#'
#' @param x Dataset with jsPsych records
#' @param .using Parser function to use on each record. Use `NULL` for auto-detection.
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
#' }
process_records <- function(x, .using = NULL) {
  if (is.null(.using)) {
    stop("Auto-detection not yet implemented.")
  }
  x %>%
    mutate(
      processed = map(raw, ~.using(.x))
    )
}
