#' Reads jsPsych data into tibble
#'
#' By default, the code expects a file with a single JSON structure
#' with white spaces and line delimiters `(single = T)`.
#' The alternative behaviour `(single = F)` expects a format with multiple
#' JSON records, each in a single line.
#' Results stored in JATOS server follow this structure.
#'
#' @param filepath File to read
#' @param single Indicates whether the file contains a single record
#' or multiple records (each on separate line)
#'
#' @return A [tibble][tibble::tibble-package] with each trial/plugin result
#' as a separate line. The tibble features following columns:
#'
#' - `trial_type`	(string)	- The name of the plugin used to run the trial.
#' - `trial_index` (numeric) - The index of the current trial across the whole experiment.
#' - `time_elapsed`	(numeric)	- The number of milliseconds between the start of the experiment and when the trial ended.
#' - `internal_node_id`	(string) - A string identifier for the current TimelineNode.
#' - `raw` (list) - list column with all plugin data
#'
#' @seealso [parse_single_record()]
#' @export
#'
#' @examples
#' read_jspsych(demo_file("jspsych-html-button-response.json"))
read_jspsych <- function(filepath, single = T) {
  results <- list()
  i <- 1
  con <- file(filepath, "r")
  if (single) {
    line <- readLines(con, n = -1L, warn = F) # don't warn about final EOL
    jd <- jsonlite::parse_json(line)
    results[[i]] <- parse_single_record(jd)
  } else {
    while (TRUE) {
      line <- readLines(con, n = 1)
      if (length(line) == 0) {
        break
      }
      jd <- jsonlite::parse_json(line)
      results[[i]] <- parse_single_record(jd)
      i <- i + 1
    }
  }
  close(con)
  results %>%
    dplyr::bind_rows(.id = "record") %>%
    dplyr::mutate(record = as.numeric(.data$record))
}

