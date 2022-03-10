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
#' @return a [tibble][tibble::tibble-package] with each trial/plugin result as a separate line
#' @export
#'
#' @examples
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
      # jd_parsed <- parse_results_for_one_person(jd, parser = parser)
      # jd_parsed$meta <- jd_parsed$meta %>%
      #   mutate(results_id = i) %>% select(results_id, everything())
      # jd_parsed$results <- jd_parsed$results %>%
      #   mutate(results_id = i) %>% select(results_id, everything())
      results[[i]] <- parse_single_record(jd)
      i <- i + 1
    }
  }
  close(con)
  results %>%
    dplyr::bind_rows(.id = "record") %>%
    dplyr::mutate(record = as.numeric(.data$record))
}
