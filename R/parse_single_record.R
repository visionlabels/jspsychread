#' Export all generic data from single session
#'
#' @param jsondata Data in JSON format
#'
#' Currently
#' `trial_type`	(string)	- The name of the plugin used to run the trial.
#' `trial_index` (numeric) - The index of the current trial across the whole experiment.
#' `time_elapsed`	(numeric)	- The number of milliseconds between the start of the experiment and when the trial ended.
#' `internal_node_id`	(string) - A string identifier for the current TimelineNode.
#'
#' @return a [tibble][tibble::tibble-package] with generic data from each trial/plugin result on separate lines
#' @export
#'
#' @examples
parse_single_record <- function(jsondata) {
  trial_list <- list()
  for (i in 1:length(jsondata)) {
    trial <- jsondata[[i]]
    # save main info
    trial_type <- trial$trial_type
    trial_index <- trial$trial_index
    time_elapsed <- trial$time_elapsed
    internal_node_id <- trial$internal_node_id
    # remove it from list
    trial$trial_type <- NULL
    trial$trial_index <- NULL
    trial$time_elapsed <- NULL
    trial$internal_node_id <- NULL
    # save data + remaining list
    trial_list[[i]] <-
      tibble::tibble(
        trial_type = trial_type,
        trial_index = trial_index,
        time_elapsed = time_elapsed,
        internal_node_id = internal_node_id,
        raw = list(trial)
      )
  }
  trial_list %>% dplyr::bind_rows()
}
