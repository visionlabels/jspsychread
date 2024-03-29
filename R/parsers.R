# ----- response parsers -----

#' html-button-response parser
#'
#' https://www.jspsych.org/7.3/plugins/html-button-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/html-button-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-html-button-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$html_button_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_html_button_response) %>%
#'   unnest(processed)
#' }
parse_html_button_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' html-keyboard-response parser
#'
#' https://www.jspsych.org/7.3/plugins/html-keyboard-response/
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/html-keyboard-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-html-keyboard-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$html_keyboard_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_html_keyboard_response) %>%
#'   unnest(processed)
#' }
parse_html_keyboard_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_character(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' html-slider-response parser
#'
#' https://www.jspsych.org/7.3/plugins/html-slider-response/
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/html-slider-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-html-slider-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$html_slider_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_html_slider_response) %>%
#'   unnest(processed)
#' }
parse_html_slider_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus),
    slider_start = v_integer(d$slider_start)
  )
}

#' image-button-response parser
#'
#' https://www.jspsych.org/7.3/plugins/image-button-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/image-button-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-image-button-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$image_button_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_image_button_response) %>%
#'   unnest(processed)
#' }
parse_image_button_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' image-keyboard-response parser
#'
#' https://www.jspsych.org/7.3/plugins/image-keyboard-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/image-keyboard-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-image-keyboard-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$image_keyboard_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_image_keyboard_response) %>%
#'   unnest(processed)
#' }
parse_image_keyboard_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_character(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' image-slider-response parser
#'
#' https://www.jspsych.org/7.3/plugins/image-slider-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/image-slider-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-image-slider-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$image_slider_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_image_slider_response) %>%
#'   unnest(processed)
#' }
parse_image_slider_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus),
    slider_start = v_integer(d$slider_start)
  )
}

#' audio-button-response parser
#'
#' https://www.jspsych.org/7.3/plugins/audio-button-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/audio-button-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-audio-button-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$audio_button_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_audio_button_response) %>%
#'   unnest(processed)
#' }
parse_audio_button_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' audio-keyboard-response parser
#'
#' https://www.jspsych.org/7.3/plugins/audio-keyboard-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/audio-keyboard-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-audio-keyboard-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$audio_keyboard_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_audio_keyboard_response) %>%
#'   unnest(processed)
#' }
parse_audio_keyboard_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_character(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' audio-slider-response parser
#'
#' https://www.jspsych.org/7.3/plugins/audio-slider-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/audio-slider-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-audio-slider-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$audio_slider_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_audio_slider_response) %>%
#'   unnest(processed)
#' }
parse_audio_slider_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus),
    slider_start = v_integer(d$slider_start)
  )
}

#' video-button-response parser
#'
#' https://www.jspsych.org/7.3/plugins/video-button-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/video-button-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-video-button-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$video_button_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_video_button_response) %>%
#'   unnest(processed)
#' }
parse_video_button_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' video-keyboard-response parser
#'
#' https://www.jspsych.org/7.3/plugins/video-keyboard-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/video-keyboard-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-video-keyboard-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$video_keyboard_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_video_keyboard_response) %>%
#'   unnest(processed)
#' }
parse_video_keyboard_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_character(d$response),
    stimulus = v_character(d$stimulus)
  )
}

#' video-slider-response parser
#'
#' https://www.jspsych.org/7.3/plugins/video-slider-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/video-slider-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-video-slider-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$video_slider_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_video_slider_response) %>%
#'   unnest(processed)
#' }
parse_video_slider_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    stimulus = v_character(d$stimulus),
    slider_start = v_integer(d$slider_start),
    start = v_real(single_value(d$start))
  )
}

#' canvas-button-response parser
#'
#' https://www.jspsych.org/7.3/plugins/canvas-button-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/canvas-button-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-canvas-button-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$canvas_button_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_canvas_button_response) %>%
#'   unnest(processed)
#' }
parse_canvas_button_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response)
  )
}

#' canvas-keyboard-response parser
#'
#' https://www.jspsych.org/7.3/plugins/canvas-keyboard-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/canvas-keyboard-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-canvas-keyboard-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$canvas_keyboard_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_canvas_keyboard_response) %>%
#'   unnest(processed)
#' }
parse_canvas_keyboard_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_character(d$response)
  )
}

#' canvas-slider-response parser
#'
#' https://www.jspsych.org/7.3/plugins/canvas-slider-response
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/canvas-slider-response)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-canvas-slider-response.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$canvas_slider_response) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_canvas_slider_response) %>%
#'   unnest(processed)
#' }
parse_canvas_slider_response <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    response = v_integer(d$response),
    slider_start = v_integer(d$slider_start)
  )
}

# ----- Utilities -----

#' call-function parser
#'
#' https://www.jspsych.org/7.3/plugins/call-function
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/call-function)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-call-function.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$call_function) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_call_function) %>%
#'   unnest(processed)
#' }
parse_call_function <- function(d) {
  tibble::tibble(
    value = list_column_of_vectors(d$value)
  )
}

#' fullscreen parser
#'
#' https://www.jspsych.org/7.3/plugins/fullscreen
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/fullscreen)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-fullscreen.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$fullscreen) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_fullscreen) %>%
#'   unnest(processed)
#' }
parse_fullscreen <- function(d) {
  tibble::tibble(
    success = v_logical(d$success)
  )
}

#' instructions parser
#'
#' https://www.jspsych.org/7.3/plugins/instructions
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/instructions)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-instructions.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$instructions) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_instructions) %>%
#'   unnest(processed) %>%
#'   unnest(view_history) # for page data
#' }
parse_instructions <- function(d) {
  view_history_list <- d$view_history
  tibble::tibble(
    rt = v_integer(d$rt),
    view_history = nested_tibble_from_list(view_history_list)
  )
}

#' preload parser
#'
#' https://www.jspsych.org/7.3/plugins/preload
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/preload)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-preload.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$preload) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_preload) %>%
#'   unnest(processed)
#' }
parse_preload <- function(d) {
  tibble::tibble(
    success = v_logical(d$success),
    timeout = v_logical(d$timeout),
    failed_images = list_column_of_vectors(v_character(d$failed_images)),
    failed_audio = list_column_of_vectors(v_character(d$failed_audio)),
    failed_video = list_column_of_vectors(v_character(d$failed_video))
  )
}

#' external-html parser
#'
#' https://www.jspsych.org/7.3/plugins/external-html
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/external-html)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-external-html.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$external_html) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_external_html) %>%
#'   unnest(processed)
#' }
parse_external_html <- function(d) {
  tibble::tibble(
    url = v_character(d$url),
    rt = v_integer(d$rt)
  )
}

# ----- Survey responses -----

#' survey-html-form parser
#'
#' https://www.jspsych.org/7.3/plugins/survey-html-form
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/survey-html-form)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-survey-html-form.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$survey_html_form) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_survey_html_form) %>%
#'   unnest(processed)
#' trials %>%
#'   filter(trial_index == 0) %>%
#'   select(-raw) %>%
#'   mutate(bind_rows(response))
#' }
parse_survey_html_form <- function(d) {
  tibble::tibble(
    response = list_column_of_vectors(d$response),
    rt = v_integer(d$rt)
  )
}

#' survey-likert parser
#'
#' https://www.jspsych.org/7.3/plugins/survey-likert
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/survey-likert)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-survey-likert.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$survey_likert) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_survey_likert) %>%
#'   unnest(processed)
#' # individual questions/responses
#' trials %>%
#'   filter(trial_index == 1) %>%
#'   unnest(response)
#' }
parse_survey_likert <- function(d) {
  responses <- d$response
  q_name <- names(responses)
  q_values <- unlist(responses)
  names(q_values) <- NULL
  q_order <- unlist(d$question_order)
  q_order1 <- q_order + 1 # start indexing with 1
  nq <- length(q_name)
  response_tbl <-
    tibble::tibble(
      question = q_name,
      response = q_values,
      order = (1:nq)
    )
  tibble::tibble(
    response = nested_tibble(response_tbl),
    rt = v_integer(d$rt),
    question_order = list_column_of_vectors(v_integer(q_order1))
  )
}

#' survey-multi-choice parser
#'
#' https://www.jspsych.org/7.3/plugins/survey-multi-choice
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/survey-multi-choice)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-survey-multi-choice.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$survey_multi_choice) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_survey_multi_choice) %>%
#'   unnest(processed)
#' trials %>%
#'   filter(trial_index == 1) %>%
#'   unnest(response)
#' }
parse_survey_multi_choice <- function(d) {
  responses <- d$response
  q_name <- names(responses)
  q_values <- unlist(responses)
  q_order <- unlist(d$question_order)
  q_order1 <- q_order + 1 # start indexing with 1
  nq <- length(q_name)
  response_tbl <-
    tibble(question = q_name, response = q_values,
           order = (1:nq))
  tibble::tibble(
    response = nested_tibble(response_tbl),
    rt = v_integer(d$rt),
    question_order = list_column_of_vectors(v_integer(q_order1))
  )
}

#' survey-multi-select parser
#'
#' https://www.jspsych.org/7.3/plugins/survey-multi-select
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/survey-multi-select)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-survey-multi-select.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$survey_multi_select) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_survey_multi_select) %>%
#'   unnest(processed)
#' trials %>%
#'   select(-raw, -question_order) %>%
#'   filter(trial_index == 0) %>%
#'   unnest(response)
#' }
parse_survey_multi_select <- function(d) {
  responses <- d$response
  nq <- length(responses)
  response_list <- list()
  for (i in 1:nq) {
    response_list[[i]] <-
      tibble(question = names(responses)[i],
             response =
               single_value(unlist(responses[[i]])),
             order = i)
  }
  q_order <- unlist(d$question_order)
  q_order1 <- q_order + 1 # start indexing with 1
  response_tbl <-
    bind_rows(response_list)
  tibble::tibble(
    response = nested_tibble(response_tbl),
    rt = v_integer(d$rt),
    question_order = list_column_of_vectors(v_integer(q_order1))
  )
}

#' survey-text parser
#'
#' https://www.jspsych.org/7.3/plugins/survey-text
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/survey-text)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-survey-text.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$survey_text) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_survey_text) %>%
#'   unnest(processed)
#' trials %>%
#'   filter(trial_index == 1) %>%
#'   unnest(response)
#' }
parse_survey_text <- function(d) {
  responses <- d$response
  q_name <- names(responses)
  q_values <- unlist(responses)
  q_order <- unlist(d$question_order)
  q_order1 <- q_order + 1 # start indexing with 1
  nq <- length(q_name)
  response_tbl <-
    tibble(question = q_name, response = q_values,
           order = (1:nq))
  tibble::tibble(
    response = nested_tibble(response_tbl),
    rt = v_integer(d$rt),
    question_order = list_column_of_vectors(v_integer(q_order1))
  )
}


# ----- Other responses -----

#' maxdiff parser
#'
#' https://www.jspsych.org/7.3/plugins/maxdiff
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/maxdiff)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-maxdiff.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$maxdiff) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_maxdiff) %>%
#'   unnest(processed)
#' }
parse_maxdiff <- function(d) {
  tibble::tibble(
    rt = v_integer(d$rt),
    label_left = v_character(d$labels$left),
    label_right = v_character(d$labels$right),
    response_left = v_character(d$response$left),
    response_right = v_character(d$response$right),
  )
}

#' animation parser
#'
#' https://www.jspsych.org/7.3/plugins/animation
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/animation)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-animation.json")
#' d  <- read_jspsych(fn)
#' anim_data <-
#'   d %>%
#'   filter(trial_type == trial_types$animation) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_animation) %>%
#'   unnest(processed)
#' anim_data %>% unnest(response)
#' anim_data %>% unnest(animation_sequence)
#' }
parse_animation <- function(d) {
  tibble::tibble(
    animation_sequence = nested_tibble_from_list(d$animation_sequence),
    response = nested_tibble_from_list(d$response)
  )
}

#' categorize-animation parser
#'
#' https://www.jspsych.org/7.3/plugins/categorize-animation
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/categorize-animation)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-categorize-animation.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$categorize_animation) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_categorize_animation) %>%
#'   unnest(processed)
#' }
parse_categorize_animation <- function(d) {
  tibble::tibble(
    stimulus = list_column_of_vectors(v_character(d$stimulus)),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct)
  )
}

#' categorize-html parser
#'
#' https://www.jspsych.org/7.3/plugins/categorize-html
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/categorize-html)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-categorize-html.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$categorize_html) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_categorize_html) %>%
#'   unnest(processed)
#' }
parse_categorize_html <- function(d) {
  tibble::tibble(
    stimulus = v_character(d$stimulus),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct)
  )
}

#' categorize-image parser
#'
#' https://www.jspsych.org/7.3/plugins/categorize-image
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/categorize-image)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-categorize-image.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$categorize_image) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_categorize_image) %>%
#'   unnest(processed)
#' }
parse_categorize_image <- function(d) {
  tibble::tibble(
    stimulus = v_character(d$stimulus),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct)
  )
}

#' iat-image parser
#'
#' https://www.jspsych.org/7.3/plugins/iat-image
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/iat-image)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-iat.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$iat_image) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_iat_image) %>%
#'   unnest(processed)
#' }
parse_iat_image <- function(d) {
  tibble::tibble(
    stimulus = v_character(d$stimulus),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct)
  )
}

#' iat-html parser
#'
#' https://www.jspsych.org/7.3/plugins/iat-html
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/iat-html)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-iat.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$iat_html) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_iat_html) %>%
#'   unnest(processed)
#' }
parse_iat_html <- function(d) {
  tibble::tibble(
    stimulus = v_character(d$stimulus),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct)
  )
}

#' reconstruction parser
#'
#' https://www.jspsych.org/7.3/plugins/reconstruction
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/reconstruction)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-reconstruction.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$reconstruction) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_reconstruction) %>%
#'   unnest(processed)
#' }
parse_reconstruction <- function(d) {
  tibble::tibble(
    start_value = v_real(d$start_value),
    final_value = v_real(d$final_value),
    rt = v_integer(d$rt)
  )
}

#' resize parser
#'
#' https://www.jspsych.org/7.3/plugins/resize
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/resize)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-resize.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$resize) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_resize) %>%
#'   unnest(processed)
#' }
parse_resize <- function(d) {
  tibble::tibble(
    final_width_px = v_integer(d$final_width_px),
    scale_factor = v_real(d$scale_factor)
  )
}

#' same-different-html parser
#'
#' https://www.jspsych.org/7.3/plugins/same-different-html
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/same-different-html)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-same-different-html.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$same_different_html) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_same_different_html) %>%
#'   unnest(processed)
#' }
parse_same_different_html <- function(d) {
  stimuli <- d$stimulus
  tibble::tibble(
    stimulus = list_column_of_vectors(v_character(stimuli)),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct),
    answer = v_character(d$answer),
    rt_stim1 = v_integer(d$rt_stim1),
    response_stim1 = v_character(d$response_stim1),
    stimulus1 = v_character(stimuli[[1]]),
    stimulus2 = v_character(stimuli[[2]])
  )
}

#' same-different-image parser
#'
#' https://www.jspsych.org/7.3/plugins/same-different-image
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/same-different-image)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-same-different-image.json")
#' d  <- read_jspsych(fn)
#' d %>%
#'   filter(trial_type == trial_types$same_different_image) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_same_different_image) %>%
#'   unnest(processed)
#' }
parse_same_different_image <- function(d) {
  stimuli <- d$stimulus
  tibble::tibble(
    stimulus = list_column_of_vectors(v_character(stimuli)),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    correct = v_logical(d$correct),
    answer = v_character(d$answer),
    rt_stim1 = v_integer(d$rt_stim1),
    response_stim1 = v_character(d$response_stim1),
    stimulus1 = v_character(stimuli[[1]]),
    stimulus2 = v_character(stimuli[[2]])
  )
}

#' cloze parser
#'
#' https://www.jspsych.org/7.3/plugins/cloze
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/cloze)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-cloze.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$cloze) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_cloze) %>%
#'   unnest(processed)
#' # for individual values
#' trials %>%
#'   unnest(response) %>%
#'   mutate(cell_value = flatten_chr(response))
#' }
parse_cloze <- function(d) {
  tibble::tibble(
    response = list_column_of_vectors(v_character(d$response))
  )
}

#' free-sort parser
#'
#' https://www.jspsych.org/7.3/plugins/free-sort
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/free-sort)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-free-sort.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$free_sort) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_free_sort) %>%
#'   unnest(processed)
#' # for locations
#' trials %>% select(-raw, -moves) %>% unnest(locations)
#' # for moves
#' trials %>% select(-raw, -locations) %>% unnest(moves)
#' }
parse_free_sort <- function(d) {
  init_locations <- bind_rows(d$init_locations)
  final_locations <- bind_rows(d$final_locations)
  moves <- bind_rows(d$moves)
  locations <-
    init_locations %>%
    left_join(final_locations, by = "src",
              suffix = c("_init", "_final"))
  tibble::tibble(
    locations = nested_tibble(locations),
    moves = nested_tibble(moves),
    rt = v_integer(d$rt)
  )
}

#' serial-reaction-time-mouse parser
#'
#' https://www.jspsych.org/7.3/plugins/serial-reaction-time-mouse
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/serial-reaction-time-mouse)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-serial-reaction-time-mouse.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$serial_reaction_time_mouse) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_serial_reaction_time_mouse) %>%
#'   unnest(processed)
#' trials %>%
#'   select(-raw) %>%
#'   mutate(ncol = map_int(.$grid, ~ ncol(.)),
#'          nrow = map_int(.$grid, ~ nrow(.)))
#' }
parse_serial_reaction_time_mouse <- function(d) {
  grid_array <- d$grid
  target <- d$target
  response <- d$response
  tibble::tibble(
    grid = list(list2_to_matrix(d$grid)),
    target_x = v_integer(target[[2]]), # [row, column] = [y, x]
    target_y = v_integer(target[[1]]),
    rt = v_integer(d$rt),
    response_x = v_integer(response[[2]]), # [row, column] = [y, x]
    response_y = v_integer(response[[1]]),
    correct = v_logical(d$correct)
  )
}

#' serial-reaction-time parser
#'
#' https://www.jspsych.org/7.3/plugins/serial-reaction-time
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/serial-reaction-time)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-serial-reaction-time.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$serial_reaction_time) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_serial_reaction_time) %>%
#'   unnest(processed)
#' trials %>%
#'   select(-raw) %>%
#'   mutate(ncol = map_int(.$grid, ~ ncol(.)),
#'          nrow = map_int(.$grid, ~ nrow(.)))
#' }
parse_serial_reaction_time <- function(d) {
  grid_array <- d$grid
  target <- d$target
  tibble::tibble(
    grid = list(list2_to_matrix(d$grid)),
    target_x = v_integer(target[[2]]), # [row, column] = [y, x]
    target_y = v_integer(target[[1]]),
    rt = v_integer(d$rt),
    response = v_character(d$response),
    correct = v_logical(d$correct)
  )
}


#' visual-search-circle parser
#'
#' https://www.jspsych.org/7.3/plugins/visual-search-circle
#'
#' @param d List with unprocessed trial data
#'
#' @return Single row tibble with results. Check [jsPsych documentation](https://www.jspsych.org/7.3/plugins/visual-search-circle)
#' for the list of available variables.
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- demo_file("jspsych-visual-search-circle.json")
#' d  <- read_jspsych(fn)
#' trials <-
#'   d %>%
#'   filter(trial_type == trial_types$visual_search_circle) %>%
#'   select(record, trial_index, raw) %>%
#'   process_records(.using = parse_visual_search_circle) %>%
#'   unnest(processed)
#' trials %>%
#'   select(record, trial_index, locations) %>%
#'   unnest(locations)
#' }
parse_visual_search_circle <- function(d) {
  xy <- list2_to_matrix(d$locations)
  colnames(xy) <- c("x", "y")
  locations <- as_tibble(xy)
  tibble::tibble(
    correct = v_logical(d$correct),
    response = v_character(d$response),
    rt = v_integer(d$rt),
    set_size = v_integer(d$set_size),
    target_present = v_logical(d$target_present),
    locations = nested_tibble(locations)
  )
}


