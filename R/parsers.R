#' jspsych-animation parser
#'
#' https://www.jspsych.org/plugins/jspsych-animation/
#'
#' @param d List with unprocessed trial data
#'
#' animation_sequence	array	An array, where each element is an object that represents a stimulus in the animation sequence. Each object has a stimulus property, which is the image that was displayed, and a time property, which is the time in ms, measured from when the sequence began, that the stimulus was displayed. The array will be encoded in JSON format when data is saved using either the .json() or .csv() functions.
#' response	array	An array, where each element is an object representing a response given by the subject. Each object has a stimulus property, indicating which image was displayed when the key was pressed, an rt property, indicating the time of the key press relative to the start of the animation, and a key_press property, indicating which key was pressed. The array will be encoded in JSON format when data is saved using either the .json() or .csv() functions.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
#' fn <- system.file("testdata", "jspsych-animation.json", package = "jspsychread")
#' d  <- read_jspsych(fn)
#' d$raw[2] %>%
#'   purrr::map(~ parse_animation(.x)) %>%
#'   dplyr::bind_rows()

parse_animation <- function(d) {
  tibble::tibble(
    animation_sequence = list(s(d$animation_sequence) %>% dplyr::bind_rows()),
    response = list(s(d$response) %>% dplyr::bind_rows())
  )
}

# jspsych-audio-button-response
# https://www.jspsych.org/plugins/jspsych-audio-button-response/

# jspsych-audio-keyboard-response
# https://www.jspsych.org/plugins/jspsych-audio-keyboard-response/

# jspsych-audio-slider-response
# https://www.jspsych.org/plugins/jspsych-audio-slider-response/

# jspsych-canvas-button-response
# https://www.jspsych.org/plugins/jspsych-canvas-button-response/

# jspsych-canvas-keyboard-response
# https://www.jspsych.org/plugins/jspsych-canvas-keyboard-response/

# jspsych-canvas-slider-response
# https://www.jspsych.org/plugins/jspsych-canvas-slider-response/

#' jspsych-call-function parser
#'
#' https://www.jspsych.org/plugins/jspsych-call-function/
#'
#' @param d List with unprocessed trial data
#'
#' value	any	The return value of the called function.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
#' fn <- system.file("testdata", "jspsych-call-function.json", package = "jspsychread")
#' d  <- read_jspsych(fn)
#' d$raw %>%
#'   purrr::map(~ parse_call_function(.x)) %>%
#'   dplyr::bind_rows()

parse_call_function <- function(d) {
  tibble::tibble(value = list(s(d$value)))
}

# jspsych-categorize-animation
# https://www.jspsych.org/plugins/jspsych-categorize-animation/

# jspsych-categorize-html
# https://www.jspsych.org/plugins/jspsych-categorize-html/

# jspsych-categorize-image¶
# https://www.jspsych.org/plugins/jspsych-categorize-image/

# jspsych-cloze¶
# https://www.jspsych.org/plugins/jspsych-cloze/

# jspsych-external-html plugin¶
# https://www.jspsych.org/plugins/jspsych-external-html/

# jspsych-free-sort plugin¶
# https://www.jspsych.org/plugins/jspsych-free-sort/

# jspsych-fullscreen plugin
# https://www.jspsych.org/plugins/jspsych-fullscreen/

#' jspsych-html-button-response parser
#'
#' https://www.jspsych.org/plugins/jspsych-html-button-response/
#'
#' @param d List with unprocessed trial data
#'
#' rt	(numeric)	The response time in milliseconds for the subject to make a response. The time is measured from when the stimulus first appears on the screen until the subject's response.
#' response	(numeric)	Indicates which button the subject pressed. The first button in the choices array is 0, the second is 1, and so on.
#' stimulus	(string)	The HTML content that was displayed on the screen.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
#' fn <- system.file("testdata", "jspsych-html-button-response.json", package = "jspsychread")
#' d  <- read_jspsych(fn)
#' d$raw %>%
#' purrr::map(~ parse_html_button_response(.x)) %>%
#' dplyr::bind_rows()

parse_html_button_response <- function(d) {
  tibble::tibble(
    rt = s(d$rt),
    response = s(d$response),
    stimulus = s(d$stimulus)
  )
}



#' jspsych-html-keyboard-response parser
#'
#' https://www.jspsych.org/plugins/jspsych-html-keyboard-response/
#'
#' @param d List with unprocessed trial data
#'
#' response	(numeric) -	Indicates which key the subject pressed.
#' rt	(numeric) -	The response time in milliseconds for the subject to make a response. The time is measured from when the stimulus first appears on the screen until the subject's response.
#' stimulus	(string) - The HTML content that was displayed on the screen.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
#' fn <- system.file("testdata", "jspsych-html-keyboard-response.json", package = "jspsychread")
#' d  <- read_jspsych(fn)
#' d$raw %>%
#'   purrr::map(~ parse_html_keyboard_response(.x)) %>%
#'   dplyr::bind_rows()

parse_html_keyboard_response <- function(d) {
  tibble::tibble(
    response = s(d$response),
    rt = s(d$rt),
    stimulus = s(d$stimulus)
  )
}

# jspsych-html-slider-response
# https://www.jspsych.org/plugins/jspsych-html-slider-response/

# jspsych-iat-html plugin
# https://www.jspsych.org/plugins/jspsych-iat-html/

# jspsych-iat-image
# https://www.jspsych.org/plugins/jspsych-iat-image/

#' jspsych-image-button-response parser
#'
#' https://www.jspsych.org/plugins/jspsych-image-button-response/
#'
#' @param d List with unprocessed trial data
#'
#' rt	(numeric) -	The response time in milliseconds for the subject to make a response. The time is measured from when the stimulus first appears on the screen until the subject's response.
#' response	(numeric) -	Indicates which button the subject pressed. The first button in the choices array is 0, the second is 1, and so on.
#' stimulus	(string) - The path of the image that was displayed.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
parse_image_button_response <- function(d) {
  tibble::tibble(
    rt = s(d$rt),
    button_pressed = s(d$button_pressed),
    stimulus = s(d$stimulus)
  )
}

#' jspsych-image-keyboard-response parser
#'
#' https://www.jspsych.org/plugins/jspsych-image-keyboard-response/
#'
#' @param d List with unprocessed trial data
#'
#' response	(numeric) -	Indicates which key the subject pressed.
#' rt	(numeric) -	The response time in milliseconds for the subject to make a response. The time is measured from when the stimulus first appears on the screen until the subject's response.
#' stimulus	(string) - The path of the image that was displayed.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
parse_image_keyboard_response <- function(d) {
  tibble::tibble(
    response = s(d$response),
    rt = s(d$rt),
    stimulus = s(d$stimulus)
  )
}

# jspsych-image-slider-response
# https://www.jspsych.org/plugins/jspsych-image-slider-response/

#' jspsych-instructions parse
#'
#' https://www.jspsych.org/plugins/jspsych-instructions/
#'
#' @param d List with unprocessed trial data
#'
#' view_history	array	An array containing the order of pages the subject viewed (including when the subject returned to previous pages) and the time spent viewing each page. Each object in the array represents a single page view, and contains keys called page_index (the page number, starting with 0) and viewing_time (duration of the page view). This will be encoded as a JSON string when data is saved using the .json() or .csv() functions.
#' rt	numeric	The response time in milliseconds for the subject to view all of the pages.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
parse_instructions <- function(d) {
  tibble::tibble(
    view_history = list(s(d$view_history) %>% dplyr::bind_rows()),
    rt = s(d$rt)
  )
}

# jspsych-maxdiff
# https://www.jspsych.org/plugins/jspsych-maxdiff/

# jspsych-preload
# https://www.jspsych.org/plugins/jspsych-preload/

# jspsych-rdk
# https://www.jspsych.org/plugins/jspsych-rdk/

# jspsych-reconstruction
# https://www.jspsych.org/plugins/jspsych-reconstruction/

# jspsych-resize
# https://www.jspsych.org/plugins/jspsych-resize/

# jspsych-same-different-html
# https://www.jspsych.org/plugins/jspsych-same-different-html/

# jspsych-same-different-image
# https://www.jspsych.org/plugins/jspsych-same-different-image/

# jspsych-serial-reaction-time
# https://www.jspsych.org/plugins/jspsych-serial-reaction-time/

# jspsych-serial-reaction-time-mouse
# https://www.jspsych.org/plugins/jspsych-serial-reaction-time-mouse/

#' jspsych-survey-html-form parser
#'
#' https://www.jspsych.org/plugins/jspsych-survey-html-form/
#'
#' @param d List with unprocessed trial data
#'
#' response	object	An object containing the response for each input. The object will have a separate key (variable) for the response to each input, with each variable being named after its corresponding input element. Each response is a string containing whatever the subject answered for this particular input. This will be encoded as a JSON string when data is saved using the .json() or .csv() functions.
#' rt	numeric	The response time in milliseconds for the subject to make a response.
#'
#' @return Single row tibble with results
#' @export
#'
#' @examples
#' fn <- system.file("testdata", "jspsych-survey-html-form.json", package = "jspsychread")
#' d  <- read_jspsych(fn)
#' d$raw %>%
#'   purrr::map(~ parse_survey_html_form(.x)) %>%
#'   dplyr::bind_rows()

parse_survey_html_form <- function(d) {
  tibble::tibble(
    response = list(s(d$response) %>% dplyr::bind_rows()),
    rt = s(d$rt)
  )
}

# jspsych-survey-likert
# https://www.jspsych.org/plugins/jspsych-survey-likert/

# jspsych-survey-multi-choice
# https://www.jspsych.org/plugins/jspsych-survey-multi-choice/

# jspsych-survey-multi-select
# https://www.jspsych.org/plugins/jspsych-survey-multi-select/

# jspsych-survey-text
# https://www.jspsych.org/plugins/jspsych-survey-text/

# jspsych-video-button-response
# https://www.jspsych.org/plugins/jspsych-video-button-response/

# jspsych-video-keyboard-response
# https://www.jspsych.org/plugins/jspsych-video-keyboard-response/

# jspsych-video-slider-response
# https://www.jspsych.org/plugins/jspsych-video-slider-response/

# jspsych-virtual-chinrest
# https://www.jspsych.org/plugins/jspsych-virtual-chinrest/

# jspsych-visual-search-circle
# https://www.jspsych.org/plugins/jspsych-visual-search-circle/

# jspsych-vsl-animate-occlusion
# https://www.jspsych.org/plugins/jspsych-vsl-animate-occlusion/

# jspsych-vsl-grid-scene
# https://www.jspsych.org/plugins/jspsych-vsl-grid-scene/

# jspsych-webgazer-calibrate
# https://www.jspsych.org/plugins/jspsych-webgazer-calibrate/

# jspsych-webgazer-init-camera
# https://www.jspsych.org/plugins/jspsych-webgazer-init-camera/

# jspsych-webgazer-validate
# https://www.jspsych.org/plugins/jspsych-webgazer-validate/
