#' Default set of parsers
#'
#' This list of used to automatically call respective parsers.
#' It is primarily used for auto-detection in `process_records`.
#' In future, it could be possible to hold alternative parsers for different
#' versions of jsPsych (or jspsychread).
#'
#' @return List of trial types and respective parser functions
#' @export
#'
#' @examples
#' default_parsers()
#' # extending parsers
#' my_parsers <- default_parsers()
#' my_form_parser <- function(x) { x } # not useful but check parsers.R for inspiration
#' my_parsers[["survey_html_form"]] <- my_form_parser
default_parsers <- function() {
  list(
    "animation" = parse_animation,
    "audio-button-response" = parse_audio_button_response,
    "audio-keyboard-response" = parse_audio_keyboard_response,
    "audio-slider-response" = parse_audio_slider_response,
    "call-function" = parse_call_function,
    "canvas-button-response" = parse_canvas_button_response,
    "canvas-keyboard-response" = parse_canvas_keyboard_response,
    "canvas-slider-response" = parse_canvas_slider_response,
    "categorize-animation" = parse_categorize_animation,
    "categorize-html" = parse_categorize_html,
    "categorize-image" = parse_categorize_image,
    "cloze" = parse_cloze,
    "external-html" = parse_external_html,
    "free-sort" = parse_free_sort,
    "fullscreen" = parse_fullscreen,
    "html-button-response" = parse_html_button_response,
    "html-keyboard-response" = parse_html_keyboard_response,
    "html-slider-response" = parse_html_slider_response,
    "iat-html" = parse_iat_html,
    "iat-image" = parse_iat_image,
    "image-button-response" = parse_image_button_response,
    "image-keyboard-response" = parse_image_keyboard_response,
    "image-slider-response" = parse_image_slider_response,
    "instructions" = parse_instructions,
    "maxdiff" = parse_maxdiff,
    "preload" = parse_preload,
    "reconstruction" = parse_reconstruction,
    "resize" = parse_resize,
    "same-different-html" = parse_same_different_html,
    "same-different-image" = parse_same_different_image,
    "serial-reaction-time" = parse_serial_reaction_time,
    "serial-reaction-time-mouse" = parse_serial_reaction_time_mouse,
    "survey-html-form" = parse_survey_html_form,
    "survey-likert" = parse_survey_likert,
    "survey-multi-choice" = parse_survey_multi_choice,
    "survey-multi-select" = parse_survey_multi_select,
    "survey-text" = parse_survey_text,
    "video-button-response" = parse_video_button_response,
    "video-keyboard-response" = parse_video_keyboard_response,
    "video-slider-response" = parse_video_slider_response,
    "virtual-chinrest" = NULL, # parse_virtual_chinrest
    "visual-search-circle" = parse_visual_search_circle,
    "webgazer-calibrate" = NULL, # parse_webgazer_calibrate
    "webgazer-init-camera" = NULL, # parse_webgazer_init_camera,
    "webgazer-validate" = NULL # parse_webgazer_validate
  )
}
