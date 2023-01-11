library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

test_that("Parsers reach accurate numbers", {
  # "jspsych-html-button-response.json"
  fn <- demo_file("jspsych-html-button-response.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$html_button_response) %>%
    process_records(.using = parse_html_button_response) %>%
    unnest(processed)
  expect_equal(d$time_elapsed, c(7419, 11133, 13140))
  expect_equal(d$rt, c(7416, 3713, NA))
  expect_equal(d$response, c(2, 0, NA))
  expect_equal(d$stimulus, c(
    "<p style=\"color: red; font-size: 48px; font-weight: bold;\">GREEN</p>",
    "<p style=\"color: green; font-size: 48px; font-weight: bold;\">GREEN</p>",
    "<p style=\"color: blue; font-size: 48px; font-weight: bold;\">RED</p>"
  ))

  # "jspsych-call-function.json"
  fn <- demo_file("jspsych-call-function.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$call_function) %>%
    process_records(.using = parse_call_function) %>%
    unnest(processed)
  expect_equal(d$time_elapsed, c(1, 1))
  expect_equal(d$value[[1]], c(1658413932894))
  expect_equal(d$value[[2]], c(1658413932894))

  # "jspsych-instructions.json"
  fn <- demo_file("jspsych-instructions.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$instructions) %>%
    process_records(.using = parse_instructions) %>%
    unnest(processed)
  expect_equal(d$time_elapsed, c(4004, 9720))
  expect_equal(d$view_history[[1]]$page_index, 0:2)
  expect_equal(d$view_history[[1]]$viewing_time, c(1938, 1149, 916))
  expect_equal(d$view_history[[2]]$page_index, 0:2)
  expect_equal(d$view_history[[2]]$viewing_time, c(2916, 1466, 1333))

  # "jspsych-survey-html-form.json"
  fn <- demo_file("jspsych-survey-html-form.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$survey_html_form) %>%
    process_records(.using = parse_survey_html_form) %>%
    unnest(processed)
  d0 <-
    d %>%
    filter(trial_index == 0) %>%
    select(-raw) %>%
    mutate(bind_rows(response))
  d1 <-
    d %>%
    filter(trial_index == 1) %>%
    select(-raw) %>%
    mutate(bind_rows(response))
  expect_equal(d$time_elapsed, c(10330, 15161))
  expect_equal(d0$first, "red")
  expect_equal(d0$second, "green")
  expect_equal(d0$third, "blue")
  expect_equal(d1$`fav-bird`, "penguin")

  # "jspsych-survey-likert.json"
  fn <- demo_file("jspsych-survey-likert.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$survey_likert) %>%
    process_records(.using = parse_survey_likert) %>%
    unnest(processed)
  # individual questions/responses
  d1 <- d %>%
    filter(trial_index == 1) %>%
    unnest(response)
  expect_equal(d$time_elapsed, c(6531, 15880))
  expect_equal(d1$question, c("Q2", "Q4", "Q0", "Q1", "Q3"))
  expect_equal(d1$response, c(2, 1, 3, 1, 3))
  expect_equal(d1$order, 1:5)

  # "jspsych-animation.json"
  fn <- demo_file("jspsych-animation.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$animation) %>%
    process_records(.using = parse_animation) %>%
    unnest(processed)
  dr <- d %>% unnest(response)
  # anim_data %>% unnest(animation_sequence)
  expect_equal(d$time_elapsed, c(3608))
  expect_equal(dr$rt, c(629, 1512, 2627))
  expect_equal(dr$key_press, c("a", "a", "a"))
  expect_equal(
    dr$stimulus,
    c("img/happy_face_3.jpg", "img/happy_face_2.jpg", "img/happy_face_1.jpg")
  )

  # "jspsych-same-different-html.json"
  fn <- demo_file("jspsych-same-different-html.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$same_different_html) %>%
    process_records(.using = parse_same_different_html) %>%
    unnest(processed)
  expect_equal(d$time_elapsed, c(4621))
  expect_equal(d$rt, c(3110))
  expect_equal(d$correct, c(T))
  expect_equal(d$answer, c("same"))
  expect_equal(d$stimulus1, "<p>Talkative</p>")
  expect_equal(d$stimulus2, "<p>Loquacious</p>")
  expect_equal(d$response, "s")

  # "jspsych-cloze.json"
  fn <- demo_file("jspsych-cloze.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$cloze) %>%
    process_records(.using = parse_cloze) %>%
    unnest(processed)
  # for individual values
  di <- d %>%
    unnest(response)
  expect_equal(d$time_elapsed, c(36177, 52907))
  expect_equal(di$response, c("hamster", "Europe", "Asia", "4", "3"))

  # "jspsych-free-sort.json"
  fn <- demo_file("jspsych-free-sort.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$free_sort) %>%
    process_records(.using = parse_free_sort) %>%
    unnest(processed)
  locations <- d %>% select(-raw, -moves) %>% unnest(locations)
  moves <- d %>% select(-raw, -locations) %>% unnest(moves)
  expect_equal(d$time_elapsed, c(16258, 32592, 44908, 53408))
  expect_equal(d$rt, c(16247, 16332, 7309, 8498))
  expect_equal(locations$x_final,
               c(108, 35, 301, 207, 69, 91, 245, 232, 288, 364, 29, 130, 4, 523))
  expect_equal(moves$x,
               c(207, 301, 108, 35, 23, 292, 91, 245, 69, 232, 29, 288, 364))

  # "jspsych-serial-reaction-time-mouse.json"
  fn <- demo_file("jspsych-serial-reaction-time-mouse.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$serial_reaction_time_mouse) %>%
    process_records(.using = parse_serial_reaction_time_mouse) %>%
    unnest(processed)
  expect_equal(d$time_elapsed,
               c(2200, 3015, 3782, 4515, 5398, 6048, 6815, 7465, 8298, 8965, 9732, 10465))
  expect_equal(d$target_y, c(2, 3, 6, 7, 1, 2, 4, 3, 1, 0, 2, 1))
  expect_equal(d$response_y, c(2, 3, 6, 7, 1, 2, 4, 3, 1, 0, 2, 1))
  expect_equal(d$correct, rep(T, 12))

  # "jspsych-visual-search-circle.json"
  fn <- demo_file("jspsych-visual-search-circle.json")
  d  <- read_jspsych(fn) %>%
    filter(trial_type == trial_types$visual_search_circle) %>%
    process_records(.using = parse_visual_search_circle) %>%
    unnest(processed)
  locations <-
    d %>%
    select(record, trial_index, locations) %>%
    unnest(locations)
  expect_equal(d$time_elapsed, c(14448, 16421, 21076, 24444))
  expect_equal(d$response, c("j", "j", "f", "j"))
  expect_equal(d$set_size, c(4, 3, 6, NA))
  expect_equal(locations$x[1:4], c(49, 224, 200, 25))

})
