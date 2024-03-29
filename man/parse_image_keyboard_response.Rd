% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{parse_image_keyboard_response}
\alias{parse_image_keyboard_response}
\title{image-keyboard-response parser}
\usage{
parse_image_keyboard_response(d)
}
\arguments{
\item{d}{List with unprocessed trial data}
}
\value{
Single row tibble with results. Check \href{https://www.jspsych.org/7.3/plugins/image-keyboard-response}{jsPsych documentation}
for the list of available variables.
}
\description{
https://www.jspsych.org/7.3/plugins/image-keyboard-response
}
\examples{
\dontrun{
fn <- demo_file("jspsych-image-keyboard-response.json")
d  <- read_jspsych(fn)
d \%>\%
  filter(trial_type == trial_types$image_keyboard_response) \%>\%
  select(record, trial_index, raw) \%>\%
  process_records(.using = parse_image_keyboard_response) \%>\%
  unnest(processed)
}
}
