% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{parse_canvas_slider_response}
\alias{parse_canvas_slider_response}
\title{canvas-slider-response parser}
\usage{
parse_canvas_slider_response(d)
}
\arguments{
\item{d}{List with unprocessed trial data}
}
\value{
Single row tibble with results. Check \href{https://www.jspsych.org/7.3/plugins/canvas-slider-response}{jsPsych documentation}
for the list of available variables.
}
\description{
https://www.jspsych.org/7.3/plugins/canvas-slider-response
}
\examples{
\dontrun{
fn <- demo_file("jspsych-canvas-slider-response.json")
d  <- read_jspsych(fn)
d \%>\%
  filter(trial_type == trial_types$canvas_slider_response) \%>\%
  select(record, trial_index, raw) \%>\%
  process_records(.using = parse_canvas_slider_response) \%>\%
  unnest(processed)
}
}
