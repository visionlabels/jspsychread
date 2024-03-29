% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{parse_iat_image}
\alias{parse_iat_image}
\title{iat-image parser}
\usage{
parse_iat_image(d)
}
\arguments{
\item{d}{List with unprocessed trial data}
}
\value{
Single row tibble with results. Check \href{https://www.jspsych.org/7.3/plugins/iat-image}{jsPsych documentation}
for the list of available variables.
}
\description{
https://www.jspsych.org/7.3/plugins/iat-image
}
\examples{
\dontrun{
fn <- demo_file("jspsych-iat.json")
d  <- read_jspsych(fn)
d \%>\%
  filter(trial_type == trial_types$iat_image) \%>\%
  select(record, trial_index, raw) \%>\%
  process_records(.using = parse_iat_image) \%>\%
  unnest(processed)
}
}
