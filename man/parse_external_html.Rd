% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{parse_external_html}
\alias{parse_external_html}
\title{external-html parser}
\usage{
parse_external_html(d)
}
\arguments{
\item{d}{List with unprocessed trial data}
}
\value{
Single row tibble with results. Check \href{https://www.jspsych.org/7.3/plugins/external-html}{jsPsych documentation}
for the list of available variables.
}
\description{
https://www.jspsych.org/7.3/plugins/external-html
}
\examples{
\dontrun{
fn <- demo_file("jspsych-external-html.json")
d  <- read_jspsych(fn)
d \%>\%
  filter(trial_type == trial_types$external_html) \%>\%
  select(record, trial_index, raw) \%>\%
  process_records(.using = parse_external_html) \%>\%
  unnest(processed)
}
}
