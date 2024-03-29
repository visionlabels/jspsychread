% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{parse_instructions}
\alias{parse_instructions}
\title{instructions parser}
\usage{
parse_instructions(d)
}
\arguments{
\item{d}{List with unprocessed trial data}
}
\value{
Single row tibble with results. Check \href{https://www.jspsych.org/7.3/plugins/instructions}{jsPsych documentation}
for the list of available variables.
}
\description{
https://www.jspsych.org/7.3/plugins/instructions
}
\examples{
\dontrun{
fn <- demo_file("jspsych-instructions.json")
d  <- read_jspsych(fn)
d \%>\%
  filter(trial_type == trial_types$instructions) \%>\%
  select(record, trial_index, raw) \%>\%
  process_records(.using = parse_instructions) \%>\%
  unnest(processed) \%>\%
  unnest(view_history) # for page data
}
}
