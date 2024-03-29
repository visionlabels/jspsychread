
# jspsychread

<!-- badges: start -->
<!-- badges: end -->

The goal of `jspsychread` is to transform the data collected in 
[jsPsych](https://www.jspsych.org/) and stored in JSON format into R. The data are then available as tibbles (one trial per line) and you can filter them etc.

## Installation

The package is in active development. 
You can install the current version 
from [GitHub](https://github.com/visionlabels/jspsychread) 
with following code:

``` r
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable verion from GitHub
devtools::install_github("visionlabels/jspsychread")
```

## How to start

Check the example below which covers the main parts of the workflow.
More detailed walk-through is included in the package 
[vignette](./articles/jspsychread.html) (see `vignette("jspsychread")`). 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
library(jspsychread)

## basic example code

fn <- demo_file("jspsych-video-button-response.json")
d  <- read_jspsych(fn)

# parse results of a specific trial types
d %>%
  filter(trial_type == trial_types$preload) %>%
  select(record, trial_index, raw) %>%
  process_records(.using = parse_preload) %>%
  unnest(processed)

d %>%
  filter(trial_type == trial_types$html_button_response) %>%
  select(record, trial_index, raw) %>%
  process_records(.using = parse_html_button_response) %>%
  unnest(processed)

d %>%
  filter(trial_type == trial_types$video_button_response) %>%
  select(record, trial_index, raw) %>%
  process_records(.using = parse_video_button_response) %>%
  unnest(processed)

```

In the examples above, you explicitly defined what function to use to 
process the trial data. 
Since version 0.3.0, `process_records` can auto-detect
the parser based on the `trial_type` column. 

It is still required that all trials in the tibble are of the same type and 
you should use `filter` or `group_by` to separate different trial types. 
It is an intentional decision. 
With diverse trial types in the same tibble, it would not be possible to `unnest` it.

You can use simpler call:

``` r
d %>%
  filter(trial_type == trial_types$html_button_response) %>%
  process_records(.using = parse_html_button_response) %>%
  unnest(processed)
```

Or even parse everything into a list of tibbles.

``` r
dl <-
  d %>% 
  group_split(trial_type) %>% 
  map(~ process_records(.x))
dl
dl %>% map(~ .x %>% unnest(processed))
```

