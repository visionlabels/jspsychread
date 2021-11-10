
# jspsychread

<!-- badges: start -->
<!-- badges: end -->

The goal of `jspsychread` is to transform the data collected in 
[jsPsych](https://www.jspsych.org/) and stored in JSON format into R. The data are then available as tibbles (one trial per line) and you can filter them etc.

## Installation

The package is in active development. 
You can install the current version from GitHub with following code:

``` r
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable verion from GitHub
devtools::install_github("jirilukavsky/motrack")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(jspsychread)
## basic example code
```

