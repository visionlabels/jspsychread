s <- function(x) { if (is.null(x)) NA else x }

demo_file <- function(fn) {
  system.file("testdata", fn, package = "jspsychread")
}
