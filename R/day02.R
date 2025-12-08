# Setup -------------------------------------------------------------------
library(here)
library(Rcpp)

# First part -------------------------------------------------------------
input <- readLines(here("data", "input02"))
sourceCpp(here("src/day02.cpp"), verbose = TRUE)

input <- strsplit(input, ",")[[1]]
result <- lapply(
  X = input, 
  FUN = \(x) {
    start_end <- strsplit(x, "-")[[1]]
    start <- start_end[1]
    end <- start_end[2]
    find_IDs(start, end)
  }
)
do.call(sum, result)

# Second part -------------------------------------------------------------
result <- lapply(
  X = input, 
  FUN = \(x) {
    start_end <- strsplit(x, "-")[[1]]
    start <- start_end[1]
    end <- start_end[2]
    find_all_silly_patterns(start, end)
  }
)
do.call(sum, result)
