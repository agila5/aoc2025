# Setup -------------------------------------------------------------------
library(here)

# First part -------------------------------------------------------------
rotate <- function(point, rotation, op_left, op_right) {
  direction <- substr(rotation, 1, 1)
  value <- substr(rotation, 2, nchar(rotation)) |> as.numeric()
  stopifnot(direction %in% c("L", "R"))
  ifelse(direction == "L", op_left(point, value), op_right(point, value))
}

rotations <- readLines(here("data", "input01"))

new_point <- 50
counter <- 0L
for (rotation in rotations) {
  new_point <- rotate(
    new_point, 
    rotation, 
    op_left = \(x, y) (x - y) %% 100, 
    op_right = \(x, y) (x + y) %% 100
  )
  if (new_point == 0) {
    counter <- counter + 1L
  }
}
print(counter)

# Second part -------------------------------------------------------------
library(Rcpp)
sourceCpp(here("src/day01.cpp"), verbose = TRUE, rebuild = TRUE)

new_point <- 50
counter <- 0L
for (rotation in rotations) {
  new_point <- rotate(
    new_point, 
    rotation, 
    op_left = rotate_left_cpp, 
    op_right = rotate_right_cpp
  )
}
print(counter)
