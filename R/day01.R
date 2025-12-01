# First part -------------------------------------------------------------
starting_point <- 50

rotate_safe_right <- function(point, value) {
  out <- point + value
  while (out > 99) {
    out <- out - 100
  }
  out
}
rotate_safe_left <- function(point, value) {
  out <- point - value
  while (out < 0) {
    out <- out + 100
  }
  out
}
      
rotate <- function(point, rotation) {
  direction <- substr(rotation, 1, 1)
  value <- substr(rotation, 2, nchar(rotation)) |> as.numeric()
  switch(
    direction, 
    "L" = rotate_safe_left(point, value), 
    "R" = rotate_safe_right(point, value), 
    stop("AAAAAAAAAAAAAARGH")
  )
}

rotations <- 
