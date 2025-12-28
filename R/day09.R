# Setup -------------------------------------------------------------------
library(here)
library(Rcpp)

# First part -------------------------------------------------------------
input <- readLines(here("data", "input09"))
input <- strsplit(input, ",") |> lapply(\(x) as.numeric(x))
sourceCpp(here("src/day09.cpp"), verbose = TRUE)

compute_max_area(input)

# Second part -------------------------------------------------------------
library(geos)
library(wk)
input <- readLines(here("data", "input09"))
input <- strsplit(input, ",") |> lapply(\(x) as.numeric(x)) |> do.call(rbind, args = _)

green_tiles <- geos_make_polygon(input[, 1], input[, 2])

build_rect_from_corners <- function(i, j, data) {
  point1 <- data[i, ]
  point2 <- data[j, ]
  if (any(point1 == point2)) {
    return(NULL)
  }
  geos_create_rectangle(
    xmin = min(point1[1], point2[1]), 
    xmax = max(point1[1], point2[1]), 
    ymin = min(point1[2], point2[2]), 
    ymax = max(point1[2], point2[2])
  )
}

max_area <- 0
pb <- txtProgressBar(min = 1, max = nrow(input) - 1, style = 3, initial = 1)
for (i in seq_len(nrow(input) - 1)) {
  for (j in seq.int(i + 1, nrow(input))) {
    rect <- build_rect_from_corners(i, j, input)
    if (is.null(rect)) next
    if (geos_within(rect, green_tiles)) {
      bbox <- wk_bbox(rect)
      area <- (rct_xmax(bbox) - rct_xmin(bbox) + 1) * (rct_ymax(bbox) - rct_ymin(bbox) + 1)
      if (area > max_area) {
        max_area <- area
      }
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)
print(max_area)

