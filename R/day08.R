# Setup -------------------------------------------------------------------
library(here)

# First part -------------------------------------------------------------
input <- readLines(here("data", "input08"))
input <- strsplit(input, ",") |> lapply(\(x) as.numeric(x)) |> do.call(rbind, args = _)
distances <- dist(input)
ord_distances <- order(distances)
groups <- as.list(seq.int(1, nrow(input)))

# See https://stackoverflow.com/questions/39005958/r-how-to-get-row-column-subscripts-of-matched-elements-from-a-distance-matri
finv <- function(k, dist_obj) {
  if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
  n <- attr(dist_obj, "Size")
  valid <- (k >= 1) & (k <= n * (n - 1) / 2)
  k_valid <- k[valid]
  j <- rep.int(NA_real_, length(k))
  j[valid] <- floor(((2 * n + 1) - sqrt((2 * n - 1)^2 - 8 * (k_valid - 1))) / 2)
  i <- j + k - (2 * n - j) * (j - 1) / 2
  cbind(i, j)
}

nmax <- 1000
for (i in seq_len(nmax)) {
  # Detect the ith closest nodes
  ids <- finv(ord_distances[[i]], distances)
  
  # Check which groups they belong to
  idxs1 <- vapply(groups, is.element, logical(1), el = ids[1, 1])
  g1 <- groups[idxs1]
  idxs2 <- vapply(groups, is.element, logical(1), el = ids[1, 2])
  g2 <- groups[idxs2]
  
  if (identical(g1, g2)) next
  
  # Remove the two groups
  groups <- groups[! (idxs1 | idxs2)]
  
  # Append the new one
  groups <- c(groups, list(c(g1, g2, recursive = TRUE)))
}

lengths(groups) |> sort(decreasing = TRUE)

# Second part -------------------------------------------------------------
rm(list = setdiff(ls(), "finv"))
input <- readLines(here("data", "input08"))
input <- strsplit(input, ",") |> lapply(\(x) as.numeric(x)) |> do.call(rbind, args = _)
distances <- dist(input)
ord_distances <- order(distances)
groups <- as.list(seq.int(1, nrow(input)))

i <- 1L
repeat {
  # Detect the ith closest nodes
  ids <- finv(ord_distances[[i]], distances)
  
  # Check which groups they belong to
  idxs1 <- vapply(groups, is.element, logical(1), el = ids[1, 1])
  g1 <- groups[idxs1]
  idxs2 <- vapply(groups, is.element, logical(1), el = ids[1, 2])
  g2 <- groups[idxs2]
  
  if (identical(g1, g2)) {
    i <- i + 1L
    next
  }
  
  # Remove the two groups
  groups <- groups[! (idxs1 | idxs2)]
  
  # Append the new one
  groups <- c(groups, list(c(g1, g2, recursive = TRUE)))
  
  # Check the stopping condition
  if (length(groups) == 1L) {
    print(ids)
    break
  }
  
  i <- i + 1L
}

input[ids, 1] |> prod()
