# Setup -------------------------------------------------------------------
library(here)
library(gmp)

# First part -------------------------------------
input <- readLines(here("data", "input05"))
sep <- which(input == "")
delims <- lapply(input[seq.int(1, sep - 1L)], \(x) strsplit(x, "-")[[1]]) |> 
  do.call(rbind, args = _) |> 
  as.bigz()
vals <- as.bigz(input[seq.int(sep + 1L, length(input))])

# Check in range
vapply(vals, \(x, mat) any(x >= mat[, 1] & x <= mat[, 2]), mat = delims, FUN.VALUE = logical(1)) |> 
  sum()

# Second part -------------------------------------------------------------
# Sort the values
idxs <- delims[, 1] |> order()
delims <- delims[idxs, ]

# Loop and merge
intervals <- list(c(delims[1, 1], delims[1, 2]))
j <- 1L

for (i in seq.int(2, nrow(delims))) {
  if (delims[i, 1, drop = TRUE] <= intervals[[j]][2]) {
    # The input is weird and there are situations where the new intervals is
    # already fully included in the old interval
    if (delims[i, 2, drop = TRUE] >= intervals[[j]][2]) {
      intervals[[j]][2] <- delims[i, 2]
    }
    next
  }
  
  j <- j + 1
  intervals[[j]] <- c(delims[i, 1], delims[i, 2])
}

lapply(intervals, \(x) x[2] - x[1] + 1) |> do.call(sum, args = _)
