# Setup -------------------------------------------------------------------
library(here)
library(igraph)
library(sf)

# First part -------------------------------------
input <- readLines(here("data", "input04"))
input <- strsplit(input, "") |> do.call(rbind, args = _)

# Supporting grid on the unit square
unit_square <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
grid <- st_make_grid(unit_square, n = c(ncol(input), nrow(input)))

# Where are the rolls of paper? 
idx_at <- t(input[rev(seq_len(ncol(input))), seq_len(nrow(input))]) == "@"
grid_at <- grid[idx_at]

# Build the graph
adj_queen <- st_relate(grid_at, pattern = "F***T****")
gobj <- graph_from_adj_list(adj_queen, mode = "all")
sum(degree(gobj) < 4)

# Second part -------------------------------------------------------------
counter <- 0L
repeat {
  # Detect the rolls of paper that can be accessed
  idx_lt4 <- degree(gobj) < 4
  sum_idx <- sum(idx_lt4)
  if (sum_idx == 0L) break ## No more papers can be accessed
  counter <- counter + sum_idx
  
  # Detect the vertices of the rolls of paper that cannot be accessed
  ss_paper <- V(gobj)[!idx_lt4]
  
  # Subset those vertices
  gobj <- subgraph(gobj, ss_paper)
}
counter
