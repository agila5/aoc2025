# Setup -------------------------------------------------------------------
library(here)
library(igraph)

# First part -------------------------------------------------------------
input <- readLines(here("data", "input11"))
names <- substr(input, 1, 3)
nodes <- substr(input, 6, nchar(input)) |> strsplit(" ", fixed = TRUE)
all_names <- c(names, do.call(c, nodes)) |> unique() |> factor()

gr <- graph_from_adj_list(lapply(nodes, \(x) factor(x, levels = all_names) |> as.numeric()))
V(gr)$name <- all_names
gr

all_simple_paths(gr, "you", "out", mode = "out") |> length()

# Second part -------------------------------------------------------------
# 1. Paths from "svt" to "fft"
svr_fft <- all_simple_paths(gr, "svr", "fft", mode = "out", cutoff = 13)
fft_dac <- all_simple_paths(gr, "fft", "dac", mode = "out", cutoff = 18)
dac_out <- all_simple_paths(gr, "dac", "out", mode = "out", cutoff = 13)
gc()

options(digits = 22)
as.numeric(length(svr_fft)) * length(fft_dac) * length(dac_out)

