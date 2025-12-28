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
# We can see that 
is_dag(gr)

# There must be a single path from svt --> fft --> dac --> out otherwise the
# graph contains loops. In my case the correct order is "svr" --> "fft" --->
# "dac" --> "out". The total number of path is just the product of the number of
# paths going through each of the three steps. This is more efficient that total
# enumeration. The cutoffs were chosen by trial and error.
svr_fft <- all_simple_paths(gr, "svr", "fft", mode = "out", cutoff = 13)
fft_dac <- all_simple_paths(gr, "fft", "dac", mode = "out", cutoff = 18)
dac_out <- all_simple_paths(gr, "dac", "out", mode = "out", cutoff = 13)
gc()

options(digits = 22)
as.numeric(length(svr_fft)) * length(fft_dac) * length(dac_out)

