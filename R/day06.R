# Setup -------------------------------------------------------------------
library(here)

# First part -------------------------------------
input <- readLines(here("data", "input06"))
chars <- strsplit(input[-length(input)], "[[:blank:]]+") |> do.call(rbind, args = _) |> matrix(nrow = length(input) - 1L)
nums <- as.numeric(chars) |> matrix(nrow = length(input) - 1L)
ops <- strsplit(tail(input, n = 1L), "[[:blank:]]+") |> unlist()

res <- mapply(
  FUN = \(i, op, mat) {
    op = match.fun(op)
    x = mat[, i]
    Reduce(op, x)
  }, 
  i = seq_len(ncol(nums)), 
  op = ops,
  MoreArgs = list(mat = nums)
)

options(digits = 20)
sum(res)

# Second part -------------------------------------------------------------
tmp <- strsplit(input, "") |> do.call(rbind, args = _) |> matrix(nrow = length(input))

op <- match.fun(tmp[nrow(tmp), 1])
nums <- c()
values <- c()
for (j in seq_len(ncol(tmp))) {
  num <- tmp[seq.int(1, nrow(tmp) - 1L), j]
  is_separator_column <- all(num == " ")
  if (is_separator_column) {
    values <- c(values, Reduce(op, nums))
    op <- match.fun(tmp[nrow(tmp), j + 1])
    nums <- c()
    next
  }
  num <- as.numeric(paste0(num, collapse = ""))
  nums <- c(nums, num)
}
sum(values) + Reduce(op, nums)
