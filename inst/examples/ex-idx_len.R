require("tibble")
require("rlang")

# Extract number of indexes for various classes

# default (vectors)
idx_len(seq_len(10))

# matrices (rows by default)
idx_len(matrix(seq_len(20), nrow = 2))

# arrays (first dimension by default)
idx_len(array(seq_len(60), dim = c(5, 4, 3)))

# data frames (rows by default)
idx_len(tibble(foo = 1:10, bar = 1:10))

# quosures are evaluated first
x <- tibble(foo = 1:10, bar = 1:10)
q <- quo(x)
q
idx_len(q)
