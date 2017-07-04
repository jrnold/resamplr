require("tibble")
require("rlang")

# Extract indexes from vectors
extract_sample(1:100, 1:10)

# For arrays: by default, extract from the first dimension
extract_sample(array(seq_len(60), dim = c(5, 4, 3)), 1:3)

# For matrices: by default, extract rows
extract_sample(array(seq_len(20), dim = c(10, 2)), 1:5)

# For data frames: by default, extract rows
extract_sample(tibble(foo = 1:10, bar = 1:10), 1:5)

# For quosures: by default, evaluate first
x <- tibble(foo = 1:10, bar = 1:10)
q <- quo(x)
q
extract_sample(q, 1:5)
