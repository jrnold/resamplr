require("purrr")

# lazy_sample is a convenience function to create lazy_sample objects
lazy_sample(mtcars, 1:5)

# lazy_sample_lst creates a list of lazy_sample objects
lazy_sample_idx(mtcars, rerun(3, nrow(mtcars)))

# lazy_sample_idx creates a data frame with a list column
# of lazy_sample objects
lazy_sample_idx(mtcars, rerun(3, nrow(mtcars)))
