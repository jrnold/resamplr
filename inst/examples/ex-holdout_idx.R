library("dplyr")

# test-training pairs can be specified by test sets
holdout_idx(mtcars, train = list(1:5, 6:10))
# or by training sets
holdout_idx(mtcars, train = list(1:20, 10:32))
# or by both (and they don't need to partition the observations)
holdout_idx(mtcars,
          train = list(1:20, 10:32),
          test = list(21:32, 1:9))

# with grouped data frames, the indices refer to groups
holdout_idx(group_by(mtcars, mpg), train = list(1L))$train
