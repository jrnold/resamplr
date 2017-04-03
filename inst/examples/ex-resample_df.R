library("dplyr")
# specify subsamples by a list of indices
resample_df(mtcars, samples = c(1:5, 5:10, 15:20))
resample_df(mtcars, samples = c(1:10, 1:15, 5:30))
# with grouped data frames, indices refer to groups
resample_df(group_by(mtcars, mpg), samples = list(c(1:2)))$sample
