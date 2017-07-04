# New lazy sample object
smpl <- LazySample$new(~ mtcars, 1:5)
# Get the indexes
smpl$idx

# Get the expression containing the data
smpl$quo

# the expression is stored as a quosure
class(smpl$quo)
# with an expression
rlang::get_expr(smpl$quo)
# and an environment
rlang::get_env(smpl$quo)

# Get the sample - it is not computed until called
smpl$sample
smpl$draw_sample()
# Get the original data
smpl$data

# Return the sample as a data frame
as.data.frame(smpl)

# LazySample objects also work with non-data frames
x <- 1:100
smpl_x <- LazySample$new(~ x, sample(length(x), 10))
smpl_x
smpl_x$sample

y <- matrix(1:300, nrow = 100)
smpl_y <- LazySample$new(~ y, sample(nrow(y), 10))
smpl_y
smpl_y$sample

# the extractor function can be customized
# for example, randomly sample the columns of a data frame
z <- as.data.frame(matrix(1:1000, nrow = 10))
smpl_z <- LazySample$new(~ z, sample(nrow(y), 10),
                         extractor = ~ .x[ , .y, drop = FALSE])
smpl_z
smpl_z$sample
