# specify test and training partitions
test <- list(1:10, 10:20, 30:nrow(mtcars))
train <- purrr::map(test, setdiff, y = test)
crossv_idx(mtcars, train = train, test = test)
