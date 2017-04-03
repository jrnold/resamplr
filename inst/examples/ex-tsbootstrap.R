# examples from boot::tsboot
lynx_fun <- function(x) {
  tsb <- as.data.frame(x)$y
  ar.fit <- ar(tsb, order.max = 25)
  ar.fit$order
}

# the stationary bootstrap with mean block length 20
lynx_1 <- tibble(y = log(lynx)) %>%
  tsbootstrap(R = 99, size = 20, type = "geom") %>%
  mutate(order = map_dbl(sample, lynx_fun))
summary(lynx_1$order)

# the fixed block bootstrap with length 20
lynx_2 <- tibble(y = log(lynx)) %>%
  tsbootstrap(R = 99, size = 20, type = "fixed") %>%
  mutate(order = map_dbl(sample, lynx_fun))
summary(lynx_2$order)
