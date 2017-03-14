# examples from zoo::rollapply
library("tidyverse")
library("broom")
library("lubridate")

# rolling mean
tibble(x = 11:15) %>%
  roll(width = 2) %>%
  mutate(mean = map_dbl(sample, function(w) mean(as.data.frame(w)$x)))

# non-overlapping means
z2 <- tibble(x = rnorm(6)) %>%
  roll(width = 3, by = 3)
map_dbl(z2$sample, function(w) mean(as.data.frame(w)$x))

# rolling regression:
# number of UK driver deaths and lags 1 and 12
seat <- tibble(
    y = log(UKDriverDeaths),
    date = as.Date(paste0(format(zoo::as.yearmon(time(UKDriverDeaths))),
                         " 15"), "%b %Y %d"),
    y1 = lag(y, 1),
    y12 = lag(y, 12)
  ) %>%
  drop_na()

# run a rolling regression with a 3-year time window
# (similar to a SARIMA(1,0,0)(1,0,0)_12 fitted by OLS) # nolint
fit <- function(w) lm(y ~ y1 + y12, data = as.data.frame(w))
rollreg <- seat %>%
     roll(width = 36, align = "right") %>%
     mutate(mod = map(sample, fit),
            coef = map(mod, tidy))
rollreg[["date"]] <- seat$date[rollreg$.id]

rollreg %>%
  unnest(coef) %>%
  filter(is.finite(std.error)) %>%
  ggplot(aes(x = date, y = estimate, ymin = estimate - 2 * std.error,
             ymax = estimate + 2 * std.error)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  facet_wrap(~ term, ncol = 1, scales = "free")
