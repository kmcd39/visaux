library(tidyverse)



tmp <-
  tibble(
         y = rnorm(500, mean = 1e7, sd = 5e6)
        ,x = rnorm(500, mean = 1e6, sd = 5e5)
        )

(tmp$y/1e6) %>% summary()

devtools::load_all()

tmp %>%
  filter(x > 0 & y > 0
         ) %>%
  ggplot(
    aes(x, y)
  ) +

  geom_point() +

  scale_x_log10(
     breaks =  ~breaks_log10(.x, c(1,3, 5))
    ,minor_breaks  =  minor.breaks_log10
    ,labels = ggplot2::waiver()
  ) +

  scale_y_log10(
     breaks = ~breaks_log10(.x, c(1, 3, 5))
    ,minor_breaks = minor.breaks_log10
    ,labels = ggplot2::waiver()
  )
