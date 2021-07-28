options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "gt",
    "yardstick",
    "broom",
    "cowplot",
    "glue",
    "tidyverse",
    "arrow"
    )
  )
purrr::map(dir("./src/R", full.names = TRUE), source)
