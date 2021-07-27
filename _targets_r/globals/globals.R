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
script_files <- dir("./src/R", full.names = TRUE)
purrr::map(script_files, source)
