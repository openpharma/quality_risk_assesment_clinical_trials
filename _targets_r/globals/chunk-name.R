options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("dplyr", "ggplot2", "readr", "tidyr"))
create_plot <- function(data) {
  ggplot(data) +
    geom_histogram(aes(x = Ozone), bins = 12) +
    theme_gray(24)
}
