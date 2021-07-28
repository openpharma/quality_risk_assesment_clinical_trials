
qract_plot_tscv <- function(df_mm, df_cv, max_year = 2020) {

  # we pick one random category to remove duplicates
  df_label <- df_mm %>%
    filter(category_id == "cnsn") %>%
    mutate(
      label = "not used",
      year_start_act = lubridate::year(start_date)
    ) %>%
    select(year_start_act, activity_id_new, index, label)

  order_lvl <- c("training", "not used b/c study previously audited", "test", "not used")
  order_col <- c("dodgerblue4", "lightgrey", "burlywood3", "darkgrey")
  order_col <- c("lightgrey", "dodgerblue4", "white", "darkgrey")

  df_prep <- df_cv %>%
    filter(category_id == "cnsn") %>%
    select(year_start_act, index_past, index_next_year) %>%
    distinct() %>%
    mutate_at(vars(index_past, index_next_year), qract_str_index_2_int_index) %>%
    mutate(
      data = list(df_label),
      data = map2(
        data, index_past,
        function(x, y) mutate(x, label = ifelse(as.integer(index) %in% y, "training", label))
      ),
      data = map2(
        data, index_next_year,
        function(x, y) mutate(x, label = ifelse(as.integer(index) %in% y, "test", label))
      ),
      data = map2(
        data, year_start_act,
        function(x, y) {
          mutate(x, label = ifelse(
            year_start_act - 1 == y & label == "not used",
            "not used b/c study previously audited",
            label
          ))
        }
      )
    ) %>%
    select(year_start_act, data) %>%
    rename(year = year_start_act) %>%
    unnest(data) %>%
    mutate(label = fct_relevel(label, order_lvl))

  plot_splits <- function(df) {
    df %>%
      ggplot(aes(year_start_act, fill = label)) +
      geom_bar(color = "grey") +
      facet_wrap(~year, ncol = 4) +
      theme_minimal() +
      scale_fill_manual(values = order_col) +
      labs(
        y = "Number of Audits and Inspections", x = "",
        title = "Time Series Cross Validation",
        fill = ""
      ) +
      scale_x_continuous(breaks = c(seq(2011, max_year - 1, 1))) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        line = element_blank(),
        aspect.ratio = 1
      )
  }

  p_splits <- df_prep %>%
    filter(year < max_year - 1) %>%
    filter(year_start_act < max_year) %>%
    plot_splits()

  p_final <- df_prep %>%
    filter(year == max_year - 1) %>%
    filter(year_start_act < max_year) %>%
    mutate(year = "final models") %>%
    plot_splits() +
    labs(title = "", y = "") +
    theme(legend.position = "None")

  cowplot::plot_grid(p_splits, p_final, nrow = 1, rel_widths = c(.75, .25))
}
