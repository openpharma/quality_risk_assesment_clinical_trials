qract_perf <- function(df_cv_pred_and_coefs, max_year = 2020) {
  df_prep <- df_cv_pred_and_coefs %>%
    mutate(pred = map(pred, "pred_valid")) %>%
    select(category_id, year_start_act, pred) %>%
    unnest(pred) %>%
    unnest(pred)

  # suppress yardstick messages
  suppressMessages({
    df_auc <- df_prep %>%
      group_by(category_id, year_start_act) %>%
      mutate(
        is_yes = obs,
        is_no = ifelse(obs == 0, 1, 0),
        n_yes = sum(is_yes, na.rm = TRUE),
        n_no = sum(is_no, na.rm = TRUE),
        sum_pred_yes = sum(pred_yes, na.rm = TRUE)
      ) %>%
      # we need to filter out values for which auc calculation will fail
      filter(n_yes > 0, n_no > 0, sum_pred_yes > 0.001) %>%
      # yardstick requires a factor variable
      mutate(
        obs = ifelse(obs == 1, "yes", "no"),
        obs = factor(obs, levels = c("yes", "no"))
      ) %>%
      yardstick::roc_auc(obs, pred_yes)
  })
  df_brier <- df_prep %>%
    mutate(
      .metric = "brier",
      .estimator = "binary",
      .estimate = (pred_yes - obs)^2
    ) %>%
    group_by(year_start_act, category_id, .metric, .estimator) %>%
    summarise(
      .estimate = mean(.estimate),
      .groups = "drop"
    )

  df_auc %>%
    bind_rows(df_brier) %>%
    filter(year_start_act < max_year - 1) %>%
    group_by(category_id, .metric) %>%
    summarise(
      mean = mean(.estimate, na.rm = TRUE),
      sd = sd(.estimate, na.rm = TRUE),
      n = sum(!is.na(.estimate)),
      .groups = "drop"
    )
}
