# cut width but make sure that minimum number of observatins can be found in
# each bucket, see examples below
qract_cut_relax <- function(x, n, min_size = 100) {
  breaks <- seq(min(x, na.rm = TRUE) * 0.9, max(x, na.rm = TRUE) * 1.1, length.out = n + 1)
  test_cut <- cut(x, breaks = breaks, include.lowest = TRUE)

  df_cut <- tibble(x = x, cut = test_cut) %>%
    mutate(rwn_ori = row_number()) %>%
    arrange(x) %>%
    mutate(rwn = row_number())

  df_corr <- df_cut %>%
    group_by(cut) %>%
    mutate(n = n()) %>%
    group_by(cut, n) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      missing = ifelse(n < min_size, min_size - n, 0),
      excess = ifelse(n > min_size, n - min_size, 0),
      perc_excess = excess / sum(excess),
      n_corr = n - round(perc_excess * sum(missing), 0),
      n_corr = ifelse(n_corr < min_size, min_size, n_corr),
      cum_n_corr = cumsum(n_corr),
      cum_n = cumsum(n)
    )

  df_cut_relax <- df_cut %>%
    mutate(
      relax_cut = map_int(rwn, ~ sum(. == df_corr$cum_n_corr[1:(nrow(df_corr) - 1)])),
      relax_cut = cumsum(relax_cut)
    ) %>%
    group_by(relax_cut) %>%
    mutate(
      n = n(),
      min = min(x),
      max = max(x)
    ) %>%
    group_by(relax_cut, n, min, max) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      max_corr = lead(min),
      max_corr = ifelse(is.na(max_corr), max, max_corr),
      lvl = paste0(
        "[",
        formatC(min, format = "e", digits = 1),
        "-",
        formatC(max_corr, format = "e", digits = 1),
        "]"
      ),
      lvl = as_factor(lvl)
    ) %>%
    unnest(data)

  df_cut_relax %>%
    arrange(rwn_ori) %>%
    pull(lvl) %>%
    return()
}

# x <- rgamma(750, shape = 5)
# n = 6
# qract_cut_relax(x, n) %>%
#   summary()
#
# cut(x, n) %>%
#   summary()
#
# qract_cut_relax(max(x) - x, n) %>%
#   summary()


qract_bin <- function(df, n_bins) {

  # catch cases with very few unique prediction values
  n_unique <- df$pred_yes %>%
    unique() %>%
    length()

  if (n_unique <= 1) {
    return(
      tibble(pred_bin = NA, obs_prop = NA, n_obs_in_bin = NA, sum_obs = NA) %>%
        mutate_at(vars(obs_prop, n_obs_in_bin, sum_obs), as.numeric) %>%
        mutate(pred_bin = as.character(pred_bin))
    )
  }

  n_bins <- ifelse(n_unique > n_bins, n_bins, n_unique)

  df %>%
    mutate(pred_bin = qract_cut_relax(pred_yes, n = n_bins)) %>%
    group_by(pred_bin) %>%
    summarise(
      obs_prop = sum(obs, na.rm = TRUE) / n(),
      n_obs_in_bin = n(),
      sum_obs = sum(obs, na.rm = TRUE),
      mean_prop_bin = mean(pred_yes, na.rm = TRUE)
    )
}

qract_bin_preds <- function(df_cv_preds_and_coefs,
                            confidence_level = 0.75,
                            n_bins = 4) {
  df_cv_preds_and_coefs %>%
    mutate(pred = map(pred, "pred_valid")) %>%
    select(category_id, year_start_act, pred) %>%
    unnest(pred) %>%
    unnest(pred) %>%
    group_by(category_id) %>%
    mutate(total_base_rate = sum(obs) / n()) %>%
    group_by(category_id, total_base_rate) %>%
    nest() %>%
    mutate(data = map(data, qract_bin, n_bins)) %>%
    unnest(data) %>%
    ungroup() %>%
    filter(!is.na(pred_bin)) %>%
    mutate(
      prop_test = pmap(
        list(sum_obs, n_obs_in_bin, total_base_rate),
        prop.test,
        conf.level = confidence_level
      ),
      prop_test = map(prop_test, broom::tidy),
      p_val = map_dbl(prop_test, "p.value"),
      conf_low = map_dbl(prop_test, "conf.low"),
      conf_high = map_dbl(prop_test, "conf.high")
    ) %>%
    select(-prop_test)
}
