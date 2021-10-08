list(
  tar_target(df_perf, qract_perf(df_cv_preds_and_coefs, config$max_year)),
  tar_target(df_calib, qract_lin_calib(df_cv_preds_and_coefs, min_sample_size = 200)),
  tar_target(df_bin, qract_bin_preds(df_cv_preds_and_coefs, n_bins = 4, confidence_level = .75)),
  tar_target(
    p_calib,
    qract_plot_calibration_pub(
      category_id_str = df_cv_preds_and_coefs$category_id %>% unique(),
      df_bin,
      df_calib,
      df_cat_lookup,
      uniform_color = "black",
      color_calib = "grey"
      )
  )
)
