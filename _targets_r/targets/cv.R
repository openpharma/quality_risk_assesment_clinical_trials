list(
  tar_target(p_tscv, qract_plot_tscv(df_mm, df_cv, config$max_year)),
  tar_target(df_cv_preds_and_coefs, qract_pred_cv(df_mm_bin, df_cv, df_form, config$id_vars))
)
