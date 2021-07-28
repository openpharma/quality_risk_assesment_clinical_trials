tar_target(
  df_forest,
  qract_forest_plots(
    df_cv_preds_and_coefs,
    df_feat_lookup,
    df_mm,
    df_cat_lookup,
    category_ids = c(
      "cnsn",
      "dtin",
      "sfty",
      "ptpe"
    )
  )
)
