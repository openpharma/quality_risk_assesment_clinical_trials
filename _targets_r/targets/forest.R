list(
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
    ),
    tar_target(
      forest_files,
      qract_save_forest_plots(
        df_forest,
        path = "./png"
      ),
      format = "file"
    )
)
