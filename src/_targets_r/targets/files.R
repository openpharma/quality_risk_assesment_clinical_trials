list(
  tar_target(file_mm, "data/in/modelling_matrix.feather", format = "file"),
  tar_target(file_mm_bin, "data/in/lasso_prep.feather", format = "file"),
  tar_target(file_form, "data/in/glm_coefs.feather", format = "file"),
  tar_target(file_cv, "data/in/indeces_annual_splits.feather", format = "file"),
  tar_target(file_cat_lookup, "data/in/category_lookup.feather", format = "file"),
  tar_target(file_feat_lookup, "data/in/feature_lookup.csv", format = "file")
)
