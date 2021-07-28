list(
  tar_target(df_mm, qract_read_and_anonymize(file_mm, config$id_vars, arrow::read_feather)),
  tar_target(df_mm_bin, qract_read_and_anonymize(file_mm_bin, config$id_vars, arrow::read_feather)),
  tar_target(df_form, qract_read_and_anonymize(file_form,  config$id_vars, arrow::read_feather)),
  tar_target(df_cv, qract_read_and_anonymize(file_cv,  config$id_vars, arrow::read_feather)),
  tar_target(df_cat_lookup, qract_read_and_anonymize(file_cat_lookup, config$id_vars, arrow::read_feather)),
  tar_target(df_feat_lookup, qract_read_and_anonymize(file_feat_lookup, config$id_vars, readr::read_csv))
)
