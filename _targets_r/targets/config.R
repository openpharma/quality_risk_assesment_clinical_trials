tar_target(
  config,
  list(
    # columns that are not features
    id_vars = c(
      'category_id',
      'modelling_category',
      'classification',
      'activity_id_new',
      'start_date',
      'index',
      'date_first_study_activity',
      'date_last_study_activity',
      'protocol',
      'file_name',
      'source_row_index',
      'audit_or_inspection',
      'site_platinum_id',
      'pi_platinum_id',
      'site_num',
      'country',
      'site_activation_date',
      'site_closed_date',
      'site_closed_date_corr',
      'study_start_date',
      'study_end_date'
     ),
     max_year = 2020
   )
)
