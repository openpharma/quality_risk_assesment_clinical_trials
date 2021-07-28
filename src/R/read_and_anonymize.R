

qract_read_and_anonymize <- function(file, id_vars, .f) {
  df <- .f(file)

  cols_irrelevant <- id_vars[!id_vars %in% c(
    "category_id",
    "activity_id_new",
    "index",
    "start_date"
  )]

  cols_irrelevant <- cols_irrelevant[cols_irrelevant %in% colnames(df)]

  df <- df %>%
    select(-one_of(cols_irrelevant))

  if ("activity_id_new" %in% colnames(df)) {
    df <- df %>%
      mutate(
        activity_id_new = dense_rank(activity_id_new),
        activity_id_new = str_pad(activity_id_new, width = 5, pad = "0")
      )
  }

  if ("start_date" %in% colnames(df)) {
    df <- df %>%
      mutate(
        start_date = lubridate::ymd(start_date),
        start_date = lubridate::year(start_date),
        start_date = glue::glue("{start_date}-01-01"),
        start_date = lubridate::ymd(start_date)
      )
  }

  return(df)
}
